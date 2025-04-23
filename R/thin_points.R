#' Spatial Thinning of Points
#'
#' This function performs spatial thinning of geographic points to reduce
#' point density while maintaining spatial representation. Points are thinned
#' based on a specified distance, grid, or decimal precision, with support for
#' multiple trials and optional grouping.
#'
#' @param data A data frame or tibble containing the input points to thin. Must contain longitude and latitude columns.
#' @param lon_col Character name of the column with longitude coordinates (default: `"lon"`).
#' @param lat_col Character name of the column with latitude coordinates (default: `"lat"`).
#' @param group_col Character name of the column for grouping points (e.g., species name, year). If `NULL`, no grouping is applied.
#' @param method Thinning method to use. One of `"distance", "grid", "precision"`.
#' @param trials Number of thinning iterations to perform (default: `10`). Must be a positive integer.
#' @param all_trials If `TRUE`, returns results of all attempts; if `FALSE`, returns the best attempt with the most points retained (default: `FALSE`).
#' @param seed Optional; an integer seed for reproducibility of results.
#' @param verbose If `TRUE`, prints progress messages (default: `FALSE`).
#' @param ... Additional arguments passed to specific thinning methods. See Details.
#'
#' @return A `GeoThinned` object (S3 class), which contains:
#' \itemize{
#'   \item `retained`: A list of logical vectors (one per trial) indicating retained points.
#'   \item `original_data`: The original input dataset.
#'   \item `method`: The thinning method used.
#'   \item `params`: A list of the thinning parameters used.
#' }
#'
#' @details
#' The following thinning methods are available:
#'
#' \describe{
#'  \item{`"distance"`}{Forces a specific minimum distance between points.}
#'  \item{`"grid"`}{Applies a grid-based thinning method.}
#'  \item{`"precision"`}{Utilizes precision-based thinning.}
#' }
#'
#' \strong{Distance-based thinning}
#'
#' The specific parameters for distance-based thinning are:
#'
#' \describe{
#'  \item{`thin_dist`}{A positive numeric value representing the thinning distance in kilometers.}
#'  \item{`search_type`}{A character string indicating the neighbor search method 'c("local_kd_tree", "k_estimation", "kd_tree", "brute")'. The defult value is 'local_kd_tree'.}
#'  \item{`distance`}{Distance metric to use 'c("haversine", "euclidean")'. Default is Haversine for geographic coordinates.}
#'  \item{`R`}{The radius of the Earth in kilometers. Default is 6371 km.}
#'  \item{`target_points`}{Optional integer specifying the number of points to retain. If 'NULL' (default), the function tries to maximize the number of points retained.}
#'  \item{`n_cores`}{Number of cores for parallel processing (only for '"local_kd_tree"'). Default is 1.}
#' }
#'
#' \strong{Grid-based thinning}
#'
#' The specific parameters for grid-based thinning are:
#'
#' \describe{
#'  \item{`thin_dist`}{A positive numeric value representing the thinning distance in kilometers.}
#'  \item{`resolution`}{A numeric value representing the resolution (in degrees) of the raster grid. If provided, this takes priority over 'thin_dist'.}
#'  \item{`origin`}{A numeric vector of length 2 (e.g., 'c(0, 0)'), specifying the origin of the raster grid (optional).}
#'  \item{`raster_obj`}{An optional 'terra::SpatRaster' object to use for grid thinning. If provided, the raster object will be used instead of creating a new one.}
#'  \item{`n`}{A positive integer specifying the maximum number of points to retain per grid cell (default: 1).}
#'  \item{`crs`}{An optional CRS (Coordinate Reference System) to project the coordinates and raster (default WGS84, 'epsg:4326'). This can be an EPSG code, a PROJ.4 string, or a 'terra::crs' object.}
#'  \item{`priority`}{A numeric vector of the same length as the number of points with numerical values indicating the priority of each point. Instead of eliminating points randomly, higher values are preferred during thinning.}
#' }
#'
#' \strong{Precision-based thinning}
#'
#' The specific parameters for precision-based thinning are:
#'
#' \describe{
#'  \item{`precision`}{A positive integer specifying the number of decimal places to which coordinates should be rounded. Default is 4.}
#'  \item{`priority`}{A numeric vector of the same length as the number of points with numerical values indicating the priority of each point. Instead of eliminating points randomly, higher values are preferred during thinning.}
#' }
#'
#' For more information on specific thinning methods and inputs, refer to their respective documentation:
#'
#' \itemize{
#'  \item `distance_thinning()`
#'  \item `grid_thinning()`
#'  \item `precision_thinning()`
#' }
#'
#' @examples
#' # Basic usage
#' set.seed(123)
#' sample_data <- data.frame(
#'   lon = runif(100, -10, 10),
#'   lat = runif(100, -5, 5)
#' )
#'
#' result <- thin_points(sample_data, method = "distance", thin_dist = 100)
#'
#' # Grouped thinning
#' sample_data$species <- sample(c("A", "B"), 100, replace = TRUE)
#' grouped_result <- thin_points(sample_data, group_col = "species",
#'                               method = "distance", thin_dist = 100)
#'
#' @export
thin_points <- function(data, lon_col = "lon", lat_col = "lat", group_col = NULL,
                        method = c("distance", "grid", "precision"),
                        trials = 10, all_trials = FALSE,
                        seed = NULL, verbose = FALSE, ...) {

  # Match the method argument
  method <- match.arg(method)

  # Validate inputs
  if (!is.numeric(trials) || length(trials) != 1 || trials <= 0) {
    stop("`trials` must be a positive integer.")
  }
  if (!is.logical(all_trials) || length(all_trials) != 1) {
    stop("`all_trials` must be a logical value.")
  }

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Check input data and columns
  # Identify longitude and latitude columns
  if (is.null(lon_col) || is.null(lat_col)) {
    # Check if columns 1 and 2 are numeric
    if (!is.numeric(data[, 1]) || !is.numeric(data[, 2])) {
      stop("Error: The first two columns must be numeric. Please specify lon_col and lat_col arguments.")
    }
    # Rename the columns
    lon_col <- "lon"
    lat_col <- "lat"
    colnames(data)[1:2] <- c(lon_col, lat_col)
  }
  if (!lon_col %in% colnames(data) || !lat_col %in% colnames(data)) {
    stop("Specified longitude or latitude columns do not exist in the data.")
  }
  if (!is.null(group_col) && !(group_col %in% colnames(data))) {
    stop("Specified grouping column does not exist in the data.")
  }

  # Start time tracking
  start_time <- Sys.time()
  if (verbose) cat("Starting spatial thinning at", format(Sys.time()), "\n")

  # Convert data to data frame if it's a tibble or matrix
  is_tbl <- inherits(data, "tbl") | inherits(data, "tbl_df")
  is_mat <- inherits(data, "matrix")
  if (is_tbl | is_mat) {
    data <- as.data.frame(data)
  }

  # Initialize results container
  exported_trials <- ifelse(all_trials, trials, 1)

  # Function to perform thinning
  perform_thinning <- function(data_subset) {
    coordinates <- as.matrix(data_subset[, c(lon_col, lat_col), drop = FALSE])
    if (verbose) cat("Thinning using method:", method, "\n")

    # Call the appropriate thinning method
    kept_points <- switch(
      method,
      "distance"  = distance_thinning(coordinates, trials = trials, all_trials = all_trials, ...),
      "grid"      = grid_thinning(coordinates, trials = trials, all_trials = all_trials, ...),
      "precision" = precision_thinning(coordinates, trials = trials, all_trials = all_trials, ...),
      stop("Invalid thinning method specified.")
    )

    if (verbose) cat("Thinning process completed.\n")
    return(kept_points)
  }

  # Thinning process
  # If group_col is provided, split the data by group
  if (!is.null(group_col)) {
    unique_groups <- unique(data[[group_col]])

    # Preallocate a list of logical vectors for each trial
    kept_points <- vector("list", exported_trials)
    for (i in seq_len(exported_trials)) {
      kept_points[[i]] <- rep(FALSE, nrow(data))
    }

    for (group in unique_groups) {
      if (verbose) cat("Processing group:", group, "\n")
      group_indices <- which(data[[group_col]] == group)
      group_data <- data[group_indices, , drop = FALSE]

      group_kept <- perform_thinning(group_data)

      for (i in seq_len(exported_trials)) {
        kept_points[[i]][group_indices] <- group_kept[[i]]
      }
    }
  } else {
    kept_points <- perform_thinning(data)
  }

  # End time tracking and calculate duration
  execution_time <- Sys.time() - start_time
  if (verbose) cat("Total execution time:", round(execution_time, 2), "seconds\n")

  all_params <- list(...)
  all_params$lon_col <- lon_col
  all_params$lat_col <- lat_col
  all_params$group_col <- group_col
  all_params$trials <- trials
  all_params$all_trials <- all_trials
  all_params$seed <- seed

  result <- as_GeoThinned(
    retained = kept_points[seq_len(exported_trials)],
    method = method,
    params = all_params,
    original_data = data
  )

  return(result)
}
