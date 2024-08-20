#' Spatial Thinning of Points
#'
#' This function performs spatial thinning of geographic points to reduce
#' point density while maintaining spatial representation. Points are thinned
#' based on a specified distance, grid, or precision, and multiple trials can be
#' performed to identify the best thinned dataset.
#'
#' @param data A data frame or tibble containing the points to thin. Must contain longitude and latitude columns.
#' @param long_col Name of the column with longitude coordinates (default: "decimalLongitude").
#' @param lat_col Name of the column with latitude coordinates (default: "decimalLatitude").
#' @param group_col Name of the column for grouping points (e.g., species name, year). If NULL, no grouping is applied.
#' @param method Thinning method to use `c("brute_force", "kd_tree", "r_tree", "round_hash", "grid", "precision")`.
#' @param trials Number of thinning iterations to perform (default: 10).
#' @param all_trials If TRUE, returns results of all attempts; if FALSE, returns the best attempt with the most points retained (default: FALSE).
#' @param target_points Optional; a numeric value specifying the exact number of points to keep. If NULL (default), maximizes the number of kept points.
#' @param seed Optional; an integer seed for reproducibility of results.
#' @param verbose If TRUE, prints progress messages (default: FALSE).
#' @param ... Additional parameters passed to specific thinning methods (e.g., thin_dist, precision, resolution, origin, R).
#'
#' @return A tibble of thinned points, or a combined result of all attempts if `all_trials` is TRUE.
#'
#' @details
#' The thinning methods available are:
#' - `brute_force`: Uses a brute force approach to thin points.
#' - `kd_tree`: Uses K-D trees for thinning.
#' - `r_tree`: Uses R-trees for thinning.
#' - `round_hash`: Uses rounding and hashing for efficient thinning.
#' - `grid`: Applies a grid-based thinning method.
#' - `precision`: Utilizes precision-based thinning.
#'
#' For more information on specific thinning methods and inputs, refer to their respective documentation:
#' - `brute_force_thinning()`
#' - `grid_thinning()`
#' - `kd_tree_thinning()`
#' - `r_tree_thinning()`
#' - `rounding_hashing_thinning()`
#' - `precision_thinning()`
#'
#' @examples
#' # Generate sample data
#' set.seed(123)
#' sample_data <- data.frame(
#'   decimalLongitude = runif(100, -180, 180),
#'   decimalLatitude = runif(100, -90, 90)
#' )
#'
#' # Perform thinning using K-D tree method
#' thinned_data <- thin_points(sample_data,
#'                              long_col = "decimalLongitude",
#'                              lat_col = "decimalLatitude",
#'                              method = "kd_tree",
#'                              trials = 5,
#'                              verbose = TRUE)
#'
#' # Perform thinning with grouping
#' sample_data$species <- sample(c("species_A", "species_B"), 100, replace = TRUE)
#' thinned_grouped_data <- thin_points(sample_data,
#'                                      long_col = "decimalLongitude",
#'                                      lat_col = "decimalLatitude",
#'                                      group_col = "species",
#'                                      method = "kd_tree",
#'                                      trials = 10)
#'
#' @export
thin_points <- function(data, long_col = NULL, lat_col = NULL, group_col = NULL,
                        method = c("brute_force", "kd_tree", "r_tree", "round_hash", "grid", "precision"),
                        trials = 10, all_trials = FALSE,
                        target_points = NULL, seed = NULL, verbose = FALSE, ...) {

  # Match the method argument
  method <- match.arg(method)

  if (!is.numeric(trials) || length(trials) != 1 || trials <= 0) {
    stop("`trials` must be a positive integer.")
  }

  if (!is.logical(all_trials) || length(all_trials) != 1) {
    stop("`all_trials` must be a logical value.")
  }

  # Set seed for reproducibility
  set.seed(seed)

  # Start time tracking
  start_time <- Sys.time()
  if (verbose) cat("Starting spatial thinning at", format(Sys.time()), "\n")

  # Convert data to data frame if it's a tibble or matrix
  is_tbl <- inherits(data, "tbl") | inherits(data, "tbl_df")
  is_mat <- inherits(data, "matrix")
  if (is_tbl | is_mat) {
    data <- as.data.frame(data)
  }

  # Identify longitude and latitude columns
  if (is.null(long_col) || is.null(lat_col)) {
    # Check if columns 1 and 2 are numeric
    if (!is.numeric(data[, 1]) || !is.numeric(data[, 2])) {
      stop("Error: The first two columns must be numeric. Please specify long_col and lat_col arguments.")
    }
    # Rename the columns
    long_col <- "long"
    lat_col <- "lat"
    colnames(data)[1:2] <- c(long_col, lat_col)
  }

  # Validate column names
  if (!long_col %in% names(data) || !lat_col %in% names(data)) {
    stop("Specified longitude or latitude columns do not exist in the data.")
  }
  if (!is.null(group_col) && !(group_col %in% names(data))) {
    stop("Specified grouping column does not exist in the data.")
  }

  # Prepare output trials based on all_trials
  exported_trials <- ifelse(all_trials, trials, 1)
  thinned_data <- vector("list", exported_trials)

  # Function to perform thinning
  perform_thinning <- function(data_subset) {
    coordinates <- as.matrix(data_subset[, c(long_col, lat_col), drop = FALSE])
    if (verbose) cat("Starting thinning process using method:", method, "\n")

    # Thinning based on the selected method
    keeped_points <- switch(
      method,
      "brute_force"    = brute_force_thinning(coordinates, trials = trials, all_trials = all_trials, ...),
      "kd_tree"        = kd_tree_thinning(coordinates, trials = trials, all_trials = all_trials, ...),
      "r_tree"         = r_tree_thinning(coordinates, trials = trials, all_trials = all_trials, ...),
      "round_hash"     = rounding_hashing_thinning(coordinates,  trials = trials, all_trials = all_trials, seed = seed, ...),
      "grid"      = grid_thinning(coordinates, trials = trials, all_trials = all_trials, ...),
      "precision" = precision_thinning(coordinates, trials = trials, all_trials = all_trials, ...),
      stop("Invalid method specified. Please choose a valid thinning method.")
    )

    if (verbose) cat("Thinning process completed.\n")
    return(keeped_points)
  }

  # Thinning process
  # If group_col is provided, split the data by group
  if (!is.null(group_col)) {
    unique_groups <- unique(data[[group_col]])
    for (group in unique_groups) {
      if (verbose) cat("Processing group:", group, "\n")
      group_data <- data[data[[group_col]] == group, ]

      # Thinning based on target_points or not
      keeped_points <- if (is.null(target_points)) {
        perform_thinning(group_data)
      } else {
        message("For specific target points, brute force method is used.")
        brute_force_thinning(as.matrix(group_data[, c(long_col, lat_col), drop = FALSE]), trials = trials, all_trials = all_trials, target_points = target_points, ...)
      }

      for (i in seq_len(exported_trials)) {
        thinned_data[[i]] <- rbind(thinned_data[[i]], group_data[keeped_points[[i]], , drop = FALSE])
      }
    }
  } else {
    keeped_points <- if (is.null(target_points)) {
      perform_thinning(data)
    } else {
      message("For specific target points, brute force method is used.")
      brute_force_thinning(as.matrix(data[, c(long_col, lat_col), drop = FALSE]), trials = trials, all_trials = all_trials, target_points = target_points, ...)
    }
    for (i in seq_len(exported_trials)) {
      thinned_data[[i]] <- data[keeped_points[[i]], , drop = FALSE]
    }
  }

  # End time tracking and calculate duration
  execution_time <- Sys.time() - start_time
  if (verbose) cat("Total execution time:", round(execution_time, 2), "seconds\n")

  # Convert thinned data back to tibble if the original data was a tibble
  if (is_tbl) {
    thinned_data <- lapply(thinned_data, tibble::as_tibble)
  } else if (is_mat){
    thinned_data <- lapply(thinned_data, as.matrix)
  }

  return(thinned_data)
}
