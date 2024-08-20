#' Perform Grid-Based Thinning of Spatial Points
#'
#' This function performs thinning of spatial points by assigning them to grid cells based on a specified resolution or thinning distance. It can either create a new raster grid or use an existing raster object.
#'
#' @param coordinates A numeric matrix or data frame with two columns representing the x (longitude) and y (latitude) coordinates of the points.
#' @param thin_dist A numeric value representing the thinning distance in kilometers. It will be converted to degrees if `resolution` is not provided.
#' @param resolution A numeric value representing the resolution (in degrees) of the raster grid. If provided, this takes priority over `thin_dist`.
#' @param origin A numeric vector of length 2 (for example, `c(0, 0)`), specifying the origin of the raster grid (optional).
#' @param raster_obj An optional `terra` SpatRaster object to use for grid thinning. If provided, the raster object will be used instead of creating a new one.
#' @param trials An integer specifying the number of trials to perform for thinning (default: 10).
#' @param all_trials A logical value indicating whether to return results for all trials (`TRUE`) or just the first trial (`FALSE`, default).
#' @param crs An optional CRS (Coordinate Reference System) to project the coordinates and raster (default WGS84). This can be an EPSG code, a PROJ.4 string, or a `terra::crs` object.
#' @param priority A of the same length as the number of points with numerical values indicating the priority of each point. Instead of eliminating points randomly, the points are preferred according to these values.
#' @return A list of logical vectors indicating which points to keep for each trial.
#' @examples
#' # Example: Grid thinning using thin_dist
#' coordinates <- matrix(c(-122.4194, 37.7749,
#'                         -122.4195, 37.7740,
#'                         -122.4196, 37.7741), ncol = 2, byrow = TRUE)
#'
#' result <- grid_thinning(coordinates, thin_dist = 10, trials = 5, all_trials = TRUE)
#' print(result)
#'
#' # Example: Grid thinning using a custom resolution
#' result_res <- grid_thinning(coordinates, resolution = 0.01, trials = 5)
#' print(result_res)
#'
#' # Example: Using a custom raster object
#' library(terra)
#' rast_obj <- terra::rast(nrows = 100, ncols = 100, xmin = -123, xmax = -121, ymin = 36, ymax = 38)
#' result_raster <- grid_thinning(coordinates, raster_obj = rast_obj, trials = 5)
#' print(result_raster)
#'
#' @export
grid_thinning <- function(coordinates, thin_dist = NULL, resolution = NULL, origin = NULL, raster_obj = NULL, trials = 10, all_trials = FALSE, crs = "epsg:4326", priority = NULL) {

  # Validate input
  if (is.null(thin_dist) && is.null(resolution) && is.null(raster_obj)) {
    stop("Either thin_dist, resolution, or raster_obj must be provided.")
  }

  if (!is.null(priority)){
    if (!is.numeric(priority) | length(priority) != nrow(coordinates)){
      stop("'priority' must be a numeric vector with same length as number of points.")
    }
  }

  # Calculate resolution if thin_dist is provided and resolution is NULL
  if (is.null(resolution) & !is.null(thin_dist)) {
    resolution <- thin_dist / 111.32  # Convert km to degrees
  }

  # Create or use provided raster
  if (is.null(raster_obj)) {
    r_grid <- terra::rast(
      xmin = min(coordinates[, 1]) - resolution,
      xmax = max(coordinates[, 1]) + resolution,
      ymin = min(coordinates[, 2]) - resolution,
      ymax = max(coordinates[, 2]) + resolution,
      resolution = resolution,
      crs = crs
    )
    if (!is.null(origin)) {
      terra::origin(r_grid) <- origin
    }
  } else {
    r_grid <- raster_obj
  }

  # Assign points to raster cells
  grid_cell <- terra::cellFromXY(r_grid, coordinates)
  unique_grid_cell <- unique(stats::na.omit(grid_cell))

  # Initialize results list for trials
  keep_points <- vector("list", ifelse(all_trials, trials, 1))

  # Main thinning loop for the specified number of trials
  for (i in seq_len(length(keep_points))) {
    if (is.null(priority)){
      sort_order <- stats::runif(nrow(coordinates))
    } else {
      sort_order <- priority
    }
    keep_points_trial <- data.table::data.table(
      id = seq_len(nrow(coordinates)),
      rand_order = sort_order,
      grid_cell = grid_cell
    )

    keep_points_trial <- keep_points_trial[order(keep_points_trial[["rand_order"]], decreasing = TRUE), ]
    keep_points_trial[, "keep"] <- !duplicated(keep_points_trial[["grid_cell"]])

    # If not saving all trials, keep only the first trial result
    keep_points[[i]] <- keep_points_trial[order(keep_points_trial[["id"]]),][["keep"]]
  }

  return(keep_points)
}
