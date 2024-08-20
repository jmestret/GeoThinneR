#' Rounding Hashing Thinning
#'
#' Performs thinning of geographical coordinates using a hashing approach and rounds the coordinates to create a grid.
#'
#' This function applies a hashing technique to group coordinates into grid cells, allowing for efficient thinning based on a specified distance.
#' It can run multiple trials to determine the best set of points to keep, or return all trials if specified.
#'
#' @param coordinates A numeric matrix of size (n x 2) containing the longitude and latitude of points, where each row represents a coordinate pair.
#' @param thin_dist A numeric value specifying the distance (in kilometers) within which points should be considered for thinning.
#' @param trials An integer indicating the number of trials to run for the thinning process. More trials may yield better results.
#' @param all_trials A logical value indicating whether to return all trials (`TRUE`) or only the best trial (`FALSE`).
#' @param euclidean Logical value indicating whether to compute the Euclidean distance (`TRUE`) or Haversine distance (`FALSE`, default).
#' @param R A numeric value representing the radius of the Earth in kilometers. Default is set to 6371.0 km.
#' @param seed Optional; an integer seed for reproducibility of results.
#' @return A logical vector indicating which points are kept after the thinning process. If `all_trials` is `TRUE`, a list of logical vectors will be returned, one for each trial.
#' @examples
#' # Generate random coordinates
#' set.seed(123)
#' coordinates <- matrix(runif(20, min = -180, max = 180), ncol = 2) # 10 random points
#'
#' # Perform rounding hashing thinning
#' result <- rounding_hashing_thinning(coordinates, thin_dist = 10, trials = 5)
#' print(result)
#'
#' # Perform thinning with all trials
#' all_results <- rounding_hashing_thinning(coordinates, thin_dist = 5000, trials = 5,
#'                                          all_trials = TRUE)
#' print(all_results)
#'
#' # Perform thinning with euclidean distance
#' result_euclidean <- rounding_hashing_thinning(coordinates, thin_dist = 10,
#'                                               trials = 5, euclidean = TRUE)
#' print(result_euclidean)
#' @export
rounding_hashing_thinning <- function(coordinates, thin_dist = 10, trials = 10, all_trials = FALSE, euclidean = FALSE, R = 6371.0, seed = NULL) {
  # Input validation
  if (!is.numeric(thin_dist) || thin_dist <= 0) {
    stop("`thin_dist` must be a positive number.")
  }

  # Handle NULL seed
  if (is.null(seed)) {
    seed <- sample(.Random.seed, 1)  # Generate a random seed
  }

  # Define distance metric
  if (euclidean){
    distance_metric <- "euclidean"
  } else {
    distance_metric <- "haversine"
  }
  .Call('_GeoThinneR_rounding_hashing_thinning', PACKAGE = 'GeoThinneR', coordinates, thin_dist, trials, all_trials, distance_metric, R, seed)
}
