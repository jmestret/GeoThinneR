#' Precision Thinning of Spatial Points
#'
#' This function performs thinning of spatial points by rounding their coordinates to a specified precision and removing duplicates.
#' It can perform multiple trials of this process and return the results for all or just the best trial.
#'
#' @param coordinates A numeric matrix or data frame with two columns representing the longitude and latitude of points.
#' @param precision An integer specifying the number of decimal places to which coordinates should be rounded. Default is 4.
#' @param trials An integer specifying the number of thinning trials to perform. Default is 10.
#' @param all_trials A logical value indicating whether to return results for all trials (`TRUE`) or just the first/best trial (`FALSE`). Default is `FALSE`.
#' @param priority A of the same length as the number of points with numerical values indicating the priority of each point. Instead of eliminating points randomly, the points are preferred accoridng to these values.
#' @return If `all_trials` is `FALSE`, returns a logical vector indicating which points were kept in the first trial.
#' If `all_trials` is `TRUE`, returns a list of logical vectors, one for each trial.
#'
#' @details
#' The function performs multiple trials to account for randomness in the order of point selection. By default, it returns the first trial,
#' but setting `all_trials = TRUE` will return the results of all trials.
#'
#' @examples
#' # Example usage
#' coordinates <- matrix(c(-123.3656, 48.4284, -123.3657, 48.4285, -123.3658, 48.4286), ncol = 2)
#' result <- precision_thinning(coordinates, precision = 3, trials = 5, all_trials = TRUE)
#' print(result)
#'
#' # Example with a single trial and lower precision
#' result_single <- precision_thinning(coordinates, precision = 2, trials = 1, all_trials = FALSE)
#' print(result_single)
#'
#' @export
precision_thinning <- function(coordinates, precision = 4, trials = 10, all_trials = FALSE, priority = NULL) {
  # Validate inputs
  if (!is.numeric(precision) || length(precision) != 1 || precision < 0) {
    stop("`precision` must be a non-negative integer.")
  }

  if (!is.null(priority)){
    if (!is.numeric(priority) | length(priority) != nrow(coordinates)){
      stop("'priority' must be a numeric vector with same length as number of points.")
    }
  }

  # Initialize results list for trials
  keep_points <- vector("list", ifelse(all_trials, trials, 1))

  # Main thinning loop for the specified number of trials
  for (i in seq_len(length(keep_points))) {
    # Create a data.table with rounded coordinates and a random order or by priority
    if (is.null(priority)){
      sort_order <- stats::runif(nrow(coordinates))
    } else {
      sort_order <- priority
    }
    keep_points_trial <- data.table::data.table(
      id = seq_len(nrow(coordinates)),
      long = round(coordinates[, 1], precision),
      lat = round(coordinates[, 2], precision),
      rand_order = sort_order
    )

    # Randomize the order to avoid bias
    keep_points_trial <- keep_points_trial[order(keep_points_trial[["rand_order"]], decreasing = TRUE), ]

    # Mark unique points based on rounded coordinates
    keep_points_trial[, "keep"] <- !duplicated(keep_points_trial[, c("long", "lat")])

    # Sort back to the original order and store the "keep" vector
    keep_points[[i]] <- keep_points_trial[order(keep_points_trial[["id"]]), ][["keep"]]
}

  return(keep_points)
}
