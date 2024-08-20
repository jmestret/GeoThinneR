#' Perform Brute Force Thinning
#'
#' This function applies a brute force algorithm to thin a set of spatial coordinates, attempting to maximize the number of points retained while ensuring a minimum distance (`thin_dist`) between any two points.
#'
#' @param coordinates A numeric matrix or data frame with two columns representing longitude and latitude (or XY coordinates if `euclidean = TRUE`).
#' @param thin_dist Numeric value representing the thinning distance in kilometers (default: 10 km).
#' @param trials Integer specifying the number of trials to run for thinning (default: 10).
#' @param all_trials Logical value indicating whether to return the results of all trials (`TRUE`) or just the best attempt with the most points retained (`FALSE`, default).
#' @param target_points Optional integer specifying the number of points to retain. If `NULL` (default), the function tries to maximize the number of points retained.
#' @param euclidean Logical value indicating whether to compute the Euclidean distance (`TRUE`) or Haversine distance (`FALSE`, default).
#' @param R Numeric value representing the Earth's radius in kilometers (default: 6371 km). Only used if `euclidean = FALSE`.
#' @return A logical vector indicating which points are kept in the best trial if `all_trials = FALSE`; otherwise, a list of logical vectors for each trial.
#' @examples
#' # Example with geographic coordinates (Haversine distance)
#' coords <- data.frame(
#'   long = c(-122.4194, -122.4195, -122.4196),
#'   lat = c(37.7749, 37.7740, 37.7741)
#' )
#' coords <- as.matrix(coords)
#'
#' result <- brute_force_thinning(coords, thin_dist = 0.1, trials = 5)
#' print(result)
#'
#' # Example computing Euclidean distance
#' result_euclidean <- brute_force_thinning(coords, thin_dist = 1, trials = 5, euclidean = TRUE)
#' print(result_euclidean)
#' @export
brute_force_thinning <- function(coordinates, thin_dist = 10, trials = 10, all_trials = FALSE, target_points = NULL, euclidean = FALSE, R = 6371) {

  # Compute distance matrix
  if (euclidean){
    total_dist_mat <- as.matrix(stats::dist(coordinates, upper = TRUE))
  } else {
    total_dist_mat <- fields::RdistEarth(x1 = coordinates, miles = FALSE, R = R)
  }
  n <- nrow(coordinates)  # Number of points

  if (is.null(target_points)){ # Try to maximize keeped points
    diag(total_dist_mat) <- thin_dist + 1 # To avoid counting self-matches
    neighbor_indices <- vector("list", n)
    for (i in seq_len(n)) {
      neighbor_indices[[i]] <- which(total_dist_mat[i, ] < thin_dist)
    }

    # Run thinning algorithm to keep as max points as possible
    keep_points <- max_thinning_algorithm(neighbor_indices, n, trials, all_trials)
  } else { # Try to select the exact number of points as much separated as possible
    diag(total_dist_mat) <- NA  # Ignore self-distances

    # Initialize results list for trials
    keep_points <- vector("list", ifelse(all_trials, trials, 1))
    keep_points[[1]] <- rep(FALSE, n)  # Start with no points kept

    for (i in seq_len(trials)) {
      keep_points_trial <- rep(FALSE, n)
      points_to_keep <- as.integer(stats::runif(1, 1, n)) # Randomly select the first point

      while (length(points_to_keep) < target_points) {
        dist_to_closest_kept <- matrixStats::rowMins(total_dist_mat[, points_to_keep, drop = FALSE], na.rm = TRUE)
        dist_to_closest_kept[points_to_keep] <- 0

        max_dist_closest <- max(dist_to_closest_kept, na.rm = TRUE)
        if (max_dist_closest < thin_dist) break  # Stop if no points are further than thin_dist

        furthest <- which(dist_to_closest_kept == max_dist_closest)
        if (length(furthest) > 1) {
          furthest <- sample(furthest, 1)  # Randomly select one if multiple furthest points
        }

        points_to_keep <- c(points_to_keep, furthest)
      }

      keep_points_trial[points_to_keep] <- TRUE

      if (all_trials) {
        keep_points[[i]] <- keep_points_trial
      } else {
        if (length(points_to_keep) == target_points) {
          keep_points[[1]] <- keep_points_trial
          break  # Exit if target points are reached
        } else if (sum(keep_points_trial) > sum(keep_points[[1]])) {
          keep_points[[1]] <- keep_points_trial  # Update if this trial kept more points
        }
      }
    }
  }

  # Return list of trials or list with best trial in first position
  return(keep_points)
}
