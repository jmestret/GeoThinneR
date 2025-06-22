#' Thinning Algorithm for Spatial Data
#'
#' This function performs the core thinning algorithm used to reduce the density of points
#' in spatial data while maintaining spatial representation. It iteratively removes the
#' points with the most neighbors until no points with neighbors remain. The algorithm
#' supports multiple trials to find the optimal thinning solution.
#'
#' @param neighbor_indices A list of integer vectors where each element contains the indices of the neighboring points for each point in the dataset.
#' @param trials A positive integer specifying the number of thinning trials to perform. Default is 10.
#' @param all_trials A logical value indicating whether to return results of all attempts (`TRUE`) or only the best attempt with the most points retained (`FALSE`). Default is `FALSE`.
#' @param priority A numeric vector of the same length as the number of points, specifying a priority weight for each point. Higher values indicate higher importance and are favored when selecting which points to retain. Priority is used to guide selection when multiple candidate points are otherwise equally valid (e.g., points in the same grid cell, with the same rounded coordinates, or with the same number of neighbors).
#'
#' @return A list of logical vectors indicating which points are kept in each trial if all_trials is TRUE; otherwise, a list with a single logical vector indicating the points kept in the best trial.
#'
#' @examples
#' # Example usage within a larger thinning function
#' neighbor_indices <- list(c(2, 3), c(1, 3), c(1, 2))
#' trials <- 5
#' all_trials <- FALSE
#' kept_points <- max_thinning_algorithm(neighbor_indices, trials, all_trials)
#' print(kept_points)
#'
#' @export
max_thinning_algorithm <- function(neighbor_indices, trials, all_trials = FALSE, priority = NULL) {
  # Compute initial neighbor counts
  n <- length(neighbor_indices)
  neighbor_counts <- lengths(neighbor_indices)

  # Initialize results list for trials
  kept_points <- vector("list", ifelse(all_trials, trials, 1))
  kept_points[[1]] <- rep(FALSE, n)  # Start with no points kept

  # Main thinning loop for the specified number of trials
  for (i in seq_len(trials)) {
    kept_points_trial <- rep(TRUE, n)
    neighbor_counts_trial <- neighbor_counts
    max_neighbors <- max(neighbor_counts_trial)

    while (max_neighbors > 0) {  # Exit loop if no neighbors remain
      # Find indices of points with the maximum neighbors
      points_to_remove <- which(neighbor_counts_trial == max_neighbors)
      if (length(points_to_remove) > 1) {
        if (!is.null(priority)) {
          points_to_remove <- points_to_remove[sample(which(priority[points_to_remove] == min(priority[points_to_remove])), 1)]
        } else {
          points_to_remove <- points_to_remove[as.integer(ceiling(stats::runif(1, 0, length(points_to_remove))))]
        }
      }

      # Recompute neighbor counts for remaining points
      neighbor_counts_trial[neighbor_indices[[points_to_remove]]] <- neighbor_counts_trial[neighbor_indices[[points_to_remove]]] - 1
      neighbor_counts_trial[points_to_remove] <- 0L

      # Mark that point as not to keep
      kept_points_trial[points_to_remove] <- FALSE

      # Identify points to remove: those with the most neighbors
      max_neighbors <- max(neighbor_counts_trial)
    }

    # Store the results of this trial
    if (all_trials) {
      kept_points[[i]] <- kept_points_trial
    } else {
      # Update the kept_points if this trial resulted in more kept points
      if (sum(kept_points_trial) > sum(kept_points[[1]])) {
        kept_points[[1]] <- kept_points_trial
      }
    }
  }

  # Return list of trials or list with best trial in first position
  return(kept_points)
}

#' Select Target Number of Points for Spatial Thinning
#'
#' This function selects a specified number of points from a spatial dataset while maximizing
#' the distance between selected points.
#'
#' @param distance_matrix A matrix of pairwise distances between points.
#' @param target_points An integer specifying the number of points to retain.
#' @param thin_dist A positive numeric value representing the thinning distance in kilometers.
#' @param trials A positive integer specifying the number of thinning trials to perform. Default is 10.
#' @param all_trials A logical value indicating whether to return results of all attempts (`TRUE`) or only the best attempt with the most points retained (`FALSE`). Default is `FALSE`.
#'
#' @return A list of logical vectors indicating which points are kept in each trial if `all_trials` is `TRUE`; otherwise, a list with a single logical vector indicating the points kept in the best trial.
#'
#' @examples
#' # Example distance matrix (3 points)
#' dist_matrix <- matrix(c(0, 2, 5,
#'                         2, 0, 3,
#'                         5, 3, 0), ncol = 3)
#'
#' # Select 2 points maximizing distance
#'result <- select_target_points(dist_matrix, target_points = 2,
#'                               thin_dist = 4, trials = 5, all_trials = TRUE)
#'
#' @export
select_target_points <- function(distance_matrix, target_points, thin_dist, trials, all_trials = FALSE) {
  # Initialize results list for trials
  n <- nrow(distance_matrix)
  kept_points <- vector("list", ifelse(all_trials, trials, 1))
  kept_points[[1]] <- rep(FALSE, n) # Start with no points kept

  for (i in seq_len(trials)) {
    kept_points_trial <- rep(FALSE, n)
    points_to_keep <- as.integer(stats::runif(1, 1, n)) # Randomly select the first point

    while (length(points_to_keep) < target_points) {
      dist_to_closest_kept <- matrixStats::rowMins(distance_matrix[, points_to_keep, drop = FALSE], na.rm = TRUE)
      dist_to_closest_kept[points_to_keep] <- 0

      max_dist_closest <- max(dist_to_closest_kept, na.rm = TRUE)
      if (max_dist_closest < thin_dist) break  # Stop if no points are further than thin_dist

      furthest <- which(dist_to_closest_kept == max_dist_closest)
      if (length(furthest) > 1) {
        furthest <- sample(furthest, 1)  # Randomly select one if multiple furthest points
      }

      points_to_keep <- c(points_to_keep, furthest)
    }

    kept_points_trial[points_to_keep] <- TRUE

    if (all_trials) {
      kept_points[[i]] <- kept_points_trial
    } else {
      if (length(points_to_keep) == target_points) {
        kept_points[[1]] <- kept_points_trial
        break  # Exit if target points are reached
      } else if (sum(kept_points_trial) > sum(kept_points[[1]])) {
        kept_points[[1]] <- kept_points_trial  # Update if this trial kept more points
      }
    }
  }

  return(kept_points)
}
