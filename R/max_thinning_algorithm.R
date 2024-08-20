#' Thinning Algorithm for Spatial Data
#'
#' This function performs the core thinning algorithm used to reduce the density of points
#' in spatial data while maintaining spatial representation. It works by iteratively removing
#' points with the most neighbors until no points with neighbors remain. The algorithm
#' supports multiple trials to find the optimal thinning solution.
#'
#' @param neighbor_indices A list of integer vectors where each element contains the indices
#'        of the neighboring points for each point in the dataset.
#' @param n The number of points in the dataset.
#' @param trials The number of thinning trials to run.
#' @param all_trials If TRUE, returns the results of all trials; if FALSE, returns the best
#'        trial with the most points retained (default: FALSE).
#' @return A list of logical vectors indicating which points are kept in each trial if
#'         all_trials is TRUE; otherwise, a single logical vector indicating the points kept
#'         in the best trial.
#' @examples
#' # Example usage within a larger thinning function
#' neighbor_indices <- list(c(2, 3), c(1, 3), c(1, 2))
#' n <- 3
#' trials <- 5
#' all_trials <- FALSE
#' keep_points <- max_thinning_algorithm(neighbor_indices, n, trials, all_trials)
#' print(keep_points)
#' @export
max_thinning_algorithm <- function(neighbor_indices, n, trials, all_trials = FALSE) {
  # Compute initial neighbor counts
  neighbor_counts <- lengths(neighbor_indices)

  # Initialize results list for trials
  keep_points <- vector("list", ifelse(all_trials, trials, 1))
  keep_points[[1]] <- rep(FALSE, n)  # Start with no points kept

  # Main thinning loop for the specified number of trials
  for (i in seq_len(trials)) {
    keep_points_trial <- rep(TRUE, n)
    neighbor_counts_trial <- neighbor_counts
    max_neighbors <- max(neighbor_counts_trial)

    while (max_neighbors > 0) {  # Exit loop if no neighbors remain
      # Find indices of points with the maximum neighbors
      points_to_remove <- which(neighbor_counts_trial == max_neighbors)
      if (length(points_to_remove) > 1) {
        points_to_remove <- points_to_remove[as.integer(stats::runif(1, 1, length(points_to_remove)))]
      }

      # Recompute neighbor counts for remaining points
      neighbor_counts_trial[neighbor_indices[[points_to_remove]]] <- neighbor_counts_trial[neighbor_indices[[points_to_remove]]] - 1
      neighbor_counts_trial[points_to_remove] <- 0L

      # Mark that point as not to keep
      keep_points_trial[points_to_remove] <- FALSE

      # Identify points to remove: those with the most neighbors
      max_neighbors <- max(neighbor_counts_trial)
    }

    # Store the results of this trial
    if (all_trials) {
      keep_points[[i]] <- keep_points_trial
    } else {
      # Update the keep_points if this trial resulted in more kept points
      if (sum(keep_points_trial) > sum(keep_points[[1]])) {
        keep_points[[1]] <- keep_points_trial
      }
    }
  }

  # Return list of trials or list with best trial in first position
  return(keep_points)
}
