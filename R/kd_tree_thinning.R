#' Perform K-D Tree ANN Thinning
#'
#' This function applies the K-D tree Approximate Nearest Neighbors (ANN) thinning algorithm on a set of spatial coordinates.
#' It can optionally use space partitioning to improve the thinning process, which is particularly useful for large datasets.
#'
#' @param coordinates A matrix of coordinates to thin, with two columns representing longitude and latitude.
#' @param thin_dist A numeric value representing the thinning distance in kilometers. Points closer than this distance to each other are considered redundant and may be removed.
#' @param trials An integer specifying the number of trials to run for thinning. Multiple trials can help achieve a better result by randomizing the thinning process. Default is 10.
#' @param all_trials A logical value indicating whether to return results of all attempts (`TRUE`) or only the best attempt with the most points retained (`FALSE`). Default is `FALSE`.
#' @param space_partitioning A logical value indicating whether to use space partitioning to divide the coordinates into grid cells before thinning. This can improve efficiency in large datasets. Default is `FALSE`.
#' @param euclidean Logical value indicating whether to compute the Euclidean distance (`TRUE`) or Haversine distance (`FALSE`, default).
#' @param R A numeric value representing the radius of the Earth in kilometers. The default is 6371 km.
#'
#' @return A list. If `all_trials` is `FALSE`, the list contains a single logical vector indicating which points are kept in the best trial. If `all_trials` is `TRUE`, the list contains a logical vector for each trial.
#' @examples
#' # Generate sample coordinates
#' set.seed(123)
#' coordinates <- matrix(runif(20, min = -180, max = 180), ncol = 2) # 10 random points
#'
#' # Perform K-D Tree thinning without space partitioning
#' result <- kd_tree_thinning(coordinates, thin_dist = 10, trials = 5, all_trials = FALSE)
#' print(result)
#'
#' # Perform K-D Tree thinning with space partitioning
#' result_partitioned <- kd_tree_thinning(coordinates, thin_dist = 5000, trials = 5,
#'                                        space_partitioning = TRUE, all_trials = TRUE)
#' print(result_partitioned)
#'
#' # Perform K-D Tree thinning with Cartesian coordinates
#' cartesian_coordinates <- long_lat_to_cartesian(coordinates[, 1], coordinates[, 2])
#' result_cartesian <- kd_tree_thinning(cartesian_coordinates, thin_dist = 10, trials = 5,
#'                                      euclidean = TRUE)
#' print(result_cartesian)
#'
#' @export
kd_tree_thinning <- function(coordinates, thin_dist = 10, trials = 10, all_trials = FALSE, space_partitioning = FALSE, euclidean = FALSE, R = 6371) {
  # Input validation
  if (!is.numeric(thin_dist) || thin_dist <= 0) {
    stop("`thin_dist` must be a positive number.")
  }

  if (!is.logical(space_partitioning)) {
    stop("`space_partitioning` must be a logical value (`TRUE` or `FALSE`).")
  }

  # Initialize a list to store neighbor indices
  n <- nrow(coordinates)
  neighbor_indices <- vector("list", n)

  # Convert geographic coordinates to Cartesian coordinates if long lat
  if (euclidean){
    cartesian_points <- coordinates
  } else {
    cartesian_points <- t(apply(coordinates, 1, function(row) long_lat_to_cartesian(row[1], row[2], R)))
  }

  # Check if space partitioning is requested
  if (space_partitioning){
    # Define the grid cell size based on the thinning distance (in degrees)
    cell_size <- thin_dist / 111.32  # Approx 111.32 km per degree

    # Assign points to grid cells
    grid_indices <- assign_coords_to_grid(coordinates, cell_size)
    unique_grids <- unique(grid_indices)

    # Iterate over each unique grid cell to find neighbors
    for (grid in unique_grids) {
      cell_points <- which(grid_indices == grid)

      if (length(cell_points) > 0) {
        # Determine neighboring cells (including diagonals)
        grid_coords <- strsplit(grid, "_")[[1]]
        grid_x <- as.numeric(grid_coords[1])
        grid_y <- as.numeric(grid_coords[2])

        # Identify neighbor cells (including diagonals)
        neighbor_grids <- paste(rep(grid_x + -1:1, each = 3), rep(grid_y + -1:1, times = 3), sep = "_")
        neighbor_grids <- neighbor_grids[neighbor_grids != grid]
        neighbor_points <- which(grid_indices %in% neighbor_grids)

        # Create a vector of original indices
        combined_indices <- c(cell_points, neighbor_points)

        # Combine points from the current cell and its neighbors
        combined_points <- cartesian_points[combined_indices, , drop = FALSE]

        # Build KD-tree and find neighbors within the specified radius
        kd_tree <- nabor::knn(combined_points, k = nrow(combined_points), radius = thin_dist)

        # Loop through each point in the current grid cell
        for (i in seq_along(cell_points)) {
          idx <- cell_points[i]
          neighbors <- kd_tree$nn.idx[i, ]

          # Remove self-reference and map back to original indices
          neighbor_indices[[idx]] <- combined_indices[neighbors[neighbors != 0 & neighbors != i]]
        }
      }
    }
  } else {
    # Build K-D tree and find neighbors within the specified radius
    kd_tree <- nabor::knn(cartesian_points, k = n, radius = thin_dist)

    # Create a list of neighbor indices excluding self-reference
    for (i in seq_len(n)) {
      non_zero_indices <- kd_tree$nn.idx[i, ]
      neighbor_indices[[i]] <- non_zero_indices[non_zero_indices != 0 & non_zero_indices != i]
    }
  }

  # Run thinning algorithm to keep as max points as possible
  keep_points <- max_thinning_algorithm(neighbor_indices, n, trials, all_trials)

  # Return list of trials or list with best trial in first position
  return(keep_points)
}
