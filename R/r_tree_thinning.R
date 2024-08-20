#' Perform R-Tree Thinning
#'
#' Applies the R-tree thinning algorithm on a set of coordinates.
#'
#' If you want to use R-trees you need to install the `rtree` package from `remotes::install_github("jmestret/rtree")`. It is a modified version from \url{https://github.com/akoyabio/rtree}.
#'
#' @param coordinates A matrix of coordinates to thin, with longitude and latitude.
#' @param thin_dist Thinning distance in kilometers.
#' @param trials Number of trials to run for thinning.
#' @param all_trials If TRUE, returns results of all attempts; if FALSE, returns the best attempt with the most points retained (default: FALSE).
#' @param space_partitioning A logical value indicating whether to use space partitioning.
#' @param euclidean Logical value indicating whether to compute the Euclidean distance (`TRUE`) or Haversine distance (`FALSE`, default).
#' @param R Radius of the Earth in kilometers (default: 6371 km).
#' @return A logical vector indicating which points are kept in the best trial if all_trials is FALSE; otherwise, a list of logical vectors for each trial.
#' @examples
#' \dontrun{
#' # Generate random coordinates
#' set.seed(123)
#' coordinates <- matrix(runif(20, min = -180, max = 180), ncol = 2) # 10 random points
#'
#' # Perform thinning without space partitioning
#' result <- r_tree_thinning(coordinates, thin_dist = 10, trials = 5)
#' print(result)
#'
#' # Perform thinning with space partitioning
#' result_space_part <- r_tree_thinning(coordinates, thin_dist = 10, trials = 5,
#'                                      space_partitioning = TRUE)
#' print(result_space_part)
#'
#' # Perform thinning with euclidean distance
#' result_euclidean <- r_tree_thinning(coordinates, thin_dist = 10, trials = 5, euclidean = TRUE)
#' print(result_euclidean)
#' }
#' @export
r_tree_thinning <- function(coordinates, thin_dist = 10, trials = 10, all_trials = FALSE, space_partitioning = FALSE, euclidean = FALSE, R = 6371) {
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

  # Define distance metric
  if (euclidean){
    distance_metric <- "euclidean"
  } else {
    distance_metric <- "haversine"
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
        combined_points <- coordinates[combined_indices, , drop = FALSE]

        # Build R-tree and find neighbors within the specified radius
        r_tree <- rtree::RTree(combined_points)
        neighbors <- rtree::withinDistance(r_tree, combined_points, thin_dist, R, distance_metric)

        # Loop through each point in the current grid cell
        for (i in seq_along(cell_points)) {
          idx <- cell_points[i]

          # Remove self-reference and map back to original indices
          neighbor_indices[[idx]] <- combined_indices[neighbors[[i]] != i]
        }
      }
    }
  } else {
    # Build the R-Tree
    r_tree <- rtree::RTree(coordinates)

    # Find neighbors within the specified distance
    neighbor_indices <- rtree::withinDistance(r_tree, coordinates, thin_dist, R, distance_metric)
    for (i in seq_len(n)) {
      neighbor_indices[[i]] <- neighbor_indices[[i]][neighbor_indices[[i]] != i]
    }
  }



  # Run thinning algorithm to keep as max points as possible
  keep_points <- max_thinning_algorithm(neighbor_indices, n, trials, all_trials)

  # Return list of trials or list with best trial in first position
  return(keep_points)
}
