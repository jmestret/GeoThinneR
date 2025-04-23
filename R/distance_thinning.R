#' Perform Distance-Based Thinning
#'
#' This function applies a distance-based thinning algorithm using a kd-tree or brute-force approach.
#' Two modified algorithms based on kd-trees (local kd-trees and estimating the maximum number of neighbors) are implemented which scale better for large datasets.
#' The function removes points that are closer than a specified distance to each other while maximizing spatial representation.
#'
#' @param coordinates A matrix of coordinates to thin, with two columns representing longitude and latitude.
#' @param thin_dist A positive numeric value representing the thinning distance in kilometers.
#' @param trials An integer specifying the number of trials to run for thinning. Default is 10.
#' @param all_trials A logical indicating whether to return results of all attempts (`TRUE`) or only the best attempt with the most points retained (`FALSE`). Default is `FALSE`.
#' @param search_type A character string indicating the neighbor search method `c("local_kd_tree", "k_estimation", "kd_tree", "brute")`. The default value is `local_kd_tree`. See details.
#' @param target_points Optional integer specifying the number of points to retain. If `NULL` (default), the function tries to maximize the number of points retained.
#' @param distance Distance metric to use `c("haversine", "euclidean")`. Default is Haversine for geographic coordinates.
#' @param R Radius of the Earth in kilometers (default: 6371 km).
#' @param n_cores Number of cores for parallel processing (only for `"local_kd_tree"`). Default is 1.
#'
#' @details
#' - `"kd_tree"`: Uses a single kd-tree for efficient nearest-neighbor searches.
#' - `"local_kd_tree"`: Builds multiple smaller kd-trees for better scalability.
#' - `"k_estimation"`: Approximates a maximum number of neighbors per point to reduce search complexity.
#' - `"brute"`: Computes all pairwise distances (inefficient for large datasets).
#'
#' @return A list. If `all_trials` is `FALSE`, the list contains a single logical vector indicating which points are kept in the best trial. If `all_trials` is `TRUE`, the list contains a logical vector for each trial.
#'
#' @examples
#' # Generate sample coordinates
#' set.seed(123)
#' result  <- matrix(runif(20, min = -180, max = 180), ncol = 2) # 10 random points
#'
#' # Perform thinning with local kd-trees
#' result_partitioned <- distance_thinning(result , thin_dist = 5000, trials = 5,
#'                                        search_type = "local_kd_tree", all_trials = TRUE)
#' print(result_partitioned)
#'
#' # Perform thinning estimating max number of neighbors
#' result_estimated <- distance_thinning(result , thin_dist = 5000, trials = 5,
#'                                        search_type = "k_estimation", all_trials = TRUE)
#' print(result_estimated)
#'
#' @export
distance_thinning <- function(coordinates, thin_dist = 10, trials = 10, all_trials = FALSE,
                              search_type = c("local_kd_tree", "k_estimation", "kd_tree", "brute"),
                              target_points = NULL, distance = c("haversine", "euclidean"),
                              R = 6371, n_cores = 1) {

  # Input validation
  if (!is.matrix(coordinates) || ncol(coordinates) != 2) {
    stop("`coordinates` must be a matrix with two columns (longitude and latitude).")
  }
  if (!is.numeric(thin_dist) || thin_dist <= 0) {
    stop("`thin_dist` must be a positive number.")
  }
  distance <- match.arg(distance)
  search_type <- match.arg(search_type)
  if (!is.null(target_points)){ # Use brute-force algorithm as we need all pairwise distances
    if (!is.numeric(target_points) || target_points <= 0) {
      stop("`target_points` must be a positive number.")
    }
    message("For specific target points, brute force method is used.")
    search_type <- "brute"
  }

  if (is.null(target_points)){
    # Determine neighbor search method
    neighbor_indices <- switch(
      search_type,
      "brute" = compute_neighbors_brute(coordinates, thin_dist, distance, R),
      "kd_tree" = compute_neighbors_kdtree(coordinates, thin_dist, nrow(coordinates), distance, R),
      "local_kd_tree" = compute_neighbors_local_kdtree(coordinates, thin_dist, distance, R, n_cores),
      "k_estimation" = compute_neighbors_kdtree(coordinates, thin_dist, estimate_k_max(coordinates, thin_dist, distance), distance, R),
      stop("Unsupported `search_type`. Choose from 'kd_tree', 'local_kd_tree', 'k_estimation', or 'brute'.")
    )

    # Run thinning algorithm to keep as max points as possible
    kept_points <- max_thinning_algorithm(neighbor_indices, trials, all_trials)
  } else { # Try to select the exact number of points as much separated as possible
    # Compute neighbors
    if (distance == "haversine") {
      dist_mat <- fields::rdist.earth(x1 = coordinates, miles = FALSE, R = R)
    } else {
      dist_mat <- as.matrix(stats::dist(coordinates, upper = TRUE))
    }
    diag(dist_mat) <- NA  # Avoid self-matches

    kept_points <- select_target_points(dist_mat, target_points, thin_dist, trials, all_trials)
  }

  return(kept_points)
}

#' Compute Neighbors Using Brute-Force
#'
#' Computes neighbors for each point in a set of coordinates using a greedy
#' approach. All pairwise distances are calculated to identify neighbors within
#' a specified distance threshold.
#'
#' @param coordinates A matrix of coordinates to thin, with two columns representing longitude and latitude.
#' @param thin_dist A positive numeric value representing the thinning distance in kilometers.
#' @param distance A character string specifying the distance metric to use `c("haversine", "euclidean")`.
#' @param R A numeric value representing the radius of the Earth in kilometers. The default is 6371 km.
#'
#' @return A list where each element corresponds to a point and contains the indices of its neighbors.
#'
#' @examples
#' set.seed(123)
#' coords <- matrix(runif(20, min = -180, max = 180), ncol = 2)
#'
#' # Compute neighbors using brute fore
#' neighbors <- compute_neighbors_brute(coords, thin_dist = 10,)
#'
#' @export
compute_neighbors_brute <- function(coordinates, thin_dist, distance = c("haversine", "euclidean"), R = 6371) {
  # Initialize a list to store neighbor indices
  n <- nrow(coordinates)
  neighbor_indices <- vector("list", n)
  distance <- match.arg(distance)

  # Compute neighbors
  if (distance == "haversine") {
    dist_mat <- fields::rdist.earth(x1 = coordinates, miles = FALSE, R = R) < thin_dist
  } else {
    dist_mat <- as.matrix(stats::dist(coordinates, upper = TRUE)) < thin_dist
  }
  diag(dist_mat) <- NA  # Avoid self-matches
  for (i in seq_len(n)) {
    neighbor_indices[[i]] <- which(dist_mat[i, ])
  }

  return(neighbor_indices)
}

#' Compute Neighbors Using kd-Tree
#'
#' Computes neighbors for each point in a set of coordinates using a kd-tree
#' for efficient neighbor searches. This method is particularly useful for large datasets.
#'
#' @param coordinates A matrix of coordinates to thin, with two columns representing longitude and latitude.
#' @param thin_dist A positive numeric value representing the thinning distance in kilometers.
#' @param k An integer specifying the maximum number of neighbors to consider for each point.
#' @param distance A character string specifying the distance metric to use `c("haversine", "euclidean")`.
#' @param R A numeric value representing the radius of the Earth in kilometers. The default is 6371 km.
#'
#' @return A list where each element corresponds to a point and contains the indices of its neighbors, excluding the point itself.
#'
#' @details
#' This function uses kd-tree (via `nabor` package) for efficient spatial searches. The kd-tree inherently works with Euclidean distances.
#' If `"haversine"` is selected, the function first converts geographic coordinates to 3D Cartesian coordinates before constructing the kd-tree.
#'
#' @examples
#' set.seed(123)
#' coords <- matrix(runif(20, min = -180, max = 180), ncol = 2)
#'
#' # Compute neighbors using kd-tree
#' neighbors <- compute_neighbors_kdtree(coords, thin_dist = 10,)
#'
#' @export
compute_neighbors_kdtree <- function(coordinates, thin_dist, k = NULL, distance = c("haversine", "euclidean"), R = 6371) {
  # Initialize a list to store neighbor indices
  n <- nrow(coordinates)
  neighbor_indices <- vector("list", n)
  distance <- match.arg(distance)
  if (is.null(k)){
    k <- n
  }

  # In kd-tree we can only use the Euclidean distance
  # When asked for the Haversine distance we transform coordinates
  # Convert geographic coordinates to Cartesian coordinates if lon lat
  if (distance == "haversine") {
    cartesian_points <- t(apply(coordinates, 1, function(row) lon_lat_to_cartesian(row[1], row[2], R)))
  } else if (distance == "euclidean"){
    cartesian_points <- coordinates
  }

  # Build kd-tree and find neighbors within the specified radius
  kd_tree <- nabor::knn(cartesian_points, k = k, radius = thin_dist)

  # Create a list of neighbor indices excluding self-reference
  for (i in seq_len(n)) {
    neighbors <- kd_tree$nn.idx[i, ]
    neighbor_indices[[i]] <- neighbors[neighbors != 0][-1] # Exclude self
  }

  return(neighbor_indices)
}

#' Compute Neighbors Using Local kd-Trees
#'
#' Divides the search area into a grid of local regions and constructs kd-trees
#' for each region to compute neighbors efficiently. Neighbor regions are
#' also considered to ensure a complete search.
#'
#' @param coordinates A matrix of coordinates to thin, with two columns representing longitude and latitude.
#' @param thin_dist A positive numeric value representing the thinning distance in kilometers.
#' @param distance A character string specifying the distance metric to use `c("haversine", "euclidean")`.
#' @param R A numeric value representing the radius of the Earth in kilometers. The default is 6371 km.
#' @param n_cores An integer specifying the number of cores to use for parallel processing. The default is 1.
#'
#' @return A list where each element corresponds to a point and contains the indices of its neighbors, excluding the point itself.
#'
#' @examples
#' set.seed(123)
#' coords <- matrix(runif(20, min = -180, max = 180), ncol = 2)
#'
#' # Compute neighbors using local kd-trees with Euclidean distance
#' neighbors <- compute_neighbors_local_kdtree(coords, thin_dist = 10, n_cores = 1)
#'
#' @importFrom foreach %dopar%
#' @export
compute_neighbors_local_kdtree <- function(coordinates, thin_dist, distance = c("haversine", "euclidean"), R = 6371, n_cores = 1) {
  # Initialize a list to store neighbor indices
  n <- nrow(coordinates)
  neighbor_indices <- vector("list", n)
  distance <- match.arg(distance)

  # In kd-tree we can only use the Euclidean distance
  # When asked for the Haversine distance we transform coordinates
  # Convert geographic coordinates to Cartesian coordinates if lon lat
  if (distance == "haversine") {
    cartesian_points <- t(apply(coordinates, 1, function(row) lon_lat_to_cartesian(row[1], row[2], R)))
    # Assign points to 3D grid using integer indexing
    grid_coords <- floor(cartesian_points / thin_dist)

    # Create a list to store points by grid cell
    grid_dict <- split(seq_len(n), list(grid_coords[,1], grid_coords[,2], grid_coords[,3]), drop = TRUE)

  } else if (distance == "euclidean"){
    cartesian_points <- coordinates

    # Assign points to 2D grid
    grid_coords <- floor(cartesian_points / thin_dist)
    grid_dict <- split(seq_len(n), list(grid_coords[,1], grid_coords[,2], 0), drop = TRUE)
  }

  #Find neighbors for each point
  process_grid_cell <- function(cell_key) {
    cell_ids <- grid_dict[[cell_key]]

    if (length(cell_ids) == 0){
      return(NULL)
    }

    # Find neighbor cells
    grid_x <- as.numeric(strsplit(cell_key, "\\.")[[1]][1])
    grid_y <- as.numeric(strsplit(cell_key, "\\.")[[1]][2])
    grid_z <- as.numeric(strsplit(cell_key, "\\.")[[1]][3])

    neighbor_keys <- c()
    for (dx in -1:1) {
      for (dy in -1:1) {
        for (dz in -1:1) {
          neighbor_keys <- c(neighbor_keys, paste(grid_x + dx, grid_y + dy, grid_z + dz, sep = "."))
        }
      }
    }

    # Collect all points in query cell and neighbor cells
    neighbor_ids <- unlist(grid_dict[neighbor_keys], use.names = FALSE)

    # Build kd-tree and find neighbors within the specified radius
    cell_points <- cartesian_points[cell_ids, , drop = FALSE]
    combined_points <- cartesian_points[neighbor_ids, , drop = FALSE]

    kd_tree <- nabor::knn(data = combined_points, query = cell_points, k = nrow(combined_points), radius = thin_dist)

    # Loop through each point in the current grid cell
    result <- vector("list", length(cell_ids))
    for (i in seq_along(cell_ids)) {
      neighbors <- kd_tree$nn.idx[i, ]

      # Remove self-reference and map back to original indices
      result[[i]] <- neighbor_ids[neighbors[neighbors != 0]][-1]
    }

    names(result) <- cell_ids
    return(result)
  }

  if (n_cores > 1) { # Parallelization
    if (parallel::detectCores() <= n_cores) {
      stop("`n_cores` must be smaller than the available number of cores. Check with `detectCores()`.")
    }

    cl <- parallel::makeCluster(n_cores)
    #parallel::clusterExport(cl, c("coordinates", "cartesian_points", "thin_dist"), envir = environment())
    doParallel::registerDoParallel(cl)

    results <- foreach::foreach(cell_key = names(grid_dict), .packages = "nabor") %dopar% {
      process_grid_cell(cell_key)
    }

    parallel::stopCluster(cl)

    # Merge results
    for (res in results) {
      if (!is.null(res)) {
        for (j in names(res)) {
          neighbor_indices[[as.integer(j)]] <- res[[j]]
        }
      }
    }
  } else {  # Single-core execution
    for (cell_key in names(grid_dict)) {
      res <- process_grid_cell(cell_key)
      if (!is.null(res)) {
        for (j in names(res)) {
          neighbor_indices[[as.integer(j)]] <- res[[j]]
        }
      }
    }
  }
  return(neighbor_indices)
}
