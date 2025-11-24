#' Check for Longitude/Latitude Coordinates
#'
#' This function checks whether a pair of coordinate vectors represent
#' geographic longitude and latitude values. The function returns `TRUE`
#' if all longitude values are within -180 (- tolerance) and 180 (+ tolerance)
#' and the latitude is within -90 (- tolerance) and 90 (+ tolerance).
#'
#' @param lon Numeric vector of longitudes in degrees.
#' @param lat Numeric vector of latitudes in degrees.
#' @param tolerance Numeric tolerance (in degrees) for checking the global
#'   longitude/latitude bounds. Default is 0.1.
#'
#' @return A logical value. `TRUE` if values are within the ranges and `FALSE` otherwise.
#'
#' @examples
#' is_lonlat(lon = c(-3, 10, 179), lat = c(40, -20, 5))
#' is_lonlat(lon = c(100000, 150000), lat = c(4500000, 4600000))
#'
#' @export
is_lonlat <- function(lon, lat, tolerance = 0.1) {
  lon_ok <- all(lon >= -180 - tolerance & lon <= 180 + tolerance, na.rm = TRUE)
  lat_ok <- all(lat >= -90  - tolerance & lat <=  90 + tolerance, na.rm = TRUE)

  return(lon_ok && lat_ok)
}

#' Convert Geographic Coordinates to Cartesian Coordinates
#'
#' This function converts geographic coordinates, given as longitude and latitude in degrees, to Cartesian coordinates (x, y, z) assuming a spherical Earth model.
#'
#' @param lon Numeric vector of longitudes in degrees.
#' @param lat Numeric vector of latitudes in degrees.
#' @param R Radius of the Earth in kilometers (default: 6371 km).
#' @return A numeric matrix with three columns (x, y, z) representing Cartesian coordinates.
#' @examples
#' lon <- c(-122.4194, 0)
#' lat <- c(37.7749, 0)
#' lon_lat_to_cartesian(lon, lat)
#' @export
lon_lat_to_cartesian <- function(lon, lat, R = 6371) {
  lat_rad <- lat * pi / 180
  lon_rad <- lon * pi / 180
  x <- R * cos(lat_rad) * cos(lon_rad)
  y <- R * cos(lat_rad) * sin(lon_rad)
  z <- R * sin(lat_rad)
  return(cbind(x, y, z))
}

#' Estimate Maximum Neighbors for kd-Tree Thinning
#'
#' This function estimates the maximum value of k (the number of nearest neighbors)
#' for kd-tree-based thinning by evaluating the densest regions of a spatial dataset.
#' The function uses a histogram-based binning approach for efficiency and low memory usage.
#'
#' @param coordinates A matrix of spatial coordinates with two columns for longitude and latitude.
#' @param thin_dist A positive numeric value representing the thinning distance in kilometers.
#'        This defines the resolution of the grid used for density calculations.
#' @param distance Distance metric used `c("haversine", "euclidean")`.
#'
#' @return A numeric value representing the maximum k (number of nearest neighbors) required
#'         for the densest regions in the dataset.
#'
#' @details
#' The function divides the spatial domain into grid cells based on the specified thinning distance.
#' Grid cell sizes are determined assuming approximately 111.32 km per degree (latitude/longitude).
#' The function identifies the densest grid cells and their immediate neighbors to compute the maximum k value.
#'
#' @examples
#' # Generate sample data
#' set.seed(123)
#' coordinates <- matrix(runif(200, min = -10, max = 10), ncol = 2)
#'
#' # Estimate k for kd-tree thinning
#' k_max <- estimate_k_max(coordinates, thin_dist = 50)
#' print(k_max)
#'
#' @export
estimate_k_max <- function(coordinates, thin_dist, distance = c("haversine", "euclidean")) {
  # Validate inputs
  if (!is.numeric(thin_dist) || thin_dist <= 0) {
    stop("`thin_dist` must be a positive number.")
  }
  if (!is.matrix(coordinates) || ncol(coordinates) != 2) {
    stop("`coordinates` must be a matrix with two columns (longitude and latitude).")
  }
  distance <- match.arg(distance)

  if (distance == "haversine") {
    # Convert thinning distance to degrees (approximation for latitude/longitude)
    cell_size <- thin_dist / 111.32
  } else {
    cell_size <- thin_dist
  }

  # Create grid breaks based on cell size
  x_breaks <- seq(min(coordinates[, 1]), max(coordinates[, 1]), by = cell_size)
  y_breaks <- seq(min(coordinates[, 2]), max(coordinates[, 2]), by = cell_size)
  x_breaks[length(x_breaks)] <- max(coordinates[, 1])
  y_breaks[length(y_breaks)] <- max(coordinates[, 2])

  if (length(x_breaks) == 1){
    x_breaks <- c(min(coordinates[, 1]), max(coordinates[, 1]))
  }
  if (length(y_breaks) == 1){
    y_breaks <- c(min(coordinates[, 2]), max(coordinates[, 2]))
  }

  # Bin and count the data
  bin_counts <- table(
    cut(coordinates[, 1], breaks = x_breaks, include.lowest = TRUE),
    cut(coordinates[, 2], breaks = y_breaks, include.lowest = TRUE)
  )
  bin_matrix <- as.matrix(bin_counts)

  # Identify the densest grid cells
  max_density_cells <- which(bin_matrix >= stats::quantile(bin_matrix, probs = 0.95), arr.ind = TRUE)

  # Calculate maximum k (including neighbors)
  max_k <- max(apply(max_density_cells, 1, function(cell) {
    # Extract the row and column of the current cell
    row <- cell[1]
    col <- cell[2]

    # Sum counts in the current cell and its neighbors
    sum(bin_matrix[
      max(1, row - 1):min(nrow(bin_matrix), row + 1),
      max(1, col - 1):min(ncol(bin_matrix), col + 1)
    ])
  }))

  return(max_k)
}

#' Compute Nearest Neighbor Distances
#'
#' Calculates nearest neighbor distances using geodesic or Euclidean distance.
#'
#' @param coordinates A matrix of coordinates with two columns.
#' @param distance A character string: "haversine" (default) or "euclidean".
#' @param R Radius of the Earth in kilometers. Default is 6371.
#'
#' @return A numeric vector of nearest neighbor distances, in meters (haversine) or in map units (euclidean).
#'
#' @examples
#' # Example with geographic (longitude/latitude) coordinates
#' set.seed(123)
#' coords_geo <- matrix(cbind(runif(10, -10, 10), runif(10, 40, 50)), ncol = 2)
#' nnd_haversine <- compute_nearest_neighbor_distances(coords_geo, distance = "haversine")
#' print(round(nnd_haversine, 2))  # in km
#'
#' # Example with projected coordinates (Euclidean)
#' coords_proj <- matrix(runif(20), ncol = 2) * 100  # e.g., meters or map units
#' nnd_euclidean <- compute_nearest_neighbor_distances(coords_proj, distance = "euclidean")
#' print(round(nnd_euclidean, 2))
#'
#' @export
compute_nearest_neighbor_distances <- function(coordinates, distance = "haversine", R = 6371) {
  if (nrow(coordinates) < 2) return(rep(NA, nrow(coordinates)))

  kd_result <- nabor::knn(coordinates, k = 2)
  nearest_idx <- kd_result$nn.idx[, 2]

  if (distance == "haversine") {
    dists <- numeric(nrow(coordinates))
    for (i in seq_len(nrow(coordinates))) {
      dists[i] <- fields::rdist.earth(coordinates[i, , drop = FALSE],
                                      coordinates[nearest_idx[i], , drop = FALSE],
                                      miles = FALSE, R = R)[1, 1]  # km
    }
  } else {
    # Return raw Euclidean distance from kd-tree
    dists <- kd_result$nn.dists[, 2]  # already in native units
  }

  return(dists)
}


#' Calculate Spatial Coverage (Convex Hull Area)
#'
#' Computes the area of the convex hull formed by the points.
#' Uses geodetic area (km2) if coordinates are lon/lat and distance = "haversine",
#' otherwise computes area in squared map units.
#'
#' @param coordinates A matrix of coordinates (longitude and latitude or planar x/y).
#' @param distance A character string: "haversine" (default) or "euclidean".
#'
#' @return A numeric value representing the convex hull area (km2 or unit2).
#'
#' @examples
#' # Geographic coordinates (lon/lat)
#' set.seed(456)
#' coords_geo <- matrix(cbind(runif(10, -10, 10), runif(10, 40, 50)), ncol = 2)
#' area_haversine <- calculate_spatial_coverage(coords_geo, distance = "haversine")
#' print(round(area_haversine, 2))  # in km2
#'
#' # Projected coordinates (Euclidean/map units)
#' coords_proj <- matrix(runif(20), ncol = 2) * 100  # e.g., map units
#' area_euclidean <- calculate_spatial_coverage(coords_proj, distance = "euclidean")
#' print(round(area_euclidean, 2))  # in unit2
#'
#' @export
calculate_spatial_coverage <- function(coordinates, distance = "haversine") {
  if (nrow(unique(coordinates)) < 3) return(0)

  if (distance == "haversine") {
    sf_pts <- sf::st_as_sf(data.frame(coordinates), coords = c(1, 2), crs = 4326)
    hull <- sf::st_convex_hull(sf::st_union(sf_pts))
    area <- as.numeric(sf::st_area(hull)) / 1e6 # km2
  } else {
    # Simple polygon area in planar units
    ch <- grDevices::chull(coordinates)
    hull_coords <- coordinates[c(ch, ch[1]), ]
    area <- abs(sum(hull_coords[-1, 1] * hull_coords[-nrow(hull_coords), 2] -
                      hull_coords[-nrow(hull_coords), 1] * hull_coords[-1, 2])) / 2
  }

  return(area)
}
