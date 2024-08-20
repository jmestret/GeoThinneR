#' Convert Geographic Coordinates to Cartesian Coordinates
#'
#' This function converts geographic coordinates, given as longitude and latitude in degrees, to Cartesian coordinates (x, y, z) assuming a spherical Earth model.
#'
#' @param long Numeric vector of longitudes in degrees.
#' @param lat Numeric vector of latitudes in degrees.
#' @param R Radius of the Earth in kilometers (default: 6371 km).
#' @return A numeric matrix with three columns (x, y, z) representing Cartesian coordinates.
#' @examples
#' long <- c(-122.4194, 0)
#' lat <- c(37.7749, 0)
#' long_lat_to_cartesian(long, lat)
#' @export
long_lat_to_cartesian <- function(long, lat, R = 6371) {
  lat_rad <- lat * pi / 180
  long_rad <- long * pi / 180
  x <- R * cos(lat_rad) * cos(long_rad)
  y <- R * cos(lat_rad) * sin(long_rad)
  z <- R * sin(lat_rad)
  return(cbind(x, y, z))
}

#' Assign Geographic Coordinates to Grid Cells
#'
#' This function assigns a set of geographic coordinates (longitude and latitude) to grid cells based on a specified cell size.
#'
#' @param coords A data frame or matrix with two columns: longitude and latitude.
#' @param cell_size Numeric value representing the size of each grid cell, typically in degrees.
#' @return A character vector of grid cell identifiers, where each identifier is formatted as "x_y", representing the grid cell coordinates.
#' @examples
#' coords <- data.frame(long = c(-122.4194, 0), lat = c(37.7749, 0))
#' cell_size <- 1
#' assign_coords_to_grid(coords, cell_size)
#' @export
assign_coords_to_grid <- function(coords, cell_size) {
  min_long <- min(coords[, 1])
  min_lat <- min(coords[, 2])

  grid_x <- floor((coords[, 1] - min_long) / cell_size)
  grid_y <- floor((coords[, 2] - min_lat) / cell_size)

  grid_indices <- paste(grid_x, grid_y, sep = "_")

  return(grid_indices)
}
