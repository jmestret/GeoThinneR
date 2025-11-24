# Compute Nearest Neighbor Distances

Calculates nearest neighbor distances using geodesic or Euclidean
distance.

## Usage

``` r
compute_nearest_neighbor_distances(
  coordinates,
  distance = "haversine",
  R = 6371
)
```

## Arguments

- coordinates:

  A matrix of coordinates with two columns.

- distance:

  A character string: "haversine" (default) or "euclidean".

- R:

  Radius of the Earth in kilometers. Default is 6371.

## Value

A numeric vector of nearest neighbor distances, in meters (haversine) or
in map units (euclidean).

## Examples

``` r
# Example with geographic (longitude/latitude) coordinates
set.seed(123)
coords_geo <- matrix(cbind(runif(10, -10, 10), runif(10, 40, 50)), ncol = 2)
nnd_haversine <- compute_nearest_neighbor_distances(coords_geo, distance = "haversine")
print(round(nnd_haversine, 2))  # in km
#>  [1] 243.85 199.19 315.93 199.19 105.54 356.71  98.62 105.54  98.62 315.93

# Example with projected coordinates (Euclidean)
coords_proj <- matrix(runif(20), ncol = 2) * 100  # e.g., meters or map units
nnd_euclidean <- compute_nearest_neighbor_distances(coords_proj, distance = "euclidean")
print(round(nnd_euclidean, 2))
#>  [1] 19.76 20.59 11.79 19.76 20.14 22.35 11.79 20.14 16.63 16.63
```
