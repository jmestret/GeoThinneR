# Calculate Spatial Coverage (Convex Hull Area)

Computes the area of the convex hull formed by the points. Uses geodetic
area (km2) if coordinates are lon/lat and distance = "haversine",
otherwise computes area in squared map units.

## Usage

``` r
calculate_spatial_coverage(coordinates, distance = "haversine")
```

## Arguments

- coordinates:

  A matrix of coordinates (longitude and latitude or planar x/y).

- distance:

  A character string: "haversine" (default) or "euclidean".

## Value

A numeric value representing the convex hull area (km2 or unit2).

## Examples

``` r
# Geographic coordinates (lon/lat)
set.seed(456)
coords_geo <- matrix(cbind(runif(10, -10, 10), runif(10, 40, 50)), ncol = 2)
area_haversine <- calculate_spatial_coverage(coords_geo, distance = "haversine")
print(round(area_haversine, 2))  # in km2
#> [1] 585502.8

# Projected coordinates (Euclidean/map units)
coords_proj <- matrix(runif(20), ncol = 2) * 100  # e.g., map units
area_euclidean <- calculate_spatial_coverage(coords_proj, distance = "euclidean")
print(round(area_euclidean, 2))  # in unit2
#> [1] 4134.5
```
