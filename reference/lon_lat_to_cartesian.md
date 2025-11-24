# Convert Geographic Coordinates to Cartesian Coordinates

This function converts geographic coordinates, given as longitude and
latitude in degrees, to Cartesian coordinates (x, y, z) assuming a
spherical Earth model.

## Usage

``` r
lon_lat_to_cartesian(lon, lat, R = 6371)
```

## Arguments

- lon:

  Numeric vector of longitudes in degrees.

- lat:

  Numeric vector of latitudes in degrees.

- R:

  Radius of the Earth in kilometers (default: 6371 km).

## Value

A numeric matrix with three columns (x, y, z) representing Cartesian
coordinates.

## Examples

``` r
lon <- c(-122.4194, 0)
lat <- c(37.7749, 0)
lon_lat_to_cartesian(lon, lat)
#>              x         y        z
#> [1,] -2699.749 -4250.942 3902.625
#> [2,]  6371.000     0.000    0.000
```
