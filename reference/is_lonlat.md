# Check for Longitude/Latitude Coordinates

This function checks whether a pair of coordinate vectors represent
geographic longitude and latitude values. The function returns \`TRUE\`
if all longitude values are within -180 (- tolerance) and 180 (+
tolerance) and the latitude is within -90 (- tolerance) and 90 (+
tolerance).

## Usage

``` r
is_lonlat(lon, lat, tolerance = 0.1)
```

## Arguments

- lon:

  Numeric vector of longitudes in degrees.

- lat:

  Numeric vector of latitudes in degrees.

- tolerance:

  Numeric tolerance (in degrees) for checking the global
  longitude/latitude bounds. Default is 0.1.

## Value

A logical value. \`TRUE\` if values are within the ranges and \`FALSE\`
otherwise.

## Examples

``` r
is_lonlat(lon = c(-3, 10, 179), lat = c(40, -20, 5))
#> [1] TRUE
is_lonlat(lon = c(100000, 150000), lat = c(4500000, 4600000))
#> [1] FALSE
```
