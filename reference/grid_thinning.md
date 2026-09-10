# Perform Grid-Based Thinning of Spatial Points

This function performs thinning of spatial points by assigning them to
grid cells based on a specified resolution or thinning distance. It can
either create a new raster grid or use an existing \`terra::SpatRaster\`
object.

## Usage

``` r
grid_thinning(
  coordinates,
  thin_dist = NULL,
  resolution = NULL,
  origin = NULL,
  raster_obj = NULL,
  n = 1,
  trials = 10,
  all_trials = FALSE,
  crs = "epsg:4326",
  priority = NULL
)
```

## Arguments

- coordinates:

  A numeric matrix or data frame with two columns representing the x
  (longitude) and y (latitude) coordinates of the points.

- thin_dist:

  A numeric value representing the thinning distance in kilometers. It
  will be converted to degrees if \`resolution\` is not provided.

- resolution:

  A numeric value representing the resolution (in degrees) of the raster
  grid. If provided, this takes priority over \`thin_dist\`.

- origin:

  A numeric vector of length 2 (e.g., \`c(0, 0)\`), specifying the
  origin of the raster grid (optional).

- raster_obj:

  An optional \`terra::SpatRaster\` object to use for grid thinning. If
  provided, the raster object will be used instead of creating a new
  one.

- n:

  A positive integer specifying the maximum number of points to retain
  per grid cell (default: 1).

- trials:

  An integer specifying the number of trials to perform for thinning
  (default: 10).

- all_trials:

  A logical value indicating whether to return results for all trials
  (\`TRUE\`) or just the first trial (\`FALSE\`, default).

- crs:

  An optional CRS (Coordinate Reference System) to project the
  coordinates and raster (default WGS84, \`epsg:4326\`). This can be an
  EPSG code, a PROJ.4 string, or a \`terra::crs\` object.

- priority:

  A numeric vector of the same length as the number of points,
  specifying a priority weight for each point. Higher values indicate
  higher importance and are favored when selecting which points to
  retain. Priority is used to guide selection when multiple candidate
  points are otherwise equally valid (e.g., points in the same grid
  cell, with the same rounded coordinates, or with the same number of
  neighbors).

## Value

A list of logical vectors indicating which points to keep for each
trial.

## Examples

``` r
# Example: Grid thinning using thin_dist
coords <- matrix(c(-122.4194, 37.7749,
                        -122.4195, 37.7740,
                        -122.4196, 37.7741), ncol = 2, byrow = TRUE)

result <- grid_thinning(coords, thin_dist = 10, trials = 5, all_trials = TRUE)
print(result)
#> [[1]]
#> [1] FALSE  TRUE  TRUE
#> 
#> [[2]]
#> [1] FALSE  TRUE  TRUE
#> 
#> [[3]]
#> [1]  TRUE  TRUE FALSE
#> 
#> [[4]]
#> [1] FALSE  TRUE  TRUE
#> 
#> [[5]]
#> [1]  TRUE  TRUE FALSE
#> 

# Example: Grid thinning using a custom resolution
result_res <- grid_thinning(coords, resolution = 0.01, n = 2, trials = 5)
print(result_res)
#> [[1]]
#> [1] TRUE TRUE TRUE
#> 

# Example: Using a custom raster object
library(terra)
#> terra 1.9.50
rast_obj <- terra::rast(nrows = 100, ncols = 100, xmin = -123, xmax = -121, ymin = 36, ymax = 38)
result_raster <- grid_thinning(coords, raster_obj = rast_obj, trials = 5)
print(result_raster)
#> [[1]]
#> [1] FALSE  TRUE FALSE
#> 
```
