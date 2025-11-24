# Spatial Thinning of Points

This function performs spatial thinning of geographic points to reduce
point density while maintaining spatial representation. Points are
thinned based on a specified distance, grid, or decimal precision, with
support for multiple trials and optional grouping. By default, only the
largest subset is returned. To return all trials, set \`all_trials =
TRUE\`.

## Usage

``` r
thin_points(
  data,
  lon_col = "lon",
  lat_col = "lat",
  group_col = NULL,
  method = c("distance", "grid", "precision"),
  trials = 10,
  all_trials = FALSE,
  seed = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame or tibble containing the input points to thin. Must
  contain longitude and latitude columns.

- lon_col:

  Character name of the column with longitude coordinates (default:
  \`"lon"\`).

- lat_col:

  Character name of the column with latitude coordinates (default:
  \`"lat"\`).

- group_col:

  Character name of the column for grouping points (e.g., species name,
  year). If \`NULL\`, no grouping is applied.

- method:

  Thinning method to use. One of \`"distance", "grid", "precision"\`.

- trials:

  Number of thinning iterations to perform (default: \`10\`). Must be a
  positive integer.

- all_trials:

  If \`TRUE\`, returns results of all attempts; if \`FALSE\`, returns
  the best attempt with the most points retained (default: \`FALSE\`).

- seed:

  Optional; an integer seed for reproducibility of results.

- verbose:

  If \`TRUE\`, prints progress messages (default: \`FALSE\`).

- ...:

  Additional arguments passed to specific thinning methods. See Details.

## Value

A \`GeoThinned\` object (S3 class), which contains:

- \`retained\`: A list of logical vectors (one per trial) indicating
  retained points.

- \`original_data\`: The original input dataset.

- \`method\`: The thinning method used.

- \`params\`: A list of the thinning parameters used.

By default, \`thin_points()\` returns only the trial with the largest
number of retained points. To access all thinning trials, set
\`all_trials = TRUE\`; otherwise, functions such as \`largest()\` and
\`get_trial()\` will always refer to the same subset.

## Details

The following thinning methods are available:

- \`"distance"\`:

  Forces a specific minimum distance between points.

- \`"grid"\`:

  Applies a grid-based thinning method.

- \`"precision"\`:

  Utilizes precision-based thinning.

**Distance-based thinning**

The specific parameters for distance-based thinning are:

- \`thin_dist\`:

  A positive numeric value representing the thinning distance in
  kilometers.

- \`search_type\`:

  A character string indicating the neighbor search method
  'c("local_kd_tree", "k_estimation", "kd_tree", "brute")'. The defult
  value is 'local_kd_tree'.

- \`distance\`:

  Distance metric to use 'c("haversine", "euclidean")'. Default is
  Haversine for geographic coordinates.

- \`R\`:

  The radius of the Earth in kilometers. Default is 6371 km.

- \`target_points\`:

  Optional integer specifying the number of points to retain. If 'NULL'
  (default), the function tries to maximize the number of points
  retained.

- \`n_cores\`:

  Number of cores for parallel processing (only for '"local_kd_tree"').
  Default is 1.

**Grid-based thinning**

The specific parameters for grid-based thinning are:

- \`thin_dist\`:

  A positive numeric value representing the thinning distance in
  kilometers.

- \`resolution\`:

  A numeric value representing the resolution (in degrees) of the raster
  grid. If provided, this takes priority over 'thin_dist'.

- \`origin\`:

  A numeric vector of length 2 (e.g., 'c(0, 0)'), specifying the origin
  of the raster grid (optional).

- \`raster_obj\`:

  An optional 'terra::SpatRaster' object to use for grid thinning. If
  provided, the raster object will be used instead of creating a new
  one.

- \`n\`:

  A positive integer specifying the maximum number of points to retain
  per grid cell (default: 1).

- \`crs\`:

  An optional CRS (Coordinate Reference System) to project the
  coordinates and raster (default WGS84, 'epsg:4326'). This can be an
  EPSG code, a PROJ.4 string, or a 'terra::crs' object.

- \`priority\`:

  A numeric vector of the same length as the number of points with
  numerical values indicating the priority of each point. Instead of
  eliminating points randomly, higher values are preferred during
  thinning.

**Precision-based thinning**

The specific parameters for precision-based thinning are:

- \`precision\`:

  A positive integer specifying the number of decimal places to which
  coordinates should be rounded. Default is 4.

- \`priority\`:

  A numeric vector of the same length as the number of points with
  numerical values indicating the priority of each point. Instead of
  eliminating points randomly, higher values are preferred during
  thinning.

For more information on specific thinning methods and inputs, refer to
their respective documentation:

- \`distance_thinning()\`

- \`grid_thinning()\`

- \`precision_thinning()\`

## Examples

``` r
# Basic usage
set.seed(123)
sample_data <- data.frame(
  lon = runif(100, -10, 10),
  lat = runif(100, -5, 5)
)

result <- thin_points(sample_data, method = "distance", thin_dist = 100)

# Grouped thinning
sample_data$species <- sample(c("A", "B"), 100, replace = TRUE)
grouped_result <- thin_points(sample_data, group_col = "species",
                              method = "distance", thin_dist = 100)
```
