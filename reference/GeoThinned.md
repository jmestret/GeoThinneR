# GeoThinned Object Constructor and Methods

Create and interact with spatial thinning results stored in a GeoThinned
object.

## Usage

``` r
as_GeoThinned(retained, method, params = list(), original_data = NULL)

new_GeoThinned(retained, method, params = list(), original_data = NULL)

# S3 method for class 'GeoThinned'
print(x, ...)

# S3 method for class 'GeoThinned'
summary(object, trial = NULL, ...)

# S3 method for class 'summary.GeoThinned'
print(x, ...)

# S3 method for class 'GeoThinned'
plot(
  x,
  trial = NULL,
  show_original = TRUE,
  col_original = "#EB714B",
  col_thinned = "#5183B3",
  pch_original = 1,
  pch_thinned = 16,
  main = NULL,
  ...
)

largest(x, ...)

# S3 method for class 'GeoThinned'
largest(x, ...)

largest_index(x, ...)

# S3 method for class 'GeoThinned'
largest_index(x, ...)

get_trial(x, trial = NULL, ...)

# S3 method for class 'GeoThinned'
get_trial(x, trial = NULL, ...)

as_sf(x, ...)

# S3 method for class 'GeoThinned'
as_sf(x, trial = NULL, crs = 4326, ...)
```

## Arguments

- retained:

  A list of logical vectors indicating retained points per trial.

- method:

  The thinning method used (e.g., "distance", "grid", "precision").

- params:

  A list of parameters used in thinning.

- original_data:

  The original unmodified data.

- x:

  An object of class `GeoThinned`.

- ...:

  Additional arguments (ignored).

- object:

  An object of class `GeoThinned`.

- trial:

  Integer index of the thinning trial to extract (for
  [`summary()`](https://rdrr.io/r/base/summary.html), `get_trial()`,
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html), `as_sf()`).
  Default \`NULL\`, which will return the largest dataset.

- show_original:

  Logical, whether to show original points.

- col_original:

  Colors for original points.

- col_thinned:

  Colors for thinned points.

- pch_original:

  Point shapes for original points.

- pch_thinned:

  Point shapes for thinned points.

- main:

  Title of the plot.

- crs:

  Coordinate reference system to assign to the resulting `sf` object
  (optional).

## Value

A `GeoThinned` object or associated results (summary, plot, trial
subset). When \`thin_points()\` is run with \`all_trials = FALSE\`, the
returned object contains only the largest trial; therefore all methods
refer to this single subset.

## See also

[`thin_points`](https://jmestret.github.io/GeoThinneR/reference/thin_points.md)
