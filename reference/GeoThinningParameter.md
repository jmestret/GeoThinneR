# Methods for GeoThinningParameter Objects

Inspect, summarize, extract, and plot the result of
[`estimate_thinning_parameter()`](https://jmestret.github.io/GeoThinneR/reference/estimate_thinning_parameter.md).
The main result is the named numeric vector \`estimated_parameters\`;
the complete group-level diagnostics are in \`estimates\`.

## Usage

``` r
# S3 method for class 'GeoThinningParameter'
print(x, ...)

# S3 method for class 'GeoThinningParameter'
summary(object, ...)

# S3 method for class 'summary.GeoThinningParameter'
print(x, ...)

# S3 method for class 'GeoThinningParameter'
as.data.frame(x, ...)

# S3 method for class 'GeoThinningParameter'
plot(
  x,
  type = c("auto", "stability", "bdtp"),
  group = NULL,
  resolution = NULL,
  show_replicates = !is.null(x$curves),
  show_ci = TRUE,
  show_threshold = TRUE,
  ...
)
```

## Arguments

- x, object:

  A \`GeoThinningParameter\` object.

- ...:

  Additional arguments passed to base graphics or data frame methods.

- type:

  \`"auto"\`, \`"stability"\`, or \`"bdtp"\`. The stability plot shows
  cumulative-median trajectories; the BD-TP plot shows SVM training
  data, the decision boundary, and predictions. The default is based on
  the fitted method.

- group:

  Optional group to plot. If \`NULL\`, the first group with an available
  trajectory is used. In BD-TP plots, selected groups are highlighted.

- resolution:

  Candidate resolutions to show in stability plots. If \`NULL\`, all
  available resolutions are plotted.

- show_replicates:

  Show individual trajectories if \`keep_curves = TRUE\`.

- show_ci:

  Show the summarized trajectory interval.

- show_threshold:

  Show the stabilization threshold and

## Value

\`print()\` and \`plot()\` return the input object invisibly.
\`summary()\` returns a compact summary object. \`as.data.frame()\`
returns the complete \`estimates\` table.

## Details

For LS-TP, \`print()\` and \`summary()\` report how many groups reached
stability. When no tested resolution is stable, the function returns
0.001 degrees as a default value. This does not mean that stability was
achieved.

For BD-TP, the results indicate whether a predicted resolution was
limited to the minimum of 0.001 degrees or falls outside the tested
resolution range. BD-TP estimates resolutions from geographical range
size, so its predictions are not necessarily among those tested.

Stability plots show the summarized cumulative-median trajectories, even
when individual replicate curves were not saved. If there are too few
environmental observations to calculate a trajectory, an informative
message is displayed.

In BD-TP plots, overlapping training points are slightly separated to
make them visible. This adjustment affects only the plot, not the
underlying data, fitted model, or predicted resolutions. Plotting is
reproducible and does not use random numbers.
