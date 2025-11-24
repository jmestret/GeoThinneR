# Precision Thinning of Spatial Points

This function performs thinning of spatial points by rounding their
coordinates to a specified precision and removing duplicates. It can
perform multiple trials of this process and return the results for all
or just the best trial.

## Usage

``` r
precision_thinning(
  coordinates,
  precision = 4,
  trials = 10,
  all_trials = FALSE,
  priority = NULL
)
```

## Arguments

- coordinates:

  A numeric matrix or data frame with two columns representing the
  longitude and latitude of points.

- precision:

  A positive integer specifying the number of decimal places to which
  coordinates should be rounded. Default is 4.

- trials:

  A positive integer specifying the number of thinning trials to
  perform. Default is 10.

- all_trials:

  A logical value indicating whether to return results for all trials
  (\`TRUE\`) or just the first/best trial (\`FALSE\`). Default is
  \`FALSE\`.

- priority:

  A numeric vector of the same length as the number of points,
  specifying a priority weight for each point. Higher values indicate
  higher importance and are favored when selecting which points to
  retain. Priority is used to guide selection when multiple candidate
  points are otherwise equally valid (e.g., points in the same grid
  cell, with the same rounded coordinates, or with the same number of
  neighbors).

## Value

If \`all_trials\` is \`FALSE\`, returns a logical vector indicating
which points were kept in the first trial. If \`all_trials\` is
\`TRUE\`, returns a list of logical vectors, one for each trial.

## Details

The function performs multiple trials to account for randomness in the
order of point selection. By default, it returns the first trial, but
setting \`all_trials = TRUE\` will return the results of all trials.

## Examples

``` r
# Example usage
coords <- matrix(c(-123.3656, 48.4284, -123.3657, 48.4285, -123.3658, 48.4286), ncol = 2)
result <- precision_thinning(coords, precision = 3, trials = 5, all_trials = TRUE)
print(result)
#> [[1]]
#> [1] TRUE TRUE TRUE
#> 
#> [[2]]
#> [1] TRUE TRUE TRUE
#> 
#> [[3]]
#> [1] TRUE TRUE TRUE
#> 
#> [[4]]
#> [1] TRUE TRUE TRUE
#> 
#> [[5]]
#> [1] TRUE TRUE TRUE
#> 

# Example with a single trial and lower precision
result_single <- precision_thinning(coords, precision = 2, trials = 1, all_trials = FALSE)
print(result_single)
#> [[1]]
#> [1] FALSE  TRUE  TRUE
#> 
```
