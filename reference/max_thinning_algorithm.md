# Thinning Algorithm for Spatial Data

This function performs the core thinning algorithm used to reduce the
density of points in spatial data while maintaining spatial
representation. It iteratively removes the points with the most
neighbors until no points with neighbors remain. The algorithm supports
multiple trials to find the optimal thinning solution.

## Usage

``` r
max_thinning_algorithm(
  neighbor_indices,
  trials,
  all_trials = FALSE,
  priority = NULL
)
```

## Arguments

- neighbor_indices:

  A list of integer vectors where each element contains the indices of
  the neighboring points for each point in the dataset.

- trials:

  A positive integer specifying the number of thinning trials to
  perform. Default is 10.

- all_trials:

  A logical value indicating whether to return results of all attempts
  (\`TRUE\`) or only the best attempt with the most points retained
  (\`FALSE\`). Default is \`FALSE\`.

- priority:

  A numeric vector of the same length as the number of points,
  specifying a priority weight for each point. Higher values indicate
  higher importance and are favored when selecting which points to
  retain. Priority is used to guide selection when multiple candidate
  points are otherwise equally valid (e.g., points in the same grid
  cell, with the same rounded coordinates, or with the same number of
  neighbors).

## Value

A list of logical vectors indicating which points are kept in each trial
if all_trials is TRUE; otherwise, a list with a single logical vector
indicating the points kept in the best trial.

## Examples

``` r
# Example usage within a larger thinning function
neighbor_indices <- list(c(2, 3), c(1, 3), c(1, 2))
trials <- 5
all_trials <- FALSE
kept_points <- max_thinning_algorithm(neighbor_indices, trials, all_trials)
print(kept_points)
#> [[1]]
#> [1]  TRUE FALSE FALSE
#> 
```
