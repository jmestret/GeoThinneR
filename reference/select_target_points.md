# Select Target Number of Points for Spatial Thinning

This function selects a specified number of points from a spatial
dataset while maximizing the distance between selected points.

## Usage

``` r
select_target_points(
  distance_matrix,
  target_points,
  thin_dist,
  trials,
  all_trials = FALSE
)
```

## Arguments

- distance_matrix:

  A matrix of pairwise distances between points.

- target_points:

  An integer specifying the number of points to retain.

- thin_dist:

  A positive numeric value representing the thinning distance in
  kilometers.

- trials:

  A positive integer specifying the number of thinning trials to
  perform. Default is 10.

- all_trials:

  A logical value indicating whether to return results of all attempts
  (\`TRUE\`) or only the best attempt with the most points retained
  (\`FALSE\`). Default is \`FALSE\`.

## Value

A list of logical vectors indicating which points are kept in each trial
if \`all_trials\` is \`TRUE\`; otherwise, a list with a single logical
vector indicating the points kept in the best trial.

## Examples

``` r
# Example distance matrix (3 points)
dist_matrix <- matrix(c(0, 2, 5,
                        2, 0, 3,
                        5, 3, 0), ncol = 3)

# Select 2 points maximizing distance
result <- select_target_points(dist_matrix, target_points = 2,
                              thin_dist = 4, trials = 5, all_trials = TRUE)
```
