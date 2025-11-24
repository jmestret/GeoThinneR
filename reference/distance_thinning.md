# Perform Distance-Based Thinning

This function applies a distance-based thinning algorithm using a
kd-tree or brute-force approach. Two modified algorithms based on
kd-trees (local kd-trees and estimating the maximum number of neighbors)
are implemented which scale better for large datasets. The function
removes points that are closer than a specified distance to each other
while maximizing spatial representation.

## Usage

``` r
distance_thinning(
  coordinates,
  thin_dist = 10,
  trials = 10,
  all_trials = FALSE,
  search_type = c("local_kd_tree", "k_estimation", "kd_tree", "brute"),
  target_points = NULL,
  priority = NULL,
  distance = c("haversine", "euclidean"),
  R = 6371,
  n_cores = 1
)
```

## Arguments

- coordinates:

  A matrix of coordinates to thin, with two columns representing
  longitude and latitude.

- thin_dist:

  A positive numeric value representing the thinning distance in
  kilometers.

- trials:

  An integer specifying the number of trials to run for thinning.
  Default is 10.

- all_trials:

  A logical indicating whether to return results of all attempts
  (\`TRUE\`) or only the best attempt with the most points retained
  (\`FALSE\`). Default is \`FALSE\`.

- search_type:

  A character string indicating the neighbor search method
  \`c("local_kd_tree", "k_estimation", "kd_tree", "brute")\`. The
  default value is \`local_kd_tree\`. See details.

- target_points:

  Optional integer specifying the number of points to retain. If
  \`NULL\` (default), the function tries to maximize the number of
  points retained.

- priority:

  A numeric vector of the same length as the number of points,
  specifying a priority weight for each point. Higher values indicate
  higher importance and are favored when selecting which points to
  retain. Priority is used to guide selection when multiple candidate
  points are otherwise equally valid (e.g., points in the same grid
  cell, with the same rounded coordinates, or with the same number of
  neighbors).

- distance:

  Distance metric to use \`c("haversine", "euclidean")\`. Default is
  Haversine for geographic coordinates.

- R:

  Radius of the Earth in kilometers (default: 6371 km).

- n_cores:

  Number of cores for parallel processing (only for
  \`"local_kd_tree"\`). Default is 1.

## Value

A list. If \`all_trials\` is \`FALSE\`, the list contains a single
logical vector indicating which points are kept in the best trial. If
\`all_trials\` is \`TRUE\`, the list contains a logical vector for each
trial.

## Details

\- \`"kd_tree"\`: Uses a single kd-tree for efficient nearest-neighbor
searches. - \`"local_kd_tree"\`: Builds multiple smaller kd-trees for
better scalability. - \`"k_estimation"\`: Approximates a maximum number
of neighbors per point to reduce search complexity. - \`"brute"\`:
Computes all pairwise distances (inefficient for large datasets).

## Examples

``` r
# Generate sample coordinates
set.seed(123)
result  <- matrix(runif(20, min = -180, max = 180), ncol = 2) # 10 random points

# Perform thinning with local kd-trees
result_partitioned <- distance_thinning(result , thin_dist = 5000, trials = 5,
                                       search_type = "local_kd_tree", all_trials = TRUE)
print(result_partitioned)
#> [[1]]
#>  [1]  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE  TRUE
#> 
#> [[2]]
#>  [1]  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
#> 
#> [[3]]
#>  [1] FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE
#> 
#> [[4]]
#>  [1]  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE FALSE  TRUE
#> 
#> [[5]]
#>  [1] FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE
#> 

# Perform thinning estimating max number of neighbors
result_estimated <- distance_thinning(result , thin_dist = 5000, trials = 5,
                                       search_type = "k_estimation", all_trials = TRUE)
print(result_estimated)
#> [[1]]
#>  [1] FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE
#> 
#> [[2]]
#>  [1] FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE
#> 
#> [[3]]
#>  [1] FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE FALSE FALSE
#> 
#> [[4]]
#>  [1] FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE
#> 
#> [[5]]
#>  [1] FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE
#> 
```
