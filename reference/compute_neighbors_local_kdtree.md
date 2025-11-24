# Compute Neighbors Using Local kd-Trees

Divides the search area into a grid of local regions and constructs
kd-trees for each region to compute neighbors efficiently. Neighbor
regions are also considered to ensure a complete search.

## Usage

``` r
compute_neighbors_local_kdtree(
  coordinates,
  thin_dist,
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

- distance:

  A character string specifying the distance metric to use
  \`c("haversine", "euclidean")\`.

- R:

  A numeric value representing the radius of the Earth in kilometers.
  The default is 6371 km.

- n_cores:

  An integer specifying the number of cores to use for parallel
  processing. The default is 1.

## Value

A list where each element corresponds to a point and contains the
indices of its neighbors, excluding the point itself.

## Examples

``` r
set.seed(123)
coords <- matrix(runif(20, min = -180, max = 180), ncol = 2)

# Compute neighbors using local kd-trees with Euclidean distance
neighbors <- compute_neighbors_local_kdtree(coords, thin_dist = 10, n_cores = 1)
```
