# Compute Neighbors Using kd-Tree

Computes neighbors for each point in a set of coordinates using a
kd-tree for efficient neighbor searches. This method is particularly
useful for large datasets.

## Usage

``` r
compute_neighbors_kdtree(
  coordinates,
  thin_dist,
  k = NULL,
  distance = c("haversine", "euclidean"),
  R = 6371
)
```

## Arguments

- coordinates:

  A matrix of coordinates to thin, with two columns representing
  longitude and latitude.

- thin_dist:

  A positive numeric value representing the thinning distance in
  kilometers.

- k:

  An integer specifying the maximum number of neighbors to consider for
  each point.

- distance:

  A character string specifying the distance metric to use
  \`c("haversine", "euclidean")\`.

- R:

  A numeric value representing the radius of the Earth in kilometers.
  The default is 6371 km.

## Value

A list where each element corresponds to a point and contains the
indices of its neighbors, excluding the point itself.

## Details

This function uses kd-tree (via \`nabor\` package) for efficient spatial
searches. The kd-tree inherently works with Euclidean distances. If
\`"haversine"\` is selected, the function first converts geographic
coordinates to 3D Cartesian coordinates before constructing the kd-tree.

## Examples

``` r
set.seed(123)
coords <- matrix(runif(20, min = -180, max = 180), ncol = 2)

# Compute neighbors using kd-tree
neighbors <- compute_neighbors_kdtree(coords, thin_dist = 10,)
```
