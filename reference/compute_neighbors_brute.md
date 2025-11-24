# Compute Neighbors Using Brute-Force

Computes neighbors for each point in a set of coordinates using a greedy
approach. All pairwise distances are calculated to identify neighbors
within a specified distance threshold.

## Usage

``` r
compute_neighbors_brute(
  coordinates,
  thin_dist,
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

- distance:

  A character string specifying the distance metric to use
  \`c("haversine", "euclidean")\`.

- R:

  A numeric value representing the radius of the Earth in kilometers.
  The default is 6371 km.

## Value

A list where each element corresponds to a point and contains the
indices of its neighbors.

## Examples

``` r
set.seed(123)
coords <- matrix(runif(20, min = -180, max = 180), ncol = 2)

# Compute neighbors using brute fore
neighbors <- compute_neighbors_brute(coords, thin_dist = 10,)
```
