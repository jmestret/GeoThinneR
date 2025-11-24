# Estimate Maximum Neighbors for kd-Tree Thinning

This function estimates the maximum value of k (the number of nearest
neighbors) for kd-tree-based thinning by evaluating the densest regions
of a spatial dataset. The function uses a histogram-based binning
approach for efficiency and low memory usage.

## Usage

``` r
estimate_k_max(coordinates, thin_dist, distance = c("haversine", "euclidean"))
```

## Arguments

- coordinates:

  A matrix of spatial coordinates with two columns for longitude and
  latitude.

- thin_dist:

  A positive numeric value representing the thinning distance in
  kilometers. This defines the resolution of the grid used for density
  calculations.

- distance:

  Distance metric used \`c("haversine", "euclidean")\`.

## Value

A numeric value representing the maximum k (number of nearest neighbors)
required for the densest regions in the dataset.

## Details

The function divides the spatial domain into grid cells based on the
specified thinning distance. Grid cell sizes are determined assuming
approximately 111.32 km per degree (latitude/longitude). The function
identifies the densest grid cells and their immediate neighbors to
compute the maximum k value.

## Examples

``` r
# Generate sample data
set.seed(123)
coordinates <- matrix(runif(200, min = -10, max = 10), ncol = 2)

# Estimate k for kd-tree thinning
k_max <- estimate_k_max(coordinates, thin_dist = 50)
print(k_max)
#> [1] 4
```
