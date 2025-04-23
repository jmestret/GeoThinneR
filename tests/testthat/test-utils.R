coords <- data.frame(
  lon = c(-122.4194, -122.4195, -122.4196, -122.4197),
  lat = c(37.7749, 37.7740, 37.7741, 37.7750)
)
coords <- as.matrix(coords)

test_that("thin_points handles invalid input", {
  expect_error(estimate_k_max (NULL, 3), "`coordinates` must be a matrix")
  expect_error(estimate_k_max(coords, thin_dist = -2), "`thin_dist` must be a positive number.")
})

test_that("calculate_spatial_coverage with Euclidean distance", {
  coords_proj <- matrix(runif(20), ncol = 2) * 100
  area_euclidean <- calculate_spatial_coverage(coords_proj, distance = "euclidean")
  expect_true(area_euclidean > 0)
})
