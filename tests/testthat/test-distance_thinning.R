set.seed(123)

coords <- data.frame(
  lon = c(-122.4194, -122.4195, -122.4196, -122.4197),
  lat = c(37.7749, 37.7740, 37.7741, 37.7750)
)
coords <- as.matrix(coords)

coords2 <- matrix(runif(100, 0, 0.2), ncol=2)

test_that("thin_points returns a GeoThinned object", {
  result <- thin_points(data.frame(lon = runif(10), lat = runif(10)), method = "distance", thin_dist = 1)
  expect_s3_class(result, "GeoThinned")
})

test_that("thin_points handles invalid input", {
  expect_error(distance_thinning(NULL), "`coordinates` must be a matrix")
  expect_error(distance_thinning(coords, thin_dist = -2), "`thin_dist` must be a positive number.")
  expect_error(thin_points(data.frame(lon = runif(10), lat = runif(10)),
                           trials = -5), "`trials` must be a positive integer.")
  expect_error(thin_points(NULL, all_trials = "Yes"), "`all_trials` must be a logical value.")

  expect_error(thin_points(data.frame(lon = runif(10), lat = runif(10)),
                           group_col = "nonexistent"), "Specified grouping column does not exist in the data.")
})

test_that("distance_thinning (brute method) works with geographic coordinates", {
  result <- distance_thinning(coords, search_type = "brute", thin_dist = 0.1, trials = 1)

  expect_length(result[[1]], nrow(coords))
  expect_true(sum(result[[1]]) < nrow(coords)) # Should thin some points
})

test_that("distance_thinning (brute method) works computing Euclidean distance", {
  result <- distance_thinning(coords, search_type = "brute", thin_dist = 1, trials = 1, distance = "euclidean")

  expect_length(result[[1]], nrow(coords))
  expect_true(sum(result[[1]]) < nrow(coords)) # Should thin some points
})

test_that("distance_thinning (brute method) returns the exact number of points when target_points is not NULL", {
  set.seed(2024)
  result <- suppressMessages(distance_thinning(coords, search_type = "brute", thin_dist = 0.1, trials = 3, all_trials = FALSE, target_points = 2, distance = "euclidean"))
  result <- suppressMessages(distance_thinning(coords, search_type = "brute", thin_dist = 0.1, trials = 3, all_trials = FALSE, target_points = 2))

  expect_length(result[[1]], 4)
  expect_equal(sum(result[[1]]), 2)
})

test_that("distance_thinning returns all trials correctly", {
  result <- distance_thinning(coords, search_type = "brute", thin_dist = 0.1, trials = 3, all_trials = TRUE)
  expect_equal(length(result), 3)
  expect_true(all(sapply(result, length) == nrow(coords)))
})

test_that("distance_thinning handles small datasets", {
  small_coords <- coords[1:2, ]
  result <- distance_thinning(small_coords, search_type = "brute", thin_dist = 0.1, trials = 1)
  expect_length(result[[1]], 2)
})

test_that("kd-tree thinning works", {
  result <- distance_thinning(coords, search_type = "kd_tree", thin_dist = 0.1, trials = 1)
  expect_length(result[[1]], nrow(coords))
  expect_true(sum(result[[1]]) < nrow(coords)) # Should thin some points

  result <- distance_thinning(coords, search_type = "kd_tree", thin_dist = 0.1, trials = 1, distance = "euclidean")
  expect_length(result[[1]], nrow(coords))
  expect_true(sum(result[[1]]) < nrow(coords)) # Should thin some points
})

test_that("kd-tree k_max estimation", {
  result <- distance_thinning(coords, search_type = "k_estimation", thin_dist = 0.1, trials = 1)
  expect_length(result[[1]], nrow(coords))
  expect_true(sum(result[[1]]) < nrow(coords)) # Should thin some points
})

test_that("local kd-trees and parallelization", {
  result <- distance_thinning(coords, search_type = "local_kd_tree", thin_dist = 0.1, trials = 1, n_cores = 2)
  expect_length(result[[1]], nrow(coords))
  expect_true(sum(result[[1]]) < nrow(coords)) # Should thin some points

  result <- distance_thinning(coords, search_type = "local_kd_tree", thin_dist = 0.1, trials = 1, distance = "euclidean")
  expect_length(result[[1]], nrow(coords))
  expect_true(sum(result[[1]]) < nrow(coords)) # Should thin some points

  expect_error(distance_thinning(coords, search_type = "local_kd_tree", thin_dist = 0.1, trials = 1, n_cores = 100),
               "`n_cores` must be smaller than the available number of cores")

})

test_that("All methods find same neighbors", {
  thin_dist <- 10
  brute <- compute_neighbors_brute(coords2, thin_dist)
  kd_tree <- compute_neighbors_kdtree(coords2, thin_dist)
  k_estimation <- compute_neighbors_kdtree(coords2, thin_dist, estimate_k_max(coords2, thin_dist))
  local_kd_tree <- compute_neighbors_local_kdtree(coords2, thin_dist)

  brute <- lapply(brute, sort)
  kd_tree <- lapply(kd_tree, sort)
  k_estimation <- lapply(k_estimation, sort)
  local_kd_tree <- lapply(local_kd_tree, sort)

  expect_true(identical(brute, kd_tree))
  expect_true(identical(brute, k_estimation))
  expect_true(identical(brute, local_kd_tree))
})


test_that("distance_thinning works with priority", {
  priority <- 1:nrow(coords)
  result_priority <- distance_thinning(coords, search_type = "brute", thin_dist = 0.1, trials = 1, priority = priority)

  # distance_thinning with wrongly formated priority
  expect_error(distance_thinning(coords, search_type = "brute", thin_dist = 0.1, trials = 1, priority = 1), "'priority' must be a numeric vector with same length as number of points.")
  expect_warning(distance_thinning(coords, search_type = "brute", thin_dist = 0.1, trials = 1, priority = c(NA, 2:nrow(coords))), "NA values found in 'priority'.")

  expect_length(result_priority[[1]], nrow(coords))
  expect_true(sum(result_priority[[1]]) < nrow(coords))
})
