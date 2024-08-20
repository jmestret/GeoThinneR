coordinates <- data.frame(lon = c(-73.935242, -74.0060, -73.5673, -73.935242),
                          lat = c(40.730610, 40.7128, 45.4215, 40.730610))
coordinates <- as.matrix(coordinates)

test_that("kd_tree_thinning basic functionality without space partitioning", {
  result <- kd_tree_thinning(coordinates, thin_dist = 10)

  expect_true(is.logical(result[[1]]))
  expect_equal(length(result[[1]]), nrow(coordinates))
})

test_that("kd_tree_thinning with space partitioning", {
  result <- kd_tree_thinning(coordinates, thin_dist = 10, space_partitioning = TRUE)

  expect_true(is.logical(result[[1]]))
  expect_equal(length(result[[1]]), nrow(coordinates))
})

test_that("kd_tree_thinning returns multiple trials correctly", {
  result <- kd_tree_thinning(coordinates, thin_dist = 10, trials = 5, all_trials = TRUE, euclidean = TRUE)

  expect_equal(length(result), 5)
  expect_true(all(sapply(result, is.logical)))
})

test_that("kd_tree_thinning handles identical coordinates", {
  coordinates <- data.frame(lon = c(-73.935242, -73.935242, -73.935242),
                            lat = c(40.730610, 40.730610, 40.730610))
  coordinates <- as.matrix(coordinates)

  result <- kd_tree_thinning(coordinates, thin_dist = 10)

  expect_equal(sum(result[[1]]), 1)
})

test_that("kd_tree_thinning validates input parameters", {
  expect_error(kd_tree_thinning(coordinates, thin_dist = -1), "`thin_dist` must be a positive number.")
  expect_error(kd_tree_thinning(coordinates, space_partitioning = "yes"), "must be a logical value")
})

test_that("kd_tree_thinning provides consistent results with fixed seed", {
  set.seed(42)
  result1 <- kd_tree_thinning(coordinates, thin_dist = 10, trials = 1, all_trials = FALSE)

  set.seed(42)
  result2 <- kd_tree_thinning(coordinates, thin_dist = 10, trials = 1, all_trials = FALSE)

  expect_equal(result1, result2)
})

test_that("kd_tree_thinning handles a single coordinate", {
  coordinates <- data.frame(lon = c(-73.935242),
                            lat = c(40.730610))
  coordinates <- as.matrix(coordinates)

  result <- kd_tree_thinning(coordinates, thin_dist = 10)

  expect_equal(length(result[[1]]), 1)
  expect_true(result[[1]])
})
