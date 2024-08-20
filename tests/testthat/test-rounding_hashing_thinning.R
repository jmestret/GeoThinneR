test_that("rounding_hashing_thinning works with valid input", {
  set.seed(123)
  coordinates <- matrix(runif(20, min = -180, max = 180), ncol = 2) # 10 random points

  # Test with default parameters
  result <- rounding_hashing_thinning(coordinates, thin_dist = 5000, trials = 5)
  expect_equal(length(result[[1]]), nrow(coordinates))
  expect_type(result[[1]], "logical")

  # Test with all_trials = TRUE
  all_results <- rounding_hashing_thinning(coordinates, thin_dist = 5000, trials = 5, all_trials = TRUE)
  expect_equal(length(all_results), 5) # Expecting 5 trials
  expect_type(all_results[[1]], "logical")

  # Test with euclidean distance
  result_euclidean <- rounding_hashing_thinning(coordinates, thin_dist = 5000, trials = 5, euclidean = TRUE)
  expect_equal(length(result_euclidean[[1]]), nrow(coordinates))
  expect_type(result_euclidean[[1]], "logical")
})

test_that("rounding_hashing_thinning handles invalid input", {
  expect_error(rounding_hashing_thinning(matrix(runif(20), ncol = 2), thin_dist = -5000), "`thin_dist` must be a positive number.")
})
