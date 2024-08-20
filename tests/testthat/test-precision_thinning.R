coordinates <- matrix(c(-123.3656, -123.3657, -123.3658, 48.4284, 48.4285, 48.4286), ncol = 2)
priority <- 1:3

test_that("precision_thinning basic functionality with single trial", {
  result <- precision_thinning(coordinates)

  expect_true(is.logical(result[[1]]))
  expect_equal(length(result[[1]]), nrow(coordinates))
})

test_that("precision_thinning handles different precisions", {
  # Precision = 4
  result_4 <- precision_thinning(coordinates, precision = 3, priority = priority)
  expect_equal(sum(result_4[[1]]), 2)

  # Precision = 2
  result_3 <- precision_thinning(coordinates, precision = 2)
  expect_equal(sum(result_3[[1]]), 1)
})

test_that("precision_thinning returns multiple trials correctly", {
  result <- precision_thinning(coordinates, trials = 5, all_trials = TRUE)

  expect_equal(length(result), 5)
  expect_true(all(sapply(result, is.logical)))
})

test_that("precision_thinning handles identical coordinates", {
  coordinates <- matrix(c(-123.3656, -123.3656, -123.3656, 48.4284, 48.4284, 48.4284), ncol = 2)
  result <- precision_thinning(coordinates, precision = 4)

  expect_equal(sum(result[[1]]), 1)
})

test_that("precision_thinning validates input parameters", {
  expect_error(precision_thinning(coordinates, precision = -1), "`precision` must be a non-negative integer.")
})

test_that("precision_thinning provides consistent results with fixed seed", {
  set.seed(42)
  result1 <- precision_thinning(coordinates, trials = 1, all_trials = FALSE)

  set.seed(42)
  result2 <- precision_thinning(coordinates, trials = 1, all_trials = FALSE)

  expect_equal(result1, result2)
})

test_that("precision_thinning handles a single coordinate", {
  coordinates <- matrix(c(-123.3656, 48.4284), ncol = 2)
  result <- precision_thinning(coordinates)

  expect_equal(length(result[[1]]), 1)
  expect_true(result[[1]])
})

test_that("precision_thinning with wrongly formated priority", {
  expect_error(precision_thinning(coordinates, precision = 2, priority = 1), "'priority' must be a numeric vector with same length as number of points.")
})

