coords <- data.frame(
  lon = c(-122.4194, -122.4195, -122.4196, -122.4197),
  lat = c(37.7749, 37.7740, 37.7741, 37.7750)
)
coords <- as.matrix(coords)

test_that("brute_force_thinning works with geographic coordinates", {
  result <- brute_force_thinning(coords, thin_dist = 0.1, trials = 1)

  expect_length(result[[1]], nrow(coords))
  expect_true(sum(result[[1]]) < nrow(coords)) # Should thin some points
})

test_that("brute_force_thinning works computing Euclidean distance", {
  result <- brute_force_thinning(coords, thin_dist = 1, trials = 1, euclidean = TRUE)

  expect_length(result[[1]], nrow(coords))
  expect_true(sum(result[[1]]) < nrow(coords)) # Should thin some points
})

test_that("brute_force_thinning returns multiple trials when all_trials = TRUE", {
  result <- brute_force_thinning(coords, thin_dist = 0.1, trials = 3, all_trials = TRUE)

  expect_length(result, 3)
  expect_true(all(sapply(result, length) == nrow(coords)))
})

test_that("brute_force_thinning returns the exact number of points when target_points is not NULL", {
  set.seed(2024)
  result <- brute_force_thinning(coords, thin_dist = 0.1, trials = 3, all_trials = FALSE, target_points = 2)

  expect_length(result[[1]], 4)
  expect_true(sum(result[[1]]) == 2)
})

test_that("brute_force_thinning update keep_points_trial", {
  set.seed(123)
  result <- brute_force_thinning(coords, thin_dist = 0.1, trials = 3, all_trials = FALSE, target_points = 4)

  expect_length(result[[1]], 4)
  expect_true(sum(result[[1]]) == 2)
})

test_that("brute_force_thinning with target_points and all_trials TRUE", {
  set.seed(345)
  result <- brute_force_thinning(coords, thin_dist = 0.1, trials = 3, all_trials = TRUE, target_points = 4)

  expect_length(result[[1]], 4)
  expect_true(sum(result[[1]]) == 2)
})
