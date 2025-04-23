# Generate sample data
set.seed(123)
sample_data <- data.frame(
  lon = runif(100, 0, 1),
  lat = runif(100, 0, 1)
)

test_that("max_thinning retains maximum number of points", {
  neighbor_indices <- compute_neighbors_brute(as.matrix(sample_data), thin_dist = 10)
  result_max_thinning <- max_thinning_algorithm(neighbor_indices, trials = 10, all_trials = TRUE)

  expect_length(result_max_thinning[[1]], nrow(sample_data))
  expect_true(sum(result_max_thinning[[1]]) < nrow(sample_data)) # Should thin some points
})

test_that("select_target_points returns the exact number of requested points", {
  distance_matrix <- fields::RdistEarth(x1 = as.matrix(sample_data), miles = FALSE)
  result_target_points <- select_target_points(distance_matrix, target_points = 20, thin_dist = 10, trials = 10, all_trials = TRUE)
  expect_true(sum(result_target_points[[1]]) == 20) # Should thin some points

  result_target_points <- select_target_points(distance_matrix, target_points = 20, thin_dist = 10, trials = 10, all_trials = FALSE)
  expect_length(result_target_points[[1]], nrow(sample_data))

  distance_matrix <- fields::RdistEarth(x1 = as.matrix(rbind(sample_data, sample_data)), miles = FALSE)
  result_target_points <- select_target_points(distance_matrix, target_points = 20, thin_dist = 100, trials = 10, all_trials = FALSE)
  expect_length(result_target_points[[1]], 2*nrow(sample_data))
})
