# Generate sample data
set.seed(123)
sample_data <- data.frame(
  lon = runif(100, -180, 180),
  lat = runif(100, -90, 90)
)

test_that("thin_points works with valid input", {
  # Test thinning using default method (distance-based thinning using kd-tree method)
  thinned_data <- thin_points(sample_data,
                              lon_col = "lon",
                              lat_col = "lat",
                              trials = 5,
                              seed = 123)
  expect_equal(length(thinned_data$retained[[1]]), 100) # Check the number of rows in the first trial
})

test_that("thin_points works with tibble and matrix input", {
  skip_if_not_installed("tibble")
  sample_tibble <- tibble::as_tibble(sample_data)

  res_tibble <- thin_points(sample_tibble, trials = 2)
  expect_s3_class(res_tibble, "GeoThinned")
  trial_data <- get_trial(res_tibble, 1)
  expect_s3_class(trial_data, "data.frame")
  expect_true(all(c("lon", "lat") %in% names(trial_data)))

  sample_matrix <- as.matrix(sample_data[, 1:2])
  colnames(sample_matrix) <- NULL
  res_matrix <- thin_points(sample_matrix, lon_col = NULL, lat_col = NULL, trials = 2)
  expect_s3_class(res_matrix, "GeoThinned")
  trial_data <- get_trial(res_matrix, 1)
  expect_s3_class(trial_data, "data.frame")
  expect_equal(ncol(trial_data), 2)
})

test_that("thin_points works with distance thinning", {
  res <- thin_points(sample_data, method = "distance", thin_dist = 10, trials = 2)
  expect_s3_class(res, "GeoThinned")
  expect_equal(length(res$retained), 1)
  expect_equal(nrow(get_trial(res, 1)), sum(res$retained[[1]]))
})

test_that("thin_points works with grid thinning", {
  res <- thin_points(sample_data, method = "grid", thin_dist = 10, trials = 2)
  expect_s3_class(res, "GeoThinned")
  expect_equal(length(res$retained), 1)
  expect_equal(nrow(get_trial(res, 1)), sum(res$retained[[1]]))
})

test_that("thin_points works with precision thinning", {
  res <- thin_points(sample_data, method = "precision", precision = 2, trials = 2)
  expect_s3_class(res, "GeoThinned")
  expect_equal(length(res$retained), 1)
  expect_equal(nrow(get_trial(res, 1)), sum(res$retained[[1]]))
})

test_that("thin_points works with grouped data", {
  sample_data$species <- sample(c("species_A", "species_B"), 100, replace = TRUE)

  res <- thin_points(sample_data,
                     group_col = "species",
                     method = "distance",
                     thin_dist = 1000,
                     trials = 2)

  expect_s3_class(res, "GeoThinned")
  expect_equal(nrow(res$original_data), 100)
  expect_equal(length(res$retained), 1)
  retained_total <- sum(res$retained[[1]])
  expect_true(retained_total < 100)  # actual thinning occurred
})

test_that("largest() returns correct trial from GeoThinned object", {
  res <- thin_points(sample_data, method = "distance", trials = 1)
  b <- largest(res)
  expect_equal(sum(res$retained[[1]]), nrow(b))
})

test_that("thin_points handles invalid inputs correctly", {
  expect_error(thin_points(NULL), "Specified longitude or latitude columns do not exist")
  expect_error(thin_points(data.frame(x = 1:10, y = 1:10), lon_col = "lon", lat_col = "lat"),
               "Specified longitude or latitude columns do not exist")
  expect_error(thin_points(sample_data, trials = -1), "`trials` must be a positive integer")
  expect_error(thin_points(sample_data, all_trials = "yes"), "`all_trials` must be a logical value")
  expect_error(thin_points(sample_data, group_col = "unknown"), "Specified grouping column does not exist")
})
