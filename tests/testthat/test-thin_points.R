# Generate sample data
set.seed(123)
sample_data <- data.frame(
  decimalLongitude = runif(100, -180, 180),
  decimalLatitude = runif(100, -90, 90)
)

test_that("thin_points works with valid input", {
  # Test thinning using K-D tree method
  thinned_data <- thin_points(sample_data,
                              long_col = "decimalLongitude",
                              lat_col = "decimalLatitude",
                              method = "kd_tree",
                              trials = 5)
  expect_equal(nrow(thinned_data[[1]]), 100) # Check the number of rows in the first trial

  # Test thinning with grouping
  sample_data$species <- sample(c("species_A", "species_B"), 100, replace = TRUE)
  thinned_grouped_data <- thin_points(sample_data,
                                      long_col = "decimalLongitude",
                                      lat_col = "decimalLatitude",
                                      group_col = "species",
                                      method = "kd_tree",
                                      trials = 10,
                                      verbose = TRUE)
  expect_equal(length(thinned_grouped_data), 1) # Expecting the best attempt
})

test_that("Thinning works with tibble input", {
  sample_tibble <- tibble::as_tibble(sample_data)

  thinned_data_tibble <- thin_points(sample_tibble,
                                     long_col = "decimalLongitude",
                                     lat_col = "decimalLatitude",
                                     method = "kd_tree",
                                     trials = 5)

  expect_equal(length(thinned_data_tibble), 1)
  expect_true("tbl_df" %in% class(thinned_data_tibble[[1]]))
  expect_true(all(c("decimalLongitude", "decimalLatitude") %in% names(thinned_data_tibble[[1]])))
})

test_that("Thinning works with matrix input", {
  sample_matrix <- as.matrix(sample_data[, 1:2])

  thinned_data_matrix <- thin_points(sample_matrix,
                                     long_col = NULL,
                                     lat_col = NULL,
                                     method = "brute_force",
                                     trials = 5)

  expect_equal(length(thinned_data_matrix), 1)
  expect_true("matrix" %in% class(thinned_data_matrix[[1]]))
  expect_true(all(c("long", "lat") %in% colnames(thinned_data_matrix[[1]])))
})

test_that("thin_points handles invalid input", {
  expect_error(thin_points(NULL), "The first two columns must be numeric. Please specify long_col and lat_col arguments")
  expect_error(thin_points(data.frame(x = 1:10, y = 1:10), long_col = "long", lat_col = "lat"),
               "Specified longitude or latitude columns do not exist in the data.")
  expect_error(thin_points(data.frame(decimalLongitude = runif(10), decimalLatitude = runif(10)),
                           trials = -5), "`trials` must be a positive integer.")
  expect_error(thin_points(data.frame(decimalLongitude = runif(10), decimalLatitude = runif(10)),
                           group_col = "nonexistent"), "Specified grouping column does not exist in the data.")
})

