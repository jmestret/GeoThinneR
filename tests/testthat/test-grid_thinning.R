coordinates <- matrix(c(-122.4194, 37.7749,
                        -122.4195, 37.7740,
                        -122.4196, 37.7741), ncol = 2, byrow = TRUE)
priority <- 1:3


test_that("grid_thinning works with thin_dist", {
  result <- grid_thinning(coordinates, thin_dist = 10, trials = 3, priority = priority)

  expect_length(result[[1]], nrow(coordinates))
  expect_true(sum(result[[1]]) < nrow(coordinates)) # Should thin some points
})

test_that("grid_thinning works with resolution", {
  result_res <- grid_thinning(coordinates, resolution = 0.01, trials = 3, origin = c(0, 0))

  expect_length(result_res[[1]], nrow(coordinates))
  expect_true(sum(result_res[[1]]) < nrow(coordinates)) # Should thin some points
})

test_that("grid_thinning works with a raster object", {
  library(terra)
  rast_obj <- terra::rast(nrows = 100, ncols = 100, xmin = -123, xmax = -121, ymin = 36, ymax = 38)

  result_raster <- grid_thinning(coordinates, raster_obj = rast_obj, trials = 3)

  expect_length(result_raster[[1]], nrow(coordinates))
  expect_true(sum(result_raster[[1]]) < nrow(coordinates)) # Should thin some points
})

test_that("grid_thinning returns multiple trials when all_trials = TRUE", {
  result <- grid_thinning(coordinates, thin_dist = 10, trials = 3, all_trials = TRUE)

  expect_length(result, 3)
  expect_true(all(sapply(result, length) == nrow(coordinates)))
})

test_that("grid_thinning without raster information", {
  expect_error(grid_thinning(coordinates), "Either thin_dist, resolution, or raster_obj must be provided.")
})

test_that("grid_thinning with wrongly formated priority", {
  expect_error(grid_thinning(coordinates, resolution = 2, priority = 1), "'priority' must be a numeric vector with same length as number of points.")
})

