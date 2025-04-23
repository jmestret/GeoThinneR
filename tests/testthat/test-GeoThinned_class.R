test_that("GeoThinned object constructor works", {
  data <- data.frame(
    lon = runif(10, -10, 10),
    lat = runif(10, 40, 50)
  )
  retained <- list(sample(c(TRUE, FALSE), 10, replace = TRUE))
  method <- "distance"
  params <- list(lon_col = "lon", lat_col = "lat")

  obj <- as_GeoThinned(retained = retained, method = method, params = params, original_data = data)

  expect_s3_class(obj, "GeoThinned")
  expect_equal(obj$method, method)
  expect_equal(obj$original_data, data)
  expect_type(obj$retained, "list")
  expect_true(all(sapply(obj$retained, is.logical)))
})

test_that("print and summary work for GeoThinned", {
  data <- data.frame(
    lon = runif(10, -10, 10),
    lat = runif(10, 40, 50)
  )
  retained <- list(rep(TRUE, 10))
  obj <- as_GeoThinned(retained, method = "distance", params = list(lon_col = "lon", lat_col = "lat"), original_data = data)

  expect_output(print(obj), "GeoThinned object")
  expect_output(summary(obj), "Summary of GeoThinneR Results")
})

test_that("plot.GeoThinned runs without error", {
  data <- data.frame(
    lon = runif(10, -10, 10),
    lat = runif(10, 40, 50)
  )
  retained <- list(rep(TRUE, 10))
  obj <- as_GeoThinned(retained, method = "distance", params = list(lon_col = "lon", lat_col = "lat"), original_data = data)

  expect_silent(plot(obj))
  expect_silent(plot(obj, show_original = FALSE))
})

test_that("get_trial and largest work", {
  data <- data.frame(
    lon = runif(10, -10, 10),
    lat = runif(10, 40, 50)
  )
  retained <- list(sample(c(TRUE, FALSE), 10, replace = TRUE),
                   sample(c(TRUE, FALSE), 10, replace = TRUE))
  obj <- as_GeoThinned(retained, method = "distance", params = list(lon_col = "lon", lat_col = "lat"), original_data = data)

  expect_true(is.data.frame(get_trial(obj, 1)))
  expect_true(is.data.frame(largest(obj)))
  expect_type(largest_index(obj), "integer")
  expect_gt(largest_index(obj), 0)
})

test_that("as_sf.GeoThinned returns an sf object", {
  data <- data.frame(
    lon = runif(10, -10, 10),
    lat = runif(10, 40, 50)
  )
  retained <- list(rep(TRUE, 10))
  obj <- as_GeoThinned(retained, method = "distance", params = list(lon_col = "lon", lat_col = "lat"), original_data = data)

  result <- as_sf(obj)
  expect_s3_class(result, "sf")
  expect_true("geometry" %in% colnames(result))
})
