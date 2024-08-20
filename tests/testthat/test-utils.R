test_that("long_lat_to_cartesian converts coordinates correctly", {
  long <- c(0, 90, -90)
  lat <- c(0, 0, 0)
  result <- long_lat_to_cartesian(long, lat)

  expected <- cbind(
    x = c(6371, 0, 0),
    y = c(0, 6371, -6371),
    z = c(0, 0, 0)
  )

  expect_equal(result, expected, tolerance = 1e-5)
})

test_that("assign_coords_to_grid assigns coordinates to correct grid cells", {
  coords <- data.frame(long = c(0.5, 1.5, 2.5), lat = c(0.5, 1.5, 2.5))
  result <- assign_coords_to_grid(coords, 1)

  expected <- c("0_0", "1_1", "2_2")

  expect_equal(result, expected)
})
