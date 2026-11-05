make_estimator_data <- function(n_groups = 2, n_per_group = 24) {
  groups <- paste0("sp", seq_len(n_groups))
  do.call(rbind, lapply(seq_along(groups), function(i) {
    offset <- seq(0.002, 0.048, length.out = n_per_group)
    data.frame(
      lon = -120 + i * 20 + offset,
      lat = -30 + i * 10 + offset,
      species = groups[[i]],
      temp = i + seq_along(offset) / 100
    )
  }))
}

make_bdtp_example_data <- function() {
  set.seed(42)
  species_names <- paste0("sp", 1:6)
  spreads <- c(0.30, 0.45, 0.65, 0.95, 1.35, 1.85)

  do.call(rbind, lapply(seq_along(species_names), function(i) {
    n <- 80
    center_lon <- -140 + 35 * (i - 1)
    center_lat <- -25 + 10 * (i - 1)
    lon <- center_lon + runif(n, -spreads[i] / 2, spreads[i] / 2)
    lat <- center_lat + runif(n, -spreads[i] / 2, spreads[i] / 2)

    data.frame(
      lon = lon,
      lat = lat,
      species = species_names[i],
      temperature = 16 + i + 0.8 * (lon - center_lon) / spreads[i] + rnorm(n, sd = 1.4)
    )
  }))
}

make_stabilization_summary <- function(median_trajectory, interval_width = 0.2) {
  data.frame(
    n_obs = seq_along(median_trajectory),
    median_trajectory = median_trajectory,
    interval_lower = median_trajectory - interval_width / 2,
    interval_upper = median_trajectory + interval_width / 2
  )
}

make_lstp_stability <- function(group, resolution, stability_reached, n_retained = 20, n_env_finite = 18) {
  data.frame(
    group = group,
    resolution = resolution,
    n_retained = rep(n_retained, length(resolution)),
    n_env_finite = rep(n_env_finite, length(resolution)),
    stabilization_threshold = ifelse(stability_reached, 10, NA),
    stabilization_fraction = ifelse(stability_reached, 0.5, NA),
    interval_width_at_stabilization = ifelse(stability_reached, 0.2, NA),
    stability_reached = stability_reached,
    stringsAsFactors = FALSE
  )
}

make_rng_data <- function() {
  cluster_lon <- rep(c(0, 2, 4, 6), each = 3)
  data.frame(
    lon = rep(cluster_lon, 2),
    lat = c(rep(40, 12), rep(42, 12)),
    species = rep(c("species_b", "species_a"), each = 12),
    temp = c(2, 7, 13, 19, 29, 37, 43, 53, 61, 67, 71, 79, 3, 11, 17, 23, 31, 41, 47, 59, 73, 83, 89, 97)
  )
}

make_bdtp_svm_fixture <- function(duplicate_diameters = FALSE) {
  diameter <- if (duplicate_diameters) {
    c(100, 100, 200, 300, 400, 500, 600, 700)
  } else {
    seq(100, 800, by = 100)
  }
  range_descriptors <- data.frame(
    group = paste0("sp", 1:8),
    range_resolution = 0.5,
    occupied_cells = 1:8,
    occupied_area_km2 = pi * (diameter / 2)^2,
    range_diameter_km = diameter
  )
  stability <- expand.grid(group = range_descriptors$group, resolution = c(0.1, 0.4, 0.7, 1), stringsAsFactors = FALSE)
  diameter_index <- match(stability$group, range_descriptors$group)
  stability$stability_reached <- with(stability, resolution <= range_descriptors$range_diameter_km[diameter_index] / 1000)
  stability$n_retained <- 21
  stability$n_env_finite <- 21
  stability$n_sampled <- 21
  stability$stability_status <- ifelse(stability$stability_reached, "stable", "stability_not_reached")
  list(stability = stability, range_descriptors = range_descriptors)
}

run_rng_estimator <- function(data, resolutions = c(0.5, 1), seed = NULL) {
  suppressWarnings(estimate_thinning_parameter(
    data,
    group_col = "species",
    env_col = "temp",
    resolutions = resolutions,
    replicates = 4,
    max_samples = 8,
    stable_window = 2,
    tail_n = 2,
    keep_curves = TRUE,
    seed = seed
  ))
}


# Public interface
test_that("basic inputs and results", {
  dat <- make_estimator_data()
  fit <- estimate_thinning_parameter(
    dat, group_col = "species", env_col = "temp",
    resolutions = c(0.001, 5), stable_window = 2, tail_n = 2,
    seed = 123
  )

  expect_s3_class(fit, "GeoThinningParameter")
  expect_named(fit$estimated_parameters, c("sp1", "sp2"))
  expect_equal(unname(fit$estimated_parameters), fit$estimates$estimated_parameter)
  expect_true(all(c("stability_reached", "stability_status", "n_sampled") %in% names(fit$stability)))
  expect_identical(as.data.frame(fit), fit$estimates)
  expect_s3_class(summary(fit), "summary.GeoThinningParameter")
  expect_output(print(fit), "Estimated thinning resolutions")
})

test_that("invalid inputs are rejected", {
  dat <- make_estimator_data(1)
  expect_error(estimate_thinning_parameter(dat), "env_col")
  expect_error(estimate_thinning_parameter(dat, env_col = "missing"), "missing")
  expect_error(estimate_thinning_parameter(dat, env_col = "lon"), "distinct")
  expect_error(estimate_thinning_parameter(dat, env_col = "temp", replicates = 1), "at least 2")
  expect_error(estimate_thinning_parameter(dat, env_col = "temp", stable_window = 0), "positive integer")
  expect_error(estimate_thinning_parameter(dat, env_col = "temp", seed = NA), "finite integer")
  dat$lon[1] <- 181
  expect_error(estimate_thinning_parameter(dat, env_col = "temp"), "Longitude")
})


test_that("invalid estimator arguments are rejected", {
  dat <- make_estimator_data(1)

  expect_error(estimate_thinning_parameter(dat[FALSE, ], env_col = "temp"), "at least one row")
  expect_error(estimate_thinning_parameter(dat, env_col = "temp", origin = 1), "origin")
  expect_error(estimate_thinning_parameter(dat, env_col = ""), "nonempty")

  duplicate <- dat
  duplicate$extra <- duplicate$lon
  names(duplicate)[names(duplicate) == "extra"] <- "lon"
  expect_error(estimate_thinning_parameter(duplicate, env_col = "temp"),"duplicated names")

  bad <- dat
  bad$lon[1] <- NA
  expect_error(estimate_thinning_parameter(bad, env_col = "temp"),"finite values")

  bad <- dat
  bad$temp <- as.character(bad$temp)
  expect_error(estimate_thinning_parameter(bad, env_col = "temp"),"numeric environmental")

  bad <- dat
  bad$species[1] <- NA
  expect_error(estimate_thinning_parameter(bad, group_col = "species", env_col = "temp"), "missing or blank")
  expect_error(estimate_thinning_parameter(dat, env_col = "temp", resolutions = numeric(0)), "finite values")
  expect_error(estimate_thinning_parameter(dat, env_col = "temp", resolutions = 0), "positive values")
  expect_error(estimate_thinning_parameter(dat, env_col = "temp", median_tolerance = -1), "non-negative")
  expect_error(estimate_thinning_parameter(dat, env_col = "temp", interval_tolerance = -1), "non-negative")
  expect_error(estimate_thinning_parameter( dat, group_col = "species", env_col = "temp", param_method = "bd-tp", range_resolution = 0 ), "positive number")
  expect_error(estimate_thinning_parameter(dat, env_col = "temp", param_method = "bd-tp"),"group_col.*required")
  expect_error(estimate_thinning_parameter(dat, env_col = "temp", keep_curves = NA), "logical value")
  expect_error(estimate_thinning_parameter(dat, env_col = "temp", verbose = NA), "logical value")
  expect_error(estimate_thinning_parameter(dat, env_col = "temp", resolutions = 0.0001), "not be below 0.001")
})


# Stability and LS-TP
test_that("cumulative medians start at two observations", {
  set.seed(123)
  curves <- GeoThinneR:::estimate_tp_stabilize_values(c(2, 10, 20, 30), replicates = 1, max_samples = 4)

  expect_equal(curves$replicate, rep(1L, 4))
  expect_equal(curves$n_obs, 1:4)
  expect_true(is.na(curves$cumulative_median[[1]]))
  expect_equal(curves$cumulative_median[-1], c(25, 20, 15))
})

test_that("replicate intervals follow the manuscript formula", {
  curves <- data.frame(
    replicate = rep(1:10, each = 2),
    n_obs = rep(1:2, times = 10),
    cumulative_median = as.vector(rbind(rep(NA_real_, 10), c(NA, 1:9)))
  )

  result <- GeoThinneR:::estimate_tp_summarise_replicates(curves)
  expected_sd <- sqrt(7.5)
  expected_se <- expected_sd / sqrt(10)

  expect_named(result, c("n_obs", "median_trajectory", "trajectory_sd", "replicates", "trajectory_se", "interval_lower", "interval_upper"))
  expect_equal(result$replicates, c(10, 10))
  expect_true(is.na(result$median_trajectory[[1]]))
  expect_equal(result$median_trajectory[[2]], 5)
  expect_equal(result$trajectory_sd[[2]], expected_sd)
  expect_equal(result$trajectory_se[[2]], expected_se)
  expect_equal(result$interval_lower[[2]], 5 - 1.96 * expected_se)
  expect_equal(result$interval_upper[[2]], 5 + 1.96 * expected_se)
})

test_that("constant trajectories meet the first valid default run", {
  set.seed(123)
  curves <- GeoThinneR:::estimate_tp_stabilize_values(rep(7, 21))
  result <- GeoThinneR:::estimate_tp_summarise_replicates(curves)
  threshold <- GeoThinneR:::estimate_tp_compute_threshold(result)

  expect_true(threshold$stability_reached)
  expect_equal(threshold$stabilization_threshold, 2)
  expect_equal(threshold$stabilization_fraction, 2 / 21)
  expect_equal(threshold$reference_value, 7)
  expect_equal(threshold$interval_width_at_stabilization, 0)
})

test_that("terminal reference uses the last available finite values", {
  result <- make_stabilization_summary(c(NA, 2, 2, 2, 2), interval_width = 0.1)
  threshold <- GeoThinneR:::estimate_tp_compute_threshold(
    result,
    median_tolerance = 0,
    interval_tolerance = 0.2,
    stable_window = 4,
    tail_n = 20
  )

  expect_equal(threshold$reference_value, 2)
  expect_true(threshold$stability_reached)
  expect_equal(threshold$stabilization_threshold, 2)
})

test_that("the first qualifying stability run is selected", {
  qualifying <- make_stabilization_summary(c(NA, rep(9, 5), rep(10, 24)))
  threshold <- GeoThinneR:::estimate_tp_compute_threshold(qualifying)
  expect_true(threshold$stability_reached)
  expect_equal(threshold$stabilization_threshold, 7)
  expect_equal(threshold$reference_value, 10)

  oscillating <- make_stabilization_summary(c(NA, rep(c(0, 1), length.out = 29)), interval_width = 0.6)
  absent <- GeoThinneR:::estimate_tp_compute_threshold(oscillating)
  expect_false(absent$stability_reached)
  expect_true(is.na(absent$stabilization_threshold))
})

test_that("stability status distinguishes untested cases", {
  make_data <- function(values) {
    data.frame(
      lon = seq(-10, -9.95, length.out = length(values)),
      lat = rep(40, length(values)),
      temp = values
    )
  }
  estimate_once <- function(values, stable_window) {
    suppressWarnings(estimate_thinning_parameter(
      make_data(values),
      env_col = "temp",
      resolutions = 0.001,
      replicates = 3,
      median_tolerance = 0,
      interval_tolerance = 0,
      stable_window = stable_window,
      tail_n = 1
    ))
  }

  stable <- estimate_once(rep(10, 6), 5)
  not_reached <- estimate_once(seq_len(6), 5)
  insufficient <- estimate_once(rep(10, 6), 6)

  expect_identical(stable$stability$stability_status, "stable")
  expect_identical(not_reached$stability$stability_status, "stability_not_reached")
  expect_identical(insufficient$stability$stability_status, "insufficient_observations")
  expect_identical(
    c(stable$stability$stability_reached,
      not_reached$stability$stability_reached,
      insufficient$stability$stability_reached),
    c(TRUE, FALSE, FALSE)
  )
})

test_that("LS-TP selects the largest stable resolution", {
  stability <- make_lstp_stability(rep("species_a", 5), c(0.001, 0.01, 0.1, 1, 2.5), c(FALSE, TRUE, FALSE, TRUE, FALSE))

  selected <- GeoThinneR:::estimate_tp_select_lstp(stability)

  expect_equal(selected$estimated_parameter, 1)
  expect_equal(selected$selection_status, "stable_parameter_selected")
  expect_equal(selected$stabilization_threshold, 10)

  two_groups <- rbind(
    make_lstp_stability(rep("species_a", 3), c(0.001, 0.01, 0.1), c(TRUE, TRUE, TRUE)),
    make_lstp_stability(rep("species_b", 3), c(0.001, 0.01, 0.1), c(TRUE, TRUE, FALSE))
  )
  selected_groups <- GeoThinneR:::estimate_tp_select_lstp(two_groups)
  expect_equal(selected_groups$estimated_parameter, c(0.1, 0.01))
})

test_that("a stable minimum differs from a fallback", {
  stability <- rbind(
    make_lstp_stability(rep("stable_minimum", 2), c(0.001, 0.01), c(TRUE, FALSE), n_retained = 12, n_env_finite = 10),
    make_lstp_stability(rep("no_stability", 2), c(0.001, 0.01), c(FALSE, FALSE), n_retained = 8, n_env_finite = 6)
  )

  selected <- GeoThinneR:::estimate_tp_select_lstp(stability)

  expect_identical(selected$group, c("stable_minimum", "no_stability"))
  expect_equal(selected$estimated_parameter, c(0.001, 0.001))
  expect_equal(selected$selection_status, c("stable_parameter_selected", "no_stable_parameter"))
  expect_equal(selected[[5]], c(12L, 8))
  expect_equal(selected[[6]], c(10L, 6))
})

test_that("missing environmental values and LS-TP fallback are reported", {
  dat <- data.frame(
    lon = seq(0, 0.08, by = 0.01),
    lat = rep(40, 9),
    species = c(rep("finite_species", 5), rep("missing_species", 4)),
    temp = c(1, 1, 1, NA, Inf, NA, NA, NA, NA)
  )

  warnings <- character()
  result <- withCallingHandlers(
    estimate_thinning_parameter(
      dat,
      group_col = "species",
      env_col = "temp",
      resolutions = 0.001,
      replicates = 3,
      max_samples = 10,
      stable_window = 2,
      tail_n = 2
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(result$stability$n_retained, c(5, 4))
  expect_equal(result$stability$n_env_finite, c(3, 0))
  expect_equal(result$estimates$estimated_parameter, c(0.001, 0.001))
  expect_equal(result$estimates$selection_status, c("stable_parameter_selected", "no_stable_parameter"))
  expect_length(warnings, 1)
  expect_match(warnings, "1 group has no stable parameter identified")
})


# Random sampling
test_that("sampling uses one continuous RNG stream", {
  data <- make_rng_data()
  resolutions <- c(0.5, 1)

  set.seed(123)
  thinned <- lapply(resolutions, function(resolution) {
    result <- thin_points(
      data,
      lon_col = "lon",
      lat_col = "lat",
      group_col = "species",
      method = "grid",
      resolution = resolution,
      origin = NULL,
      n = 1,
      trials = 1,
      all_trials = FALSE,
      seed = NULL,
      verbose = FALSE
    )
    data[result$retained[[1]], , drop = FALSE]
  })

  expected <- list()
  for (group in unique(data$species)) {
    for (resolution_i in seq_along(resolutions)) {
      group_data <- thinned[[resolution_i]][thinned[[resolution_i]]$species == group, , drop = FALSE]
      curves <- GeoThinneR:::estimate_tp_stabilize_values(
        group_data$temp,
        replicates = 4,
        max_samples = 8
      )
      curves$group <- group
      curves$resolution <- resolutions[[resolution_i]]
      expected[[paste(group, resolutions[[resolution_i]], sep = "__")]] <- curves
    }
  }
  expected <- do.call(rbind, expected)

  actual <- run_rng_estimator(data, seed = 123)

  expect_equal(actual$curves, expected, ignore_attr = TRUE)
  expect_equal(actual$settings$seed, 123)
})

test_that("explicit and caller seeds produce the same result", {
  data <- make_rng_data()

  explicit <- run_rng_estimator(data, seed = 123)
  set.seed(123)
  ambient <- run_rng_estimator(data, seed = NULL)

  expect_identical(explicit$curves, ambient$curves)
  expect_identical(explicit$trajectories, ambient$trajectories)
  expect_identical(explicit$stability, ambient$stability)
  expect_identical(explicit$estimated_parameters, ambient$estimated_parameters)
  expect_equal(explicit$settings$seed, 123)
  expect_null(ambient$settings$seed)
})


# Geographic ranges
test_that("unique occupied cells and area", {
  skip_if_not_installed("terra")
  dat <- data.frame(
    species = c("equator", "equator", "high"),
    lon = c(20.1, 20.1, 20.1),
    lat = c(0.1, 0.1, 70.1)
  )
  result <- GeoThinneR:::estimate_tp_range_diameter(dat, "lon", "lat", "species", resolution = 0.5)
  grid <- terra::rast(ncols = 720, nrows = 360,
                      xmin = -180, xmax = 180, ymin = -90, ymax = 90,
                      crs = "EPSG:4326")
  cells <- terra::cellFromXY(grid, as.matrix(dat[c(1, 3), c("lon", "lat")]))
  areas <- terra::values(terra::cellSize(grid, unit = "km"), mat = FALSE)[cells]

  expect_equal(result$occupied_cells, c(1, 1))
  expect_equal(result$occupied_area_km2, areas)
  expect_equal(result$range_diameter_km, 2 * sqrt(areas / pi))
  expect_gt(result$occupied_area_km2[1], result$occupied_area_km2[2])
})

test_that("range resolution must align with the global grid", {
  skip_if_not_installed("terra")

  dat <- data.frame(species = "sp1", lon = 0, lat = 0)

  for (resolution in c(0.5, 0.4, 1, 180 / 7)) {
    descriptor <- GeoThinneR:::estimate_tp_range_diameter(dat, "lon", "lat", "species", resolution = resolution)
    expect_identical(descriptor$range_resolution, resolution)
    expect_gt(descriptor$range_diameter_km, 0)
  }

  for (resolution in c(0.7, 1.3)) {
    expect_error(
      GeoThinneR:::estimate_tp_range_diameter(dat, "lon", "lat", "species", resolution = resolution),
      "must divide both 180 degrees of latitude and 360 degrees of longitude"
    )
  }
})


# BD-TP
test_that("BD-TP returns a model and predictions", {
  skip_if_not_installed("e1071")
  fit <- estimate_thinning_parameter(
    make_bdtp_example_data(), group_col = "species",
    env_col = "temperature", param_method = "bd-tp",
    resolutions = c(0.025, 0.05, 0.1, 0.2, 0.4, 0.8),
    replicates = 5, max_samples = 60, seed = 123
  )
  expect_equal(nrow(fit$estimates), 6)
  expect_length(fit$estimated_parameters, 6)
  expect_true(all(is.finite(fit$estimates$estimated_parameter)))
  expect_equal(nrow(fit$model$svm_data), sum(fit$stability$n_sampled >= 3, na.rm = TRUE))
  expect_setequal(as.character(fit$model$svm_data$stab), c("TRUE", "FALSE"))
  expect_output(print(summary(fit)), "BD-TP rule")
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit({
    if (grDevices::dev.cur() > 1) grDevices::dev.off()
    unlink(file)
  }, add = TRUE)
  expect_invisible(plot(fit))
})

test_that("stability needs 21 observations; SVM training needs three", {
  skip_if_not_installed("e1071")

  set.seed(123)
  curves_20 <- GeoThinneR:::estimate_tp_stabilize_values(rep(7, 20), replicates = 2, max_samples = 20)
  summary_20 <- GeoThinneR:::estimate_tp_summarise_replicates(curves_20)
  threshold_20 <- GeoThinneR:::estimate_tp_compute_threshold(summary_20, stable_window = 20, tail_n = 20)

  set.seed(123)
  curves_21 <- GeoThinneR:::estimate_tp_stabilize_values(rep(7, 21), replicates = 2, max_samples = 21)
  summary_21 <- GeoThinneR:::estimate_tp_summarise_replicates(curves_21)
  threshold_21 <- GeoThinneR:::estimate_tp_compute_threshold(summary_21, stable_window = 20, tail_n = 20)

  expect_equal(sum(is.finite(summary_20$median_trajectory)), 19)
  expect_false(threshold_20$stability_reached)
  expect_equal(sum(is.finite(summary_21$median_trajectory)), 20)
  expect_true(threshold_21$stability_reached)

  fixture <- make_bdtp_svm_fixture()
  unstable_rows <- which(!fixture$stability$stability_reached)
  insufficient_row <- unstable_rows[[1]]
  evaluable_unstable_row <- unstable_rows[[2]]
  too_few_row <- unstable_rows[[3]]

  # Twenty samples cannot establish stability, but enter the SVM as negative.
  fixture$stability$stability_status[insufficient_row] <- "insufficient_observations"
  fixture$stability$n_sampled[insufficient_row] <- 20
  fixture$stability$n_env_finite[insufficient_row] <- 20

  # Fewer than three samples are excluded from SVM training.
  fixture$stability$stability_status[too_few_row] <- "insufficient_observations"
  fixture$stability$n_sampled[too_few_row] <- 2
  fixture$stability$n_env_finite[too_few_row] <- 2

  result <- GeoThinneR:::estimate_tp_select_bdtp(fixture$stability, fixture$range_descriptors)
  training_keys <- paste(result$model$svm_data$group, result$model$svm_data$resolution)
  insufficient_key <- paste(fixture$stability$group[insufficient_row], fixture$stability$resolution[insufficient_row])
  too_few_key <- paste(fixture$stability$group[too_few_row], fixture$stability$resolution[too_few_row])
  evaluable_unstable_key <- paste(fixture$stability$group[evaluable_unstable_row], fixture$stability$resolution[evaluable_unstable_row])

  expect_identical(fixture$stability$stability_status[insufficient_row], "insufficient_observations")
  expect_true(insufficient_key %in% training_keys)
  expect_false(too_few_key %in% training_keys)
  expect_true(evaluable_unstable_key %in% training_keys)
  expect_identical(as.character(result$model$svm_data$stab[match(insufficient_key, training_keys)]), "FALSE")
  expect_identical(as.character(result$model$svm_data$stab[match(evaluable_unstable_key, training_keys)]), "FALSE")
  expect_equal(result$model$n_insufficient_in_svm, 1)
})

test_that("BD-TP uses a linear SVM boundary", {
  skip_if_not_installed("e1071")
  fixture <- make_bdtp_svm_fixture()
  result <- GeoThinneR:::estimate_tp_select_bdtp(
    fixture$stability, fixture$range_descriptors
  )
  model <- result$model$svm_model

  expect_equal(model$type, 0)
  expect_equal(model$kernel, 0)
  expect_equal(model$cost, 1)
  expect_false(model$compprob)
  expect_true(all(model$scaled))
  expect_identical(colnames(model$SV), c("range_diameter_km", "resolution"))
  expect_named(result$model$calibration, c("diameter_range_km", "resolution_range", "lower_bound"))
  expect_equal(nrow(result$model$boundary), 2)
  expect_lt(max(abs(result$model$boundary$decision_value)), 1e-10)

  hyperplane <- result$model$hyperplane
  predictions <- stats::predict(model, newdata = result$model$svm_data, decision.values = TRUE)
  manual <- with(
    result$model$svm_data,
    hyperplane$original_weights[["range_diameter_km"]] * range_diameter_km + hyperplane$original_weights[["resolution"]] * resolution + hyperplane$original_constant
  )
  expect_equal(manual, as.numeric(attr(predictions, "decision.values")), tolerance = 1e-10)
})

test_that("BD-TP requires both SVM classes", {
  skip_if_not_installed("e1071")
  fixture <- make_bdtp_svm_fixture()
  one_class <- fixture$stability
  # Make all previously stable combinations unstable.
  # Retain the insufficient-observation classifications.
  one_class$stability_status[one_class$stability_status == "stable"] <- "stability_not_reached"
  one_class$stability_reached[] <- FALSE

  expect_error(GeoThinneR:::estimate_tp_select_bdtp(one_class, fixture$range_descriptors), "requires both stable and unstable")
})

test_that("SVM keeps insufficient cases and duplicate predictors", {
  skip_if_not_installed("e1071")
  fixture <- make_bdtp_svm_fixture(duplicate_diameters = TRUE)
  insufficient <- which(!fixture$stability$stability_reached)[1:2]
  fixture$stability$stability_status[insufficient] <- "insufficient_observations"
  fixture$stability$n_sampled[insufficient] <- 20
  fixture$stability$n_env_finite[insufficient] <- 20
  fit <- GeoThinneR:::estimate_tp_select_bdtp(fixture$stability, fixture$range_descriptors)
  training <- fit$model$svm_data
  expect_equal(nrow(training), nrow(fixture$stability))
  expect_equal(fit$model$n_insufficient_in_svm, 2)
  expect_equal(sum(training$range_diameter_km == 100 & training$resolution == 0.1), 2L)
})

test_that("BD-TP caps predictions and reports insufficient cases", {
  skip_if_not_installed("e1071")
  fixture <- make_bdtp_svm_fixture()
  row <- which(!fixture$stability$stability_reached)[1]
  fixture$stability$stability_status[row] <- "insufficient_observations"
  fixture$stability$n_sampled[row] <- 10
  result <- GeoThinneR:::estimate_tp_select_bdtp(fixture$stability, fixture$range_descriptors)
  estimates <- result$estimates
  expect_equal(estimates$estimated_parameter, pmax(estimates$raw_predicted_resolution, 0.001))
  expect_identical(estimates$lower_bound_applied, estimates$raw_predicted_resolution < 0.001)
  expect_equal(result$model$n_insufficient_in_svm, 1)
  calibration <- result$model$calibration$resolution_range
  expect_identical(estimates$resolution_extrapolated, estimates$raw_predicted_resolution < calibration[1] | estimates$raw_predicted_resolution > calibration[2])
  fit <- GeoThinneR:::new_GeoThinningParameter(
    estimates = estimates, stability = fixture$stability,
    settings = list(param_method = "bd-tp", env_col = "temp"),
    range_descriptors = fixture$range_descriptors, model = result$model
  )
  expect_equal(summary(fit)$n_insufficient_in_svm, 1)
})

# Using the results
test_that("sample cap and sorted candidate resolutions", {
  dat <- make_estimator_data(1, 45)
  fit <- estimate_thinning_parameter(
    dat, env_col = "temp", resolutions = c(0.01, 0.001, 0.01),
    replicates = 2, max_samples = 12, stable_window = 2,
    tail_n = 2, seed = 123
  )
  expect_equal(fit$settings$resolutions, c(0.001, 0.01))
  expect_true(all(fit$stability$n_sampled <= 12, na.rm = TRUE))
  expect_identical(fit$stability$resolution, c(0.001, 0.01))
})

test_that("LS-TP estimates can be used with thin_points", {
  dat <- make_estimator_data(1)
  fit <- estimate_thinning_parameter(
    dat, env_col = "temp", resolutions = 0.001,
    stable_window = 2, tail_n = 2, seed = 123
  )
  thinned <- thin_points(
    dat, method = "grid",
    resolution = unname(fit$estimated_parameters[["all"]]),
    seed = 123, verbose = FALSE
  )
  expect_s3_class(thinned, "GeoThinned")
  expect_true(any(thinned$retained[[1]]))
})

test_that("tolerances control stability", {
  dat <- make_estimator_data(1)
  dat$temp <- sin(seq_len(nrow(dat)) / 3)
  args <- list(data = dat, env_col = "temp", resolutions = 0.001,
               replicates = 5, stable_window = 4, tail_n = 4, seed = 123)
  strict <- suppressWarnings(do.call(
    estimate_thinning_parameter,
    c(args, list(median_tolerance = 0, interval_tolerance = 0))
  ))
  loose <- suppressWarnings(do.call(
    estimate_thinning_parameter,
    c(args, list(median_tolerance = 10, interval_tolerance = 10))
  ))
  expect_false(strict$stability$stability_reached)
  expect_true(loose$stability$stability_reached)
})

test_that("stability plots work with and without replicate curves", {
  dat <- make_estimator_data(1)
  args <- list(data = dat, env_col = "temp", resolutions = 0.001,
               replicates = 3, stable_window = 2, tail_n = 2, seed = 123)
  without <- do.call(estimate_thinning_parameter,
                     c(args, list(keep_curves = FALSE)))
  with <- do.call(estimate_thinning_parameter,
                  c(args, list(keep_curves = TRUE)))
  file <- tempfile(fileext = ".pdf")
  grDevices::pdf(file)
  on.exit({
    if (grDevices::dev.cur() > 1) grDevices::dev.off()
    unlink(file)
  }, add = TRUE)
  expect_invisible(plot(without, show_replicates = FALSE))
  expect_error(plot(without, show_replicates = TRUE), "were not stored")
  expect_invisible(plot(with, show_replicates = TRUE))
})

test_that("plotting without trajectories fails clearly", {
  dat <- data.frame(
    lon = c(0, 0.01), lat = c(40, 40), species = "short", temp = c(1, 2)
  )
  result <- suppressWarnings(estimate_thinning_parameter(
    dat,
    group_col = "species",
    env_col = "temp",
    resolutions = 0.001,
    replicates = 2,
    stable_window = 2,
    tail_n = 2
  ))
  expect_identical(result$stability$stability_status, "insufficient_observations")
  expect_error(plot(result, group = "short"), "No valid summarized stability trajectory")
})


test_that("print and summary report important diagnostics", {
  # More than ten groups, none with enough observations for stability.
  dat <- make_estimator_data(n_groups = 11, n_per_group = 3)

  ls <- suppressWarnings(estimate_thinning_parameter(
    dat,
    group_col = "species",
    env_col = "temp",
    resolutions = 0.001,
    replicates = 2,
    stable_window = 20,
    seed = 123
  ))

  printed <- paste(capture.output(print(ls)), collapse = "\n")
  expect_match(printed, "1 more groups")
  expect_match(printed, "11 groups have no stable tested resolution")

  summarized <- paste(capture.output(print(summary(ls))), collapse = "\n")
  expect_match(summarized, "Stable run starts")
  expect_match(summarized, "Not stable")
  expect_match(summarized, "11 groups have no stable tested resolution")

  # Check the singular fallback message too.
  one <- suppressWarnings(estimate_thinning_parameter(
    make_estimator_data(1, 3),
    env_col = "temp",
    resolutions = 0.001,
    replicates = 2,
    stable_window = 20,
    seed = 123
  ))

  expect_output(print(one), "1 group has no stable tested resolution")
  expect_output(print(summary(one)), "1 group has no stable tested resolution")

  # BD-TP diagnostics
  if (requireNamespace("e1071", quietly = TRUE)) {
    fixture <- make_bdtp_svm_fixture()
    result <- GeoThinneR:::estimate_tp_select_bdtp(fixture$stability, fixture$range_descriptors)

    # Set flags explicitly to test the print method.
    estimates <- result$estimates
    estimates$lower_bound_applied[1] <- TRUE
    estimates$resolution_extrapolated[1] <- TRUE

    bd <- GeoThinneR:::new_GeoThinningParameter(
      estimates = estimates,
      stability = fixture$stability,
      settings = list(param_method = "bd-tp", env_col = "temp"),
      range_descriptors = fixture$range_descriptors,
      model = result$model
    )

    bd_print <- paste(capture.output(print(bd)), collapse = "\n")
    expect_match(bd_print, "Predictions capped at 0.001")
    expect_match(bd_print, "Predictions outside tested range")

    bd_summary <- paste(capture.output(print(summary(bd))), collapse = "\n")
    expect_match(bd_summary, "BD-TP rule")
    expect_match(bd_summary, "SVM training")
    expect_match(bd_summary, "Capped")
    expect_match(bd_summary, "Extrapolated")
  }
})

