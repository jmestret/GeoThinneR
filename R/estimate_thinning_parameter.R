#' Estimate Spatial Thinning Parameters
#'
#' Estimates grid-based spatial thinning parameters by evaluating the
#' stability of the cumulative median of an environmental variable
#' across candidate grid resolutions. Parameters are selected using
#' the Last Stable Thinning Parameterization (LS-TP) or predicted
#' from geographic range size using the Biogeographic-size Dependent
#' Thinning Parameterization (BD-TP) (Ibáñez et al. 2026).
#'
#' @param data A data frame containing the input points to thin. Must contain longitude and latitude coordinates, one numeric environmental variable, and optionally a grouping column.
#' @param lon_col,lat_col Longitude and latitude column names, in decimal degrees (WGS84). Defaults are `"lon"` and `"lat"`.
#' @param group_col Optional grouping column (e.g., species). If `NULL`, all records are treated as one group.
#' @param env_col A character name of the column with the numeric environmental variable used to evaluate stability. Tolerances use the units of this variable.
#' @param param_method Parameterization method to use. One of `"ls-tp"` (Last Stable Thinning Parameterization) or `"bd-tp"` (Biogeographic-size Dependent Thinning Parameterization).
#' @param range_resolution Resolution in degrees of the global WGS84 occupancy grid used to calculate physical occupied-cell area for BD-TP (default: `0.5`). The resolution must divide both 180 degrees of latitude and 360 degrees of longitude into whole cells. Only used when `param_method = "bd-tp"`.
#' @param resolutions Numeric vector of candidate grid resolutions, in degrees, to evaluate.
#' @param replicates Number of random sampling replicates used to estimate cumulative moving median trajectories (default: `10`).
#' @param max_samples Maximum number of retained observations sampled without replacement per group and resolution (default: `500`).
#' @param median_tolerance Maximum absolute difference, in the units of the environmental variable `env_col`, between the median trajectory and terminal reference (default `0.3`).
#' @param interval_tolerance Maximum trajectory-interval width, in the units of the environmental variable `env_col` (default `0.5`).
#' @param stable_window Minimum number of consecutive observation counts required for stability (default: `20`).
#' @param tail_n Number of final observation counts used to calculate the terminal reference (default: `20`).
#' @param seed Optional integer seed for reproducibility.
#' @param keep_curves Logical. Whether to retain replicate-level moving-median curves. Summarized trajectories needed for stability plots are always retained. Default is `FALSE`.
#' @param verbose Show a progress bar if `TRUE`.
#' @param origin Optional two-value grid origin. Changing the origin may change cell assignments and estimates.
#' @param n Number of records retained per grid cell. This is fixed at `1` and is an explicit argument to prevent partial matching with `replicates`.
#'
#' @details
#' For every candidate resolution, one grid-thinning realization is generated.
#' Up to `max_samples` retained values are sampled without replacement in
#' `replicates` random orders. Their cumulative medians are summarized at each
#' observation count by the replicate median and by
#' `median +/- 1.96 * sd / sqrt(B)`. The standard deviation is calculated across
#' replicate cumulative medians and `B` is the replicate count. This interval is
#' a consistency measure for randomized trajectories.
#'
#' The terminal reference is the mean of up to the final `tail_n`
#' median trajectory values. `stabilization_threshold` is the first observation
#' count in the earliest run of `stable_window` consecutive positions meeting
#' both tolerances. The trajectory at observation count 1 is undefined, so
#' at least `stable_window + 1` observations are needed to assess stability.
#' Shorter trajectories are marked `"insufficient_observations"` in `stability`.
#'
#' LS-TP selects the largest tested stable resolution. If none is stable,
#' it returns `0.001` degrees with `selection_status = "no_stable_parameter"`.
#' This is a fallback, not a validated stable resolution.
#'
#' BD-TP uses the areas of occupied cell calculated with `terra::cellSize()` and a
#' linear SVM to predict a resolution from geographic range size. Combinations
#' with at least three sampled finite environmental values enter the SVM.
#' Those without demonstrated stability, including insufficient combinations,
#' form the stability not demonstrated class. Their original status remains in `stability`.
#' Predicted resolutions are bounded below at `0.001` degrees;
#' `resolution_extrapolated` marks predictions outside the tested range.
#'
#' Estimated parameters are not guaranteed to eliminate sampling bias or retain
#' the complete ecological niche. Estimation and application are separate:
#' subsequently call `thin_points()` with one estimated resolution.
#'
#' @return A `GeoThinningParameter` object containing:
#' \itemize{
#'   \item `estimated_parameters`: main named numeric estimates.
#'   \item `estimates`: group-level estimates and diagnostics.
#'   \item `stability`: one row per group and candidate resolution.
#'   \item `settings`: reproducibility and methodological settings.
#'   \item `trajectories`: summarized cumulative-median trajectories.
#'   \item `curves`: optional replicate trajectories.
#'   \item `range_descriptors`: BD-TP geographical descriptors, otherwise `NULL`.
#'   \item `model`: BD-TP SVM and boundary, otherwise `NULL`.
#'   \item `call`: matched function call.
#' }
#'
#' LS-TP `estimates` contain `group`, `estimated_parameter`,
#' `stabilization_threshold`, `stabilization_fraction`, `n_retained`,
#' `n_env_finite`, `interval_width_at_stabilization`, and `selection_status`.
#'
#' BD-TP `estimates` contain `group`, `estimated_parameter`,
#' `raw_predicted_resolution`, `range_diameter_km`, `any_stable_resolution`,
#' `lower_bound_applied`, and `resolution_extrapolated`.
#'
#' `stability` contains `group`, `resolution`, `n_retained`, `n_env_finite`,
#' `n_sampled`, `stabilization_threshold`, `stabilization_fraction`,
#' `reference_value`, `interval_width_at_stabilization`, `stability_reached`, and
#' `stability_status`. BD-TP additionally includes `range_diameter_km`.
#' `n_sampled` is the maximum cumulative sample size used, not the number of
#' original occurrence records. `stability_status` is one of `"stable"`,
#' `"stability_not_reached"`, or `"insufficient_observations"`.
#'
#' `trajectories` contain `n_obs`, `median_trajectory`, `trajectory_sd`,
#' `replicates`, `trajectory_se`, `interval_lower`, `interval_upper`, `group`,
#' and `resolution`. Curves contain `replicate`, `n_obs`, `cumulative_median`,
#' `group`, and `resolution`.
#'
#' @references
#' Ibáñez, H., Bidegain, G., Sáez de Ocáriz, H., Mestre-Tomás, J.,
#' Verbruggen, H., Jueterbock, A. O., & Becerro, M. A. (2026). Stop guessing
#' thinning distances: a data-driven framework for spatial thinning
#' parametrization for ecological modelling. bioRxiv, 2026-10.
#' https://doi.org/10.64898/2026.10.23.759308
#'
#' @examples
#' # Simulate six species with different geographical range sizes.
#' set.seed(42)
#' species_names <- paste0("sp", 1:6)
#' spreads <- c(0.30, 0.45, 0.65, 0.95, 1.35, 1.85)
#'
#' example_data <- do.call(rbind, lapply(seq_along(species_names), function(i) {
#'   n <- 80
#'   center_lon <- -140 + 35 * (i - 1)
#'   center_lat <- -25 + 10 * (i - 1)
#'
#'   lon <- center_lon + runif(n, -spreads[i] / 2, spreads[i] / 2)
#'   lat <- center_lat + runif(n, -spreads[i] / 2, spreads[i] / 2)
#'
#'   data.frame(
#'     lon = lon,
#'     lat = lat,
#'     species = species_names[i],
#'     temperature = 16 + i + 0.8 * (lon - center_lon) / spreads[i] + rnorm(n, sd = 1.4))
#' }))
#'
#' candidate_resolutions <- c(0.025, 0.05, 0.1, 0.2, 0.4, 0.8)
#'
#' # LS-TP: find the largest stable grid resolution for each species.
#' ls_fit <- estimate_thinning_parameter(
#'   example_data,
#'   group_col = "species",
#'   env_col = "temperature",
#'   resolutions = candidate_resolutions,
#'   replicates = 5,
#'   max_samples = 60,
#'   seed = 123
#' )
#'
#' print(ls_fit)
#' summary(ls_fit)
#' ls_fit$estimated_parameters
#' plot(ls_fit, group = "sp2", resolution = c(0.05, 0.1, 0.2))
#'
#' # BD-TP: predict thinning resolution from geographical range diameter.
#' if (requireNamespace("e1071", quietly = TRUE)) {
#'   bd_fit <- estimate_thinning_parameter(
#'     example_data,
#'     group_col = "species",
#'     env_col = "temperature",
#'     param_method = "bd-tp",
#'     resolutions = candidate_resolutions,
#'     replicates = 5,
#'     max_samples = 60,
#'     seed = 123
#'   )
#'
#'   bd_fit$estimated_parameters
#'   plot(bd_fit)
#' }
#'
#' # Apply the estimated LS-TP resolution to one species.
#' sp4 <- subset(example_data, species == "sp4")
#' thinned_sp4 <- thin_points(
#'   sp4,
#'   method = "grid",
#'   resolution = unname(ls_fit$estimated_parameters[["sp4"]]),
#'   seed = 123
#' )
#' plot(thinned_sp4)
#'
#' @export
estimate_thinning_parameter <- function(data,
                                        lon_col = "lon",
                                        lat_col = "lat",
                                        group_col = NULL,
                                        env_col,
                                        param_method = c("ls-tp", "bd-tp"),
                                        range_resolution = 0.5,
                                        resolutions = c(0.001, 0.01, 0.1, 1, 2.5, 5, 7.5, 10, 12.5, 15, 17.5),
                                        replicates = 10,
                                        max_samples = 500,
                                        median_tolerance = 0.3,
                                        interval_tolerance = 0.5,
                                        stable_window = 20,
                                        tail_n = 20,
                                        seed = NULL,
                                        keep_curves = FALSE,
                                        verbose = FALSE,
                                        origin = NULL,
                                        n = 1
                                        ) {
  # Validate inputs
  param_method <- match.arg(param_method)
  if (!is.null(origin) && (!is.numeric(origin) || length(origin) != 2 || any(!is.finite(origin)))) {
    stop("`origin` must be `NULL` or two finite numeric values.")
  }

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (nrow(data) == 0) {
    stop("`data` must contain at least one row.")
  }
  if (missing(env_col)) {
    stop("`env_col` must be a single non-missing, nonempty character column name.")
  }

  column_arguments <- list(
    lon_col = lon_col,
    lat_col = lat_col,
    env_col = env_col
  )
  if (!is.null(group_col)) {
    column_arguments$group_col <- group_col
  }
  for (argument_name in names(column_arguments)) {
    column_name <- column_arguments[[argument_name]]
    if (!is.character(column_name) || length(column_name) != 1 || is.na(column_name) || !nzchar(trimws(column_name))) {
      stop("`", argument_name, "` must be a single non-missing, nonempty character column name.")
    }
  }

  selected_columns <- unname(unlist(column_arguments, use.names = FALSE))
  if (anyDuplicated(selected_columns)) {
    stop("`lon_col`, `lat_col`, `env_col`, and non-NULL `group_col` must ", "refer to distinct columns.")
  }

  missing_cols <- setdiff(selected_columns, names(data))
  if (length(missing_cols) > 0) {
    stop("The following columns are missing from `data`: ", paste(missing_cols, collapse = ", "))
  }
  duplicated_cols <- unique(selected_columns[vapply(selected_columns, function(column_name) sum(names(data) == column_name) > 1, logical(1))])
  if (length(duplicated_cols) > 0) {
    stop("The following selected columns have duplicated names in `data`: ", paste(duplicated_cols, collapse = ", "))
  }
  if (!is.numeric(data[[lon_col]]) || !is.numeric(data[[lat_col]])) {
    stop("`lon_col` and `lat_col` must refer to numeric columns.")
  }

  if (any(!is.finite(data[[lon_col]]) | !is.finite(data[[lat_col]]))) {
    stop("Longitude and latitude columns must contain only finite values.")
  }
  if (any(data[[lon_col]] < -180 | data[[lon_col]] > 180) || any(data[[lat_col]] < -90 | data[[lat_col]] > 90)) {
    stop("Longitude values must be within [-180, 180] and latitude values ", "within [-90, 90].")
  }
  reference_grid <- terra::rast(
    ncols = 360, nrows = 180,
    xmin = -180, xmax = 180,
    ymin = -90, ymax = 90,
    crs = "EPSG:4326"
  )
  coordinate_cells <- terra::cellFromXY(reference_grid, as.matrix(data[, c(lon_col, lat_col)]))
  if (any(is.na(coordinate_cells))) {
    stop("All coordinates must be assignable to the EPSG:4326 reference grid.")
  }
  if (!is.numeric(data[[env_col]])) {
    stop("`env_col` must refer to one numeric environmental variable: ", env_col)
  }
  if (!is.null(group_col)) {
    group_values <- data[[group_col]]
    group_labels <- as.character(group_values)
    invalid_group <- is.na(group_values) | is.na(group_labels) | !nzchar(trimws(group_labels))
    if (any(invalid_group)) {
      stop("`group_col` must not contain missing or blank group values.")
    }
  }
  if (!is.numeric(resolutions) || length(resolutions) == 0 || any(!is.finite(resolutions))) {
    stop("`resolutions` must be a numeric vector with finite values.")
  }
  if (any(resolutions <= 0)) {
    stop("`resolutions` must contain positive values.")
  }

  integer_settings <- list(
    replicates = replicates,
    max_samples = max_samples,
    stable_window = stable_window,
    tail_n = tail_n
  )
  for (setting_name in names(integer_settings)) {
    setting_value <- integer_settings[[setting_name]]
    if (!is.numeric(setting_value) || length(setting_value) != 1 || !is.finite(setting_value) || setting_value <= 0 || setting_value != floor(setting_value)) {
      stop("`", setting_name, "` must be a positive integer.")
    }
    if (setting_value > .Machine$integer.max) {
      stop("`", setting_name, "` must not exceed `.Machine$integer.max`.")
    }
  }
  if (replicates < 2) {
    stop("`replicates` must be at least 2 to compute trajectory intervals.")
  }
  if (!is.numeric(median_tolerance) || length(median_tolerance) != 1 || !is.finite(median_tolerance) || median_tolerance < 0) {
    stop("`median_tolerance` must be a non-negative number.")
  }
  if (!is.numeric(interval_tolerance) || length(interval_tolerance) != 1 || !is.finite(interval_tolerance) || interval_tolerance < 0) {
    stop("`interval_tolerance` must be a non-negative number.")
  }
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed) || seed != floor(seed)) {
      stop("`seed` must be `NULL` or a single finite integer.")
    }
    if (abs(seed) > .Machine$integer.max) {
      stop("`seed` must not exceed `.Machine$integer.max` in absolute value.")
    }
  }

  if (!is.numeric(n) || length(n) != 1 || !is.finite(n) || n != 1) {
    stop("`n` must be 1.")
  }

  if (param_method == "bd-tp") {
    if (!is.numeric(range_resolution) || length(range_resolution) != 1 || !is.finite(range_resolution) || range_resolution <= 0) {
      stop("`range_resolution` must be a positive number.")
    }
    if (is.null(group_col)) {
      stop("`group_col` is required when `param_method = \"bd-tp\"`.")
    }
    if (!requireNamespace("e1071", quietly = TRUE)) {
      stop("Package 'e1071' is required for BD-TP. Please install it to use `param_method = \"bd-tp\"`.")
    }
  }

  if (!is.logical(keep_curves) || length(keep_curves) != 1 || is.na(keep_curves)) {
    stop("`keep_curves` must be a logical value.")
  }
  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("`verbose` must be a logical value.")
  }

  replicates <- as.integer(replicates)
  max_samples <- as.integer(max_samples)
  stable_window <- as.integer(stable_window)
  tail_n <- as.integer(tail_n)
  if (!is.null(seed)) {
    seed <- as.integer(seed)
  }
  resolutions <- sort(unique(resolutions))

  lower_bound <- 0.001
  if (param_method == "ls-tp" && lower_bound > min(resolutions)) {
    stop("LS-TP candidate `resolutions` must not be below 0.001 degrees.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Define groups (e.g. species)
  if (is.null(group_col)) {
    group_col_internal <- ".GeoThinneR_group"
    while (group_col_internal %in% names(data)) {
      group_col_internal <- paste0(group_col_internal, "_")
    }
    data[[group_col_internal]] <- "all"
  } else {
    group_col_internal <- group_col
  }

  # The supplement uses first-observed species order for stabilization
  groups <- unique(as.character(data[[group_col_internal]]))

  range_descriptor <- NULL
  if (param_method == "bd-tp") {
    range_descriptor <- estimate_tp_range_diameter(
      data = data,
      lon_col = lon_col,
      lat_col = lat_col,
      group_col = group_col_internal,
      resolution = range_resolution
    )
  }

  thinned_datasets <- vector("list", length(resolutions))
  stability_rows <- vector("list", length(groups) * length(resolutions))
  trajectory_list <- list()
  curves_list <- list()
  row_i <- 0
  step <- 0
  total_steps <- length(groups) * length(resolutions)

  progress <- NULL
  if (isTRUE(verbose)) {
    progress <- utils::txtProgressBar(min = 0, max = total_steps, style = 3)
    on.exit(close(progress), add = TRUE)
  }

  # One grid realization.
  for (resolution_i in seq_along(resolutions)) {
    resolution <- resolutions[[resolution_i]]
    thin_args <- list(
      data = data,
      lon_col = lon_col,
      lat_col = lat_col,
      group_col = group_col_internal,
      method = "grid",
      resolution = resolution,
      origin = origin,
      n = n,
      trials = 1,
      all_trials = FALSE,
      seed = NULL,
      verbose = FALSE
    )
    thinned <- do.call(thin_points, thin_args)
    kept <- thinned$retained[[1]]
    thinned_datasets[[resolution_i]] <- data[kept, , drop = FALSE]
  }

  # Iterate for each combination of group and candidate resolution parameter
  for (group in groups) {
    for (resolution_i in seq_along(resolutions)) {
      resolution <- resolutions[[resolution_i]]
      thinned_data <- thinned_datasets[[resolution_i]]
      group_rows <- as.character(thinned_data[[group_col_internal]]) == group
      n_retained <- sum(group_rows)

      values <- thinned_data[[env_col]][group_rows]
      values <- values[is.finite(values)]
      n_env_finite <- length(values)

      if (n_env_finite < 3) {
        row_i <- row_i + 1
        stability_rows[[row_i]] <- data.frame(
          group = group,
          resolution = resolution,
          n_retained = n_retained,
          n_env_finite = n_env_finite,
          n_sampled = NA,
          stabilization_threshold = NA,
          stabilization_fraction = NA,
          reference_value = NA,
          interval_width_at_stabilization = NA,
          stability_reached = FALSE,
          stability_status = "insufficient_observations",
          stringsAsFactors = FALSE
        )

        step <- step + 1
        if (!is.null(progress)) {
          utils::setTxtProgressBar(progress, step)
        }
        next
      }

      curves <- estimate_tp_stabilize_values(
        values = values,
        replicates = replicates,
        max_samples = max_samples
      )

      if (isTRUE(keep_curves)) {
        curve_key <- paste(group, resolution, sep = "__")
        curves$group <- group
        curves$resolution <- resolution
        curves_list[[curve_key]] <- curves
      }

      summary_df <- estimate_tp_summarise_replicates(curves)
      trajectory_key <- paste(group, resolution, sep = "__")
      summary_df$group <- group
      summary_df$resolution <- resolution
      trajectory_list[[trajectory_key]] <- summary_df

      threshold <- estimate_tp_compute_threshold(
        summary_df = summary_df,
        median_tolerance = median_tolerance,
        interval_tolerance = interval_tolerance,
        stable_window = stable_window,
        tail_n = tail_n
      )
      n_valid_positions <- sum(is.finite(summary_df$median_trajectory))
      stability_status <- if (isTRUE(threshold$stability_reached)) {
        "stable"
      } else if (n_valid_positions < stable_window) {
        "insufficient_observations"
      } else {
        "stability_not_reached"
      }

      row_i <- row_i + 1
      stability_rows[[row_i]] <- data.frame(
        group = group,
        resolution = resolution,
        n_retained = n_retained,
        n_env_finite = n_env_finite,
        n_sampled = if (nrow(summary_df) == 0) NA else max(summary_df$n_obs, na.rm = TRUE),
        stabilization_threshold = threshold$stabilization_threshold,
        stabilization_fraction = threshold$stabilization_fraction,
        reference_value = threshold$reference_value,
        interval_width_at_stabilization = threshold$interval_width_at_stabilization,
        stability_reached = threshold$stability_reached,
        stability_status = stability_status,
        stringsAsFactors = FALSE
      )

      step <- step + 1
      if (!is.null(progress)) {
        utils::setTxtProgressBar(progress, step)
      }
    }
  }

  if (isTRUE(verbose)) {
    utils::setTxtProgressBar(progress, total_steps)
    cat("\n")
  }

  stability <- do.call(rbind, stability_rows[seq_len(row_i)])
  stability_group_order <- match(as.character(stability$group), groups)
  stability <- stability[order(stability_group_order, stability$resolution), , drop = FALSE]
  rownames(stability) <- NULL

  model <- NULL
  if (param_method == "bd-tp") {
    range_index <- match(as.character(stability$group), as.character(range_descriptor$group))
    if (any(is.na(range_index))) {
      stop("Every stability group must have a geographic range descriptor.")
    }
    stability$range_diameter_km <- range_descriptor$range_diameter_km[range_index]
    bdtp <- estimate_tp_select_bdtp(
      stability = stability,
      range_descriptor = range_descriptor
    )
    estimates <- bdtp$estimates
    model <- bdtp$model
  } else {
    estimates <- estimate_tp_select_lstp(stability)
    no_stable <- estimates$selection_status == "no_stable_parameter"
    if (any(no_stable)) {
      n_no_stable <- sum(no_stable)
      warning(
        n_no_stable,
        if (n_no_stable == 1) " group has" else " groups have",
        " no stable parameter identified under the specified criterion; ",
        "the returned lower bound is an operational placeholder, not a ",
        "validated stable thinning parameter. See `estimates$selection_status`.",
        call. = FALSE
      )
    }
  }

  settings <- list(
    param_method = param_method,
    resolutions = resolutions,
    grid = list(origin = origin, crs = "EPSG:4326"),
    lon_col = lon_col,
    lat_col = lat_col,
    group_col = group_col,
    env_col = env_col,
    replicates = replicates,
    max_samples = max_samples,
    median_tolerance = median_tolerance,
    interval_tolerance = interval_tolerance,
    stable_window = stable_window,
    tail_n = tail_n,
    lower_bound = lower_bound,
    seed = seed
  )
  if (param_method == "bd-tp") {
    settings$range_resolution <- range_resolution
    settings$svm <- list(kernel = "linear", scale = TRUE, cost = 1)
  }

  curves_out <- NULL
  if (isTRUE(keep_curves) && length(curves_list) > 0) {
    curves_out <- do.call(rbind, curves_list)
  }

  trajectories_out <- NULL
  if (length(trajectory_list) > 0) {
    trajectories_out <- do.call(rbind, trajectory_list)
    rownames(trajectories_out) <- NULL
  }

  new_GeoThinningParameter(
    estimates = estimates,
    stability = stability,
    settings = settings,
    trajectories = trajectories_out,
    curves = curves_out,
    call = match.call(),
    range_descriptors = range_descriptor,
    model = model
  )
}

#' Internal GeoThinningParameter constructor
#'
#' @noRd
new_GeoThinningParameter <- function(estimates,
                                     stability,
                                     settings = list(),
                                     trajectories = NULL,
                                     curves = NULL,
                                     call = NULL,
                                     range_descriptors = NULL,
                                     model = NULL) {
  estimated_parameters <- stats::setNames(estimates$estimated_parameter, estimates$group)

  structure(
    list(
      estimated_parameters = estimated_parameters,
      estimates = estimates,
      stability = stability,
      settings = settings,
      trajectories = trajectories,
      curves = curves,
      range_descriptors = range_descriptors,
      model = model,
      call = call
    ),
    class = "GeoThinningParameter"
  )
}

#' Methods for GeoThinningParameter Objects
#'
#' Inspect, summarize, extract, and plot the result of
#' \code{\link{estimate_thinning_parameter}()}. The main result is the named numeric vector
#' `estimated_parameters`; the complete group-level diagnostics are in `estimates`.
#'
#' @param x,object A `GeoThinningParameter` object.
#' @param group Optional group to plot. If `NULL`, the first group with
#'   an available trajectory is used. In BD-TP plots, selected groups are
#'   highlighted.
#' @param ... Additional arguments passed to base graphics or data frame methods.
#'
#' @return `print()` and `plot()` return the input object invisibly. `summary()`
#'   returns a compact summary object. `as.data.frame()` returns the
#'   complete `estimates` table.
#'
#' @name GeoThinningParameter
NULL

#' @rdname GeoThinningParameter
#' @method print GeoThinningParameter
#' @export
print.GeoThinningParameter <- function(x, ...) {
  stable_by_group <- tapply(x$stability$stability_reached, x$stability$group, any, na.rm = TRUE)

  cat("GeoThinningParameter - ", toupper(x$settings$param_method), "\n", sep = "")
  cat("Environmental variable: ", x$settings$env_col, "\n", sep = "")
  cat("Groups evaluated: ", nrow(x$estimates), "\n", sep = "")
  cat("Resolutions tested: ", length(unique(x$stability$resolution)), "\n", sep = "")
  cat("Groups with stability: ", sum(stable_by_group), "/", length(stable_by_group), "\n\n", sep = "")

  cat("Estimated thinning resolutions (degrees):\n")
  if (length(x$estimated_parameters) <= 10) {
    print(x$estimated_parameters)
  } else {
    print(utils::head(x$estimated_parameters, 10))
    cat("... ", length(x$estimated_parameters) - 10, " more groups. Use x$estimated_parameters for all values.\n", sep = "")
  }

  if (identical(x$settings$param_method, "ls-tp")) {
    n_without_stability <- sum(x$estimates$selection_status == "no_stable_parameter")
    if (n_without_stability > 0) {
      cat("\nNote: ", n_without_stability,
          if (n_without_stability == 1) " group has" else " groups have",
          " no stable tested resolution. The returned 0.001 degrees is a default",
          " value, not a validated stable resolution.\n", sep = "")
    }
  } else if (identical(x$settings$param_method, "bd-tp")) {
    n_capped <- sum(x$estimates$lower_bound_applied)
    n_extrapolated <- sum(x$estimates$resolution_extrapolated)
    if (n_capped > 0 || n_extrapolated > 0) {
      cat("\nPredictions capped at 0.001 degrees: ", n_capped, "\n", sep = "")
      cat("Predictions outside tested range: ", n_extrapolated, "\n", sep = "")
    }
  }

invisible(x)
}

#' @rdname GeoThinningParameter
#' @method summary GeoThinningParameter
#' @export
summary.GeoThinningParameter <- function(object, ...) {
  stable_by_group <- tapply(object$stability$stability_reached, object$stability$group, any, na.rm = TRUE)

  out <- list(
    param_method = object$settings$param_method,
    env_col = object$settings$env_col,
    n_groups = nrow(object$estimates),
    n_resolutions = length(unique(object$stability$resolution)),
    n_stable_groups = sum(stable_by_group),
    estimates_preview = utils::head(object$estimates, 10),
    n_estimates = nrow(object$estimates)
  )

  if (identical(object$settings$param_method, "ls-tp")) {
    out$n_without_stability <- sum(object$estimates$selection_status == "no_stable_parameter")
  } else if (identical(object$settings$param_method, "bd-tp")) {
    out$bdtp_coefficients <- object$model$coefficients
    svm_classes <- as.character(object$model$svm_data$stab)
    out$n_svm_stable <- sum(svm_classes == "TRUE")
    out$n_svm_unstablee <- sum(svm_classes == "FALSE")
    out$n_insufficient_in_svm <- object$model$n_insufficient_in_svm
  }

  class(out) <- "summary.GeoThinningParameter"
  out
}

#' @rdname GeoThinningParameter
#' @method print summary.GeoThinningParameter
#' @export
print.summary.GeoThinningParameter <- function(x, ...) {
  cat("Summary of Thinning Parameter Estimates\n")
  cat("---------------------------------------\n")
  cat("Parameterization method: ", toupper(x$param_method), "\n", sep = "")
  cat("Environmental variable: ", x$env_col, "\n", sep = "")
  cat("Groups evaluated: ", x$n_groups, "\n", sep = "")
  cat("Resolutions evaluated: ", x$n_resolutions, "\n", sep = "")
  cat("Groups with stability: ", x$n_stable_groups, "/", x$n_groups, "\n", sep = "")

  if (identical(x$param_method, "ls-tp")) {
    display <- data.frame(
      "Group" = x$estimates_preview$group,
      "Resolution (\u00B0)" = signif(x$estimates_preview$estimated_parameter, 5),
      "Stable run starts (n)" = x$estimates_preview$stabilization_threshold,
      "Retained" = x$estimates_preview$n_retained,
      "Status" = ifelse(x$estimates_preview$selection_status == "stable_parameter_selected", "Stable", "Not stable"),
      check.names = FALSE
    )
  } else {
    intercept <- unname(x$bdtp_coefficients[["intercept"]])
    slope <- unname(x$bdtp_coefficients[["slope"]])
    cat("BD-TP rule: resolution = ", format(intercept, digits = 6), if (slope < 0) " - " else " + ", format(abs(slope), digits = 6), " * range diameter (km)\n", sep = "")
    cat("\nSVM training:\n")
    cat("  Stability demonstrated: ", x$n_svm_stable, "\n", sep = "")
    cat("  Stability not demonstrated: ", x$n_svm_unstable, "\n", sep = "")
    cat("  Of which insufficient observations: ", x$n_insufficient_in_svm, "\n", sep = "")

    display <- data.frame(
      "Group" = x$estimates_preview$group,
      "Range (km)" = round(x$estimates_preview$range_diameter_km, 2),
      "Resolution (\u00B0)" = signif(x$estimates_preview$estimated_parameter, 5),
      "Capped" = ifelse(x$estimates_preview$lower_bound_applied, "Yes", "No"),
      "Extrapolated" = ifelse(x$estimates_preview$resolution_extrapolated, "Yes", "No"),
      check.names = FALSE
    )
  }

  cat("\n")
  print(display, row.names = FALSE, right = FALSE)

  n_more <- x$n_estimates - nrow(x$estimates_preview)
  if (n_more > 0) {
    cat("... ", n_more, " more groups. Use as.data.frame(fit) for all results.\n", sep = "")
  }

  if (identical(x$param_method, "ls-tp")) {
    cat("\nStability is identified when both criteria are met for 20 consecutive observation counts (by default). The value shown is the first count in that sequence.\n", sep = "")
    if (x$n_without_stability > 0) {
      cat("Note: ", x$n_without_stability,
          if (x$n_without_stability == 1) " group has" else " groups have",
          " no stable tested resolution. The returned 0.001 degrees is a default",
          " value, not a validated stable resolution.\n", sep = "")
    }
  } else {
    cat("\nBD-TP estimates thinning resolutions from species range size. Unlike LS-TP, the predicted resolutions are not necessarily among those tested.\n")
  }

  invisible(x)
}


#' @rdname GeoThinningParameter
#' @method as.data.frame GeoThinningParameter
#' @export
as.data.frame.GeoThinningParameter <- function(x, ...) {
  as.data.frame(x$estimates, ...)
}

#' Offset overlapping BD-TP training points for display
#'
#' Apply a small deterministic horizontal offset only to exact duplicate
#' predictor pairs. The returned coordinates are for plotting and never enter
#' the fitted model or stored range descriptors.
#'
#' @param diameter Numeric range diameters.
#' @param resolution Numeric thinning resolutions.
#' @param x_span Positive numeric width of the displayed diameter axis.
#'
#' @return Numeric display coordinates for `diameter`.
#'
#' @noRd
estimate_tp_plot_overlap_offsets <- function(diameter, resolution, x_span) {
  display_diameter <- diameter
  finite <- is.finite(diameter) & is.finite(resolution)
  if (!any(finite) || !is.finite(x_span) || x_span <= 0) {
    return(display_diameter)
  }

  keys <- paste(
    format(diameter[finite], digits = 17, scientific = FALSE, trim = TRUE),
    format(resolution[finite], digits = 17, scientific = FALSE, trim = TRUE),
    sep = "\r"
  )
  key_levels <- unique(keys)
  finite_rows <- which(finite)

  for (key in key_levels) {
    rows <- finite_rows[keys == key]
    if (length(rows) > 1) {
      display_diameter[rows] <- diameter[rows] + seq(-0.006 * x_span, 0.006 * x_span, length.out = length(rows))
    }
  }

  display_diameter
}

#' @rdname GeoThinningParameter
#' @param type `"auto"`, `"stability"`, or `"bdtp"`. The stability plot
#'   shows cumulative-median trajectories; the BD-TP plot shows SVM training
#'   data, the decision boundary, and predictions. The default is based on
#'   the fitted method.
#' @param resolution Candidate resolutions to show in stability plots.
#'   If `NULL`, all available resolutions are plotted.
#' @param show_replicates Show individual trajectories if `keep_curves = TRUE`.
#' @param show_ci Show the summarized trajectory interval.
#' @param show_threshold Show the stabilization threshold and
#' @details
#' For LS-TP, `print()` and `summary()` report how many groups reached
#' stability. When no tested resolution is stable, the function returns
#' 0.001 degrees as a default value. This does not mean that stability
#' was achieved.
#'
#' For BD-TP, the results indicate whether a predicted resolution was
#' limited to the minimum of 0.001 degrees or falls outside the tested
#' resolution range. BD-TP estimates resolutions from geographical range
#' size, so its predictions are not necessarily among those tested.
#'
#' Stability plots show the summarized cumulative-median trajectories,
#' even when individual replicate curves were not saved. If there are
#' too few environmental observations to calculate a trajectory,
#' an informative message is displayed.
#'
#' In BD-TP plots, overlapping training points are slightly separated
#' to make them visible. This adjustment affects only the plot, not
#' the underlying data, fitted model, or predicted resolutions.
#' Plotting is reproducible and does not use random numbers.
#' @method plot GeoThinningParameter
#' @export
plot.GeoThinningParameter <- function(x,
                                      type = c("auto", "stability", "bdtp"),
                                      group = NULL,
                                      resolution = NULL,
                                      show_replicates = !is.null(x$curves),
                                      show_ci = TRUE,
                                      show_threshold = TRUE,
                                      ...) {
  type <- match.arg(type)

  if (identical(type, "auto")) {
    if (identical(x$settings$param_method, "bd-tp") && !is.null(x$model)) {
      type <- "bdtp"
    } else {
      type <- "stability"
    }
  }

  if (identical(type, "stability")) {
    if (!is.logical(show_replicates) || length(show_replicates) != 1 || is.na(show_replicates)) {
      stop("`show_replicates` must be a single non-missing logical value.")
    }
    if (isTRUE(show_replicates) && is.null(x$curves)) {
      stop("Replicate curves were not stored. Re-run with `keep_curves = TRUE` or use `show_replicates = FALSE`.")
    }
    if (!is.logical(show_ci) || length(show_ci) != 1 || is.na(show_ci)) {
      stop("`show_ci` must be a single non-missing logical value.")
    }
    if (!is.logical(show_threshold) || length(show_threshold) != 1 || is.na(show_threshold)) {
      stop("`show_threshold` must be a single non-missing logical value.")
    }
    evaluated_groups <- unique(as.character(x$stability$group))
    if (is.null(x$trajectories) || nrow(x$trajectories) == 0) {
      if (!is.null(group) && length(group) == 1 && group %in% evaluated_groups) {
        stop("No valid summarized stability trajectory is available for group `", group, "`. This can occur when fewer than three finite environmental values are available.")
      }
      stop("No valid summarized stability trajectories are available in this object. At least three finite environmental values are required for a trajectory.")
    }

    trajectories <- x$trajectories
    curves <- x$curves
    env_col <- x$settings$env_col

    groups <- unique(as.character(trajectories$group))

    if (is.null(group)) {
      group <- groups[1]
    }
    if (length(group) != 1 || is.na(group)) {
      stop("`group` must be a single non-missing group identifier.")
    }
    if (!group %in% evaluated_groups) {
      stop("`group` must be one of the groups evaluated in `x$stability`.")
    }
    if (!group %in% groups) {
      stop("No valid summarized stability trajectory is available for group `", group, "`. This can occur when fewer than three finite environmental values are available.")
    }

    group_trajectories <- trajectories[as.character(trajectories$group) == group, , drop = FALSE]
    available_resolutions <- sort(unique(group_trajectories$resolution))

    if (is.null(resolution)) {
      resolutions_to_plot <- available_resolutions
    } else {
      if (!is.numeric(resolution) || any(!is.finite(resolution))) {
        stop("`resolution` must be a finite numeric vector.")
      }

      missing_resolutions <- setdiff(resolution, available_resolutions)
      if (length(missing_resolutions) > 0) {
        stop("The following `resolution` values are not available for the selected group: ", paste(missing_resolutions, collapse = ", "))
      }

      resolutions_to_plot <- sort(unique(resolution))
    }

    n_panels <- length(resolutions_to_plot)
    if (n_panels == 0) {
      stop("No resolutions are available for plotting.")
    }

    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    if (n_panels > 1) {
      n_col <- ceiling(sqrt(n_panels))
      n_row <- ceiling(n_panels / n_col)
      graphics::par(mfrow = c(n_row, n_col))
    }

    for (panel_i in seq_along(resolutions_to_plot)) {
      resolution_i <- resolutions_to_plot[panel_i]

      summary_df <- group_trajectories[group_trajectories$resolution == resolution_i, , drop = FALSE]

      if (nrow(summary_df) == 0) {
        stop("No summarized stability trajectory is available for group `", group, "` and resolution ", resolution_i, ".")
      }

      resolution_curves <- NULL
      if (isTRUE(show_replicates)) {
        resolution_curves <- curves[as.character(curves$group) == group & curves$resolution == resolution_i, , drop = FALSE]
      }

      plot_stability <- x$stability

      stability_filter <- as.character(plot_stability$group) == group & plot_stability$resolution == resolution_i
      stability_row <- plot_stability[stability_filter, , drop = FALSE]

      if (nrow(stability_row) > 1) {
        stability_row <- stability_row[1, , drop = FALSE]
      }

      y_values <- c(
        summary_df$median_trajectory,
        if (isTRUE(show_ci)) c(summary_df$interval_lower, summary_df$interval_upper),
        if (isTRUE(show_replicates)) resolution_curves$cumulative_median,
        if (nrow(stability_row) == 1) stability_row$reference_value
      )
      y_values <- y_values[is.finite(y_values)]

      if (length(y_values) == 0) {
        stop("No finite trajectory values are available for group `", group, "` and resolution ", resolution_i, ".")
      }

      ylim <- range(y_values, na.rm = TRUE)
      if (diff(ylim) == 0) {
        ylim <- ylim + c(-0.5, 0.5)
      }

      graphics::plot(
        summary_df$n_obs,
        summary_df$median_trajectory,
        type = "n",
        xlab = "Observation count",
        ylab = paste("Cumulative median of", env_col),
        main = paste(group, "\nresolution =", resolution_i),
        ylim = ylim,
        ...
      )

      if (isTRUE(show_replicates)) {
        replicate_split <- split(resolution_curves, resolution_curves$replicate)

        invisible(lapply(replicate_split, function(d) {
          d <- d[order(d$n_obs), , drop = FALSE]
          graphics::lines(d$n_obs, d$cumulative_median, col = grDevices::adjustcolor("grey60", alpha.f = 0.35), lwd = 1)
        }))
      }

      if (isTRUE(show_ci)) {
        graphics::lines(summary_df$n_obs, summary_df$interval_lower, lty = 2, lwd = 1, col = "grey40")
        graphics::lines(summary_df$n_obs, summary_df$interval_upper, lty = 2, lwd = 1, col = "grey40")
      }

      graphics::lines(summary_df$n_obs, summary_df$median_trajectory, lwd = 2, col = "black")

      if (isTRUE(show_threshold) && nrow(stability_row) == 1) {
        if (is.finite(stability_row$reference_value)) {
          graphics::abline(h = stability_row$reference_value, lty = 3, lwd = 2, col = "grey30")
        }

        if (isTRUE(stability_row$stability_reached) && is.finite(stability_row$stabilization_threshold)) {
          graphics::abline(v = stability_row$stabilization_threshold, lty = 2, lwd = 2, col = "black")
        }
      }

      if (n_panels == 1) {
        legend_items <- c("Median trajectory")
        legend_lty <- c(1)
        legend_lwd <- c(2)
        legend_col <- c("black")

        if (isTRUE(show_replicates)) {
          legend_items <- c("Replicate trajectories", legend_items)
          legend_lty <- c(1, legend_lty)
          legend_lwd <- c(1, legend_lwd)
          legend_col <- c(grDevices::adjustcolor("grey60", alpha.f = 0.35), legend_col)
        }

        if (isTRUE(show_ci)) {
          legend_items <- c(legend_items, "95% trajectory interval")
          legend_lty <- c(legend_lty, 2)
          legend_lwd <- c(legend_lwd, 1)
          legend_col <- c(legend_col, "grey40")
        }

        if (isTRUE(show_threshold) && nrow(stability_row) == 1) {
          if (is.finite(stability_row$reference_value)) {
            legend_items <- c(legend_items, "Terminal reference")
            legend_lty <- c(legend_lty, 3)
            legend_lwd <- c(legend_lwd, 2)
            legend_col <- c(legend_col, "grey30")
          }

          if (isTRUE(stability_row$stability_reached) && is.finite(stability_row$stabilization_threshold)) {
            legend_items <- c(legend_items, "Stabilization threshold")
            legend_lty <- c(legend_lty, 2)
            legend_lwd <- c(legend_lwd, 2)
            legend_col <- c(legend_col, "black")
          }
        }

        graphics::legend("topright", legend = legend_items, lty = legend_lty, lwd = legend_lwd, col = legend_col, bty = "n", cex = 0.8)
      }
    }

    return(invisible(x))
  }

  if (identical(type, "bdtp")) {
    if (!identical(x$settings$param_method, "bd-tp")) {
      stop("`type = \"bdtp\"` is only available for BD-TP objects.")
    }
    if (is.null(x$model) || is.null(x$model$svm_data) || is.null(x$model$boundary) || is.null(x$model$coefficients)) {
      stop("BD-TP model information is not available in this object.")
    }

    svm_data <- x$model$svm_data
    boundary <- x$model$boundary
    estimates <- x$estimates

    status <- as.character(svm_data$stability_status)

    x_values <- c(
      svm_data$range_diameter_km,
      boundary$range_diameter_km,
      estimates$range_diameter_km
    )
    y_values <- c(
      svm_data$resolution,
      boundary$resolution,
      estimates$estimated_parameter
    )

    xlim <- range(x_values[is.finite(x_values)], na.rm = TRUE)
    ylim <- range(y_values[is.finite(y_values)], na.rm = TRUE)

    if (diff(xlim) == 0) xlim <- xlim + c(-0.5, 0.5)
    if (diff(ylim) == 0) ylim <- ylim + c(-0.5, 0.5)

    display_diameter <- estimate_tp_plot_overlap_offsets(svm_data$range_diameter_km, svm_data$resolution, diff(xlim))
    xlim <- range(c(xlim, display_diameter), na.rm = TRUE)

    stable <- as.character(svm_data$stab) == "TRUE"

    graphics::plot(
      display_diameter,
      svm_data$resolution,
      pch = ifelse(stable, 16, 1),
      col = ifelse(stable, "black", "grey50"),
      xlab = "Range diameter (km)",
      ylab = "Thinning resolution (degrees)",
      main = "BD-TP decision boundary",
      xlim = xlim,
      ylim = ylim,
      ...
    )

    graphics::abline(a = unname(x$model$coefficients[["intercept"]]), b = unname(x$model$coefficients[["slope"]]), lwd = 2, col = "black")
    graphics::points(estimates$range_diameter_km, estimates$estimated_parameter, pch = 17, col = "steelblue4")

    if (!is.null(group)) {
      highlight <- estimates[as.character(estimates$group) %in% group, , drop = FALSE]

      if (nrow(highlight) == 0) {
        warning("No selected BD-TP estimate found for `group`.")
      } else {
        graphics::points(highlight$range_diameter_km, highlight$estimated_parameter, pch = 8, cex = 1.4, lwd = 2, col = "steelblue4")
        graphics::text(highlight$range_diameter_km, highlight$estimated_parameter, labels = highlight$group, pos = 4, cex = 0.75, col = "steelblue4")
      }
    }

    graphics::legend(
      "topright",
      legend = c(
        "Stable combination",
        "Combination without demonstrated stability",
        "BD-TP decision boundary",
        "BD-TP prediction"
      ),
      pch = c(16, 1, NA, 17),
      lty = c(NA, NA, 1, NA),
      lwd = c(NA, NA, 2, NA),
      col = c("black", "grey50", "black", "steelblue4"),
      bty = "n",
      cex = 0.8
    )

    return(invisible(x))
  }

  invisible(x)
}

#' Generate cumulative-median trajectories
#'
#' Internal helper used by `estimate_thinning_parameter()` to generate replicate
#' cumulative-median trajectories from a numeric environmental variable.
#'
#' For each replicate, the function randomly samples up to `max_samples`
#' environmental values and calculates the cumulative median as observations are
#' progressively added.
#'
#' @param values Numeric vector of environmental values.
#' @param replicates Positive integer. Number of random subsampling replicates.
#' @param max_samples Positive integer. Maximum number of observations sampled per replicate.
#'
#' @return A data frame with columns:
#' \itemize{
#'   \item `replicate`: replicate identifier.
#'   \item `n_obs`: cumulative observation count.
#'   \item `cumulative_median`: cumulative median.
#' }
#'
#' @noRd
estimate_tp_stabilize_values <- function(values, replicates = 10, max_samples = 500) {
  if (length(values) < 3) {
    return(data.frame(
      replicate = integer(),
      n_obs = integer(),
      cumulative_median = numeric()
    ))
  }

  n_sample <- min(max_samples, length(values))
  out <- vector("list", replicates)

  for (replicate in seq_len(replicates)) {
    sampled_values <- values[sample.int(length(values), size = n_sample, replace = FALSE)]
    cumulative_median <- vapply(seq_along(sampled_values), function(i) stats::median(sampled_values[seq_len(i)], na.rm = TRUE), numeric(1))
    cumulative_median[seq_along(cumulative_median) < 2] <- NA

    out[[replicate]] <- data.frame(
      replicate = replicate,
      n_obs = seq_along(sampled_values),
      cumulative_median = cumulative_median
    )
  }

  do.call(rbind, out)
}

#' Summarise replicate cumulative-median trajectories
#'
#' Internal helper used by `estimate_thinning_parameter()` to summarise replicate
#' cumulative-median trajectories across observation counts.
#'
#' For each cumulative observation count, the function calculates the median
#' cumulative-median value across replicates, the standard deviation, the
#' replicate count, the standard error, and the 95%
#' normal-approximation trajectory interval.
#'
#' @param curves Data frame returned by `estimate_tp_stabilize_values()`, with columns `replicate`, `n_obs`, and `cumulative_median`.
#'
#' @return A data frame with columns:
#' \itemize{
#'   \item `n_obs`: cumulative observation count.
#'   \item `median_trajectory`: median cumulative-median value across replicates.
#'   \item `trajectory_sd`: standard deviation across replicates.
#'   \item `replicates`: number of replicate values.
#'   \item `trajectory_se`: standard error across replicates.
#'   \item `interval_lower`: lower limit of the trajectory interval.
#'   \item `interval_upper`: upper limit of the trajectory interval.
#' }
#'
#' @noRd
estimate_tp_summarise_replicates <- function(curves) {
  if (nrow(curves) == 0) {
    return(data.frame(
      n_obs = integer(),
      median_trajectory = numeric(),
      trajectory_sd = numeric(),
      replicates = integer(),
      trajectory_se = numeric(),
      interval_lower = numeric(),
      interval_upper = numeric()
    ))
  }

  split_curves <- split(curves$cumulative_median, curves$n_obs)
  n_obs <- as.integer(names(split_curves))
  median_trajectory <- vapply(split_curves, stats::median, numeric(1), na.rm = TRUE)
  trajectory_sd <- vapply(split_curves, stats::sd, numeric(1), na.rm = TRUE)
  # Missing trajectory values remain part of the replicate count used in the standard error.
  replicates <- vapply(split_curves, length, integer(1))
  trajectory_se <- trajectory_sd / sqrt(replicates)

  data.frame(
    n_obs = n_obs,
    median_trajectory = median_trajectory,
    trajectory_sd = trajectory_sd,
    replicates = replicates,
    trajectory_se = trajectory_se,
    interval_lower = median_trajectory - 1.96 * trajectory_se,
    interval_upper = median_trajectory + 1.96 * trajectory_se,
    row.names = NULL
  )
}

#' Find the stability threshold
#'
#' Internal helper used by `estimate_thinning_parameter()` to identify the first
#' observation count at which the environmental descriptor becomes stable.
#'
#' Stability is evaluated using the tail-reference criterion implemented in the
#' original LS-TP workflow: the summarized cumulative median must remain within
#' `median_tolerance` of the final tail mean, and the trajectory-interval width
#' must be less than or equal to `interval_tolerance`, for at least `stable_window`
#' consecutive observation counts.
#'
#' @param summary_df Data frame returned by `estimate_tp_summarise_replicates()`.
#' @param median_tolerance Non-negative numeric value. Maximum absolute difference allowed between the summarized cumulative-median trajectory and the final tail mean.
#' @param interval_tolerance Non-negative numeric value. Maximum allowed width of the trajectory interval.
#' @param stable_window Positive integer. Minimum number of consecutive observation counts that must satisfy the stability criteria.
#' @param tail_n Positive integer. Maximum number of final finite summarized trajectory values used to calculate the terminal reference.
#'
#' @return A list with:
#' \itemize{
#'   \item `stabilization_threshold`: first observation count at which stability is reached.
#'   \item `stabilization_fraction`: proportion of the maximum observation count required for stability.
#'   \item `reference_value`: mean of the final finite summarized trajectory values.
#'   \item `interval_width_at_stabilization`: trajectory-interval width at the stability threshold.
#'   \item `stability_reached`: logical indicating whether stability was reached.
#' }
#'
#' @noRd
estimate_tp_compute_threshold <- function(summary_df,
                                          median_tolerance = 0.3,
                                          interval_tolerance = 0.5,
                                          stable_window = 20,
                                          tail_n = 20) {
  summary_df <- summary_df[order(summary_df$n_obs), , drop = FALSE]
  n_sampled <- if (nrow(summary_df) == 0) NA else max(summary_df$n_obs, na.rm = TRUE)
  finite_trajectory <- summary_df$median_trajectory[is.finite(summary_df$median_trajectory)]
  reference_values <- utils::tail(finite_trajectory, min(tail_n, length(finite_trajectory)))
  reference_value <- if (length(reference_values) == 0) NA else mean(reference_values)

  width <- summary_df$interval_upper - summary_df$interval_lower
  diff_final <- abs(summary_df$median_trajectory - reference_value)
  stable_flag <- is.finite(diff_final) & is.finite(width) & (diff_final <= median_tolerance) & (width <= interval_tolerance)

  if (length(finite_trajectory) < stable_window) {
    return(list(
      stabilization_threshold = NA,
      stabilization_fraction = NA,
      reference_value = reference_value,
      interval_width_at_stabilization = NA,
      stability_reached = FALSE
    ))
  }

  run_seq <- rle(stable_flag)
  idx_end <- cumsum(run_seq$lengths)
  idx_start <- idx_end - run_seq$lengths + 1
  ok <- which(run_seq$values & run_seq$lengths >= stable_window)

  if (length(ok) == 0) {
    return(list(
      stabilization_threshold = NA,
      stabilization_fraction = NA,
      reference_value = reference_value,
      interval_width_at_stabilization = NA,
      stability_reached = FALSE
    ))
  }

  start_idx <- idx_start[ok[1]]
  n_obs_stable <- summary_df$n_obs[start_idx]
  interval_width_at_stabilization <- width[start_idx]

  list(
    stabilization_threshold = n_obs_stable,
    stabilization_fraction = n_obs_stable / n_sampled,
    reference_value = reference_value,
    interval_width_at_stabilization = interval_width_at_stabilization,
    stability_reached = TRUE
  )
}

#' Select the LS-TP thinning parameter
#'
#' Internal helper used by `estimate_thinning_parameter()` to select the Last
#' Stable Thinning Parameterization (LS-TP) value for each group.
#'
#' For each group, the function selects the largest candidate resolution that
#' reached stability. If no candidate resolution reached stability, the fixed
#' operational lower bound of 0.001 degrees is returned.
#'
#' @param stability Data frame of group-resolution stability results generated by `estimate_thinning_parameter()`.
#'
#' @return A data frame with one row per group and columns:
#' \itemize{
#'   \item `group`: group identifier.
#'   \item `estimated_parameter`: estimated LS-TP parameter.
#'   \item `stabilization_threshold`: stability threshold for the estimated resolution.
#'   \item `stabilization_fraction`: proportion of the maximum observation count required for stability.
#'   \item `n_retained`: number of spatial records retained at the estimated resolution, if tested.
#'   \item `n_env_finite`: number of finite environmental measurements at the estimated resolution, if tested.
#'   \item `interval_width_at_stabilization`: trajectory-interval width at the stability threshold.
#'   \item `selection_status`: explicit stable-selection or no-stability status.
#' }
#'
#' @noRd
estimate_tp_select_lstp <- function(stability) {
  lower_bound <- 0.001
  required_cols <- c(
    "group",
    "resolution",
    "n_retained",
    "stabilization_threshold",
    "stabilization_fraction",
    "interval_width_at_stabilization",
    "stability_reached"
  )
  missing_cols <- setdiff(required_cols, names(stability))
  if (length(missing_cols) > 0) {
    stop("`stability` is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }
  if (nrow(stability) == 0) {
    stop("`stability` must contain at least one group-resolution row.")
  }
  if (!is.numeric(stability$resolution) || any(!is.finite(stability$resolution)) || any(stability$resolution <= 0)) {
    stop("`stability$resolution` must contain positive finite numeric values.")
  }
  if (lower_bound > min(stability$resolution)) {
    stop("LS-TP candidate resolutions must not be below 0.001 degrees.")
  }
  if (!is.logical(stability$stability_reached) || anyNA(stability$stability_reached)) {
    stop("`stability$stability_reached` must contain non-missing logical values.")
  }

  group_labels <- as.character(stability$group)
  if (anyNA(group_labels) || any(!nzchar(trimws(group_labels)))) {
    stop("`stability$group` must not contain missing or blank values.")
  }
  stability$group <- group_labels
  if (anyDuplicated(stability[c("group", "resolution")])) {
    stop("`stability` must contain one row per group and tested resolution.")
  }
  if (!"n_env_finite" %in% names(stability)) {
    stability$n_env_finite <- stability$n_retained
  }

  groups <- unique(group_labels)
  out <- vector("list", length(groups))

  for (i in seq_along(groups)) {
    group <- groups[i]
    group_stability <- stability[stability$group == group, , drop = FALSE]
    stable_resolutions <- group_stability$resolution[group_stability$stability_reached]

    if (length(stable_resolutions) == 0) {
      estimated <- lower_bound
      estimated_candidates <- group_stability$resolution == estimated
      estimated_row <- if (any(estimated_candidates)) {
        group_stability[which(estimated_candidates)[1], , drop = FALSE]
      } else {
        NULL
      }
      selection_status <- "no_stable_parameter"
    } else {
      estimated <- max(stable_resolutions)
      estimated_row <- group_stability[group_stability$resolution == estimated, , drop = FALSE][1, , drop = FALSE]
      selection_status <- "stable_parameter_selected"
    }

    out[[i]] <- data.frame(
      group = group,
      estimated_parameter = estimated,
      stabilization_threshold = if (is.null(estimated_row)) NA_integer_ else estimated_row$stabilization_threshold,
      stabilization_fraction = if (is.null(estimated_row)) NA_real_ else estimated_row$stabilization_fraction,
      n_retained = if (is.null(estimated_row)) NA_integer_ else estimated_row$n_retained,
      n_env_finite = if (is.null(estimated_row)) NA_integer_ else estimated_row$n_env_finite,
      interval_width_at_stabilization = if (is.null(estimated_row)) NA_real_ else estimated_row$interval_width_at_stabilization,
      selection_status = selection_status,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, out)
}

#' Calculate BD-TP range-size descriptors
#'
#' Internal helper used by `estimate_thinning_parameter()` to calculate the
#' range-size descriptor used by BD-TP. Occurrence records are assigned to a
#' global WGS84 grid of resolution `resolution`. The physical areas of unique
#' occupied cells are summed and converted to the diameter of an equivalent-area
#' circle. Cell areas are calculated with `terra::cellSize(..., unit = "km")`.
#'
#' @param data Data frame containing coordinates and groups.
#' @param lon_col Character name of the longitude column.
#' @param lat_col Character name of the latitude column.
#' @param group_col Character name of the grouping column.
#' @param resolution Numeric grid resolution in degrees.
#'
#' @return A data frame with one row per group. It records the geographic grid
#'   resolution, occupied-cell count and physical area, and calculated
#'   `range_diameter_km`.
#'
#' @noRd
estimate_tp_range_diameter <- function(data,
                                       lon_col,
                                       lat_col,
                                       group_col,
                                       resolution) {
  if (!is.numeric(resolution) || length(resolution) != 1 || !is.finite(resolution) || resolution <= 0) {
    stop("`range_resolution` must be a positive finite number.")
  }

  max_area_rows <- 1000000
  minimum_resolution <- 180 / max_area_rows
  if (resolution < minimum_resolution) {
    stop(
      "The requested `range_resolution` is too fine for the supported BD-TP ",
      "range calculation. Use a value of at least ",
      format(minimum_resolution, scientific = FALSE),
      " degrees (at most ",
      format(max_area_rows, big.mark = ",", scientific = FALSE),
      " latitude rows)."
    )
  }

  cell_counts <- c(latitude = 180 / resolution, longitude = 360 / resolution)
  rounded_cell_counts <- round(cell_counts)
  whole_cell_tolerance <- 1*10^-6
  if (any(abs(cell_counts - rounded_cell_counts) > whole_cell_tolerance)) {
    stop(
      "`range_resolution` must divide both 180 degrees of latitude and ",
      "360 degrees of longitude into whole cells so the global grid retains ",
      "the exact [-180, 180] by [-90, 90] extent."
    )
  }

  nrows <- as.integer(rounded_cell_counts[["latitude"]])
  ncols <- as.integer(rounded_cell_counts[["longitude"]])

  r <- terra::rast(
    nrows = nrows,
    ncols = ncols,
    xmin = -180, xmax = 180,
    ymin = -90, ymax = 90,
    crs = "EPSG:4326"
  )
  # Sum the physical areas of occupied WGS84 cells.
  # Cell areas are identical across each latitude row, so one aligned column
  # gives the same cellSize() values without materializing every global cell.
  grid_resolution <- terra::res(r)
  row_area_grid <- terra::rast(
    nrows = nrow(r),
    ncols = 1,
    xmin = terra::xmin(r),
    xmax = terra::xmin(r) + grid_resolution[[1]],
    ymin = terra::ymin(r),
    ymax = terra::ymax(r),
    crs = "EPSG:4326"
  )
  row_area_km2 <- terra::values(terra::cellSize(row_area_grid, unit = "km"), mat = FALSE)

  coords <- as.matrix(data[, c(lon_col, lat_col)])
  cells <- terra::cellFromXY(r, coords)
  if (anyNA(cells)) {
    stop("All coordinates must be finite geographic values within the exact global range grid.")
  }
  groups <- as.character(data[[group_col]])
  # `distinct(species)` in the supplementary materials preserves first-observed order
  group_levels <- unique(groups)

  out <- vector("list", length(group_levels))
  for (i in seq_along(group_levels)) {
    group <- group_levels[i]
    group_cells <- unique(cells[groups == group])
    occupied_cells <- length(group_cells)
    group_rows <- terra::rowFromCell(r, group_cells)
    occupied_area_km2 <- sum(row_area_km2[group_rows])
    range_diameter_km <- 2 * sqrt(occupied_area_km2 / pi)

    out[[i]] <- data.frame(
      group = group,
      range_resolution = resolution,
      occupied_cells = occupied_cells,
      occupied_area_km2 = occupied_area_km2,
      range_diameter_km = range_diameter_km,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, out)
}

#' Extract an analytical linear-SVM hyperplane
#'
#' Recover the original-unit decision function from an `e1071::svm()` linear
#' classifier fitted with predictor scaling, then solve it for the thinning
#' resolution. Predictor names are used explicitly so coefficient order cannot
#' silently change the extracted rule.
#'
#' @param svm_model A fitted linear `e1071::svm()` model.
#' @param diameter_col Name of the range-diameter predictor.
#' @param resolution_col Name of the thinning-resolution predictor.
#'
#' @return A list containing rule coefficients and original/scaled decision
#'   function coefficients.
#'
#' @noRd
estimate_tp_extract_svm_hyperplane <- function(svm_model,
                                               diameter_col = "range_diameter_km",
                                               resolution_col = "resolution") {
  predictor_names <- colnames(svm_model$SV)
  required_predictors <- c(diameter_col, resolution_col)
  if (!setequal(predictor_names, required_predictors)) {
    stop("The fitted SVM must contain exactly the predictors `", diameter_col, "` and `", resolution_col, "`.")
  }
  if (is.null(svm_model$x.scale)) {
    stop("The BD-TP hyperplane requires an SVM fitted with `scale = TRUE`.")
  }

  scaled_weights <- drop(t(svm_model$coefs) %*% svm_model$SV)
  names(scaled_weights) <- predictor_names
  center <- svm_model$x.scale[["scaled:center"]][predictor_names]
  predictor_scale <- svm_model$x.scale[["scaled:scale"]][predictor_names]

  if (any(!is.finite(center)) || any(!is.finite(predictor_scale)) || any(predictor_scale <= 0)) {
    stop("The fitted SVM contains invalid predictor scaling information.")
  }

  original_weights <- scaled_weights / predictor_scale
  original_constant <- -svm_model$rho - sum(scaled_weights * center / predictor_scale)
  resolution_weight <- original_weights[[resolution_col]]
  zero_tolerance <- 1*10^-6
  if (!is.finite(resolution_weight) || abs(resolution_weight) <= zero_tolerance) {
    stop("The SVM resolution coefficient is numerically zero; the analytical boundary cannot be expressed as a thinning resolution.")
  }

  coefficients <- c(
    intercept = -original_constant / resolution_weight,
    slope = -original_weights[[diameter_col]] / resolution_weight
  )

  list(
    coefficients = coefficients,
    original_weights = original_weights,
    original_constant = unname(original_constant),
    scaled_weights = scaled_weights,
    scaled_intercept = unname(-svm_model$rho),
    predictor_center = center,
    predictor_scale = predictor_scale
  )
}

#' Select BD-TP thinning parameters
#'
#' Internal helper used by `estimate_thinning_parameter()` to estimate thinning
#' parameters using the Biogeographic-size Dependent Thinning Parameterization
#' (BD-TP). A linear SVM classifies combinations with demonstrated stability
#' against those without it, using range diameter and thinning resolution.
#' The boundary is extracted from the scaled support vectors, coefficients,
#' and `rho`.
#'
#' @param stability Data frame of group-resolution stability results.
#' @param range_descriptor Data frame returned by `estimate_tp_range_diameter()`.
#'
#' @details
#' Combinations with at least three sampled finite observations enter the
#' SVM. The stability not demonstrated class indicates that stability was not demonstrated,
#' including when too few observations were available to assess it.
#' Diagnostic statuses are retained in `stability`.
#'
#' @return A list with `estimates` and `model`.
#'
#' @noRd
estimate_tp_select_bdtp <- function(stability, range_descriptor) {
  lower_bound <- 0.001
  if (!requireNamespace("e1071", quietly = TRUE)) {
    stop("Package 'e1071' is required for BD-TP. Please install it to fit the model.")
  }

  descriptor_groups <- as.character(range_descriptor$group)
  if (any(is.na(descriptor_groups)) || any(!nzchar(descriptor_groups)) || anyDuplicated(descriptor_groups)) {
    stop("`range_descriptor` must contain one uniquely identified row per group.")
  }
  stability_groups <- as.character(stability$group)
  range_index <- match(stability_groups, descriptor_groups)
  if (any(is.na(range_index))) {
    stop("Every stability group must have a geographic range descriptor.")
  }

  svm_df <- data.frame(
    group = stability_groups,
    range_diameter_km = range_descriptor$range_diameter_km[range_index],
    resolution = as.numeric(stability$resolution),
    stab = factor(stability$stability_reached, levels = c(FALSE, TRUE)),
    stability_status = as.character(stability$stability_status),
    stringsAsFactors = FALSE
  )
  # Insufficient combinations with at least three sampled values enter the SVM
  # as negatives, but keep their diagnostic status.
  svm_eligible <- !is.na(stability$n_sampled) & stability$n_sampled >= 3
  svm_keep <- svm_eligible & is.finite(svm_df$range_diameter_km) & is.finite(svm_df$resolution) & !is.na(svm_df$stab)
  svm_df <- svm_df[svm_keep, , drop = FALSE]

  n_insufficient_in_svm <- sum(svm_keep & stability$stability_status == "insufficient_observations")

  if (length(unique(svm_df$stab)) < 2) {
    stop("BD-TP requires both stable and unstable group-resolution combinations.")
  }
  if (length(unique(svm_df$range_diameter_km)) < 2) {
    stop("BD-TP requires at least two distinct range-size descriptor values.")
  }

  # Other e1071 arguments retain their relevant defaults, including
  # C-classification, cost = 1 and probability = FALSE.
  svm_model <- e1071::svm(
    stab ~ range_diameter_km + resolution,
    data = svm_df,
    kernel = "linear",
    scale = TRUE
  )
  hyperplane <- estimate_tp_extract_svm_hyperplane(svm_model)
  intercept <- unname(hyperplane$coefficients[["intercept"]])
  slope <- unname(hyperplane$coefficients[["slope"]])

  diameter_range <- range(svm_df$range_diameter_km, na.rm = TRUE)
  resolution_range <- range(svm_df$resolution, na.rm = TRUE)

  # Two analytical endpoints are sufficient to draw the fitted line. No
  # prediction grid is needed to determine or display the analytical rule.
  boundary_df <- data.frame(
    range_diameter_km = diameter_range,
    resolution = intercept + slope * diameter_range
  )
  boundary_pred <- stats::predict(svm_model, newdata = boundary_df, decision.values = TRUE)
  boundary_df$decision_value <- as.numeric(attr(boundary_pred, "decision.values"))
  decision_orientation <- c(
    positive = svm_model$levels[[svm_model$labels[[1]]]],
    negative = svm_model$levels[[svm_model$labels[[2]]]]
  )

  estimates <- range_descriptor
  estimates$raw_predicted_resolution <- slope * estimates$range_diameter_km + intercept
  estimates$estimated_parameter <- pmax(estimates$raw_predicted_resolution, lower_bound)
  estimates$lower_bound_applied <- estimates$raw_predicted_resolution < lower_bound
  estimates$resolution_extrapolated <- estimates$raw_predicted_resolution < resolution_range[[1]] | estimates$raw_predicted_resolution > resolution_range[[2]]
  any_stable_resolution <- tapply(stability$stability_reached, stability_groups, any, na.rm = TRUE)
  estimates$any_stable_resolution <- unname(any_stable_resolution[as.character(estimates$group)])
  estimates$any_stable_resolution[is.na(estimates$any_stable_resolution)] <- FALSE

  estimates <- estimates[, c(
    "group",
    "estimated_parameter",
    "raw_predicted_resolution",
    "range_diameter_km",
    "any_stable_resolution",
    "lower_bound_applied",
    "resolution_extrapolated"
  ), drop = FALSE]

  list(
    estimates = estimates,
    model = list(
      svm_model = svm_model,
      svm_data = svm_df,
      n_insufficient_in_svm = n_insufficient_in_svm,
      boundary = boundary_df,
      coefficients = c(intercept = intercept, slope = slope),
      hyperplane = hyperplane,
      decision_orientation = decision_orientation,
      calibration = list(
        diameter_range_km = diameter_range,
        resolution_range = resolution_range,
        lower_bound = lower_bound
      )
    )
  )
}
