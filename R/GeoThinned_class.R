#' GeoThinned Object Constructor and Methods
#'
#' Create and interact with spatial thinning results stored in a GeoThinned object.
#'
#' @param retained A list of logical vectors indicating retained points per trial.
#' @param method The thinning method used (e.g., "distance", "grid", "precision").
#' @param params A list of parameters used in thinning.
#' @param original_data The original unmodified data.
#' @param x An object of class \code{GeoThinned}.
#' @param object An object of class \code{GeoThinned}.
#' @param trial Integer index of the thinning trial to extract (for \code{summary()}, \code{get_trial()}, \code{plot()}, \code{as_sf()}). Default `NULL`, which will return the largest dataset.
#' @param crs Coordinate reference system to assign to the resulting \code{sf} object (optional).
#' @param show_original Logical, whether to show original points.
#' @param col_original Colors for original points.
#' @param col_thinned Colors for thinned points.
#' @param pch_original Point shapes for original points.
#' @param pch_thinned Point shapes for thinned points.
#' @param main Title of the plot.
#' @param ... Additional arguments (ignored).
#'
#' @details
#' When the 's2' package is not installed or the installed version is lower than 1.1.0,
#' the spatial coverage metric for geographic coordinates from the \code{summary()} is not computed.
#' This is an optional summary metric and does not affect the thinning process or any other package functionality.
#'
#' @return A \code{GeoThinned} object or associated results (summary, plot, trial subset).
#' When `thin_points()` is run with `all_trials = FALSE`, the returned object contains only the largest trial; therefore all methods refer to this single subset.
#' @seealso \code{\link{thin_points}}
#' @rdname GeoThinned
#' @export
as_GeoThinned <- function(retained, method, params = list(), original_data = NULL) {
  new_GeoThinned(retained = retained, method = method, params = params, original_data = original_data)
}

#' @rdname GeoThinned
#' @export
new_GeoThinned <- function(retained, method, params = list(), original_data = NULL) {
  structure(
    list(
      retained = retained,
      method = method,
      params = params,
      original_data = original_data
    ),
    class = "GeoThinned"
  )
}

#' @method print GeoThinned
#' @rdname GeoThinned
#' @export
print.GeoThinned <- function(x, ...) {
  cat("GeoThinned object\n")
  cat("Method used:", x$method, "\n")
  cat("Number of trials:", x$params$trials, paste0("(", length(x$retained), " trials returned)\n"))
  cat("Points retained in largest trial:", sum(x$retained[[largest_index(x)]]), "\n")
  invisible(x)
}

#' @method summary GeoThinned
#' @rdname GeoThinned
#' @export
summary.GeoThinned <- function(object, trial = NULL, ...) {
  if (is.null(trial)) {
    trial <- largest_index(object)
  }
  if (trial < 1 || trial > length(object$retained)) {
    stop("Invalid trial index. 'trial' must be between 1 and ", length(object$retained), ".")
  }

  kept <- object$retained[[trial]]
  lon_col <- object$params$lon_col
  lat_col <- object$params$lat_col
  distance_type <- if (!is.null(object$params$distance)) {object$params$distance} else {"haversine"}

  original_coords <- as.matrix(object$original_data[, c(lon_col, lat_col)])
  thinned_coords <- as.matrix(object$original_data[kept, c(lon_col, lat_col)])

  n_orig <- nrow(original_coords)
  n_thin <- sum(kept)
  point_retention <- n_thin / n_orig

  nnd_orig <- compute_nearest_neighbor_distances(original_coords, distance = distance_type)
  nnd_thin <- compute_nearest_neighbor_distances(thinned_coords, distance = distance_type)

  if (object$method %in% c("grid", "precision") && distance_type == "haversine") {
    message("Note: Nearest neighbor distances and spatial coverage are computed using Haversine geometry by default.")
  }

  if (n_thin < 3) {
    warning("Fewer than 3 points in thinned set. Spatial coverage may not be meaningful.")
  }

  has_updated_s2 <- requireNamespace("s2", quietly = TRUE) && utils::packageVersion("s2") >= "1.1.0"

  if (distance_type != "haversine" || has_updated_s2) {
    coverage_orig <- calculate_spatial_coverage(original_coords, distance = distance_type)
    coverage_thin <- calculate_spatial_coverage(thinned_coords, distance = distance_type)
  } else {
    message("Spatial coverage is an optional summary metric and is not computed because package 's2' (>= 1.1.0) is not available. Please install or update 's2' to enable this metric.")
    coverage_orig <- NA
    coverage_thin <- NA
  }

  summary <- list(
    trial = trial,
    method = object$method,
    distance_type = distance_type,
    n_points = list(original = n_orig, thinned = n_thin),
    retention_ratio = point_retention,
    nnd_summary = list(
      original = summary(nnd_orig),
      thinned = summary(nnd_thin)
    ),
    spatial_coverage = c(original = coverage_orig, thinned = coverage_thin)
  )

  class(summary) <- "summary.GeoThinned"
  print(summary)
  invisible(summary)
}

#' @method print summary.GeoThinned
#' @rdname GeoThinned
#' @export
print.summary.GeoThinned <- function(x, ...) {
  nnd_units <- if (x$distance_type == "haversine") {"km"} else {"map units"}
  area_units <- if (x$distance_type == "haversine") {"km2"} else {"map units2"}

  cat("Summary of GeoThinneR Results\n")
  cat("-----------------------------\n")
  cat("Trial summarized  :", x$trial, "\n")
  cat("Method used       :", x$method, "\n")
  cat("Distance metric   :", x$distance_type, "\n\n")

  cat("Number of points:\n")
  cat(sprintf("  %-15s %10d\n", "Original", x$n_points$original))
  cat(sprintf("  %-15s %10d\n", "Thinned", x$n_points$thinned))
  cat(sprintf("  %-15s %10.3f\n\n", "Retention ratio", x$retention_ratio))

  cat("Nearest Neighbor Distances [", nnd_units, "]\n", sep = "")
  nnd_tbl <- rbind(
    Original = unname(round(x$nnd_summary$original[c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")], 3)),
    Thinned = unname(round(x$nnd_summary$thinned[c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")], 3))
  )
  colnames(nnd_tbl) <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")
  print(nnd_tbl, right = TRUE)
  cat("\n")

  cat("Spatial Coverage (Convex Hull Area) [", area_units, "]\n", sep = "")
  cat(sprintf("  %-15s %10.3f\n", "Original", x$spatial_coverage["original"]))
  cat(sprintf("  %-15s %10.3f\n", "Thinned", x$spatial_coverage["thinned"]))
}

#' @method plot GeoThinned
#' @rdname GeoThinned
#' @importFrom graphics points legend
#' @export
plot.GeoThinned <- function(x, trial = NULL,
                            show_original = TRUE,
                            col_original = "#EB714B",
                            col_thinned = "#5183B3",
                            pch_original = 1,
                            pch_thinned = 16,
                            main = NULL,
                            ...) {
  # Validate trial index
  if (is.null(trial)) {
    trial <- largest_index(x)
  }
  if (trial < 1 || trial > length(x$retained)) {
    stop("Invalid trial index. 'trial' must be between 1 and ", length(x$retained), ".")
  }

  kept <- x$retained[[trial]]
  if (!is.logical(kept) || length(kept) != nrow(x$original_data)) {
    stop("Invalid retained vector: must be logical and match original data length.")
  }

  lon_col <- x$params$lon_col
  lat_col <- x$params$lat_col
  coords <- x$original_data[, c(lon_col, lat_col)]

  if (is.null(main)) {
    main <- paste("GeoThinneR - Trial", trial)
  }

  if (show_original) {
    plot(coords, col = col_original, pch = pch_original,
         xlab = "Longitude", ylab = "Latitude",
         main = main, ...)
    points(coords[kept, ], col = col_thinned, pch = pch_thinned)
    legend("topleft",
           legend = c("Removed", "Retained"),
           col = c(col_original, col_thinned),
           pch = c(pch_original, pch_thinned),
           bty = "o", bg="#faf5ef")
  } else {
    plot(coords[kept, ], col = col_thinned, pch = pch_thinned,
         xlab = "Longitude", ylab = "Latitude",
         main = main, ...)
  }
}

#' @rdname GeoThinned
#' @export
largest <- function(x, ...) {
  UseMethod("largest")
}

#' @method largest GeoThinned
#' @rdname GeoThinned
#' @export
largest.GeoThinned <- function(x, ...) {
  x$original_data[x$retained[[largest_index(x)]], , drop = FALSE]
}

#' @rdname GeoThinned
#' @export
largest_index <- function(x, ...) {
  UseMethod("largest_index")
}

#' @method largest_index GeoThinned
#' @rdname GeoThinned
#' @export
largest_index.GeoThinned <- function(x, ...) {
  sizes <- sapply(x$retained, sum)
  which.max(sizes)
}

#' @rdname GeoThinned
#' @export
get_trial <- function(x, trial = NULL, ...) {
  UseMethod("get_trial")
}

#' @method get_trial GeoThinned
#' @rdname GeoThinned
#' @export
get_trial.GeoThinned <- function(x, trial = NULL, ...) {
  if (is.null(trial)) {
    trial <- largest_index(x)
  }
  x$original_data[x$retained[[trial]], , drop = FALSE]
}

#' @rdname GeoThinned
#' @export
as_sf <- function(x, ...) {
  UseMethod("as_sf")
}

#' @method as_sf GeoThinned
#' @rdname GeoThinned
#' @export
as_sf.GeoThinned <- function(x, trial = NULL, crs = 4326, ...) {
  if (is.null(trial)) {
    trial <- largest_index(x)
  }

  retained_ids <- x$retained[[trial]]
  retained_points <- x$original_data[retained_ids, , drop = FALSE]

  # Extract lon/lat columns
  lon_col <- x$params$lon_col
  lat_col <- x$params$lat_col
  if (is.null(lon_col) || is.null(lat_col)) {
    stop("Could not determine longitude and latitude columns.")
  }

  # Convert to sf
  sf::st_as_sf(retained_points, coords = c(lon_col, lat_col), crs = crs, ...)
}
