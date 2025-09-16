All notable changes to this project will be documented in this file.

# GeoThinneR (development version)

## Added

* Support for the `priority` parameter in the `"distance"` thinning method. When multiple candidate points have the same number of neighbors, the point with the lowest priority is removed (#1).
* Support for `NA` values in the `priority` vector. These are treated as the lowest priority and trigger a warning.

## Changed

* The `"grid"` and `"precision"` methods now handle `priority` ties by randomly selecting among equally prioritized points.
* Updated the vignette section on the `priority` parameter.
* Added `s2 (>= 1.1.0)` to *Suggests* in the DESCRIPTION file.

## Fixed

* `print()` now reports both the number of trials run and returned (#3).
* `summary()` no longer errors when `s2` is missing or too old. Now spatial coverage is set to `NA` with a message (#3).

# GeoThinneR 2.0.0 - 24/04/2025

**Breaking changes**. This version is backward-incompatible with earlier versions of GeoThinneR.

## Major changes

* Introduced a new `GeoThinned` S3 class to represent thinning results. All calls to `thin_points()` now return a `GeoThinned` object instead of a list of logical vectors or data frames.
* Added new helper methods for `GeoThinned` objects:
  - `print()` and `summary()` for inspection.
  - `plot()` for visual comparison of thinned vs. original data.
  - `largest()` to retrieve the trial with the most retained points.
  - `largest_index()` to get the index of the best trial.
  - `get_trial()` to access specific trials.
* Redesigned `thin_points()` to return structured output and provide a more consistent user interface.
* Output structure and behavior of all thinning methods has been updated for compatibility with the new object-oriented design.
* We have improved the thinning algorithms by implementing two new modifications of the kd-tree method: local kd-trees (`local_kd_tree`) and restricted search (`k_estimation`).
* The round and hashing method (`round_hash`) now is deprecated and has been removed from the package because of presenting lower accuracy than the other distance-based methods.
* The thinning method argument `method` now includes the three main thinning strategies implemented in GeoThinneR for better understanding and usability. These are: "distance", "grid", and "precision".
* The `search_type` argument has been included for distance-based thinning (`method = "distance"`) to select between the search algorithms ("brute", "kd_tree", "local_kd_tree", and "k_estimation").
* The `n` argument for the grid-based thinning (`method = "grid"`) now lets you specify the number of points to retain per grid cell.

## Minor changes

* The `long_col` argument now is called `lon_col` for consistency with the `lat_col` argument.
* The default column names (`lon_col` and `lat_col') have been changed from "decimalLongitude" and "decimalLatitude" to "lon" and "lat".
* Improved documentation, error messages, and reproducibility behavior across all functions.
* Updated the package vignette to reflect all changes.

# GeoThinneR 1.1.0 - 03/10/2024

* Removed non-CRAN dependency (`rtree`) and associated functions related to R-Trees structure thinning method (`r_tree_thinning()`).
* Minor improvements to documentation.

# GeoThinneR 1.0.0 - 02/09/2024 

* Initial release!
