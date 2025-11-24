## Release summary

This is the CRAN submission of GeoThinneR v2.1.0.

#### Added

* Added `is_lonlat()` helper function to check for valid longitude/latitude ranges.
* A warning is printed when coordinates lie outside the typical global longitude/latitude ranges.
* `summary()` now allows to choose which trial to summarise using the `trial` argument.
* Support for the `priority` parameter in the `"distance"` thinning method. When multiple candidate points have the same number of neighbors, the point with the lowest priority is removed (#1).
* Support for `NA` values in the `priority` vector. These are treated as the lowest priority and trigger a warning.

#### Changed

* The `"grid"` and `"precision"` methods now handle `priority` ties by randomly selecting among equally prioritized points.
* Updated the vignette section on the `priority` parameter.
* Added `s2 (>= 1.1.0)` to *Suggests* in the DESCRIPTION file.

#### Fixed

* `print()` now reports both the number of trials run and returned (#3).
* `summary()` no longer errors when `s2` is missing or too old. Now spatial coverage is set to `NA` with a message (#3).

## Test environments

The package was tested on the following environments:

* Windows Server 2022 x64 (build 20348)
* Debian GNU/Linux trixie/sid

## R CMD check results

The package passed `R CMD check` on all tested platforms.

0 errors | 0 warnings | 0 notes
