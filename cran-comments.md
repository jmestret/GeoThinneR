## Release summary

This is the CRAN submission of GeoThinneR v2.1.3.

#### Added

* Added a `duplicates` argument to distance thinning, allowing exact duplicate coordinates to be removed before neighbor searches or retained for processing.

#### Changed

* Moved some helper functions to internal while keeping the user thinning API unchanged.

#### Fixed

* Fixed an issue introduced in version 2.1.0 where priority ordering was reversed in grid- and precision-based thinning.
* Fixed `sample()` indexing when tie breaking in distance thinning, ensuring that a unique lowest-priority candidate is correctly selected for removal.
* Improved consistency between Haversine and kd-tree distance thresholds by using the equivalent 3D chord distance.
* Corrected geographic nearest neighbor distances printed in `summary()`.

## Test environments

The package was tested on the following environments:

* Windows Server 2022 x64 (build 20348)
* Debian GNU/Linux trixie/sid

## R CMD check results

The package passed `R CMD check` on all tested platforms.

0 errors | 0 warnings | 0 notes
