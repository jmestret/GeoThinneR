## Release summary

This is the CRAN submission of GeoThinneR v2.0.0.

This release is backward-incompatible with earlier versions of GeoThinneR.

Key changes:
- Introduction of the new `GeoThinned` S3 class for storing and managing thinning results.
- Revised `thin_points()`, with new argument structure and multiple thinning methods.
- Support for advanced neighbor search strategies (e.g., local kd-tree, k-max estimation).
- Improved performance and parallel processing options.
- Additional functionality and helper tools for post-analysis.

## Test environments

The package was tested on the following environments:

* Windows Server 2022 x64 (build 20348)
* Debian GNU/Linux trixie/sid

## R CMD check results

The package passed `R CMD check` on all tested platforms.

0 errors | 0 warnings | 0 notes
