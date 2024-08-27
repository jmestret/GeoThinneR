## Resubmission

This is a resubmission to address the feedback from the previous CRAN submission.

* Moved the Note to the Description field in the DESCRIPTION file as requested.
* Shortened the package title to meet the 65-character limit.
* Replaced `\dontrun{}` with `\donttest{}` for examples that can be executed.
* Wrapped examples that require packages in `Suggests` using `if(requireNamespace("pkgname", quietly = TRUE)){}` to avoid errors if those packages are not installed.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
