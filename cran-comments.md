## Resubmission

This is a resubmission to address the feedback from the previous CRAN submission.

* The non-CRAN package `rtree` has been removed from `Suggests`.
* Associated functionalities depending on `rtree` have been removed or adjusted accordingly.

## Test environments

The package was tested on the following environments:

* Windows Server 2022 x64 (build 20348)
* Debian GNU/Linux trixie/sid

## R CMD check results

The package passed `R CMD check` on all tested platforms with the following results.
There was one NOTE:

```
New submission

Package was archived on CRAN

Possibly misspelled words in DESCRIPTION:
  Elseberg (12:46)
  al (12:58)
  et (12:55)
  geospatial (8:33)
```

* This is a new submission after the package was archived.
* The flagged words in the DESCRIPTION are proper nouns or standard terminology.

```
CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2024-09-18 as issues were not corrected
    in time.
```

* All previously noted issues have now been corrected, and the package is ready for resubmission.
