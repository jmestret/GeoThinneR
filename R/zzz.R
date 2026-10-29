# Display citation information when attaching GeoThinneR
.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage(
      "Type 'citation(\"GeoThinneR\")' to cite the associated R Journal article."
    )
  }
}
