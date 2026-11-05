# Display citation information when attaching GeoThinneR
.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage(
      "Type 'citation(\"GeoThinneR\")' to see which references to cite when using GeoThinneR."
    )
  }
}
