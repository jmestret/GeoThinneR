#' Loggerhead Sea Turtle (\emph{Caretta caretta}) Occurrence Records in the Mediterranean Sea
#'
#' This dataset contains georeferenced occurrence records of the Loggerhead Sea Turtle
#' (\emph{Caretta caretta}) within the Mediterranean Sea from 1962 to 2025.
#' The data were obtained from the Global Biodiversity Information Facility (GBIF)
#' and filtered to include only marine points falling within a defined Mediterranean polygon.
#' Duplicated coordinates were removed.
#'
#' @format A data frame with 8340 rows and 5 variables:
#' \describe{
#'   \item{decimalLongitude}{Numeric. Longitude coordinates (WGS84).}
#'   \item{decimalLatitude}{Numeric. Latitude coordinates (WGS84).}
#'   \item{year}{Integer. Year in which the observation was recorded.}
#'   \item{species}{Character. The species name (\emph{Caretta caretta}).}
#'   \item{coordinateUncertaintyInMeters}{Numeric. Positional uncertainty of the coordinates, in meters.}
#' }
#'
#' @details
#' The data were accessed through the GBIF API and clipped spatially using
#' a Mediterranean Sea polygon (https://www.marineregions.org/gazetteer.php?p=details&id=1905).
#' All records were filtered to ensure presence-only data with valid geographic coordinates.
#' Duplicate coordinate pairs were removed to reduce redundancy.
#'
#' This dataset is used as a real-world example throughout the GeoThinneR package and the accompanying article.
#'
#' For reproducibility, the GBIF download script is included in \code{data-raw/caretta_download.R}.
#'
#' The Mediterranean Sea polygon used to spatially filter the records was download from Marineregions.org:
#' \url{https://www.marineregions.org/gazetteer.php?p=details&id=1905}. It is distributed with the package as
#' \code{inst/extdata/mediterranean_sea.gpkg} and used in the \code{data-raw/caretta_download.R} script.
#'
#' @source Global Biodiversity Information Facility (GBIF) \cr
#' GBIF Occurrence Download https://doi.org/10.15468/dl.9jcjrm \cr
#' Accessed from R via \pkg{rgbif} (https://github.com/ropensci/rgbif) on 2025-02-07
#'
#' @usage data("caretta")
#' @keywords datasets
"caretta"


#' Yellowfin Tuna (\emph{Thunnus albacares}) Worldwide Occurrence Records
#'
#' This dataset contains georeferenced occurrence records of the Yellowfin Tuna
#' (\emph{Thunnus albacares}) from 1950 to 2025, obtained globally from the Global Biodiversity
#' Information Facility (GBIF). The dataset was filtered to include presence-only records
#' with valid geographic coordinates and year, and duplicate coordinates were removed.
#'
#' @format A data frame with 80,163 rows and 3 variables:
#' \describe{
#'   \item{decimalLongitude}{Numeric. Longitude coordinates (WGS84).}
#'   \item{decimalLatitude}{Numeric. Latitude coordinates (WGS84).}
#'   \item{year}{Integer. Year in which the observation was recorded.}
#' }
#'
#' @details
#' The data were accessed through the GBIF API and used for benchmarking thinning algorithms
#' in the GeoThinneR package. Coordinates include marine records, and
#' the data were globally sampled across many decades.
#'
#' For reproducibility, the GBIF download script is included in \code{data-raw/thunnus_download.R}.
#'
#' @source Global Biodiversity Information Facility (GBIF) \cr
#' GBIF Occurrence Download https://doi.org/10.15468/dl.xsyrkh \cr
#' Accessed from R via \pkg{rgbif} (https://github.com/ropensci/rgbif) on 2025-02-07
#'
#' @usage data("thunnus")
#' @keywords datasets
"thunnus"




