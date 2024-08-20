#' Loggerhead Sea Turtle (\emph{Caretta caretta}) Occurrences in the Mediterranean Sea
#'
#' This dataset contains a subset of global occurrences of the Loggerhead Sea Turtle (\emph{Caretta caretta}), filtered for records in the Mediterranean Sea. The data were sourced from the Global Biodiversity Information Facility (GBIF).
#'
#' @format A data frame with 6785 rows and 5 columns:
#' \describe{
#'   \item{decimalLongitude}{Numeric. Longitude coordinates (WGS84).}
#'   \item{decimalLatitude}{Numeric. Latitude coordinates (WGS84).}
#'   \item{year}{Integer. The year in which the occurrence was recorded.}
#'   \item{species}{Character. The scientific name of the species, i.e., \emph{Caretta caretta}.}
#'   \item{coordinateUncertaintyInMeters}{Numeric. The uncertainty of the coordinates in meters.}
#' }
#'
#' @details
#' The dataset has been filtered to include only records within the Mediterranean Sea. The occurrence data cover multiple years, which provides information on the temporal distribution of the species in this region.
#'
#' @source Global Biodiversity Information Facility (GBIF), \url{https://www.gbif.org/species/8894817}
#'
#' @usage data("caretta")
"caretta"
