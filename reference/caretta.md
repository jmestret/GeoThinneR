# Loggerhead Sea Turtle (*Caretta caretta*) Occurrence Records in the Mediterranean Sea

This dataset contains georeferenced occurrence records of the Loggerhead
Sea Turtle (*Caretta caretta*) within the Mediterranean Sea from 1962 to
2025. The data were obtained from the Global Biodiversity Information
Facility (GBIF) and filtered to include only marine points falling
within a defined Mediterranean polygon. Duplicated coordinates were
removed.

## Usage

``` r
data("caretta")
```

## Format

A data frame with 8340 rows and 5 variables:

- decimalLongitude:

  Numeric. Longitude coordinates (WGS84).

- decimalLatitude:

  Numeric. Latitude coordinates (WGS84).

- year:

  Integer. Year in which the observation was recorded.

- species:

  Character. The species name (*Caretta caretta*).

- coordinateUncertaintyInMeters:

  Numeric. Positional uncertainty of the coordinates, in meters.

## Source

Global Biodiversity Information Facility (GBIF)  
GBIF Occurrence Download https://doi.org/10.15468/dl.9jcjrm  
Accessed from R via rgbif (https://github.com/ropensci/rgbif) on
2025-02-07

## Details

The data were accessed through the GBIF API and clipped spatially using
a Mediterranean Sea polygon
(https://www.marineregions.org/gazetteer.php?p=details&id=1905). All
records were filtered to ensure presence-only data with valid geographic
coordinates. Duplicate coordinate pairs were removed to reduce
redundancy.

This dataset is used as a real-world example throughout the GeoThinneR
package and the accompanying article.

For reproducibility, the GBIF download script is included in
`data-raw/caretta_download.R`.

The Mediterranean Sea polygon used to spatially filter the records was
download from Marineregions.org:
<https://www.marineregions.org/gazetteer.php?p=details&id=1905>. It is
distributed with the package as `inst/extdata/mediterranean_sea.gpkg`
and used in the `data-raw/caretta_download.R` script.
