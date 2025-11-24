# Yellowfin Tuna (*Thunnus albacares*) Worldwide Occurrence Records

This dataset contains georeferenced occurrence records of the Yellowfin
Tuna (*Thunnus albacares*) from 1950 to 2025, obtained globally from the
Global Biodiversity Information Facility (GBIF). The dataset was
filtered to include presence-only records with valid geographic
coordinates and year, and duplicate coordinates were removed.

## Usage

``` r
data("thunnus")
```

## Format

A data frame with 80,163 rows and 3 variables:

- decimalLongitude:

  Numeric. Longitude coordinates (WGS84).

- decimalLatitude:

  Numeric. Latitude coordinates (WGS84).

- year:

  Integer. Year in which the observation was recorded.

## Source

Global Biodiversity Information Facility (GBIF)  
GBIF Occurrence Download https://doi.org/10.15468/dl.xsyrkh  
Accessed from R via rgbif (https://github.com/ropensci/rgbif) on
2025-02-07

## Details

The data were accessed through the GBIF API and used for benchmarking
thinning algorithms in the GeoThinneR package. Coordinates include
marine records, and the data were globally sampled across many decades.

For reproducibility, the GBIF download script is included in
`data-raw/thunnus_download.R`.
