# Internal GeoThinned Constructor

Low-level constructor used internally to create a \`GeoThinned\` object.

## Usage

``` r
new_GeoThinned(retained, method, params = list(), original_data = NULL)
```

## Arguments

- retained:

  A list of logical vectors indicating retained points per trial.

- method:

  The thinning method used (e.g., "distance", "grid", "precision").

- params:

  A list of parameters used in thinning.

- original_data:

  The original unmodified data.

## Value

A \`GeoThinned\` object.
