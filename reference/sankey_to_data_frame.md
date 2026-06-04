# Convert a sankey diagram to a data frame

Convert a sankey diagram to a data frame

## Usage

``` r
sankey_to_data_frame(p, simple = T)
```

## Arguments

- p:

  sankey diagram object

- simple:

  Flag (TRUE/FALSE) for whether you want simplified information (a
  single data frame with source target and number of students) or
  original data (two data frames one for links one for nodes)

## Details

this function takes a sankey diagram (sankeyNetwork object) and converts
the underlying data into a data frame (or list of data frames).
