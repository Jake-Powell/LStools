# Remove columns from pathways

A function to remove columns from the pathway tibble, merging now
identical rows and summing their No_students.

## Usage

``` r
pathway_remove_columns(
  pathways,
  rm_columns = NA,
  keep_columns = NA,
  convert_na = T,
  count_column = "No_students"
)
```

## Arguments

- pathways:

  data frame containing pathways.

- rm_columns:

  The columns names in pathways that are to be removed.

- keep_columns:

  The columns names in pathways that are to be kept.

- convert_na:

  Logical (TRUE/FALSE) for whether we convert NA.

- count_column:

  Default = 'No_students'. The name of the column containing the number
  of students.

## Value

pathways with columns removed and merged counts.
