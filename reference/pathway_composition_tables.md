# A function to get the summary of the describers at each stage of the pipeline.

A function to get the summary of the describers at each stage of the
pipeline.

## Usage

``` r
pathway_composition_tables(
  pathways,
  describers,
  stages,
  year_column = NULL,
  do_SDC = FALSE,
  round_to = 10,
  issue_level = 5
)
```

## Arguments

- pathways:

  data frame containing pathways.

- describers:

  A character vector of column names in pathways which we want to
  describe the stages (such as FSM, Sex, IDACI, etc)

- stages:

  A character vector of column names in pathways we split by the columns
  given in describers. (columns such as KS1, KS2, etc)

- year_column:

  The column name referring to the reference year of the record (such as
  age that students took their GCSEs). Default is \`NULL\` which implies
  all records come from the same year.

- do_SDC:

  Flag (TRUE/FALSE) for whether statistical disclosure is performed.

- round_to:

  number to round to

- issue_level:

  number, where values with fewer students have their value omitted (set
  to "-").
