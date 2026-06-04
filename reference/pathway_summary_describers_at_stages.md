# A function to get the summary of the describers at each stage of the pipeline.

A function to get the summary of the describers at each stage of the
pipeline.

## Usage

``` r
pathway_summary_describers_at_stages(pathways, describers, stages)
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
