# A function to get the summary of the describers at each transition in the pipeline.

A function to get the summary of the describers at each transition in
the pipeline.

## Usage

``` r
pathway_stage_transition_by_describer(
  pathways,
  describers,
  stages,
  return_type = "list of transitions"
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
  given in describers. (columns such as KS1, KS2, etc), note here that
  the order of stages need to be in chronological order.

- return_type:

  output return type, by default a list of lists of the describers
  nested with a list of transitions. Any other return_type value will
  return a compressed version of a list of data frames for each
  describer where the columns are the describer values and the rows the
  transitions.
