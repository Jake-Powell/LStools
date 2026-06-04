# Create traced sankey diagram

Create traced sankey diagram

## Usage

``` r
sankey_trace(
  pathways,
  flow_columns,
  split_by,
  union = F,
  colors = c("red", "gray", "antiquewhite"),
  fontFamily = NULL,
  fontSize = 7
)
```

## Arguments

- pathways:

  student pathways with number of students.

- flow_columns:

  Column names (or index) of the 'stages' in pathways that we want to
  create a sankey diagram of.

- split_by:

  a character vector describing the filters to split the data by. Of the
  format \`XX: YY\` where XX is the stage and YY the node, such as 'FSM:
  Yes'.

- union:

  Flag (TRUE/FALSE) for whether we want the union of filters (only
  across single stages)

- colors:

  a character vector of length three \`c(colA, colB, colC)\` where colA
  is the link colour for traced students, colB is the link colour for
  other students and colC is the node colour.

- fontFamily:

  font family for the node text labels.

- fontSize:

  numeric font size in pixels for the node text labels.

## Value

traced sankey diagram
