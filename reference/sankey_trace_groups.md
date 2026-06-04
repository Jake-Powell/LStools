# Create sankey diagram where we show flow across multiple groups (using a single column)

Create sankey diagram where we show flow across multiple groups (using a
single column)

## Usage

``` r
sankey_trace_groups(
  pathways,
  flow_columns,
  group_column,
  fontFamily = NULL,
  fontSize = 7,
  do_SDC = FALSE,
  colors = viridis::viridis(10),
  ...
)
```

## Arguments

- pathways:

  student pathways with number of students.

- flow_columns:

  Column names (or index) of the 'stages' in pathways that we want to
  create a sankey diagram of.

- group_column:

  the name (or index) of the group column

- fontFamily:

  font family for the node text labels.

- fontSize:

  numeric font size in pixels for the node text labels.

- do_SDC:

  Flag (TRUE/FALSE) for whether you want to do SDC via the
  \`SDC_to_links()\` function.

- colors:

  a character vector of length at least the number of groups to colour
  the edges in the sankey diagram

- ...:

  arguments to \`SDC_to_links()\` or \`networkD3::sankeyNetwork()\`.

## Value

sankey diagram
