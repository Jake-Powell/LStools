# Create sankey diagram of the filtered data.

Create sankey diagram of the filtered data.

## Usage

``` r
sankey_filtered(
  pathways,
  flow_columns,
  filters = "",
  union = F,
  fontFamily = NULL,
  fontSize = 7,
  do_SDC = FALSE,
  ...
)
```

## Arguments

- pathways:

  student pathways with number of students.

- flow_columns:

  Column names (or index) of the 'stages' in pathways that we want to
  create a sankey diagram of.

- filters:

  a character vector describing the filters to apply to the data. Of the
  format \`XX: YY\` where XX is the stage and YY the node, such as 'FSM:
  Yes'.

- union:

  Flag (TRUE/FALSE) for whether we want the union of filters (only
  across single stages)

- fontFamily:

  font family for the node text labels.

- fontSize:

  numeric font size in pixels for the node text labels.

- do_SDC:

  Flag (TRUE/FALSE) for whether you want to do SDC via the
  \`SDC_to_links()\` function.

- ...:

  arguments to \`SDC_to_links()\` or \`networkD3::sankeyNetwork()\`.

## Value

sankey diagram
