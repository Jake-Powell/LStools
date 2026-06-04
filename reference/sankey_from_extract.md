# Create sankey diagram from excel file or extracted data

Create sankey diagram from excel file or extracted data

## Usage

``` r
sankey_from_extract(
  filepath = NULL,
  extract = NULL,
  fontFamily = NULL,
  skip = 3,
  fontSize = 7
)
```

## Arguments

- filepath:

  filepath to the .xlsx file containing the sankey diagram information.
  Assume the first 3 rows are to be removed but this can be altered with
  the \`skip\` input.

- extract:

  data frame containing the sankey information, with column names
  including Source, Target and Number of Students.

- fontFamily:

  font family for the node text labels.

- skip:

  Number of rows to be skipped when loading the excel file.

- fontSize:

  numeric font size in pixels for the node text labels.

## Value

sankey diagram
