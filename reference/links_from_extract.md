# Get node and links from excel file or extracted data

Get node and links from excel file or extracted data

## Usage

``` r
links_from_extract(filepath = NULL, extract = NULL, skip = 3)
```

## Arguments

- filepath:

  filepath to the .xlsx file containing the sankey diagram information.
  Assume the first 3 rows are to be removed but this can be altered with
  the \`skip\` input.

- extract:

  data frame containing the sankey information, with column names
  including Source, Target and Number of Students.

- skip:

  Number of rows to be skipped when loading the excel file.

## Value

list containing nodes and links
