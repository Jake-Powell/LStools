# Write data frame to openxlsx workbook

Write data frame to openxlsx workbook

## Usage

``` r
data_frame_to_workbook(
  append_to = NULL,
  data = NULL,
  sheet = "Data",
  title = "",
  source = ""
)
```

## Arguments

- append_to:

  either NULL for a new workbook or a workbook to append the sheet to.

- data:

  data frame

- sheet:

  Sheet name

- title:

  title for excel sheet

- source:

  source information of the data frame.

## Value

workbook
