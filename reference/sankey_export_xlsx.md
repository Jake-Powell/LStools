# Write sankey to xlsx

Write sankey to xlsx

## Usage

``` r
sankey_export_xlsx(
  p,
  title = "Sankey Information",
  source = "",
  filepath = paste0(getwd(), "/sankey_info.xlsx")
)
```

## Arguments

- p:

  sankey diagram object

- title:

  title for excel spreadsheet

- source:

  sources used for sankey diagram (e.g. Source: National Pupil Database)

- filepath:

  the file path to save the .xlsx file to.

## Details

This function writes the sankey diagram information (flows) to an .xlsx
file with title (\`title\`) and subtitle (\`source\`) ready for SDC
checks / outputting.
