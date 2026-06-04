# Write data frame to excel with title and source

Write data frame to excel with title and source

## Usage

``` r
data_frame_to_xlsx(
  data,
  title = "",
  source = "",
  filepath = paste0(getwd(), "/data_frame_info.xlsx")
)
```

## Arguments

- data:

  data frame

- title:

  title for excel sheet

- source:

  source information of the data frame.

- filepath:

  path to save the excel file.

## Examples

``` r
if(FALSE){
mtcars[1:10,] |> data_frame_to_excel(title = 'First 10 rows of mtcars',
source = 'Source: R', filepath = 'first_10_rows_mtcars.xlsx')
}
```
