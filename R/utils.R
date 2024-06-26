

#' Write data frame to excel with title and source
#'
#' @param data data frame
#' @param title title for excel sheet
#' @param source source information of the data frame.
#' @param filepath path to save the excel file.
#'
#'
#' @examples
#' if(F){
#' mtcars[1:10,] |> data_frame_to_excel(title = 'First 10 rows of mtcars',
#' source = 'Source: R', filepath = 'first_10_rows_mtcars.xlsx')
#' }
data_frame_to_xlsx <- function(data,
                                title = '',
                                source = '',
                                filepath = paste0(getwd(), '/data_frame_info.xlsx')){
  # xlsx format values
  main_title = openxlsx::createStyle(fontSize = 14, textDecoration = "bold", halign = "left")
  second_title = openxlsx::createStyle(fontSize = 10, textDecoration = "italic", halign = "left")
  student_count = openxlsx::createStyle(numFmt = "#,##0")

  # Create and save the excel file
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Data")
  openxlsx::writeData(wb, 1, x = title, startRow = 1, startCol = 1)
  openxlsx::writeData(wb, 1, x = source, startRow = 2, startCol = 1)
  openxlsx::writeDataTable(wb,
                           sheet = "Data",
                           x = data,
                           startRow = 4,
                           rowNames = FALSE,
                           withFilter = TRUE)

  openxlsx::addStyle(wb = wb, sheet = 1, rows = 1, cols = 1, style = main_title)
  openxlsx::addStyle(wb = wb, sheet = 1, rows = 2, cols = 1, style = second_title)
  openxlsx::setColWidths(wb, sheet=1, cols = 1:3, widths = 'auto')

  # Save workbook
  openxlsx::saveWorkbook(wb,  file = filepath, overwrite = TRUE)

}
