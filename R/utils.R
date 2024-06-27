#' Write data frame to excel with title and source
#'
#' @param data data frame
#' @param title title for excel sheet
#' @param source source information of the data frame.
#' @param filepath path to save the excel file.
#'
#'
#' @examples
#' if(FALSE){
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

#' Write data frame to openxlsx workbook
#'
#' @param data data frame
#' @param title title for excel sheet
#' @param source source information of the data frame.
#' @param sheet Sheet name
#' @param append_to either NULL for a new workbook or a workbook to append the sheet to.
#'
#' @return workbook
data_frame_to_workbook <- function(append_to = NULL,
                                   data = NULL,
                                   sheet = 'Data',
                                   title = '',
                                   source = ''){
  # xlsx format values
  main_title = openxlsx::createStyle(fontSize = 14, textDecoration = "bold", halign = "left")
  second_title = openxlsx::createStyle(fontSize = 10, textDecoration = "italic", halign = "left")
  student_count = openxlsx::createStyle(numFmt = "#,##0")

  # Create and save the excel file
  if(!is.null(append_to)){
    wb <-  append_to
  }else{
    wb <- openxlsx::createWorkbook()

  }
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet = sheet, x = title, startRow = 1, startCol = 1)
  openxlsx::writeData(wb, sheet = sheet, x = source, startRow = 2, startCol = 1)
  openxlsx::writeDataTable(wb,
                           sheet = sheet,
                           x = data,
                           startRow = 4,
                           rowNames = FALSE,
                           withFilter = TRUE)

  openxlsx::addStyle(wb = wb, sheet = sheet, rows = 1, cols = 1, style = main_title)
  openxlsx::addStyle(wb = wb, sheet = sheet, rows = 2, cols = 1, style = second_title)
  # openxlsx::setColWidths(wb, sheet=sheet, cols = 2:ncol(data), widths = 'auto')

  # Save workbook
  return(wb)
}


#' Apply Statistical disclosure to numbers
#'
#' @param numbers numbers to apply SDC to.
#' @param round_to number to round to
#' @param issue_level number, where numbers with fewer students have their value omitted (set to "Below issue_level").
#'
#' @return vector containing the numbers after performing SDC
#' @export
#'
apply_SDC <- function(numbers, round_to = 10, issue_level = 5){
  new_numbers = numbers
  new_numbers[new_numbers <= issue_level] = paste0('Below ', issue_level)
  new_numbers[new_numbers != paste0('Below ', issue_level)] = new_numbers[new_numbers != paste0('Below ', issue_level)] |>
    as.numeric() |>
    plyr::round_any(round_to)

  new_numbers
}
