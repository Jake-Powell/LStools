
#'  create_stage_composition()
#'
#' Creates a markdown report looking at the composition of an individual stage.
#'
#' @details  This function creates a markdown report that produces a table of values for the stage (column in the data) of interest. It also can breakdown the composition of the stage by other columns in the data y using `describer_columns` input. Where horizontal bar charts are created showing the number of students within unique groups within the stage and each column given in `describer_columns`.
#'
#' Moreover, statistical control can be toggled via the `do_SDC` flag, where the level of statistical disclosure control can be fine tuned using `round_to` and `issue_level`.
#'
#' @param raw raw data where each row is a unique student
#' @param stage_column the column name (or index) within raw that we want to explore.
#' @param describer_columns column names (or indices) where we explore the relationship with `stage_column`.
#' @param do_SDC Flag (TRUE/FALSE) for whether statistical control is applied, the level of statistic control can be varied using `round_to` or `issue_level`.
#' @param round_to numeric, value to round number of students to default is `5`.
#' @param issue_level numeric, and count of students less than `issue_level` will be redacted. This is performed prior to rounding.
#' @param report_kind What kind of report to produce `interactive` (html document) or `static` (word document).
#' @param reference_docx p
#' @param output_file The name of the file to output (with extension)
#' @param output_dir The directory to produce the output file.
#' @param report_name text to add to the filename if output_file is not provided.

#'
#' @export
#'
create_stage_composition <- function(raw,
                                     stage_column,
                                     describer_columns,
                                     do_SDC = FALSE,
                                     round_to = 5,
                                     issue_level = 5,
                                     report_name = NULL,
                                     report_kind = 'interactive',
                                     reference_docx = NULL,
                                     output_file = NULL,
                                     output_dir = NULL
){
  # 1) Setup.
  # A) Check inputs.

  # B) Choose output file name.
  if(report_kind == 'interactive'){
    if(is.null(output_file) & is.null(report_name)){
      output_file = 'stage_composition.html'
    }
    else if(is.null(output_file) & !is.null(report_name)){
      output_file = paste0(report_name, '_stage_composition.html')
    }
  }
  else if(report_kind == 'static'){
    if(is.null(output_file) & is.null(report_name)){
      output_file = 'stage_composition.docx'
    }
    else if(is.null(output_file) & !is.null(report_name)){
      output_file = paste0(report_name, '_stage_composition.docx')
    }
  }else{
    stop('Invalid report_kind input!')
  }

  # Set the output directory if not specified.
  if(is.null(output_dir)){
    output_dir = getwd()
  }


  # 2) Render basic stats_static document.
  if(report_kind == 'static'){
    if(is.null(reference_docx)){
      rmarkdown::render(paste0(system.file(package = "LStools"), "/reports/stage_composition.Rmd"),
                        params = list(raw = raw,
                                      describer_columns = describer_columns,
                                      do_SDC = do_SDC,
                                      round_to = round_to,
                                      issue_level = issue_level,
                                      stage_column = stage_column,
                                      report_name = report_name,
                                      report_kind = report_kind,
                                      reference_docx = reference_docx),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(toc = TRUE, toc_depth = 4))
    }
    else{
      rmarkdown::render(paste0(system.file(package = "LStools"), "/reports/stage_composition.Rmd"),
                        params = list(raw = raw,
                                      describer_columns = describer_columns,
                                      do_SDC = do_SDC,
                                      round_to = round_to,
                                      issue_level = issue_level,
                                      stage_column = stage_column,
                                      report_name = report_name,
                                      report_kind = report_kind,
                                      reference_docx = reference_docx
                        ),
                        output_file = output_file,
                        output_dir = output_dir,
                        output_format = rmarkdown::word_document(reference_docx = reference_docx, toc = TRUE, toc_depth = 4))
    }

  }

  if(report_kind == 'interactive'){
    rmarkdown::render(paste0(system.file(package = "LStools"), "/reports/stage_composition.Rmd"),
                      params = list(raw = raw,
                                    describer_columns = describer_columns,
                                    stage_column = stage_column,
                                    do_SDC = do_SDC,
                                    round_to = round_to,
                                    issue_level = issue_level,
                                    report_name = report_name,
                                    report_kind = report_kind,
                                    reference_docx = reference_docx),
                      output_file = output_file,
                      output_dir = output_dir,
                      output_format = rmarkdown::html_document(toc = TRUE, toc_depth = 4,  toc_float =  TRUE, theme = 'cerulean', highlight = 'tango'))
  }

}
