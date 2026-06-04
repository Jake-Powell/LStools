# create_stage_composition_report()

Creates a markdown report looking at the composition of an individual
stage.

## Usage

``` r
create_stage_composition_report(
  raw,
  stage_column,
  describer_columns,
  do_SDC = FALSE,
  round_to = 10,
  issue_level = 5,
  report_name = NULL,
  report_kind = "interactive",
  reference_docx = NULL,
  output_file = NULL,
  output_dir = NULL
)
```

## Arguments

- raw:

  raw data where each row is a unique student

- stage_column:

  the column name (or index) within raw that we want to explore.

- describer_columns:

  column names (or indices) where we explore the relationship with
  \`stage_column\`.

- do_SDC:

  Flag (TRUE/FALSE) for whether statistical control is applied, the
  level of statistic control can be varied using \`round_to\` or
  \`issue_level\`.

- round_to:

  numeric, value to round number of students to default is \`10\`.

- issue_level:

  numeric, and count of students less than \`issue_level\` will be
  redacted. This is performed prior to rounding.

- report_name:

  text to add to the filename if output_file is not provided.

- report_kind:

  What kind of report to produce \`interactive\` (html document) or
  \`static\` (word document).

- reference_docx:

  p

- output_file:

  The name of the file to output (with extension)

- output_dir:

  The directory to produce the output file.

## Details

This function creates a markdown report that produces a table of values
for the stage (column in the data) of interest. It also breakdowns the
composition of the stage by other columns in the data y using
\`describer_columns\`. Where horizontal bar charts are created showing
the number of students within unique groups within the stage and each
column given in \`describer_columns\`, together with the underlying data
tables - which can be downloaded.

Moreover, statistical control can be toggled via the \`do_SDC\` flag,
where the level of statistical disclosure control can be fine tuned
using \`round_to\` and \`issue_level\`.
