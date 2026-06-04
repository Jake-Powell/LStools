# Convert raw information (individual students) into pathways

This function is to convert raw student information into flows of
students through the pipeline. E.g. in raw each row is a unique student
and each column contains information about the student such as grade or
ethnicity. The function assumes each column is a variable we want to
calculate flows for. Therefore, omit columns you don't need before using
this function.

## Usage

``` r
raw_to_pathway(raw)
```

## Arguments

- raw:

  raw data where each row is a unique student.

## Value

pathways, the number of students for each unique pathway.
