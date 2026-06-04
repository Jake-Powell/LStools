# Apply Statistical disclosure to numbers

Apply Statistical disclosure to numbers

## Usage

``` r
apply_SDC(numbers, round_to = 10, issue_level = 5)
```

## Arguments

- numbers:

  numbers to apply SDC to.

- round_to:

  number to round to

- issue_level:

  number, where numbers with fewer students have their value omitted
  (set to "Below issue_level").

## Value

vector containing the numbers after performing SDC
