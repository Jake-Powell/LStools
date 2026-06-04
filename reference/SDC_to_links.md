# Statistical disclosure to sankey

Statistical disclosure to sankey

## Usage

``` r
SDC_to_links(links, round_to = 5, issue_level = 5)
```

## Arguments

- links:

  links of a sankey diagram

- round_to:

  number to round to

- issue_level:

  number, where links with fewer students have their value omitted (set
  to "Below issue_level").

## Value

vector containing the links after performing SDC
