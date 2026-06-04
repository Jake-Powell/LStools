# Set font family for networkD3 Sankey and preserve other onRender hooks

Set font family for networkD3 Sankey and preserve other onRender hooks

## Usage

``` r
set_font_family(p, fontFamily = "Arial, Helvetica, sans-serif", wait_ms = 100)
```

## Arguments

- p:

  A networkD3 htmlwidget (e.g., from sankeyNetwork()).

- fontFamily:

  A CSS font-family string, e.g. "Arial" or "Arial, sans-serif".

- wait_ms:

  Extra delay before reapplying (handles async rendering).

## Value

The same widget with an extra render hook appended.
