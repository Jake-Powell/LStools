# Append JavaScript to an Existing htmlwidgets Render Hook

Appends JavaScript code to an existing `htmlwidgets` render hook,
allowing multiple widget customisations to be applied via successive
calls. This is useful when composing functions that each add their own
`onRender()` behaviour without overwriting previously registered
JavaScript.

## Usage

``` r
append_onRender(p, js_code)
```

## Arguments

- p:

  An `htmlwidget` object.

- js_code:

  A character string containing JavaScript code to be executed when the
  widget is rendered. The code should be a valid `onRender()` callback
  function.

## Value

The modified `htmlwidget` object.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- networkD3::sankeyNetwork(...)

p <- append_onRender(
  p,
  "function(el) { console.log('First hook'); }"
)

p <- append_onRender(
  p,
  "function(el) { console.log('Second hook'); }"
)
} # }
```
