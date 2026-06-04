
#' Append JavaScript to an Existing htmlwidgets Render Hook
#'
#' Appends JavaScript code to an existing \code{htmlwidgets} render hook,
#' allowing multiple widget customisations to be applied via successive
#' calls. This is useful when composing functions that each add their own
#' \code{onRender()} behaviour without overwriting previously registered
#' JavaScript.
#'
#' @param p An \code{htmlwidget} object.
#' @param js_code A character string containing JavaScript code to be
#'   executed when the widget is rendered. The code should be a valid
#'   \code{onRender()} callback function.
#'
#' @return The modified \code{htmlwidget} object.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' p <- networkD3::sankeyNetwork(...)
#'
#' p <- append_onRender(
#'   p,
#'   "function(el) { console.log('First hook'); }"
#' )
#'
#' p <- append_onRender(
#'   p,
#'   "function(el) { console.log('Second hook'); }"
#' )
#' }
append_onRender <- function(p, js_code) {
  existing <- p$x$jsHooks$render

  if (is.null(existing)) {
    htmlwidgets::onRender(p, js_code)
  } else {
    combined <- paste0(existing, "\n", js_code)
    htmlwidgets::onRender(p, combined)
  }
}


#' Functions to customise the styling of the sankey diagrams
#'
#' @details
#'  - `add_column_names()` this function can be used to add names above stages (columns) in the the sankey diagram.
#'
#' - `add_node_hover_text()` this can be used to change the hover information over nodes.
#'
#' - `add_link_hover_text()` this can be used to changed the hover information of links.
#'
#' - `update_node_title()` this can be used to change the displayed node label in the sankey diagram
#'
#' - `update_sankey_colour()` this can be used to change the node/link colouring of the sankey diagram (sankey_filtered, or any sankey diagram created using the networkD3 package). You can omit the link colours and provide a type for link colouring dependent on source or target nodes (using `type`).
#'
#'
#'
#' @param p html widget of a sankey diagram
#' @param fontSize numeric font size in pixels for the node text labels.
#' @param fontFamily font family for the node text labels.
#' @param colors_node a character vector of length the number of nodes in the sankey (`p |> get_nodes() |> length()`) specifying the desired colour for the nodes.
#' @param colors_link a character vector of length the number of links in the sankey (`p |> get_links() |> length()`) specifying the desired colour for the links
#' @param type special colour systems where the links are the same colour as the nodes they come from (`type = "source"`) or the colour of the nodes they go towards (`type = "target"`)
#' @param link_alpha numeric between 0 and 1 specifying the transparency to apply to link colours (closer to zero the larger the transparency)
#' @param hovertext a character vector of either the same length as nodes or links (depending on the function) to overwrite the hover information in the sankey diagram.
#' @param titles a character vector of the same length as the number of stages in the sankey diagram specifying the title to but above each stage.
#' @param labels a character vector of the same length as the number of nodes in the sankey diagram specifying the new label for each node.
#'
#' @export
#'
add_column_names <- function(p, titles, fontSize = p$x$options$fontSize, fontFamily = p$x$options$fontFamily){
  labels = titles
  js = paste0('
  function(el) {
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i).sort(function(a, b){return a - b});
    var labels = [',
              paste(shQuote(labels, type="cmd"), collapse=", "),
              '];
    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d)
        .attr("y", 12)
        .text(labels[i])
        .style("font-size", "',fontSize,'px")
        .style("font-family", "',fontFamily,'");
    })
  }
')
  append_onRender(p, js)

}

#' @rdname add_column_names
#' @export
add_node_hover_text <- function(p, hovertext){
  p$x$nodes$state <- hovertext
  js = '
            function(el, x) {
                d3.selectAll(".node").select("title foreignObject body pre")
                .text(function(d) { return d.state; });
            }
            '
  append_onRender(p, js)

}

#' @rdname add_column_names
#' @export
add_link_hover_text <- function(p, hovertext){
  p$x$links$name <- hovertext
  js = '
  function(el, x) {
  d3.selectAll(".link").select("title foreignObject body pre")
  .text(function(d) { return d.name; });
  }
  '
  append_onRender(p, js)

}

#' @rdname add_column_names
#' @export
update_node_labels = function(p, labels = NULL) {

  if (length(labels) == length(p$x$nodes$name)) {
    p$x$nodes$name <- labels
  }

  append_onRender(
    p,
    '
    function(el) {

      d3.select(el)
        .selectAll(".node text")
        .each(function() {

          var text = d3.select(this);

          var x = text.attr("x");
          var y = text.attr("y");

          var lines = text.text().split("\\n");

          if(lines.length > 1) {

            text.text("");

            lines.forEach(function(line, i) {

              text.append("tspan")
                .attr("x", x)
                .attr("dy", i === 0 ? 0 : "1.1em")
                .text(line);

            });
          }

        });

    }
    '
  )
}


#' @rdname add_column_names
#' @export
update_sankey_colour <- function(p ,colors_node = NULL, colors_link = NULL, type = 'target', link_alpha = 1){
  if(length(colors_node) == length(p$x$nodes$name) & is.null(colors_link)){
    nodes = p$x$nodes
    all_links = p$x$links
    #Give each node a unique colour group
    node_group = paste0('node_',1:nrow(nodes)) # All others will vary.
    node_col = colors_node

    # Group links on their target node.
    link_group = paste0('link_',1:nrow(all_links))
    link_col = rep('gray', nrow(all_links))
    if(type == 'target'){
      target_node_col = node_col[all_links[,2]+1]
      target_node_col_alpha =colorspace::adjust_transparency(target_node_col, alpha =link_alpha) # Add transparenc
      link_col = target_node_col_alpha
    }
    if(type == 'source'){
      target_node_col = node_col[all_links[,1]+1]
      target_node_col_alpha =colorspace::adjust_transparency(target_node_col, alpha =link_alpha) # Add transparency
      link_col = target_node_col_alpha
    }


    my_color <- paste('d3.scaleOrdinal() .domain([',
                      paste(shQuote(c(unique(node_group),unique(link_group)), type="cmd"), collapse=", "),
                      ']) .range([',
                      paste(shQuote(c(node_col,link_col), type="cmd"), collapse=", ") ,
                      '])')

    p$x$nodes$group = node_group
    p$x$nodes$colouring = node_col
    p$x$links$group = link_group
    p$x$links$colouring = link_col
    p$x$options$colourScale = my_color

  }
  if(length(colors_node) == length(p$x$nodes$name) & length(colors_link) == length(p$x$links$source)){
    nodes = p$x$nodes
    all_links = p$x$links
    #Give each node a unique colour group
    node_group = paste0('node_',1:nrow(nodes)) # All others will vary.
    node_col = colors_node

    # Group links on their target node.
    link_group = paste0('link_',1:nrow(all_links))
    link_col = colors_link

    my_color <- paste('d3.scaleOrdinal() .domain([',
                      paste(shQuote(c(unique(node_group),unique(link_group)), type="cmd"), collapse=", "),
                      ']) .range([',
                      paste(shQuote(c(node_col,link_col), type="cmd"), collapse=", ") ,
                      '])')

    p$x$nodes$group = node_group
    p$x$nodes$colouring = node_col
    p$x$links$group = link_group
    p$x$links$colouring = link_col
    p$x$options$colourScale = my_color

  }
  append_onRender(p, '')
  # p
}

#' Set font family for networkD3 Sankey and preserve other onRender hooks
#'
#' @param p          A networkD3 htmlwidget (e.g., from sankeyNetwork()).
#' @param fontFamily A CSS font-family string, e.g. "Arial" or "Arial, sans-serif".
#' @param wait_ms    Extra delay before reapplying (handles async rendering).
#' @return           The same widget with an extra render hook appended.
#' @export
set_font_family <- function(
    p,
    fontFamily = "Arial, Helvetica, sans-serif",
    wait_ms = 100
) {
  # Stash for reuse by other hooks
  p$x$options$fontFamily <- fontFamily

  # Safer JS string literal via JSON encoding
  ff_json <- jsonlite::toJSON(fontFamily, auto_unbox = TRUE)

  js <- sprintf(
    'function(el, x) {
       var ff = (x && x.options && x.options.fontFamily) ? x.options.fontFamily : %s;

       function apply() {
         var svg = d3.select(el).select("svg");
         if (svg.empty()) return;

         // Apply to node labels, link labels (if present), and any extra <text> you add.
         svg.selectAll(".node text, .link text, text")
            .style("font-family", ff, "important");
       }

       // Apply immediately and then again after short delays
       apply();
       setTimeout(apply, 0);
       setTimeout(apply, %d);

       // Re-apply if the widget mutates (e.g., redraws labels)
       if (window.MutationObserver) {
         var mo = new MutationObserver(function() { apply(); });
         mo.observe(el, { childList: true, subtree: true });
       }
     }',
    ff_json, as.integer(wait_ms)
  )
  append_onRender(p, js)
}


