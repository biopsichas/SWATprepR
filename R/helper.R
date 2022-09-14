
# Internal functions --------------------------------------------------------------------

#' Function to put option remove hide or show all lines in chart and this function also print chart.
#'
#' @param graph plotly graph object
#' @return plotly graph object with option to romove or show all lines
#' @importFrom plotly plotly_build layout
#' @keywords internal
#'
#' @examples
#' ##hide_show_print_graph(fig)

hide_show_print_graph <- function(graph){
  plotly_build(graph) %>%
    layout(updatemenus = list(
      list(type = "buttons", direction = "right", xanchor = "center", yanchor = "top", 
           showactive = FALSE, x = 0.3, y = 1.0,
           buttons = list(
             list(method = "restyle",
                  args = list("visible", "all"),
                  label = "show all"),
             list(method = "restyle",
                  args = list("visible", "legendonly"),
                  label = "hide all")))))
}