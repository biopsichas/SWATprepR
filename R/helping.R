
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

hide_show <- function(graph){
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

#' Function to get maximum min and max dates available for all time series data for all stations for all parameters. 
#'
#' @param meteo_lst nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' @return list with 2 values: minimum and maximum value for all available data.
#' @importFrom lubridate now
#' @export
#'
#' @examples
#' ##get_nx_date_full(meteo_lst)

get_dates <- function(meteo_lst){
  meteo_lst <- meteo_lst$data
  min_date <- as.POSIXct(as.Date(now()), "%Y-%m-%d", tz = "UTC")
  max_date <- as.POSIXct("1900-01-01", "%Y-%m-%d", tz = "UTC")
  for (n in names(meteo_lst)){
    for(p in names(meteo_lst[[n]])){
      if(min_date >= min(meteo_lst[[n]][[p]]$DATE)){
        min_date <-  min(meteo_lst[[n]][[p]]$DATE)
      }
      if(max_date <= max(meteo_lst[[n]][[p]]$DATE)){
        max_date <-  max(meteo_lst[[n]][[p]]$DATE)
      }
    }
  }
  ##Returning a list of two dates
  return(list(min_date = min_date, max_date = max_date))
}
