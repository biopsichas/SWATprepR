
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

#' Reading station data from excel templates
#'
#' @param template_path path to *.xlsx file. 
#' @param epsg_code EPSG code for station coordinates system.
#' @return sf dataframe with station information.
#' @importFrom readxl read_xlsx
#' @importFrom sf st_as_sf
#' @importFrom dplyr mutate %>%
#' @importFrom purrr map
#' @keywords internal
#'
#' @examples
#' ##read_stations("templates/weather_data.xlsx", 4326)

read_stations <- function(template_path, epsg_code){
  read_xlsx(template_path, "Stations") %>% 
    st_as_sf(coords = c("Long", "Lat"), crs = epsg_code) %>% 
    mutate(Long = unlist(map(geometry,1)),
         Lat = unlist(map(geometry,2)))
}

#' Function to get maximum min and max dates available for all time series data for all stations for all parameters. 
#'
#' @param meteo_lst nested list of lists with dataframes with Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' @return list with 2 values: minimum and maximum value for all available data.
#' @importFrom lubridate now
#' @export
#'
#' @examples
#' ##get_nx_date_full(meteo_lst)

get_dates <- function(meteo_lst){
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
