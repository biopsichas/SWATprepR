
# Loading functions --------------------------------------------------------------------

#' Function providing loading of calibration data templates
#' (data should have been cleaned before).
#'
#' @param template_path path to *.xlsx file. 
#' @param epsg_code EPSG code for station coordinates (default 4326 for WGS 84 coordinate system)  system
#' @return list of two dataframes: stations dataframe contains station information,  
#' data contains measurement data
#' @importFrom dplyr mutate %>%
#' @importFrom readxl read_xlsx
#' @importFrom sf st_as_sf
#' @importFrom purrr map
#' @export 
#'
#' @examples
#' ##load_calibration_data_template("templates/calibration_data.xlsx")
#' 

load_calibration_data_template <- function(template_path, epsg_code = 4326){
  print("Loading data from template.")
  ##Loading station locations
  st_location <- read_xlsx(template_path, "Stations") %>% 
    st_as_sf(coords = c("long", "lat"), crs = epsg_code) %>% 
    mutate(long = unlist(map(geometry,1)),
           lat = unlist(map(geometry,2)))
  df_comb <- read_xlsx(template_path, "Data") %>%
    mutate(DATE = as.POSIXct(DATE, "%Y-%m-%d", tz = "UTC"))
  print("Loading of data is finished. Dataframe stations has station info, data has measured values.")
  
  return(list(stations = st_location, data = df_comb))
}

 