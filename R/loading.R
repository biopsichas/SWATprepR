
# Template loading functions --------------------------------------------------------------------

#' Function providing loading for data templates (with single and multiple data sheets)
#' (data should have been cleaned before).
#'
#' @param template_path path to *.xlsx file. 
#' @param epsg_code EPSG code for station coordinates (default 4326 for WGS 84 coordinate system)  system
#' @return list of two: stations dataframe, which contains station information,  
#' second member of list contains contains measurement data in dataframe (if data is in one sheet)
#' or nested list of dataframes with each parameter in separate dataframe. 
#' @importFrom dplyr mutate %>%
#' @importFrom readxl read_xlsx excel_sheets
#' @importFrom tidyr drop_na
#' @export 
#'
#' @examples
#' ##Two types of templates could be used
#' # 1) Example of template for weather data
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "svatools")
#' met_lst <- load_template(temp_path, 3035)
#' str(met_lst)
#' ## 2) Example of template for calibration data
#' temp_path <- system.file("extdata", "calibration_data.xlsx", package = "svatools")
#' cal_data <- load_template(temp_path)
#' str(cal_data)


load_template <- function(template_path, epsg_code = 4326){
  print("Loading data from template.")
  ##Loading station location and other info
  st <- load_stations(template_path, epsg_code)
  ##Getting sheet names
  ids <- excel_sheets(template_path) %>% 
    .[!. %in% "Stations"]
  ##Loading data with one data sheet
  if(length(ids) == 1 && ids == "Data"){
    r <- read_xlsx(template_path, ids) %>%
      mutate(DATE = as.POSIXct(DATE, "%Y-%m-%d", tz = "UTC"))
    ##Loading data with many data sheets
  }else if(length(ids) > 0){
    r <- list()
    for (id in ids){
      print(paste("Reading station", id, "data."))
      df <- read_xlsx(template_path, id, guess_max = 10000) 
      for (p in names(df)[-1]){
        if(p == "RELHUM" && (min(df[[p]], na.rm = TRUE) < 0 | max(df[[p]], na.rm = TRUE) > 1)){
          warning(paste0(p, " variable values at station ", id, " should be between 0 and 1. Please check, correct your input data and reload template!!!")) 
        }
        if(p == "TMP_MAX" && (min(df[[p]], na.rm = TRUE) < -89.2 | max(df[[p]], na.rm = TRUE) > 70.7)){
          warning(paste0(p, " variable values at station ", id, " should be between -89.2 and 70.7 C (the lowest and highest recorded on Earth). Please check, correct your input data and reload template!!!")) 
        }
        if(p == "TMP_MIN" && (min(df[[p]], na.rm = TRUE) < -89.2 | max(df[[p]], na.rm = TRUE) > 70.7)){
          warning(paste0(p, " variable values at station ", id, " should be between -89.2 and 70.7 C (the lowest and highest recorded on Earth). Please check, correct your input data and reload template!!!")) 
        }
        if(p == "WNDSPD" && (min(df[[p]], na.rm = TRUE) < 0 | max(df[[p]], na.rm = TRUE) > 104)){
          warning(paste0(p, " variable values at station ", id, " should be between 0 and 104 m/s (the highest recorded natural surface wind velocity). Please check, correct your input data and reload template!!!")) 
        }
        if(p == "SLR" && (min(df[[p]], na.rm = TRUE) < 0 | max(df[[p]], na.rm = TRUE) > 1000)){
          warning(paste0(p, " variable values at station ", id, " should be between 0 and 1000 W/m2 (maximum received at the Earth's surface). Please check, correct your input data and reload template!!!")) 
        }
        if(p == "PCP" && (min(df[[p]], na.rm = TRUE) < 0 | max(df[[p]], na.rm = TRUE) >  1825)){
          warning(paste0(p, " variable values at station ", id, " should be between 0 and 1825 mm/d (maximum 1-day recorded rainfall). Please check, correct your input data and reload template!!!")) 
        }
        r[[id]][[p]] <- df[,c("DATE", p)] %>% 
          drop_na() %>% 
          mutate(DATE = as.POSIXct(DATE, "%Y-%m-%d", tz = "UTC"))
      }
    }
  }else{
    warning("Your template doesn't have data sheets to read.")
    r <- NA
  }
  print("Loading of data is finished.")
  return(list(stations = st, data = r))
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
#' \dontrun{
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "svatools")
#' stations <- load_stations(temp_path, 3035)
#' str(stations)
#' library(sf)
#' library(mapview)
#' mapview(st_transform(stations, 4326))
#' }

load_stations <- function(template_path, epsg_code){
  read_xlsx(template_path, "Stations") %>% 
    st_as_sf(coords = c("Long", "Lat"), crs = epsg_code) %>% 
    mutate(Long = unlist(map(geometry,1)),
           Lat = unlist(map(geometry,2)))
}

#' Function loading climate data csv files
#'
#' @param f_path character providing path to climate date folder (i.e. "inst/extdata/CORDEX-BC")
#' @param f_lst list providing file name for each weather parameter (default list("PCP" = "prec-1", 
#' "SLR" = "solarRad-1", "RELHUM" = "relHum-1", "TMP_MAX" = "Tmax-1", "TMP_MIN" = "Tmin-1", 
#' "WNDSPD" = "windSpeed-1"))
#' @return nested list of lists with dataframes. 
#' Nested structure meteo_lst -> RCM_MOD-> Parameter -> Dataframe (DATE, PARAMETER).
#' @importFrom dplyr mutate
#' @importFrom utils read.csv
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' temp_path <- paste0(system.file("extdata", package = "svatools"), "/CORDEX-BC")
#' cli_lst <- load_climate(temp_path)
#' str(cli_lst)
#' }

load_climate <- function(f_path, f_lst = list("PCP" = "prec-1", "SLR" = "solarRad-1", "RELHUM" = "relHum-1", "TMP_MAX" = "Tmax-1", 
                                              "TMP_MIN" = "Tmin-1", "WNDSPD" = "windSpeed-1")){
  r <- list()
  for(c in sub(".*\\/", "", list.dirs(f_path, recursive = FALSE))){
    for (m in sub(".*\\/", "", list.dirs(paste0(f_path, "/", c), recursive = FALSE))){
      for(i in names(f_lst)){
        r[[paste0(c, "_", m)]][[i]] <- read.csv(paste0(f_path, "/", c, "/", m, "/", f_lst[i][[1]], ".csv"), 
                                                header = F, col.names = c("DATE", i)) %>% 
          mutate(DATE = as.POSIXct(DATE, "%Y-%m-%d", tz = "UTC"))
      }
    }
  }
  print("Loading of data is finished.")
  return(r)
}

# Loading SWAT text file --------------------------------------------------------------------

#' Loading SWAT+ text files into dataframe
#'
#' @param tbl_name character, name of the file to be read eaxmple ('rout_unit.con').
#' @param proj_path character, path to SWAT+ txtinout folder (example "my_model").
#' @param row_data_start numeric, row number from which data are being written.
#' @param row_col_names numeric, row nu,mber in which column names are being written.
#' @importFrom vroom vroom_lines
#' @importFrom stringr str_trim str_split
#' @importFrom dplyr %>% mutate across all_of bind_rows
#' @importFrom purrr map map_lgl map_df set_names
#' @return dataframe with information from text file
#' @export
#' 
#' @examples
#' \dontrun{
#' df <- read_tbl('rout_unit.con', 'model_folder', 3, 2) 
#' }

read_tbl <- function(tbl_name, proj_path, row_data_start, row_col_names) {
  tbl_path <- paste(proj_path, tbl_name, sep = '/')
  ##Reading column names
  col_names <- vroom_lines(tbl_path, skip = row_col_names - 1, n_max = 1) %>%
    str_trim(.) %>%
    str_split(., '[:space:]+') %>%
    unlist()
  ##Reading file body into list
  tbl <- vroom_lines(tbl_path, skip = row_data_start - 1) %>%
    str_trim(.) %>%
    str_split(., '\t[:space:]+|[:space:]+')
  
  ##Checking if all columns have names
  col_length_true <- max(unlist(map(tbl, length)),rm.na=TRUE)
  col_length_catched <- length(col_names)
  ##If not, adding missing names 'vvv'+number
  if(col_length_true>col_length_catched){
    col_names <- c(col_names, paste0("vvv",seq(1:(col_length_true-col_length_catched ))))
    ##Identifying number columns
    is_num <- tbl[[which(sapply(tbl, length)==col_length_true)[1]]] %>% as.numeric() %>% 
      suppressWarnings() %>% map_lgl(., ~!is.na(.x)) %>% which()
  } else {
    is_num <- tbl[[1]] %>% as.numeric() %>% suppressWarnings() %>% 
      map_lgl(., ~!is.na(.x)) %>% which()
  }
  ##Returning dataframe
  tbl <- tbl %>%
    map(., ~set_names(.x, col_names[c(1:length(.x))])) %>%
    map_df(., ~bind_rows(.x)) %>%
    mutate(across(all_of(is_num), ~ as.numeric(.x)))
  return(tbl)
}