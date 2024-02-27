
# Loading templates --------------------------------------------------------------------

#' Load Data Templates
#'
#' This function facilitates the loading of data templates, which include 
#' both station information and measurement data (e.g. calibration, weather, 
#' soil, point source data.). 
#'
#' @param template_path Character, the path to the *.xlsx file containing the data template.
#' @param epsg_code (optional) Integer, EPSG code for station coordinates. 
#' Default \code{epsg_code = 4326}, which stands for WGS 84 coordinate system.
#' @return A nested list with dataframes. 
#'   Nested structure: \code{meteo_lst -> data -> Station ID -> Parameter -> 
#'   Dataframe (DATE, PARAMETER)}, 
#'   \code{meteo_lst -> stations -> Dataframe (ID, Name, Elevation, Source, 
#'   geometry, Long, Lat)}.
#' @importFrom dplyr mutate %>%
#' @importFrom readxl read_xlsx excel_sheets
#' @importFrom tidyr drop_na
#' @importFrom lubridate yday month day year
#' @importFrom utils head tail  
#' @export 
#'
#' @examples
#' \dontrun{
#' ## Three types of templates could be used
#' # 1) Example of template for weather data
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "SWATprepR")
#' met_lst <- load_template(temp_path, 3035)
#' 
#' ## 2) Example of template for calibration data
#' temp_path <- system.file("extdata", "calibration_data.xlsx", package = "SWATprepR")
#' cal_data <- load_template(temp_path)
#' 
#' ## 3) Example of template for point source:
#' temp_path <- system.file("extdata", "pnt_data.xls", package = "SWATprepR")
#' pnt_data <- load_template(temp_path)
#' }
#' @keywords loading

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
    ##Identifying point source template and adding missing columns
    if("flo" %in% names(r)){
      pars <- c("jday",	"mo", "day_mo",	"yr",	"ob_typ",	"ob_name", "flo",	"sed",	"orgn", "sedp",	"no3",	"solp",	"chla",	"nh3",	"no2",	
                "cbod",	"dox",	"sand",	"silt", "clay",	"sag", "lag",	"gravel", "tmp")
      r[setdiff(pars, names(r))] <- 0  
      r[,c("ob_name", "ob_typ", "jday", "mo", "day_mo", "yr")] <- data.frame(r$name, "pt", yday(r$DATE), month(r$DATE), day(r$DATE), year(r$DATE))
      r <- select(r, all_of(c(pars, "DATE")))
    }
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
  for(n in names(r)){
    for(v in names(r[[n]])){
      df <- r[[n]][[v]]
      if(length(df[,1][[1]][duplicated(df[,1][[1]])])>0){
        dates_text <- paste(unique(df[,1][[1]][duplicated(df[,1][[1]])]), collapse = ", ")
        warning(paste0("Station:", n, ", Variable:" , v, " covers period of " , 
                       head(df[,1], 1)[[1]], " to ", tail(df[,1], 1)[[1]], 
        ". However, it has dublicated values at ", dates_text,", which migth create problems with some functions of this package. Please remove dublicated inputs and reload template."))
      }
    }
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
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "SWATprepR")
#' stations <- load_stations(temp_path, 3035)
#' str(stations)
#' library(sf)
#' library(mapview)
#' mapview(st_transform(stations, 4326))
#' }

load_stations <- function(template_path, epsg_code){
  df <- read_xlsx(template_path, "Stations") 
  if(epsg_code == 4326){
    ## wgs84 min and max coordinates
    if(min(df$Long) < -180 | max(df$Long) > 180 | min(df$Lat) < -90 | max(df$Lat) > 90){
      warning("Your station coordinates are not in WGS84 system. Please check, correct your input data and reload template!!!") 
    }
  }
  df <- df %>% 
    st_as_sf(coords = c("Long", "Lat"), crs = epsg_code) %>% 
    mutate(Long = unlist(map(geometry,1)),
           Lat = unlist(map(geometry,2)))
  return(df)
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
#' temp_path <- paste0(system.file("extdata", package = "SWATprepR"), "/CORDEX-BC")
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

# Loading SWAT text files --------------------------------------------------------------------

#' Loading SWAT+ text files into dataframe
#'
#' @param tbl_name character, name of the file to be read example ('rout_unit.con').
#' @param proj_path character, path to SWAT+ txtinout folder (example "my_model").
#' @param row_data_start numeric, row number from which data are being written. Optional, 
#'  \code{default = 3}.
#' @param row_col_names numeric, row nu,mber in which column names are being written.
#'  Optional, \code{default = 2}.
#' @importFrom vroom vroom_lines
#' @importFrom stringr str_trim str_split
#' @importFrom dplyr %>% mutate across all_of bind_rows
#' @importFrom purrr map map_lgl map_df set_names
#' @return dataframe with information from text file
#' 
#' @examples
#' \dontrun{
#' df <- read_tbl('rout_unit.con', 'model_folder', 3, 2) 
#' }
#' @keywords internal

read_tbl <- function(tbl_name, proj_path, row_data_start = 3, row_col_names = 2) {
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
  ##Preventing any duplicated columns from messing things up
  if(any(duplicated(col_names))){
    ind <- which(duplicated(col_names))
    col_names[ind] <- paste0(col_names[ind],"2")
  }
  ##Returning dataframe
  tbl <- tbl %>%
    map(., ~set_names(.x, col_names[c(1:length(.x))])) %>%
    map_df(., ~bind_rows(.x)) %>%
    mutate(across(all_of(is_num), ~ as.numeric(.x)))
  return(tbl)
}

#' Load SWAT+ weather text files into R
#' 
#' This function reads SWAT+ weather input files from a specified folder into R,
#' organizing the data into a nested list structure for easy access and analysis.
#'
#' @param input_folder character, path to folder with SWAT+ weather input files 
#' (e.g., "my_model").
#' @importFrom sf st_as_sf st_drop_geometry
#' @importFrom purrr map pmap map_df map2
#' @importFrom vroom vroom_lines
#' @importFrom stringr str_extract
#' @importFrom dplyr bind_rows mutate select group_by ungroup everything row_number distinct filter left_join
#' @importFrom tibble enframe
#' @importFrom tidyr unnest spread
#' @return 
#' A nested list with dataframes. 
#'   Nested structure: \code{meteo_lst -> data -> Station ID -> Parameter -> 
#'   Dataframe (DATE, PARAMETER)}, 
#'   \code{meteo_lst -> stations -> Dataframe (ID, Name, Elevation, Source, 
#'   geometry, Long, Lat)}.
#' @export
#'
#' @examples
#' \dontrun{
#'   met_lst <- load_swat_weather("my_folder")
#' }
#' @keywords loading

load_swat_weather <- function(input_folder){
  ##Set function execution timer
  start_t <- Sys.time()
  print(paste0("Loading of SWAT+ weather data is started from ", input_folder, " directory."))
  ##Identifying all weather files
  fs <- list.files(input_folder, recursive = F, pattern="*.pcp|*.slr|*.hmd|*.tmp|*.wnd")
  ##Check if any files found in directory.
  if(length(fs)==0){
    stop(paste0("No SWAT+ weather files have been found in ", input_folder, " directory!!!"))
  }
  ##Reading and setting lists to work on
  ##Preparing dics files types to variables
  p_lst <- list("pcp" = c("PCP"), 
                "slr" = c("SLR"), 
                "hmd" = c("RELHUM"), 
                "tmp" = c("TMP_MAX", "TMP_MIN"), 
                "wnd" = c("WNDSPD")) ##Parameters dics
  ##Reading scenario info from directories 2 last levels
  s_info <- unlist(strsplit(input_folder, "/")) ##source information 
  s_info <- paste0(s_info[c(length(s_info)-1, length(s_info))], collapse = '/')
  ##Preparing list of variables for each file
  f_type <- map(fs, ~p_lst[sub(".*\\.", "",.x)][[1]])
  ##Preparing list of file names
  f_name <- map(fs, ~gsub("\\..*","",.x))
  ##Reading ids from file names
  id <- map(fs, ~str_extract(toupper(.), "ID([\\d]+)"))
  ##Check if ids are in names
  if (any(is.na(unique(unlist(id))))){
    warning("All file names should contain 'id' or 'ID' text + number to identify station. 
            Numbers will be extracted from file names and used as station IDs.")
    id <- paste0("ID", as.numeric(gsub("\\D", "", fs)))
    if(any(is.na(id))){
      stop("Station IDs could not be extracted from file names. Probably some 
           stations have no numbers in their names. Please, check file names.")
    }
  }
  ##Reading all files into list of lists
  rlist <- map(fs, ~vroom_lines(paste0(input_folder,"/",.x), skip = 2))
  
  ##Preparing 
  ##Preparing station info dataframe
  st_info <- pmap(list(id, rlist, f_name), function(x, y, z){
    l <- as.numeric(unlist(strsplit(y[1], " +")))
    if(is.na(l[1])){i <- 1} else {i <- 0} ##in case there is gap (not number) in the station info line
    list("ID" = x, "Name" = z, "Elevation" = l[5+i], "Source" = s_info, "Long" = l[4+i], "Lat" =l[3+i])}) %>% 
    map_df(bind_rows) %>% 
    unique %>% 
    st_as_sf(coords = c("Long", "Lat"), crs = 4326, remove = F)
  
  ##Transforming data in main list of list 
  rlist <- rlist %>% 
    map(~.x[c(2:length(.x))]) %>% ##Dropping line with coordinate info
    map(~strsplit(.x, " +")) %>% ##Splitting characters in lines by space
    map(enframe) %>% ##Putting into dataframe
    map(~unnest(.x,value) %>%  ##Splitting column with nested lists into multiple columns
          group_by(name) %>%
          mutate(col=seq_along(name)) %>%
          spread(key=col, value=value) %>% 
          ungroup) %>% 
    map2(f_type, function(x, y){ 
      colnames(x) <- c("n", "y", "d", y)
      d <- as.POSIXct(strptime(paste(x$y, x$d), format="%Y %j", tz = "UTC")) ##Converting year + day into date
      x <- lapply(x[y],as.numeric) ##Variable columns into numeric type
      x[["DATE"]] <- d 
      as.data.frame(x) %>% 
        select(DATE, everything()) ##Building new dataframe and selecting columns
    })
  
  ##Finding if any stations with same code have different coordinates
  station_non_unique <- st_info %>% 
    st_drop_geometry %>% 
    select(ID, Long, Lat) %>% 
    distinct %>%
    group_by(ID) %>% 
    filter(n()>1)
  
  ## In case of non-unique stations, assigning new IDs and warning for the user
  ## To know how they are recoded
  if(nrow(station_non_unique) > 0){
    warning("Your station IDs (obtained from file names) contains non-unique coordinate values. 
          This will be corrected by assinging new ID numbers. If you don't want this to happen,
          please make sure that your station IDs are unique (have only one set of Lat, Long).")
    st_info <- st_info %>% 
      left_join(st_info %>% st_drop_geometry %>% 
                  select(Long, Lat) %>% 
                  distinct %>% 
                  mutate(ID_up = paste0("ID", row_number())) , by = c("Long", "Lat")) %>% 
      mutate(ID = ID_up) %>% 
      select(-ID_up)
    id <- st_info$ID
    warning("Following IDs were assigned to the files")
    options(warning.length = 8170)
    warning(map2(id, fs, ~paste0(.x, " ",.y, ", "))%>% unlist)
    options(warning.length = 1000)
  }
  
  ##Transforming list into nested list of list LIST>ID>VARIABLE>DATAFRAME
  ##Final list of lists used in collecting transformed lists in correct form
  cc <- c()
  print_warning <- TRUE
  ##For each station id
  for(id1 in unique(unlist(id))){
    l <- rlist[which(id == id1)]; v <- f_type[which(id == id1)]; c <- c()
    ##For each variable
    for(i in seq(1:length(v))){
      ll <- l[i]; vv <- v[[i]]
      if(length(vv) == 1){
        names(ll) <- vv
        ##If PCP < 0.2 assign 0
        if(vv == "PCP" && min(ll[[1]][[vv]], na.rm = TRUE) < 0.2){
          if(print_warning){
            warning(paste0("Some of the precipitation values are below 0.2 mm. 
                         They will be assigned 0."))
            print_warning <- FALSE
          }
          ll[[vv]][[vv]] <- ifelse(ll[[vv]][[vv]] < 0.2, 0, ll[[vv]][[vv]])
        }
        c <- c(c, ll)
        ##Splitting temperatire min and max
      } else if (length(vv) == 2){
        for(n in vv){
          ll1 <- list(ll[[1]][c("DATE", n)])
          names(ll1) <- n
          c <- c(c, ll1)
        }
      }
    }
    ##Collecting results of transformation
    cc[[id1]] <- c
  }
  ## Checking if any of the stations have dublicated variable names
  ## If yes, removing them. Data is left from the first occurence.
  any_dublitates <- map(names(cc), ~duplicated(unlist(names(cc[[.x]]))))
  if(any(unlist(any_dublitates))){
    station_with_dublicates <- st_info$ID[unlist(map(any_dublitates, ~any(.x)))]
    warning(paste0("Stations ", station_with_dublicates, " have dublicated variable names. 
                 They will be removed together with data."))
    cc <- map(cc, ~.x[!duplicated(unlist(names(.x)))])
  }
  ##In case of multiple stations with same ID, combining them into one. First 
  ## occurrence is left.
  st_info <- st_info %>% 
    st_drop_geometry %>% 
    select(ID) %>%
    distinct %>% 
    left_join(st_info, by = "ID", multiple = "first") %>% 
    st_as_sf()
  
  diff_t <- Sys.time() - start_t
  print(paste0("Data loading succesfully finished in ", round(as.numeric(diff_t), 2), " ", units(diff_t), "."))
  gc()
  return(list(stations = st_info, data = cc))
}

# Loading other --------------------------------------------------------------------

#' Extract EMEP atmospheric deposition data for a catchment
#'
#' This function extracts EMEP atmospheric deposition data averaged for 
#' a specified catchment.
#'
#' @param catchment_boundary_path Path to the basin boundary shape file.
#' @param t_ext (optional) String, specifying the EMEP data to access. 
#'   'year' for yearly averages, 'month' for monthly averages. Default 
#'   \code{t_ext = 'year'}.
#' @param start_year (optional) Integer representing the starting year for data
#'  extraction. Default \code{start_year = 1990}.
#' @param end_year Integer representing the ending year for data extraction. 
#' Default \code{end_year = 2022}.
#' @importFrom sf st_transform st_read st_bbox
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom dplyr bind_rows
#' @return A dataframe with columns "DATE", "NH4_RF", "NO3_RF", "NH4_DRY", and 
#' "NO3_DRY". 
#'   Values are in SWAT+ units.
#'   "NH4_RF" - ammonia nitrogen in rainfall (mg/l), 
#'   "NO3_RF" - nitrate nitrogen in rainfall (mg/l), 
#'   NH4_DRY - ammonia nitrogen dry deposition (kg/ha/yr), 
#'   "NO3_DRY" - nitrate nitrogen dry deposition (kg/ha/yr).
#' @export 
#' @examples
#' \dontrun{
#'   # Specify the path to the basin boundary shape file
#'   basin_path <- system.file("extdata", "GIS/basin.shp", package = "SWATprepR")
#'   
#'   # Get atmospheric deposition data for the catchment
#'   df <- get_atmo_dep(basin_path)
#'   
#'   # Plot results
#'   ggplot(pivot_longer(df, !DATE, names_to = "par", values_to = "values"), 
#'   aes(x = DATE, y = values))+
#'     geom_line()+
#'     facet_wrap(~par, scales = "free_y")+
#'     theme_bw()
#' }
#' @references 
#' See the EMEP website for more information: \url{https://www.emep.int/mscw/mscw_moddata.html}
#' @keywords loading
#' @seealso 
#' Please read about SWAT+ atmospheric input data on \url{https://swatplus.gitbook.io/io-docs/introduction/climate/atmo.cli}.

get_atmo_dep <- function(catchment_boundary_path, t_ext = "year", start_year = 1990, end_year = 2022){
  ##Part url link to emep data (more info found here https://www.emep.int/mscw/mscw_moddata.html)
  url_prt <- "https://thredds.met.no/thredds/dodsC/data/EMEP/2023_Reporting/EMEP01_rv5.0_"
  ##Getting borders of the catchment
  basin <-st_transform(st_read(catchment_boundary_path, quiet = TRUE), 4326)
  bb <- st_bbox(basin)
  ##Setting dataframe for results
  df <- data.frame(YR = integer(), MO = integer(), DAY = integer(), 
                   NH4_RF = numeric(), NO3_RF = numeric(), 
                   NH4_DRY = numeric(), NO3_DRY = numeric())
  ##Loop to extract data
  for(u in seq(start_year, end_year)){
    print(paste("Working on year", u))
    if(!u %in% c(2021, 2022)){
      uu <- "_rep2023.nc"
    } else {
      uu <- ".nc"
    }
    ##Assembling URL for each year
    url <- paste0(url_prt, t_ext, ".", u, "met_", ifelse(u == 2022, 2021, u), "emis", uu)
    ##Opening file and getting indexes for data.
    r <- open.nc(url)
    lon <- var.get.nc(r, "lon")
    lat <- var.get.nc(r, "lat")
    ilon <- which(lon>bb[1] & lon<bb[3])
    ilat <- which(lat>bb[2] & lat<bb[4])
    ##For small catchments (data grid size is 50X50 km2). Smaller than EMEP model grid.
    if(length(ilon) == 0){
      ilon <- which.min(abs(lon - (bb[1]+bb[3])/2))
    }
    if(length(ilat) == 0){
      ilat <- which.min(abs(lat - (bb[2]+bb[4])/2))
    }
    ##Reading parameters and converting to right units and averaging them over extracted grid.
    if(t_ext == "year"){
      prec <- var.get.nc(r, "WDEP_PREC")[ilon, ilat]
      dry_oxn <- mean(var.get.nc(r, "DDEP_OXN_m2Grid")[ilon, ilat]/100) 
      wet_oxn <- mean(var.get.nc(r, "WDEP_OXN")[ilon, ilat]/prec)
      dry_rdn <- mean(var.get.nc(r, "DDEP_RDN_m2Grid")[ilon, ilat]/100)
      wet_rdn <- mean(var.get.nc(r, "WDEP_RDN")[ilon, ilat]/prec)

      # SWAT+ input and output units are in pure nitrogen, documentation is confusing
      # In case conversion is needed, use the following:
      # 4.4268 is the conversion factor from N to NO3, /100 is conversion mg/m2 to kg/ha
      # 1.2878 is the conversion factor from N to NH4, /100 is conversion mg/m2 to kg/ha
      # Example:
      # dry_oxn <- mean(var.get.nc(r, "DDEP_OXN_m2Grid")[ilon, ilat]*CONVERSION FACTOR/100) 
      
      ##Saving results
      df[nrow(df)+1,] <- c(u, 1, 1, wet_rdn, wet_oxn, dry_rdn, dry_oxn)
    } else if(t_ext %in% c("month")){
      prec <- var.get.nc(r, "WDEP_PREC")[ilon, ilat,]
      di <- length(dim(prec)) ##Dimension of extracted array
      dry_oxn <- apply(var.get.nc(r, "DDEP_OXN_m2Grid")[ilon, ilat,]*4.4268/100, di, mean)
      wet_oxn <- apply(var.get.nc(r, "WDEP_OXN")[ilon, ilat,]*4.4268/prec, di, mean)
      dry_rdn <- apply(var.get.nc(r, "DDEP_RDN_m2Grid")[ilon, ilat,]*1.2878/100, di, mean)
      wet_rdn <- apply(var.get.nc(r, "WDEP_RDN")[ilon, ilat,]*1.2878/prec, di, mean)
      ##Saving results
      if(t_ext == "month"){
        df<- bind_rows(df, data.frame(YR = u, MO = seq(1, length(dry_oxn)), DAY = 1, 
                                      NH4_RF = wet_rdn, NO3_RF = wet_oxn, 
                                      NH4_DRY = dry_rdn, NO3_DRY =dry_oxn))
      } else {
        df<- bind_rows(df, data.frame(YR = u, MO = 0, DAY = seq(1, length(dry_oxn)), 
                                      NH4_RF = wet_rdn, NO3_RF = wet_oxn, 
                                      NH4_DRY = dry_rdn, NO3_DRY =dry_oxn))
      }
    } else {
      stop("Wrong t_ext!!! Should be one of these strings: 'year' or 'month'.")
    }
    print(paste("Finished data extraction for year", u))
  }
  ##Adding DATE
  if(t_ext != "day"){
    df$DATE <- as.Date(paste(df$YR, df$MO, df$DAY), format="%Y %m %j")
  } else {
    df$DATE <- as.Date(paste(df$YR, df$DAY), format="%Y %j")
  }
  return(df[c("DATE", "NH4_RF", "NO3_RF" , "NH4_DRY", "NO3_DRY")])
}

#' Extract Climate Data from CORDEX NetCDF into Nested List
#'
#' This function extracts climate data from the CORDEX-BC dataset and organizes 
#' it into a nested list.
#'
#' @param dir_path Character, path to the CORDEX-BC folder (e.g., "climate/CORDEX-BC").
#' NetCDF data to be recognized by the function should be saved with these specific file names:
#'  - "prec.nc" for precipitation,
#'  - "relHum.nc" for relative humidity,
#'  - "solarRad.nc" for solar radiation,
#'  - "Tmax.nc" for maximum daily temperature,
#'  - "Tmin.nc" for minimum daily temperature,
#'  - "windSpeed.nc" for wind speed.
#' Ensure that the NetCDF files are correctly named and stored in the specified 
#' directory for the function to recognize them.'
#' @param location Character or list. If character, provide the path to the catchment 
#'   boundary file (e.g., "GIS/basin.shp"). If a list, use the nested list with 
#'   dataframes. The nested structure is same as prepared by 
#'   using \code{\link{load_template}} or \code{\link{load_swat_weather}} functions.
#' @importFrom elevatr get_elev_point
#' @importFrom sf st_read st_transform st_as_sf st_crs st_overlaps st_centroid
#' @importFrom raster brick rasterToPolygons extract
#' @importFrom dplyr select rename mutate 
#' @importFrom tidyr drop_na
#' @importFrom purrr map
#' @return Nested list. The first nesting level is for RCP, the second for RCM model numbers,
#'   and the rest follows the structure of meteo_lst. Nested structure: \code{meteo_lst -> 
#'   data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER)}, 
#'   \code{meteo_lst -> stations -> Dataframe (ID, Name, Elevation, Source, geometry, 
#'   Long, Lat)}. This nested list structure can be used with 
#'   other package functions (e.g., \code{plot_weather(result$rcp26$"1", "PCP", "month", "sum")}).
#' @export
#'
#' @examples
#' \dontrun{
#'   # Specify the path to the catchment boundary file
#'   basin_path <- system.file("extdata", "GIS/basin.shp", package = "SWATprepR") 
#'   
#'   # Specify the path to the CORDEX-BC data
#'   data_path <- "climate/CORDEX-BC"
#'   
#'   # Extract and organize climate data
#'   result <- load_netcdf_weather(data_path, basin_path)
#' }
#' @keywords loading

load_netcdf_weather <- function(dir_path, location){
  ##Checking inputs
  if(!is.character(dir_path)){
    stop("Your input to function parameter 'dir_path' is not character!!! Please correct this.")
  }
  fs <- list.files(dir_path, recursive = T)
  if(length(fs)==0){
    stop(paste0("No netCDF data were found on ", dir_path, 
                " path!!! \n Please check your path and consult function documentation."))
  }
  if(!is.character(location)&!is.list(location)){
    stop(paste0("`location` parameter input is ", class(location), 
                ". Only possible inputs are of character or list type. \n 
                Please consult function documentation."))
  }
  ##In case path to shape is given
  if (is.character(location)){
    basin <- st_read(location, quiet = TRUE) %>% 
      st_transform(4326)
    ##Grid vector created
    grid_vec <- st_as_sf(rasterToPolygons(brick(paste0(dir_path, "/", fs[[1]]))[[1]]), crs = st_crs(4326))
    ##Finding overlap with basin
    suppressMessages(suppressWarnings({touch_basin <- st_overlaps(grid_vec, basin)}))
    touch_basin[lengths(touch_basin) == 0] <- 0
    ##If no overlap stop it
    if(all(unlist(touch_basin)==0)){
      stop("You basin boundary and netCDF data do not overlap!!! Please correct data or use different set/s.")
    }
    ##Filter vector grid and use centroid to create monitoring point attribute data
    grid_vec$touch_basin <- unlist(touch_basin)
    grid_vec <- grid_vec[grid_vec$touch_basin == 1,]
    suppressWarnings({grid_centroid <- st_centroid(grid_vec)})
    st <- grid_centroid %>% 
      select() %>% 
      get_elev_point(src = "aws") %>% 
      rename(Elevation = elevation) %>% 
      mutate(ID = paste0("ID",rownames(.)),
             Name = paste0("ID",rownames(.)),
             Source = "Grid",
             Long = unlist(map(geometry,1)),
             Lat = unlist(map(geometry,2))) %>% 
      select(ID, Name, Elevation, Source, geometry, Long, Lat)
    ##If meteo_lst is used, use sf data.frame for stations 
  } else if (is.list(location)){
    st <-  location[["stations"]]
    if (!grepl("4326", st_crs(st)$input)){
      st <- st_transform(st, 4326)
      print("Coordinate system checked and transformed to EPSG:4326.")
    } 
  }
  ##Load netCDF
  cl_list <- list()
  for (f in fs){
    if(grepl("prec|Tmax|Tmin|solarRad|windSpeed|relHum", f)){
      if (grepl("prec", f)){
        p <- "PCP"
      } else if (grepl("Tmax", f)){
        p <- "TMP_MAX"
      } else if (grepl("Tmin", f)){
        p <- "TMP_MIN"
      } else if (grepl("solarRad", f)){
        p <- "SLR"
      } else if (grepl("windSpeed", f)){
        p <- "WNDSPD"
      } else if (grepl("relHum", f)){
        p <- "RELHUM"
      } 
      
      fdir <- unlist(strsplit(f, "/"))
      nc <- brick(paste0(dir_path, "/", f))
      ##Extract values for stations and create data.frame
      ex_m <- raster::extract(nc, st)
      df <- cbind.data.frame(nc@z[[1]],t(ex_m)[1:(ncol(ex_m)),])
      colnames(df) <- c("DATE", st$ID)
      rownames(df) <- NULL
      if (p == "RELHUM" && mean(ex_m, na.rm = TRUE)>1){
        df[,st$ID] <- df[,st$ID]/100
        warning("RELHUM values are larger than 1. Correction is applied by dividing it with 100. Values should be between 0 to 1.")
      } 
      ##Removing columns without records (case no overlap)
      df <- df[,colSums(is.na(df))==0]
      ##Creating meteo_lst list 
      cl_list[[fdir[1]]][[fdir[2]]][["stations"]] <- st
      for(id in st$ID){
        if(id %in% names(df)){
          cl_list[[fdir[1]]][[fdir[2]]][["data"]][[id]][[p]] <- df[,c("DATE", id)] %>% 
            drop_na() %>% 
            mutate(DATE = as.POSIXct(DATE, "%Y-%m-%d", tz = "UTC"))
          colnames(cl_list[[fdir[1]]][[fdir[2]]][["data"]][[id]][[p]]) <- c("DATE", p)
          ##If PCP below 0.2, than 0
          if(p == "PCP" && min(cl_list[[fdir[1]]][[fdir[2]]][["data"]][[id]][[p]][[p]], na.rm = TRUE)<0.2){
            cl_list[[fdir[1]]][[fdir[2]]][["data"]][[id]][[p]][[p]] <- ifelse(
              cl_list[[fdir[1]]][[fdir[2]]][["data"]][[id]][[p]][[p]]<0.2, 0, 
              cl_list[[fdir[1]]][[fdir[2]]][["data"]][[id]][[p]][[p]])
          }
        }
      }
      print(paste0("Working on ", f))
    }
  }
  print("Extraction of data is finished.")
  return(cl_list)
}

#' Convert SWAT+ soils.sol data to usersoil format
#'
#' This function reads the soils.sol file from a SWAT+ project folder and converts
#' the data into usersoil format to examine/update with other functions.
#'
#' @param project_path Character, path to the SWAT+ project folder (example "my_model").
#' @importFrom purrr pmap
#' @importFrom dplyr bind_rows bind_cols select mutate mutate_at
#' @importFrom tidyr pivot_longer pivot_wider
#' @return A data frame representing user soil data.
#' @export
#' @examples
#' \dontrun{
#' usersoil <- sol_to_usersoil("my_model")
#' }
#' @seealso \code{\link{get_usersoil_table}}, \code{\link{usersoil_to_sol}}
#' @keywords loading

sol_to_usersoil <- function(project_path){
  # Read soil data
  soil_data <- read_tbl("soils.sol", project_path)
  # Separate soil data for each soil type
  st <- soil_data[is.na(as.numeric(soil_data$name)),c(1:7)]
  # Separate soil data for each soil layer 
  soil_layers <- soil_data[!is.na(as.numeric(soil_data$name)), c(1:14)]
  # Rename columns
  colnames(soil_layers) <- c("SOL_Z", "SOL_BD", "SOL_AWC", "SOL_K", "SOL_CBN", 
                             "CLAY", "SILT", "SAND", "ROCK", "SOL_ALB", "USLE_K", 
                             "SOL_EC", "SOL_CAL", "SOL_PH")
  # Create user soil table and return
  pmap(list(st$name, st$nly, st$hyd_grp, st$dp_tot, st$anion_excl, st$perc_crk, st$texture), 
       function(x1, x2, x3, x4, x5, x6, x7){bind_cols(data.frame(SNAM = rep(x1,x2) %>% unlist(.),
                                                                 LAYER_NB = seq(1,x2), 
                                                                 NLAYERS = x2,
                                                                 HYDGRP = x3,
                                                                 SOL_ZMX = x4,
                                                                 ANION_EXCL = x5,
                                                                 SOL_CRK = x6,
                                                                 TEXTURE = x7))}) %>%
    bind_rows(.) %>% 
    bind_cols(soil_layers) %>% 
    mutate_at(., vars(SOL_Z:SOL_PH), ~as.numeric(.)) %>%
    pivot_longer(-c(SNAM, LAYER_NB, NLAYERS, HYDGRP, SOL_ZMX, ANION_EXCL, SOL_CRK, TEXTURE), 
                 names_to = "PAR_NAME", values_to = "PAR_VAL") %>% 
    mutate(PAR_NAME = paste0(PAR_NAME, LAYER_NB)) %>% 
    select(-LAYER_NB) %>%
    pivot_wider(names_from = PAR_NAME, values_from = PAR_VAL)
}
