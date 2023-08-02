
# Loading templates --------------------------------------------------------------------

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
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "SWATprepR")
#' met_lst <- load_template(temp_path, 3035)
#' str(met_lst)
#' ## 2) Example of template for calibration data
#' temp_path <- system.file("extdata", "calibration_data.xlsx", package = "SWATprepR")
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
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "SWATprepR")
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
#' @export
#' 
#' @examples
#' \dontrun{
#' df <- read_tbl('rout_unit.con', 'model_folder', 3, 2) 
#' }

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

#' Loading SWAT+ weather files to R
#'
#' @param input_folder character, path to folder with SWAT+ weather input files (example "my_model").
#' @importFrom sf st_as_sf
#' @importFrom stringr str_extract
#' @importFrom dplyr bind_rows mutate select 
#' @importFrom stats setNames
#' @return nested list of lists with dataframes. Nested structure list -> stations, 
#' list -> Variable -> Dataframe (DATE, VARIABLE).
#' @export
#'
#' @examples
#' \dontrun{
#' met_lst <- load_swat_weather("my_folder")
#' }

load_swat_weather <- function(input_folder){
  print(paste0("Loading of SWAT+ weather data is started from ", input_folder, " directory."))
  ##Identifying all weather files
  fs <- list.files(input_folder, recursive = F, pattern="*.pcp|*.slr|*.hmd|*.tmp|*.wnd")
  if(length(fs)==0){
    stop(paste0("No SWAT+ weather files have been found in ", input_folder, " directory!!!"))
  }
  ##Preparing list and dics required for loop
  rlist <- list() ##resulting nested list of lists
  stations <- NULL ##dataframe for station data
  p_lst <- list("pcp" = c("PCP"), 
                "slr" = c("SLR"), 
                "hmd" = c("RELHUM"), 
                "tmp" = c("TMP_MAX", "TMP_MIN"), 
                "wnd" = c("WNDSPD")) ##Parameters dics
  s_info <- unlist(strsplit(input_folder, "/")) ##source information 
  s_info <- paste0(s_info[c(length(s_info)-1, length(s_info))], collapse = '/')
  nb <- length(fs)
  ##Main loop
  for (i in seq(1,nb)){
    ##Getting variable type
    f_type <- p_lst[sub(".*\\.", "", fs[i])][[1]]
    ##Loading data from text file
    df <- read_tbl(fs[i], input_folder)
    ##Getting station ID and saving it into dafaframe
    id <- str_extract(toupper(fs[i]), "ID([\\d]+)")
    if(!is.na(id)){
      st_info <- data.frame(ID = id)
    } else {
      if(!is.null(stations)){
        st_info <- data.frame(ID = max(as.numeric(str_extract(stations$ID, "(?<=ID)\\d+")))+1)
      } else {
        st_info <- data.frame(ID = 1)
      }
      id <- st_info$ID
    }
    ##Filling station information for the file laoded
    st_info[c("Name", "Elevation", "Source", "Long", "Lat")] <- list(gsub("\\..*","",fs[i]), 
                                                                     as.numeric(df[[1,"elev"]]), 
                                                                     s_info, 
                                                                     as.numeric(df[[1,"lon"]]),  
                                                                     as.numeric(df[[1,"lat"]]))
    st_info <- st_as_sf(st_info, coords = c("Long", "Lat"), crs = 4326, remove = F) 
    ##Checking is station information alreade in stations dataframe
    if(!st_info$geometry %in% stations$geometry){
      if(!is.null(stations)){
        stations <- bind_rows(stations, st_info)
      } else {
        stations <- st_info
      }
    }
    ##Identifying if file is for temperature or other parameters, processing and saving time series data
    if(length(f_type)==1){
      ts <- df[2:dim(df)[1], 1:3] %>% 
        mutate(DATE = as.POSIXct(strptime(paste(nbyr, tstep), format="%Y %j", tz = "UTC"))) %>% 
        select(DATE, lat) %>% 
        setNames(c("DATE",f_type)) 
      ##If PCP below 0.2, than 0
      if(f_type == "PCP" && min(ts[["PCP"]], na.rm = TRUE) < 0.2){
        ts$PCP <- ifelse(ts[["PCP"]] < 0.2, 0, ts[["PCP"]])
      }
      rlist[[id]][[f_type]] <- ts
    } else if(length(f_type)==2){
      ts <- df[2:dim(df)[1], 1:4] %>% 
        mutate(DATE = as.POSIXct(strptime(paste(nbyr, tstep), format="%Y %j", tz = "UTC"))) %>% 
        select(DATE, lat, lon) %>% 
        setNames(c("DATE",f_type)) 
      rlist[[id]][[f_type[1]]] <- ts[,c("DATE", f_type[1])]
      rlist[[id]][[f_type[2]]] <- ts[,c("DATE", f_type[2])]
    }
    cat("\014")
    print(paste0(format(round(100*i/nb, 2), nsmall = 2), "% of data are loaded."))
  }
  print("Data loading finished succesfully.")
  return(list(stations = stations, data = rlist))
}

# Loading other --------------------------------------------------------------------

#' Extract EMEP atmospheric deposition data for a catchment
#'
#' @param catchment_boundary_path path to basin boundary shape file.
#' @param t_ext string, which EMEP data to access 'year' for yearly averages, 'month' - monthly averages. Optional (default - 'year').
#' @param start_year integer year to start data extraction. Optional (default - 1990).
#' @param end_year integer year to end data extraction. Optional (default - 2020).
#' @importFrom sf st_transform st_read st_bbox
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom dplyr bind_rows
#' @return dafaframe with "DATE", "NH4_RF", "NO3_RF" , "NH4_DRY"  and "NO3_DRY" columns. Values in SWAT+ units.
#' "NH4_RF" - ammonia in rainfall (mg/l), "NO3_RF" - nitrate in rainfall (mg/l), 
#' NH4_RF - ammonia deposition (kg/ha/yr), "NO3_DRY" - nitrate dry deposition (kg/ha/yr)
#' @export 
#' @examples
#' \dontrun{
#' basin_path <- system.file("extdata", "GIS/basin.shp", package = "SWATprepR")
#' df <- get_atmo_dep(basin_path)
#' ##Plot results
#' ggplot(pivot_longer(df, !DATE, names_to = "par", values_to = "values"), aes(x = DATE, y = values))+
#' geom_line()+
#' facet_wrap(~par, scales = "free_y")+
#' theme_bw()
#' }

get_atmo_dep <- function(catchment_boundary_path, t_ext = "year", start_year = 1990, end_year = 2020){
  ##Part url link to emep data (more info found here https://www.emep.int/mscw/mscw_moddata.html)
  url_prt <- "https://thredds.met.no/thredds/dodsC/data/EMEP/2022_Reporting/EMEP01_rv4.45_"
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
    if(u != 2020){
      uu <- "_rep2022.nc"
    } else {
      uu <- ".nc"
    }
    ##Assembling URL for each year
    url <- paste0(url_prt, t_ext, ".", u, "met_", u, "emis", uu)
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
      dry_oxn <- mean(var.get.nc(r, "DDEP_OXN_m2Grid")[ilon, ilat]*4.4268/100)
      wet_oxn <- mean(var.get.nc(r, "WDEP_OXN")[ilon, ilat]*4.4268/prec)
      dry_rdn <- mean(var.get.nc(r, "DDEP_RDN_m2Grid")[ilon, ilat]*1.2878/100)
      wet_rdn <- mean(var.get.nc(r, "WDEP_RDN")[ilon, ilat]*1.2878/prec)
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

#' Extract climate data into nested list of lists 
#'
#' @param dir_path character, path to CORDEX-BC folder (example climate/CORDEX-BC").
#' @param location character or list. In case of character, path should be provided to 
#' catchment boundary file (example GIS/basin.shp). In case of list, nested list of lists 
#' with dataframes. Nested structure meteo_lst -> data -> Station ID -> Parameter -> 
#' Dataframe (DATE, PARAMETER). List of list could be obtained using \code{load_template()}
#' with prepared excel template.
#' @importFrom elevatr get_elev_point
#' @importFrom sf st_read st_transform st_as_sf st_crs st_overlaps st_centroid
#' @importFrom raster brick rasterToPolygons extract
#' @importFrom dplyr select rename mutate 
#' @importFrom tidyr drop_na
#' @importFrom purrr map
#' @return Nested lists of lists. First nesting level is for RCP, second for RCM model numbers,
#' the rest is the same as in meteo_lst. This part could be used with other package functions 
#' (example  plot_weather(result$rcp26$1), "PCP", "month", "sum").
#' @export
#'
#' @examples
#' \dontrun{
#' basin_path <- system.file("extdata", "GIS/basin.shp", package = "SWATprepR") 
#' data_path <- "climate/CORDEX-BC"
#' result <- load_netcdf_weather(data_path, basin_path)
#' }

load_netcdf_weather <- function(dir_path, location){
  ##Checking inputs
  if(!is.character(dir_path)){
    stop("Your input to function parameter 'dir_path' is not character!!! Please correct this.")
  }
  fs <- list.files(dir_path, recursive = T)
  if(length(fs)==0){
    stop(paste0("No netCDF data were found on ", dir_path, " path!!! 
                Please check your path and consult function documentation."))
  }
  if(!is.character(location)|is.list(location)){
    stop(paste0("`location` parameter input is ", class(location), 
                "Only possible inputs are of character or list type. 
                Please consult function documentation."))
  }
  ##In case path to shape is given
  if (is(class(location), "character")){
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
  } else if (is(class(location), "list")){
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