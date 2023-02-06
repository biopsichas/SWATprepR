
# Weather from interpolation ---------------------------------------------------

#' Writing input files (all except TMP)
#'
#' @param write_path path to folder where results should be written.
#' @param sp_df SpatialPointsDataFrame with resulting interpolated data.
#' @param meteo_lst nested list of lists with dataframes. 
#' @param par weather variable (i.e. "PCP", "SLR", etc).
#' @importFrom utils write.table
#' @return weather data text files for virtual stations (created during interpolation) 
#' in format usable by the SWAT model.
#' @export
#'
#' @examples
#' \dontrun{
#' write_input_files("./output/", sp_df, "PCP")
#' }

write_input_files <- function(write_path, sp_df, meteo_lst, par){
  ##Preparing time series files and writing them into output folder
  df <- df_t(sp_df)
  ##Getting starting date for time series
  starting_date <- as.character(format(get_dates(meteo_lst)$min_date, "%Y%m%d"))
  ##Loop to write all input files
  for(i in 1:ncol(df)){
    df_tmp <- df[i]
    names(df_tmp)[1] <- starting_date
    write.table(df_tmp, paste0(write_path, "ST_", i, "_", par, ".txt"), append = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = TRUE, quote = FALSE)
  }
  return(print(paste0(par, " input files was was written successfully into ", write_path)))
}


#' Writing input files for TMP data
#'
#' @param write_path path to folder where results should be written.
#' @param sp_df_mx SpatialPointsDataFrame with resulting interpolated data for TMP_MAX variable.
#' @param sp_df_mn SpatialPointsDataFrame with resulting interpolated data for TMP_MIN variable.
#' @param meteo_lst nested list of lists with dataframes. 
#' @importFrom utils write.table
#' @return temperature weather data text files for virtual stations (created during interpolation) 
#' in format usable by the SWAT model.
#' @export
#'
#' @examples
#' \dontrun{
#' write_input_files_tmp("./output/", sp_df_mx, sp_df_mn)
#' }

write_input_files_tmp <- function(write_path, sp_df_mx, sp_df_mn, meteo_lst){
  ##Preparing TMP_MAX and TMP_MIN time series files 
  df_mx <- df_t(sp_df_mx)
  df_mn <- df_t(sp_df_mn)
  # ##Getting starting date for time series
  starting_date_mx <- as.character(format(get_dates(meteo_lst)$min_date, "%Y%m%d"))
  ##Checking if all data is OK
  if (dim(df_mx)[2] == dim(df_mn)[2]){
    ##Loop to write all input files
    for(i in 1:ncol(df_mx)){
      suppressMessages(df_tmp <- bind_cols(df_mx[i], df_mn[i]))
      write.table(df_tmp, paste0(write_path, "ST_", i, "_TMP.txt"), append = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      fConn <- file(paste0(write_path, "ST_", i, "_TMP.txt"), 'r+')
      Lines <- readLines(fConn)
      writeLines(c(starting_date_mx, Lines), con = fConn)
      close(fConn)
    }
  } else {
    stop("TMP_MAX and TMP_MIN data have problems. Starting dates are different or there are differences in stations. Please correct it!")
  }
  return(print(paste0("TMP input files was was written successfully into ", write_path)))
}

#' Writing reference file for weather data
#'
#' @param write_path path to folder where results should be written.
#' @param sp_df SpatialPointsDataFrame with resulting interpolated data.
#' @param par weather variable (i.e. "PCP", "SLR", etc).
#' @importFrom sf st_as_sf st_transform st_coordinates st_drop_geometry
#' @importFrom dplyr mutate bind_cols mutate_if row_number rename select
#' @importFrom utils write.table
#' @return weather virtual stations (created during interpolation) reference file
#' in format usable by the SWAT model.
#' @export
#'
#' @examples
#' \dontrun{
#' write_ref_file("./output/", sp_df, "PCP")
#' }

write_ref_file <- function(write_path, sp_df, par){
  ##Prepare .txt reference df
  ref <- sp_df@coords %>% 
    as.data.frame %>% 
    st_as_sf(coords = c("x", "y"), crs = sp_df@proj4string@projargs) %>% 
    st_transform(4326) %>% 
    mutate(Long = sf::st_coordinates(.)[,1],
           Lat = sf::st_coordinates(.)[,2]) %>% 
    bind_cols(sp_df@data["DEM"] %>% rename(Elevation = DEM)) %>% 
    st_drop_geometry() %>% 
    mutate_if(is.numeric, ~round(.,5)) %>% 
    mutate(ID = row_number()) %>% 
    mutate(Name = paste0("ST_", ID, "_", par)) %>% 
    select(ID, Name, Lat, Long, Elevation)
  
  write.table(ref, write_path, append = FALSE, sep = ",", dec = ".", row.names = FALSE, col.names = TRUE, quote = FALSE)
  return(print(paste0(par, " reference file was was written successfully to ", write_path)))
}

# Input for WGNmaker ------------------------------------------------------

#' Preparing input files for WGNmaker. 
#' 
#' WGNmaker excel macro could be downloaded from following link: https://swat.tamu.edu/media/41583/wgen-excel.zip
#'  
#' @param write_path path to folder where results should be written.
#' @param meteo_lst nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' @param wgn_stations_lst List of selected stations for each parameter to be used in data preparation. 
#' Example list(PCP = "ID12",  SLR = "ID11", RELHUM = "ID12", TMP_MAX = "ID12", 
#' TMP_MIN = "ID12", WNDSPD = "ID12", MAXHHR = "ID12")
#' @param station_name string with name of station.
#' @importFrom dplyr %>% left_join mutate select
#' @importFrom xlsx write.xlsx
#' @return Excel files needed for WGNmaker
#' @export
#'
#' @examples
#' \dontrun{
#' write_wgnmaker_files("./wgn/", meteo_lst, wgn_stations_lst, "Station1")
#' }

write_wgnmaker_files <- function(write_path, meteo_lst, wgn_stations_lst, station_name){
  print("Files for WGNmaker is being prepared")
  meteo_lst <- meteo_lst$data
  p_lst_wgn <- list("PCP" = "_pcp", "SLR" = "_slr", "RELHUM" = "_dwp", "TMP_MAX" = "_tmp", 
                    "TMP_MIN" = "_tmp", "WNDSPD" = "_wnd", "MAXHHR" = "_hhr")
  t_df <- NULL
  for (n in names(wgn_stations_lst)){
    if(!startsWith(n, 'TMP') & n != "RELHUM"){
      df <- meteo_lst[[wgn_stations_lst[[n]]]][[n]] %>% 
        mutate(DATE = format(DATE, "%m/%d/%Y"))
    } else if(startsWith(n, 'TMP')){
      ##Join Max and Min in one file
      df <- meteo_lst[[wgn_stations_lst[[n]]]][[n]] %>% 
        mutate(DATE = format(DATE, "%m/%d/%Y"))
      if(is.null(t_df)){
        t_df <- df
        next
      }else{
        df <- t_df %>% 
          left_join(df, by = "DATE")
      }
    } else if(n == "RELHUM"){
      ##Calculation of Dew point from relative humidity 
      ##Equation is provided on https://swat.tamu.edu/software/ Dewpoint Estimation documentation 
      df <- meteo_lst[[wgn_stations_lst[[n]]]][[n]] %>% 
        left_join(meteo_lst[[wgn_stations_lst[["TMP_MAX"]]]][["TMP_MAX"]], by = "DATE") %>% 
        left_join(meteo_lst[[wgn_stations_lst[["TMP_MIN"]]]][["TMP_MIN"]], by = "DATE") %>% 
        mutate(Esmx = 0.6108 * exp((17.27 * TMP_MAX) / (TMP_MAX + 237.3)),
               Esmn = 0.6108 * exp((17.27 * TMP_MIN) / (TMP_MIN + 237.3))) %>% 
        mutate(Es = (Esmx + Esmn)/2) %>% 
        mutate(Ea = RELHUM * Es * 10 / 100) %>% 
        mutate(DWP = (234.18 * log10(Ea) - 184.2) / (8.204-log10(Ea))) %>% 
        select(DATE, DWP) %>% 
        mutate(DATE = format(DATE, "%m/%d/%Y")) 
    }
    ##Writing files 
    ##Prepare path
    f <- paste0(write_path, station_name, p_lst_wgn[[n]], ".xls")
    ##Check if exist, then delete
    if (file.exists(f)){file.remove(f)}
    ##Write 
    write.xlsx(as.data.frame(df), file = f, row.names = F, append = F, showNA=FALSE)
    print(paste("File has been written to", f))
  }
  return(print(paste0("Files for WGNmaker is prepared and located in ", write_path)))
}

# Write template ------------------------------------------------------

#' Write weather meteo_lst into template .xlsx file
#'
#' @param meteo_lst nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' @param write_path (optional) character path to folder where results should be written (default "").
#' @param f_name (optional) character name of the file (default 'weather_data.xlsx'). 
#' @importFrom xlsx write.xlsx2 write.xlsx
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr %>% select full_join arrange mutate
#' @return .xlsx file written in template format, which could de read by \code{\link{load_template}} function.
#' @export
#'
#' @examples
#' \dontrun{
#' write_weather_template(met_lst)
#' }

write_weather_template <- function(meteo_lst, write_path = "", f_name = "weather_data.xlsx"){
  ##Removing existing file if in path
  f <- paste0(write_path, f_name)
  if (file.exists(f)){file.remove(f)}
  ##Writing station info
  write.xlsx(as.data.frame(meteo_lst$stations %>% 
                             st_drop_geometry %>%
                             select(ID, Name, Elevation, Source, Long, Lat)), 
             file = f, sheetName = "Stations", row.names = F, append = FALSE, showNA=FALSE)
  options(xlsx.date.format="YYYY-MM-DD")
  ##Writing data
  dt <- meteo_lst$data
  for (n in names(dt)){
    df <- NULL
    print(paste0("Working on station ", n))
    for (par in names(dt[[n]])){
      if(is.null(df)){
        df <- dt[[n]][[par]]
      } else {
        df <- df %>%
          full_join(dt[[n]][[par]], by = "DATE")
      }
    }
    write.xlsx2(as.data.frame(df %>% mutate(DATE = as.Date(DATE)) %>% arrange(DATE)), file = f, sheetName = n, row.names = F,
                append = T, showNA=FALSE)
    ##Cleaning not get error from Java 
    gc()
  }
  return(paste0("Writing was successful. Your file is in ", f))
}

