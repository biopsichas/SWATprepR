
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

