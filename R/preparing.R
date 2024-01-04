
# Extracting to interpolate -----------------------------------------------

#' Function to get data to be used in the interpolation
#'
#' @param meteo_lst nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' @param par is weather variable to extract (i.e. "PCP", "SLR", etc)
#' @importFrom dplyr left_join select everything %>%
#' @importFrom tibble rownames_to_column
#' @importFrom sf st_crs st_set_geometry
#' @importFrom sp coordinates<- proj4string<- CRS
#' @importFrom stringr str_to_title
#' @return SpatialPointsDataFrame with columns as days and rows as stations.
#' @keywords internal
#' 
#' @examples
#' ##get_data_to_interpolate (meteo_lst, "PCP")

get_data_to_interpolate <- function(meteo_lst, par){
  ##Getting building continuous dataframe with min, max dates
  df <- data.frame(DATE = seq(as.POSIXct(get_dates(meteo_lst)$min_date, "%Y-%m-%d", tz = "UTC"),
                              as.POSIXct(get_dates(meteo_lst)$max_date, "%Y-%m-%d", tz = "UTC"), by = "day"))
  stations <- meteo_lst$stations
  meteo_lst <- meteo_lst$data
  ##Extracting data for selected parameter from meteo_lst
  for (n in names(meteo_lst)){
    if(par %in% names(meteo_lst[[n]])){
      df <- left_join(df, meteo_lst[[n]][[par]] %>%
                        `colnames<-`(c("DATE", n)), by = "DATE")
    }
  }
  ##ID to names of stations
  id_station <- stations %>% 
    st_set_geometry(NULL) %>% 
    mutate(Name = paste0(str_to_title(Name), " (", Source, ")")) %>% 
    select(ID, Name, Lat, Long)
  ##Transforming extracted data
  df <- as.data.frame(t(df[-1])) %>% 
    rownames_to_column(var = "ID") %>% 
    left_join(id_station[-2], by = "ID") %>% 
    select(Lat, Long, everything(), -ID)
  ## Converting df data to sp
  coordinates(df) <-  ~Long + Lat
  ##Adding crs
  suppressWarnings(proj4string(df) <-  CRS(SRS_string = st_crs(stations)$input))
  return(df)
}

#' Main interpolation function
#'
#' @param meteo_lst nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' @param grd sp SpatialGrid grid for the interpolation. 
#' @param par character representing weather variable to extract (i.e. "PCP", "SLR", etc).
#' @param shp  sf dataframe for defining basin boundary shape.
#' @param dem_data_path path to DEM raster data in same projection as weather station.
#' @param idw_exponent numeric value for exponent parameter to be used in interpolation. 
#' (optional, default value is 2).
#' @importFrom sp coordinates<- proj4string<- CRS over
#' @importFrom methods as
#' @importFrom raster extract raster
#' @importFrom sf st_crs st_transform read_sf
#' @importFrom methods slot slot<-
#' @return SpatialPointsDataFrame with interpolated data.
#' @keywords internal
#' @examples
#' \dontrun{
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "SWATprepR")
#' DEM_path <- system.file("extdata", "GIS/DEM.tif", package = "SWATprepR")
#' basin_path <- system.file("extdata", "GIS/basin.shp", package = "SWATprepR")
#' met_lst <- load_template(temp_path, 3035)
#' get_interpolated_data(met_lst, grd, "PCP", read_sf(basin_path), DEM_path, 2)
#' }

get_interpolated_data <- function(meteo_lst, grd, par, shp, dem_data_path, idw_exponent){
  ##Preparing data for interpolation and grid
  df <- get_data_to_interpolate(meteo_lst, par)
  ##Loading data
  DEM <- raster(dem_data_path)
  ##Defining coordinate system
  m_proj <- st_crs(meteo_lst$stations)$input
  if (m_proj != st_crs(shp)$input){
    shp <- st_transform(shp, m_proj)
  }
  ##Dealing with coordinates
  suppressWarnings(proj4string(DEM) <-  CRS(SRS_string = m_proj))
  ##Converting into sp
  grd_p <- as(grd, "SpatialPoints")
  shp_sp <- as(shp, 'Spatial')
  ##Just to be sure they are exact
  slot(grd_p, "proj4string") <- slot(shp_sp, "proj4string")
  ##extracting points from the grid for the catchment and saving results
  meteo_pts <- as(grd_p[!is.na(over(grd_p, shp_sp)[1]),], "SpatialPointsDataFrame")
  meteo_pts@data <- data.frame(DEM = raster::extract(DEM, meteo_pts))
  ##Adding DEM values
  nb <- dim(df)[[2]]
  for (i in seq(1,nb)){
    cat("\014")
    print(paste0(format(round(100*i/nb, 2), nsmall = 2), "% ", par," finished."))
    meteo_pts <- suppressMessages(get_interpolation(df[i], meteo_pts, grd, i, idw_exponent))
  }
  return(meteo_pts)
}

#' Interpolate Weather Data 
#'
#' This function interpolates weather data for a SWAT model and saves results 
#' into nested list format
#'
#' @param meteo_lst A nested list of lists with dataframes. 
#'   Nested structure: \code{meteo_lst -> data -> Station ID -> Parameter -> 
#'   Dataframe (DATE, PARAMETER)}, 
#'   \code{meteo_lst -> stations -> Dataframe (ID, Name, Elevation, Source, 
#'   geometry, Long, Lat)}. \cr\cr
#'   meteo_lst can be created using \code{\link{load_template}} function using 
#'   'xlsx' template file or it could to be created with \code{\link{load_swat_weather}}
#'   function loading information from SWAT+ model setup weather files.
#' @param catchment_boundary_path Character, path to the basin boundary shape file.
#' @param dem_data_path Character, path to DEM raster data in the same projection 
#' as the weather station.
#' @param grid_spacing Numeric, value for the distance between grid points. 
#'   Units of the coordinate system should be used.
#' @param p_vector (optional) Character vector representing weather variables to 
#' interpolate. Default is all variables selected 
#' \code{p_vector = c("PCP", "SLR", "RELHUM", "WNDSPD", "TMP_MAX", "TMP_MIN")}.
#' @param idw_exponent (optional) Numeric value for the exponent parameter to 
#' be used in interpolation. Default \code{idw_exponent = 2}.
#' @importFrom sf st_zm st_bbox st_read st_crs st_transform
#' @return A nested list of lists with interpolation results.
#'    A nested list of lists with dataframes. 
#'   Nested structure: \code{meteo_lst -> data -> Station ID -> Parameter -> 
#'   Dataframe (DATE, PARAMETER)}, 
#'   \code{meteo_lst -> stations -> Dataframe (ID, Name, Elevation, Source, 
#'   geometry, Long, Lat)}.
#' @export
#' @examples
#' \dontrun{
#'   # Specify paths to weather  data, basing shapeand DEM
#'   temp_path <- system.file("extdata", "weather_data.xlsx", package = "SWATprepR")
#'   DEM_path <- system.file("extdata", "GIS/DEM.tif", package = "SWATprepR")
#'   basin_path <- system.file("extdata", "GIS/basin.shp", package = "SWATprepR")
#'
#'   # Load weather data template
#'   met_lst <- load_template(temp_path, 3035)
#'
#'   # Interpolate and write SWAT model input files
#'   interpolate(met_lst, basin_path, DEM_path, 2000) 
#' }
#' @keywords gap-filling

interpolate <- function(meteo_lst, catchment_boundary_path, dem_data_path, grid_spacing, 
                        p_vector = c("PCP", "SLR", "RELHUM", "WNDSPD", "TMP_MAX", "TMP_MIN"), idw_exponent = 2){
  ##List to save interpolation results for examining
  results <- list()
  p_lst <- list("PCP" = "pcp", "SLR" = "solar", "RELHUM" = "rh", "TMP_MAX" = "tmp", 
                "TMP_MIN" = "tmp", "WNDSPD" = "wind")
  ##Reading and defining coordinate system
  shp <- st_read(catchment_boundary_path, quiet = TRUE)
  ##In case shape file is saved in Polygon (MultiPolygonZ) format with Z values as well
  if(dim(shp[['geometry']][[1]][[1]])[2] == 3){
    shp <- st_zm(shp)
  }
  m_proj <- st_crs(meteo_lst$stations)$input
  if (m_proj != st_crs(shp)$input){
    shp <- st_transform(shp, m_proj)
  }
  ##Getting vector of max extent border coordinates
  b <- st_bbox(shp)
  s <- st_bbox(meteo_lst$stations)
  bb <- c(min(b[1], s[1]), min(b[2], s[2]), max(b[3], s[3]), max(b[4], s[4]))
  ##Making a grid to interpolate to
  grd <- get_grid(bb, grid_spacing, st_crs(shp)$wkt)
  ##Loop for all parameter
  for (p in p_vector){
    ##Interpolation case for which has data at more than 1 station and not TMP
    if(get_nb_st_with_data(meteo_lst, p)>1){
      results[[p]] <- get_interpolated_data(meteo_lst, grd, p, shp, dem_data_path, idw_exponent)
    }
  }
  cat("\014") 
  print("Interpolation is finished.")
  ##Converting into list of lists 
  start_date <- get_dates(meteo_lst)$min_date
  end_date <- get_dates(meteo_lst)$max_date
  meteo_lst_int <- transform_to_list(results, start_date, end_date)
  print("Results converted into nested list format.")
  return(meteo_lst_int)
}

# Weather data -----------------------------------------------

#' Fill missing variables from the closest stations with available data
#'
#' This function fills missing variables by interpolating values from the 
#' closest stations that have data.
#'
#' @param meteo_lst A nested list of lists with dataframes. 
#'   Nested structure: \code{meteo_lst -> data -> Station ID -> Parameter -> 
#'   Dataframe (DATE, PARAMETER)}.
#'   Nested \code{meteo_lst -> stations -> Dataframe (ID, Name, Elevation, Source, 
#'   geometry, Long, Lat)}. \cr\cr
#'   meteo_lst can be created using \code{\link{load_template}} function using 
#'   'xlsx' template file or it could to be created with \code{\link{load_swat_weather}}
#'   function loading information from SWAT+ model setup weather files.
#' @param par_fill (optional) A vector of variables to be filled. Default is 
#' \code{par_fill = c("TMP_MAX", "TMP_MIN","PCP", "RELHUM", "WNDSPD", "SLR")}.
#' @importFrom sf st_distance
#' @importFrom dplyr filter %>% 
#' @return A list of dataframes with filled data. Updated list is for meteo_lst$data.
#'
#' @examples
#' \dontrun{
#'   # Load weather data from an Excel file
#'   temp_path <- system.file("extdata", "weather_data.xlsx", package = "SWATprepR")
#'   met_lst <- load_template(temp_path, 3035)
#'   
#'   # Fill for missing variables
#'   met_lst$data <- fill_with_closest(met_lst, c("TMP_MAX", "TMP_MIN"))
#' }
#' @keywords internal

fill_with_closest <- function(meteo_lst, par_fill = c("TMP_MAX", "TMP_MIN","PCP", "RELHUM", "WNDSPD", "SLR")){
  ##Initializing vectors, lists
  df_list <- meteo_lst$data
  stations <- names(df_list)
  av_list <- list()
  ##Checking missing variables for every station
  for(p in par_fill){
    c <- c()
    for(st in stations){
      if(!is.null(df_list[[st]][[p]])){
        c <- c(c, st)
      }
    }
    av_list[[p]] <- c
  }
  ##Loop to fill data for missing stations for provided variables in par_fill
  for (p in par_fill){
    ##Stations with data for selected variable
    sel <- meteo_lst$stations %>% filter(ID %in% as.vector(av_list[[p]]))
    ##Matrix for distances between stations
    m <- st_distance(meteo_lst$stations, sel)
    ##Adding names
    rownames(m) <- meteo_lst$stations$ID
    colnames(m) <- sel$ID
    ##For each station missing selected variable
    for(st in stations[!stations %in% av_list[[p]]]){
      ##Finding closest station with data 
      st_fill <- colnames(m)[which(m[st,] == min(m[st,]))]
      df_list[[st]][[p]] <- meteo_lst$data[[st_fill]][[p]]
    }
  }
  return(df_list)
}

#' Generate Weather Generator (WGN) Data for SWAT+ Model
#'
#' This function generates weather generator (WGN) data for a SWAT+ model 
#' based on meteorological data.
#'
#' @param meteo_lst Nested list with dataframes. 
#'   Nested structure: \code{meteo_lst -> data -> Station ID -> Parameter -> 
#'   Dataframe (DATE, PARAMETER)}, 
#'   \code{meteo_lst -> stations -> Dataframe (ID, Name, Elevation, Source, 
#'   geometry, Long, Lat)}. \cr\cr
#'   meteo_lst can be created using \code{\link{load_template}} function using 
#'   'xlsx' template file or it could to be created with \code{\link{load_swat_weather}}
#'   function loading information from SWAT+ model setup weather files.
#' @param TMP_MAX (optional) Dataframe with two columns: DATE : POSIXct, TMP_MAX : num. 
#' This parameter refers to data, which should be used instead if TMP_MAX variable 
#' is missing for a station. Default \code{TMP_MAX = NULL}, data of the closest 
#' station with data will be used. Units: Celsius.
#' @param TMP_MIN (optional) Dataframe with two columns: DATE : POSIXct, TMP_MIN : num. 
#'   This parameter refers to data, which should be used instead if TMP_MIN variable 
#'   is missing for a station. Default \code{TMP_MIN = NULL}, indicating that data of the closest 
#'   station with data will be used. Units: Celsius.
#' @param PCP (optional) Dataframe with two columns: DATE : POSIXct, PCP : num. 
#'   This parameter refers to data, which should be used instead if PCP variable 
#'   is missing for a station. Default \code{PCP = NULL}, indicating that data of the closest 
#'   station with data will be used. Units: mm/day.
#' @param RELHUM (optional) Dataframe with two columns: DATE : POSIXct, RELHUM : num. 
#'   This parameter refers to data, which should be used instead if RELHUM variable 
#'   is missing for a station. Default \code{RELHUM = NULL}, indicating that data of the closest 
#'   station with data will be used. Units: ratio 0-1.
#' @param WNDSPD (optional) Dataframe with two columns: DATE : POSIXct, WNDSPD : num. 
#'   This parameter refers to data, which should be used instead if WNDSPD variable 
#'   is missing for a station. Default \code{WNDSPD = NULL}, indicating that data of the closest 
#'   station with data will be used. Units: m/s.
#' @param MAXHHR (optional) Dataframe with two columns: DATE : POSIXct, MAXHHR : num. 
#'   This parameter refers to data, which should be used instead if MAXHHR variable 
#'   is missing for a station. Default \code{MAXHHR = NULL}, indicating that data of the closest 
#'   station with data will be used. Units: mm.
#' @param SLR (optional) Dataframe with two columns: DATE : POSIXct, SLR : num. 
#'   This parameter refers to data, which should be used instead if SLR variable 
#'   is missing for a station. Default \code{SLR = NULL}, indicating that data of the closest 
#'   station with data will be used. Units: MJ/m2.
#' @importFrom stats aggregate sd
#' @importFrom sf st_coordinates st_transform st_crs st_drop_geometry
#' @importFrom dplyr %>% rename mutate bind_rows select
#' @importFrom lubridate month
#' @importFrom readr parse_number
#' @return List of two dataframes: wgn_st - WGN station data, wgn_data - WGN data.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   temp_path <- system.file("extdata", "weather_data.xlsx", package = "SWATprepR")
#'   met_lst <- load_template(temp_path, 3035)
#'   # Generate WGN data
#'   wgn <- prepare_wgn(met_lst,
#'                      TMP_MAX = met_lst$data$ID10$TMP_MAX,
#'                      TMP_MIN = met_lst$data$ID10$TMP_MIN,
#'                      PCP = met_lst$data$ID9$PCP,
#'                      RELHUM = met_lst$data$ID9$RELHUM,
#'                      WNDSPD = met_lst$data$ID10$WNDSPD,
#'                      MAXHHR = met_lst$data$ID11$MAXHHR,
#'                      SLR = met_lst$data$ID9$SLR)
#' }
#' @keywords parameters
#' @seealso \code{\link{load_template}}

prepare_wgn <- function(meteo_lst, TMP_MAX = NULL, TMP_MIN = NULL, PCP = NULL, RELHUM = NULL, WNDSPD = NULL, MAXHHR = NULL, SLR = NULL){
  ##Extracting relevant parts
  data <- meteo_lst$data
  st_df <- meteo_lst$stations
  ##Checking and making sure coordinate system is correct
  if(!grepl("4326", st_crs(st_df)$input)){
    st_df <- st_transform(st_df, 4326)
    print("Coordinate system checked and transformed to EPSG:4326.")
  }
  ##Converting station data to dataframe with needed columns
  st_df <- st_df %>% 
    rename(NAME = Name, ELEVATION = Elevation) %>% 
    mutate(LONG = st_coordinates(.)[,1],
           LAT = st_coordinates(.)[,2]) %>% 
    st_drop_geometry() %>% 
    select(ID, NAME, LAT, LONG, ELEVATION) 
  ##Checking if variables are not missing 
  stations <- names(data)
  all_p <- c("TMP_MAX", "TMP_MIN","PCP", "RELHUM", "WNDSPD", "MAXHHR", "SLR")
  c <- c()
  ##Checking missing variables for every stations 
  for(st in stations){
    missing_p <- all_p[!all_p %in% as.vector(names(data[[st]]))]
    c <- c(c, missing_p[!missing_p %in% c])
  }
  ##Checking if those variables have not been provided to function
  c_f <- c()
  for (c1 in c){
    if(is.null(eval(parse(text=c1)))){
      c_f <- c(c_f, c1)
    }
  }
  ##Missing variables error (in case there are missing variables and they were not provided)
  missing_maxhhr <- FALSE
  if (length(c_f) != 0){
    if (length(c_f) == 1 && c_f == "MAXHHR"){
      missing_maxhhr <- TRUE
    } else{
      warning(paste("These variables", paste(as.character(c_f), sep="' '", collapse=", "), "are missing for some of the stations.
      Closest stations with data will be used to fill existing gaps.", if("MAXHHR" %in% c_f){"MAXHHR will be calculated by PCP*0.38."}, "\n", 
      "Please use optional function parameters, if you want specific data to be used in filling missing variables for stations."))
      data <- fill_with_closest(meteo_lst, c_f)
    }
  }
  ##Setting dataframes to save results
  res_wgn_mon <- NULL
  res_wgn_stat <- NULL
  wgn_mon <- data.frame(matrix(data=NA, nrow=12, ncol=17))
  names(wgn_mon) <- c("id","wgn_id","month","tmp_max_ave","tmp_min_ave","tmp_max_sd",
                      "tmp_min_sd","pcp_ave","pcp_sd","pcp_skew","wet_dry","wet_wet",
                      "pcp_days","pcp_hhr","slr_ave","dew_ave","wnd_ave")
  wgn_mon$month <- rep(seq(1,12))
  ##Loop to calculate all parameters for each station
  for (j in 1:length(stations)){
    print(paste0("Working on station ", stations[j], ":", st_df[st_df$ID == stations[j], "NAME"]))
    ##Adding station data
    wgn_stat <- st_df[st_df$ID == stations[j],]
    wgn_stat$ID <- parse_number(stations[j])
    ##Getting stations data
    df <- data[[stations[j]]]
    ##Filling list for particular station with missing variables (provided in function.)
    for (p in all_p){
      if(!p %in% names(df)){
        df[[p]] <- eval(parse(text=p))
      }
    }
    ##Transforming to dataframe
    df <- list_to_df(df) %>% 
      mutate(mon = month(DATE))
    ##Writing number of year data available for PCP
    wgn_stat$RAIN_YRS <- nyears <- nyears(df, "PCP")
    ##Filling weather generator data
    wgn_mon$tmp_max_ave <- aggregate(TMP_MAX~mon, df, mean)[,2]
    wgn_mon$tmp_min_ave <- aggregate(TMP_MIN~mon, df, mean)[,2]
    wgn_mon$tmp_max_sd <- aggregate(TMP_MAX~mon, df, sd)[,2]
    wgn_mon$tmp_min_sd <- aggregate(TMP_MIN~mon, df, sd)[,2]
    wgn_mon$pcp_ave <- aggregate(PCP~mon, df, mean)[,2]
    if (missing_maxhhr){
      wgn_mon$pcp_hhr <- aggregate(PCP~mon, df, my.pcpmhhr)[,2]
    } else {
      wgn_mon$pcp_hhr <- aggregate(MAXHHR~mon, df, max)[,2]
    }
    wgn_mon$pcp_days <- aggregate(PCP~mon, df, my.pcpd, nyears)[,2]
    wgn_mon$pcp_sd <- aggregate(PCP~mon, df, sd)[,2]
    wgn_mon$pcp_skew <- aggregate(PCP~mon, df, my.skew)[,2]
    wgn_mon$wet_dry <- aggregate(PCP~mon, df, my.pwd)[,2]
    wgn_mon$wet_wet <- aggregate(PCP~mon, df, my.pww)[,2]
    wgn_mon$slr_ave <- aggregate(SLR~mon, df, mean)[,2]
    wgn_mon$dew_ave <- aggregate(RELHUM~mon, df, mean)[,2]
    wgn_mon$wnd_ave <- aggregate(WNDSPD~mon, df, mean)[,2]
    wgn_mon$wgn_id <- parse_number(stations[j])
    ##Saving results
    if(!is.null(res_wgn_mon)){
      res_wgn_mon <- bind_rows(res_wgn_mon, wgn_mon)
      res_wgn_stat <- bind_rows(res_wgn_stat, wgn_stat)
    } else {
      res_wgn_mon <- wgn_mon
      res_wgn_stat <- wgn_stat
    }
  }
  ##Just filing row numbers
  res_wgn_mon$id <- c(1:dim(res_wgn_mon)[1])
  return(list(wgn_st = res_wgn_stat, wgn_data = res_wgn_mon))
}

#' Prepare or Update Climate Data Text Input Files in SWAT+ Model
#'
#' This function prepares or updates climate data text input files in a SWAT+ 
#' model based on the provided meteo_lst.
#'
#' @param meteo_lst Nested list with dataframes. 
#'   Nested structure: \code{meteo_lst -> data -> Station ID -> Parameter -> 
#'   Dataframe (DATE, PARAMETER)}, 
#'   \code{meteo_lst -> stations -> Dataframe (ID, Name, Elevation, Source, 
#'   geometry, Long, Lat)}. \cr\cr
#'   meteo_lst can be created using \code{\link{load_template}} function using 
#'   'xlsx' template file or it could to be created with \code{\link{load_swat_weather}}
#'   function loading information from SWAT+ model setup weather files.
#' @param write_path Character, path to the SWAT+ txtinout folder (example "my_model").
#' @param period_starts (optional) Character, date string (example '1991-01-01'). 
#' Default \code{period_starts = NA}, stands for all available in data.
#' @param period_ends (optional) Character, date string (example '2020-12-31'). 
#' Default \code{period_ends = NA}, stands for all available in data.
#' @importFrom purrr map
#' @importFrom dplyr filter %>% mutate select mutate_if mutate_at mutate_all rename full_join contains
#' @importFrom sf st_as_sf
#' @importFrom lubridate year
#' @importFrom utils read.delim
#' @return Fills or updates multiple weather-related text files in the SWAT+ model.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   prepare_climate(meteo_lst, "output", "1991-01-01", "2020-12-31")
#' }
#' @seealso \code{\link{load_template}}
#' @keywords writing

prepare_climate <- function(meteo_lst, write_path, period_starts = NA, period_ends = NA){
  ##Checking input
  if(!is.list(meteo_lst)){
    stop("Make sure your meteo_lst input is list of lists described in function description!!!")
  }
  if(!is.character(write_path)){
    stop("write_path should be character type!")
  }
  ##Dealing with dates
  if(is.na(period_starts)){
    period_starts <- get_dates(meteo_lst)$min_date
  } else {
    tryCatch({
      period_starts <- as.Date(period_starts)
    },
    error = function(e){
      stop("Make sure your period start date is provided in this format '1991-01-01'!!!")
    })
  }
  if(is.na(period_ends)){
    period_ends <- get_dates(meteo_lst)$max_date
  } else {
    tryCatch({
      period_ends <- as.Date(period_ends)
    },
    error = function(e){
      stop("Make sure your period end date is provided in this format '2020-12-31'!!!")
    })
  }
  if(period_starts>=period_ends){
    stop("Make sure your 'period_starts' parameter is earier date than 'period_ends'!!!")
  }

  ##Heading in weather files
  hd_txt <-  paste0(": written by SWATprepR R package on ", Sys.time(), " for SWAT+ rev.60.5.4")
  ##Filtering list to a defined period
  meteo_lst$data <- map(meteo_lst$data, ~map(., ~filter(., DATE >= period_starts & DATE <= period_ends)))
  ##Preparing wgn parameters
  wgn <- prepare_wgn(meteo_lst)
  
  ##Writing weather-wgn.cli file
  fname <- "weather-wgn.cli"
  ##First line to be printed into file
  text_l <- paste0(fname, hd_txt)
  ##Stations info
  df1 <- wgn$wgn_st %>% 
    mutate(ID = paste0("ID", ID)) %>% 
    select(-NAME) %>% 
    mutate_at(vars(c(LAT, LONG, ELEVATION)), ~sprintf(., fmt = '%#.5f'))
  ##Station data
  df2 <- wgn$wgn_data %>% 
    mutate(ID = paste0("ID", wgn_id)) %>% 
    select(-c(id, month, wgn_id)) %>% 
    mutate_if(is.numeric, ~sprintf(., fmt = '%#.5f'))
  ##Defining spacing in written files 
  st_hd <- c('%-30s', rep('%-13s', 2), '%-15s', '%-3s')
  st_dt <- c(rep('%13s', 14))
  ##Printing heading line
  write.table(text_l, paste0(write_path, "/", fname), append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  ##Loop to each station
  for (id in df1$ID){
    ##Filtering
    s1 <- df1[df1$ID == id,]
    s2 <- subset(df2[df2$ID == id,], select = -ID)
    ##Heading for ID
    df_to_txt(write_path, fname, s1, st_hd)
    ##Parameters for layer
    write.table(paste(sprintf(st_dt, names(s2)), collapse = ' '), paste0(write_path, "/", fname), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    df_to_txt(write_path, fname, s2, st_dt)
  }
  print(paste0(fname, " file was successfully written."))
  
  ##Writing weather-sta.cli file
  fname <- "weather-sta.cli"
  ##File heading line
  text_l <- paste0(fname, hd_txt)
  write.table(text_l, paste0(write_path, "/", fname), append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  ##Creating station names from coordinates (example s52699n18600e)
  station_names <- mutate_all(df1[c("LAT", "LONG")], ~sprintf(., fmt = '%#.6s')) %>% 
    mutate(name =  gsub("\\.", "", as.character(paste0("s", LAT, "n", LONG, "e"))))
  ##Writing file names for different variables
  weather_sta_cli <- data.frame(name=station_names$name, 
                                wgn = df1$ID, 
                                pcp = paste0("sta_", tolower(df1$ID), ".pcp"),
                                tmp = paste0("sta_", tolower(df1$ID), ".tmp"),
                                slr = paste0("sta_", tolower(df1$ID), ".slr"),
                                hmd = paste0("sta_", tolower(df1$ID), ".hmd"),
                                wnd = paste0("sta_", tolower(df1$ID), ".wnd"),
                                wnd_dir = "null",
                                atmo_dep = "atmodep.cli")
  ##Spacing
  st_hd <- c('%-26s', '%6s', rep('%25s', 7))
  write.table(paste(sprintf(st_hd, names(weather_sta_cli)), collapse = ' '), paste0(write_path, "/", fname), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  ##Filing file with dataframe information
  df_to_txt(write_path, fname, weather_sta_cli, st_hd)
  print(paste0(fname, " file was successfully written."))
  
  ##Writing weather variable files
  p_lst <- list("pcp" = list("PCP", "Precipitation"), 
                "slr" = list("SLR", "Solar radiation"), 
                "hmd" = list("RELHUM", "Relative humidity"), 
                "tmp" = list(c("TMP_MAX", "TMP_MIN"), "Temperature"), 
                "wnd" = list("WNDSPD", "Wind speed"), 
                "wnd_dir" = list("WND_DIR", "Wind direction"), 
                "atmo_dep" = list("ATMO_DEP", "Atmospheric deposition"))
  ##Preparing general info for station
  df1_cli <- df1 %>% 
    mutate_at(vars(LAT, LONG, ELEVATION), ~format(round(as.numeric(.), 3), nsmall = 3)) %>% 
    rename(lat = LAT, lon = LONG, elev = ELEVATION, nbyr = RAIN_YRS) %>% 
    mutate(tstep = 0) %>% 
    select(ID, nbyr, tstep, lat, lon, elev) %>% 
    mutate(ID = tolower(ID))
  ##Loop to write for each variable
  for(cli in names(weather_sta_cli[c(3:7)])){
    ##Writing reference file (where all variable files are listed)
    fname <- paste0(cli, ".cli")
    text_l <-  paste0(fname,": ", p_lst[[cli]][[2]], " file names - file written by SWATprepR R package ", Sys.time())
    write.table(text_l, paste0(write_path, "/", fname), append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    write.table("filename", paste0(write_path, "/", fname), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    write.table(weather_sta_cli[cli], paste0(write_path, "/", fname), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    ##For each station
    for(id in df1_cli$ID){
      s2 <- subset(df1_cli[df1_cli$ID == id,], select = -ID)
      fname <- paste0("sta_", id, ".", cli)
      ##Heading line
      text_l <-  paste0(fname,": ", p_lst[[cli]][[2]], " data - file written by SWATprepR R package ", Sys.time())
      write.table(text_l, paste0(write_path, "/", fname), append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      ##Heading names
      st_hd <- c('%-6s', rep('%7s', 4))
      write.table(paste(sprintf(st_hd, names(s2)), collapse = ' '), paste0(write_path, "/", fname), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      ##Info for a station
      df_to_txt(write_path, fname, s2, st_hd)
      ##Identifying temperature as should be joined two variables
      if(cli == "tmp"){
        wdf <- meteo_lst[["data"]][[toupper(id)]][[p_lst[[cli]][[1]][[1]]]] %>% 
          full_join(meteo_lst[["data"]][[toupper(id)]][[p_lst[[cli]][[1]][[2]]]], by = "DATE")
        st_dt <- c('%-6s', rep('%7s', 3))
      } else {
        wdf <- meteo_lst[["data"]][[toupper(id)]][[p_lst[[cli]][[1]]]]
        st_dt <- c('%-6s', rep('%7s', 2))
      }
      ##Writing file for each variable and station
      wdf <- wdf %>%
        mutate(year = year(DATE), day = yday(DATE)) %>% 
        select(year, day, contains(p_lst[[cli]][[1]])) %>% 
        mutate_at(vars(contains(p_lst[[cli]][[1]])), ~format(round(as.numeric(.), 3), nsmall = 3)) 
      df_to_txt(write_path, fname, wdf, st_dt)
    }
    print(paste0(cli, " files were successfully written."))
  }
  
  ##Updating all required files
  ##Preparing GIS info for find nearest 
  wst_sf <- st_as_sf(station_names, coords = c("LONG", "LAT"), crs = 4326)
  ##Defining spacing of output files and updating each file
  spacing <- c('%8s', '%-12s', rep('%12s', 5), '%8s', '%16s', rep('%8s', 4))
  update_wst_txt('aquifer.con', write_path, wst_sf, spacing)
  update_wst_txt('hru.con', write_path, wst_sf, spacing)
  spacing <- c('%8s', '%-12s', rep('%12s', 5), '%8s', '%16s', rep('%8s', 4), '%12s', '%8s', rep('%12s', 2))
  update_wst_txt('reservoir.con', write_path, wst_sf, spacing)
  update_wst_txt('chandeg.con', write_path, wst_sf, spacing)
  spacing <- c('%8s', '%-12s', rep('%12s', 5), '%8s', '%16s', rep('%8s', 4), '%12s', '%8s', rep('%12s', 46))
  update_wst_txt('rout_unit.con', write_path, wst_sf, spacing)
  
  ##Creating temp folder to save all updated files before overwriting them into main directory
  f_dir <- paste(write_path, "temp", sep = '/')
  if(!dir.exists(f_dir)){
    dir.create(f_dir)
  } 
  ##Modifying time.sim to be same as weather data
  fname <- "time.sim"
  if(!file.exists(paste(write_path, fname, sep = "/"))){
    warning(paste("'time.sim' file was not found in", write_path, "and was not updated. 
                Please make sure 'yrc_start' is", year(period_starts), "and 'yrc_end' is", year(period_ends), "."))
  } else {
    f_write <- paste(f_dir, fname, sep = "/")
    file.create(f_write)
    time_sim <- read.delim(paste(write_path, fname, sep = "/"))
    write.table(paste0(fname, hd_txt), f_write, append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    st_hd <- c(rep('%9s', 5))
    write.table(paste(sprintf(st_hd, unlist(strsplit(time_sim[1,1], "\\s+"))), collapse = ' '), f_write, append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    time_sim_v <- as.numeric(unlist(strsplit(time_sim[2,1], "\\s+"))[-1])
    time_sim_v[c(2, 4)] <- c(year(period_starts), year(period_ends))
    write.table(paste(sprintf(st_hd, time_sim_v), collapse = ' '), f_write, append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    print(paste0(fname, " file was successfully written."))
  }
  ##Coping files from temp folder into main and deleting temp folder
  invisible(file.copy(paste(f_dir, list.files(f_dir), sep = "/"), write_path, overwrite = TRUE))
  unlink(paste(f_dir), recursive = TRUE)
  ##Done
  print(paste0("Climate data were successfully written in ", write_path))
}

# Preparing soils -----------------------------------------------

#' Prepare user soil table for SWAT model
#'
#' This function prepares a user soil table for the SWAT model based on the 
#' provided CSV file.
#'
#' @param csv_path Character path to the CSV file (e.g., "usersoil_lrew.csv"). 
#'   The file should be comma-separated with minimum columns of:
#'   - SNAM - name of soil type, 
#'   - NLAYERS - number of soil layers in the soil, and for each soil layer i (minimum 1).
#'   - SOL_Zi - depth from soil surface to bottom of layer in mm; 
#'   - CLAYi - clay content (particles <0.002 mm) in % soil weight;
#'   - SILTi - silt content (particles between 0.002 and 0.05 mm) in % soil weight; 
#'   - SANDi - sand content (particles between 0.05 and 2 mm) in % soil weight; 
#'   - SOL_CBNi - organic carbon content (% soil weight) for each available soil layer.
#' @param hsg (optional) Logical, TRUE - prepare soil hydrological groups, 
#' FALSE - no soil hydrological group preparation will be done. 
#' Default \code{ hsg = FALSE}. If \code{hsg = TRUE}, three additional columns 
#' should be in an input table: \cr 
#'  - 'Impervious' - depth to impervious layer (allowed values are "<50cm", 
#'  "50-100cm", ">100cm");
#'  - 'Depth' - depth to water table  (allowed values are "<60cm", "60-100cm", 
#'  ">100cm");
#'  - 'Drained' - whether soil is drained (allowed values are "Y" for drained 
#'  areas, "N" for areas without working tile drains). \cr
#'  More information can be found in the SWAT+ modeling protocol 
#' \href{https://doi.org/10.5281/zenodo.7463395}{Table 3.3}.
#' @param keep_values (optional) Logical or character vector, TRUE - keep old values (new 
#' values only will be left where 0 or NA values are present in an input table), 
#' FALSE - keep only new values. A character vector can also be used to keep only 
#' specified columns. For instance, c("HYDGRP", "ROCK1") would keep values of 
#' soil hydro groups and rock data for the first layer, while c("HYDGRP", "ROCK") 
#' would keep values in all rock data columns. Default \code{keep_values = FALSE}.
#' @param nb_lyr (optional) Integer, the number of layers resulting user soil 
#' data should contain. Default \code{nb_lyr = NA}, which stands for the 
#' same number as in the input data.
#' @importFrom dplyr select ends_with starts_with left_join
#' @importFrom readr parse_number
#' @importFrom utils type.convert
#' @importFrom methods is
#' @return A dataframe with a fully formatted and filled table of soil parameters 
#' for the SWAT model.
#' @export
#'
#' @examples
#' \dontrun{
#'   usersoils <- get_usersoil_table("table.csv")
#'   write.csv(usersoils, "usersoils.csv", row.names=FALSE, quote=FALSE)
#' }
#' @references 
#' SWAT+ Modeling Protocol: Soil Physical Data Chapter
#' 
#' This function utilizes the PTF functions and methods described in pages 82-91. 
#' For detailed information, refer to: \url{https://doi.org/10.5281/zenodo.7463395}
#' @keywords parameters
#' @seealso 
#' This function requires the euptf2 package. 
#' Please read more how to install, use and what is in it on \url{https://github.com/tkdweber/euptf2}.

get_usersoil_table <- function(csv_path, hsg = FALSE, keep_values = FALSE, nb_lyr = NA){
  ##Reading
  df_save <- df <- read.csv2(csv_path, sep = ",") %>% 
    type.convert(as.is = TRUE)
  is_OK <- TRUE
  c_names <- c("SOL_Z", "SOL_BD", "SOL_AWC", "SOL_K", "SOL_CBN", "CLAY", "SILT", "SAND", "ROCK", 
               "SOL_ALB", "USLE_K", "SOL_EC", "SOL_CAL", "SOL_PH")
  ##Checking all 
  if(dim(df)[2] < 7){
    stop("Your data were not read properly. If your use .csv file path, please ensure column separator is comma. 
       Minimum columns available should be 'SNAM', 'NLAYERS' and for each layer 'SOL_Z', 'CLAY', 'SILT', 'SAND', 'SOL_CBN' (for example 'SOL_Z1', 'CLAY1', etc.).")
  }
  if(!"SNAM" %in% names(df)){
    warning("'SNAM' column is missing!!! Please correct this.")
    is_OK <- FALSE
  } else if (any(nchar(df$SNAM)<1)){
    warning("'SNAM' values are missing!!! Please correct this.")
    is_OK <- FALSE
  }
  
  if(!"NLAYERS" %in% names(df)){
    warning("'NLAYERS' column is missing!!!")
    is_OK <- FALSE
  } else if (any(df$NLAYERS<1)|!any(is.numeric(df$NLAYERS))){
    warning("'SNAM' values are missing or input is not all numbers!!! Please correct this.")
    is_OK <- FALSE
  }
  
  if(!"SOL_Z1" %in% names(df)){
    warning("'SOL_Z1' column is missing!!!")
    is_OK <- FALSE
  } else if (any(df[["SOL_Z1"]]<0 | df[["SOL_Z1"]]>3000) | !any(is.numeric(df$SOL_Z1))){
    warning("At least some of 'SOL_Z1' values are missing or input values are not between 0 - 3000!!! Please correct this.")
    is_OK <- FALSE
  }
  
  for (p in c('CLAY1', 'SILT1', 'SAND1', 'SOL_CBN1')){
    if(!p %in% names(df)){
      warning(paste0(p, " column is missing!!!"))
      is_OK <- FALSE
    } else if (any(df[[p]]<0 | df[[p]]>100) | !any(is.numeric(df[[p]]))){
      warning(paste0("At least some of ", p," values are missing or input values are not between 0 - 100!!! Please correct this."))
      is_OK <- FALSE
    }
  }
  
  if(hsg){
    if(!"Impervious" %in% names(df)){
      warning("'Impervious' column is missing!!!")
      is_OK <- FALSE
    } else if (!unique(df[["Impervious"]]) %in% c("<50cm", "50-100cm", ">100cm")){
      warning("'Impervious' column can have only such values: '<50cm', '50-100cm', '>100cm'!!! Please correct this.")
      is_OK <- FALSE
    }
    if(!"Depth" %in% names(df)){
      warning("'Depth' column is missing!!!")
      is_OK <- FALSE
    } else if (!unique(df[["Depth"]]) %in% c("<60cm", "60-100cm", ">100cm")){
      warning("'Depth' column can have only such values: '<60cm', '60-100cm', '>100cm'!!! Please correct this.")
      is_OK <- FALSE
    }
    if(!"Drained" %in% names(df)){
      warning("'Drained' column is missing!!!")
      is_OK <- FALSE
    } else if (!unique(df[["Drained"]]) %in% c("N", "Y")){
      warning("'Drained' column only two entry options possible: 'Y' - for drained areas, 'N' - for areas without working tile drains!!! Please correct this.")
      is_OK <- FALSE
    }
  }
  
  if(!is_OK){
    stop("Please make sure all indicated problems are solved before using this function!!!")
  }
  
  ##Selecting only required layers
  df <- select(df, SNAM, NLAYERS, starts_with(c("SOL_Z", 'CLAY', 'SILT', 'SAND', 'SOL_CBN')), -starts_with("SOL_ZMX"))
  
  ##Getting rid of empty layers
  max_lyr_start <- max_lyr <- max(parse_number(names(df[c(3:dim(df)[2])])), na.rm = TRUE)
  while (all(select(df, ends_with(as.character(max_lyr)))==0)) {
    df <- select(df, -ends_with(as.character(max_lyr)))
    max_lyr <- max_lyr - 1
  }
  
  ##Calculating parameters
  soilp <- get_soil_parameters(df)
  
  ##Solving multiple layers problems 
  for(i in 1:max_lyr){
    for(ii in 1:nrow(soilp)){
      soilp[ii, paste0(c_names, i)] <- if(!soilp[ii,paste0("SOL_Z", i)] > 0) 0 else  as.vector(unlist(soilp[ii, paste0(c_names, i)]))
    }
  }
  
  ##Calculating soil hydro groups
  if(hsg){
    df_hsg <- left_join(df_save, soilp %>% select(SNAM, starts_with("SOL_K")), by = "SNAM")
    c <- c()
    for (i in 1:nrow(df_hsg)){
      c <- c(c, get_hsg(df_hsg[i,"Impervious"], 
                        df_hsg[i,"Depth"], 
                        df_hsg[i,"Drained"], 
                        df_hsg[i,c(paste0("SOL_Z", 1:df_hsg$NLAYERS[i]),paste0("SOL_K", 1:df_hsg$NLAYERS[i]))]))
    }
    soilp$HYDGRP <- c
    print("Soil hydrological groups were calculated.")
  }
  
  ##Taking care of number of layers
  if(!is.na(nb_lyr) && is(nb_lyr, "numeric") && nb_lyr%%1==0 && nb_lyr > max_lyr){
    print(paste0("Adding additional ", nb_lyr - max_lyr_start, " layer(s) added."))
    max_lyr_start <- nb_lyr
  }
  
  for(i in seq(max_lyr+1, max_lyr_start)){
    soilp[paste0(c("SOL_Z", "SOL_BD", "SOL_AWC", "SOL_K", "SOL_CBN", "CLAY", "SILT", "SAND", "ROCK", 
                   "SOL_ALB", "USLE_K", "SOL_EC", "SOL_CAL", "SOL_PH"), i)] <- 0
  }
  
  ##Overwriting results with existing values in input table
  if(is.character(keep_values) || keep_values){
    ##In case keep_values is vector of characters
    if(is.character(keep_values)){
      for(sn in keep_values){
        soilp[, grepl(sn , names(soilp))] <- df_save[, grepl(sn , names(df_save))]
        col_names <- names(df_save[grepl(sn , names(df_save))])
        print(paste0(paste(col_names, collapse = ", "), " column", if(length(col_names)==1) "" else "s",  
                     if(length(col_names)==1) " was " else " were ", "kept."))
      }
    } else {
      names(df_save) <- paste0(names(df_save), ".x")
      soil_par_names <- setdiff(names(soilp), "SNAM")
      soilp <- left_join(soilp, df_save, by = c("SNAM" = "SNAM.x"))
      for(sn in soil_par_names){
        if((paste0(sn, ".x") %in% names(df_save)) && !sn %in% c("MUID", "S5ID", "CMPPCT", "HYDGRP", "ANION_EXCL", "SOL_CRK", "TEXTURE")){
          soilp[,sn] <- ifelse(soilp[,paste0(sn, ".x")] > 0, soilp[,paste0(sn, ".x")], soilp[,sn])
        } else if(sn %in% c("MUID", "S5ID", "CMPPCT", "HYDGRP", "ANION_EXCL", "SOL_CRK", "TEXTURE") && (paste0(sn, ".x") %in% names(df_save))){
          soilp[,sn] <- ifelse(!is.na(soilp[,paste0(sn, ".x")]), soilp[,paste0(sn, ".x")], soilp[,sn])
        } 
      }
      soilp <- select(soilp, -ends_with(".x"))
      print("Values existing in the input table were kept.")
    }
  }
  
  return(soilp)
}

#' Function to fill soil parameters
#'
#' @param soilp dataframe with SNAM,	NLAYERS	columns and SOL_Z, CLAY, SILT, SAND,	OC columns for each available soil layer.
#' @importFrom euptf2 euptfFun 
#' @importFrom readxl read_excel
#' @importFrom dplyr rename_at vars
#' @importFrom tidyselect all_of
#' @importFrom Rdpack reprompt
#' @return dataframe with fully formatted and filled table of soil parameters for SWAT model.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' soil <- get_soil_parameters(temp_path)
#' }

get_soil_parameters <- function(soilp){
  if (!requireNamespace("euptf2", quietly = TRUE)) {
    stop("The euptf2 package must be installed to use this functionality. \n Please run devtools::install_github('tkdweber/euptf2')")
  }
  soilp$rownum <- 1:nrow(soilp)
  sol_z <- names(soilp)[grep("SOL_Z.*", names(soilp))]
  soilp["SOL_ZMX"] <- do.call(pmax, c(soilp[sol_z], list(na.rm=TRUE)))
  ##Loop to fill parameters for each layer
  for(i in seq_along(sol_z)){
    soilp[,paste0("BD", i)] <- ifelse(soilp[,paste0("SOL_CBN", i)] < 12, 
                                      1.72 - 0.294 * (soilp[,paste0("SOL_CBN", i)] ^ 0.5), 
                                      0.074 + 2.632 * exp(-0.076*( soilp[,paste0("SOL_CBN", i)])))
    soilp[,paste0("SOL_BD", i)] <- ifelse(soilp[,paste0("SOL_CBN", i)] > 0.58,  soilp[,paste0("BD", i)] + 0.009 * soilp[,paste0("CLAY", i)], 
                                          soilp[,paste0("BD", i)] + 0.005 * soilp[,paste0("CLAY", i)]+ 0.001 * soilp[,paste0("SILT", i)])
    if(i == 1){
      soilp[paste0("DEPTH_M", i)] <- soilp[paste0("SOL_Z", i)] * 0.05
    } else {
      soilp[paste0("DEPTH_M", i)] <- ((soilp[paste0("SOL_Z", i)] - soilp[paste0("SOL_Z", i - 1)])/2 + 
                                        soilp[paste0("SOL_Z", i - 1)])/10
    }
    input <- soilp[c("rownum", paste0("DEPTH_M", i), paste0("BD", i), paste0("SOL_CBN", i), paste0("CLAY", i),
                     paste0("SILT", i), paste0("SAND", i))]
    names(input)[1:7] <- c("rownum","DEPTH_M","BD", "OC", "USCLAY", "USSILT", "USSAND")
    d <- 0
    if (sum(input$DEPTH_M > 0, na.rm=TRUE) == 1){
      d <- 1
      input[nrow(input)+1,] <- list(nrow(input)+1, 200, 1, 0.01, 1, 1, 98)
    }
    tryCatch({
      pred_VG1 <- euptf2::euptfFun(ptf = "PTF07", predictor = input, target = "VG", query = "predictions")
    },
    error = function(e) {
      stop("euptfFun function from euptf2 package couldn't be used. Please make sure euptf2 package is correctly installed and could be loaded.")
    })
    names(pred_VG1)[2:6] <- c("THS","THR", "ALP", "N", "M")
    input <- input[c(1:nrow(input)-d),]
    pred_VG <- merge(pred_VG1[c(1:nrow(pred_VG1)-d), c(1:6,8,10:14)], input[,c(1,2)], by="rownum", all.y=TRUE)
    # correct VG parameters for organic soils if organic soils are present in the data
    pred_VG$THR <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M <= 30), 0.111, pred_VG$THR)
    pred_VG$THS <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M <= 30), 0.697, pred_VG$THS)
    pred_VG$ALP <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M <= 30), 0.0069, pred_VG$ALP)
    pred_VG$N <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M <= 30), 1.4688, pred_VG$N)
    
    pred_VG$THR <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M > 30), 0.000, pred_VG$THR)
    pred_VG$THS <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M > 30), 0.835, pred_VG$THS)
    pred_VG$ALP <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M > 30), 0.0113, pred_VG$ALP)
    pred_VG$N <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M > 30), 1.2256, pred_VG$N)

    FC <- pred_VG$THR+(pred_VG$THS-pred_VG$THR)*((1+(((pred_VG$N-1)/pred_VG$N)^(1-2*pred_VG$N)))^((1-pred_VG$N)/pred_VG$N))
    WP <- pred_VG$THR+((pred_VG$THS-pred_VG$THR)/((1+pred_VG$ALP*(15000^pred_VG$N))^(1-(1/pred_VG$N))))

    soilp[paste0("SOL_AWC", i)] <- ifelse(FC-WP < 0, 0.001, FC-WP)
    soilp[paste0("SOL_K", i)] <- (4.65 * (10^4) * pred_VG$THS * (pred_VG$ALP^2))*0.41666001

    # compute albedo
    # method of Gascoin et al. (2009) from Table 6 of Abbaspour, K.C., AshrafVaghefi, S., Yang, H. & Srinivasan, R. 2019. Global soil, landuse, evapotranspiration, historical and future weather databases for SWAT Applications. Scientific Data, 6:263.
    soilp[paste0("SOL_ALB", i)] <- 0.15+0.31*exp(-12.7*FC)

    # compute USLE erodibility K factor
    # method published in Sharpley and Williams (1990) based on Table 5 of Abbaspour, K.C., AshrafVaghefi, S., Yang, H. & Srinivasan, R. 2019. Global soil, landuse, evapotranspiration, historical and future weather databases for SWAT Applications. Scientific Data, 6:263.
    ES <- 0.2+0.3*exp(-0.0256*input$USSAND*(1-(input$USSILT/100)))
    ECT <- (input$USSILT/(input$USCLAY+input$USSILT))^0.3
    EOC <- 1-(0.25*input$OC/(input$OC+exp(3.72-2.95*input$OC)))
    EHS <- 1-(0.7*(1-input$USSAND/100)/((1-input$USSAND/100)+exp(-5.51+22.9*(1-input$USSAND/100))))
    soilp[paste0("USLE_K", i)] <-  ES*ECT*EOC*EHS
    soilp[paste0("ROCK", i)] <- 0
    soilp[paste0("SOL_EC", i)] <- 0
    soilp[paste0("SOL_CAL", i)] <- 0
    soilp[paste0("SOL_PH", i)] <- 0
  }
  ##Formating table
  soilpf <- data.frame(OBJECTID = soilp$rownum, MUID = "", SEQN = 1, SNAM = soilp$SNAM, S5ID = "", CMPPCT = 1, NLAYERS = soilp$NLAYERS, 
                       HYDGRP = "", SOL_ZMX = soilp$SOL_ZMX, ANION_EXCL = 0.5, SOL_CRK = 0.5, TEXTURE = "")
  
  for(i in seq_along(sol_z)){
    sel_cols <- paste0(c("SOL_Z", "SOL_BD", "SOL_AWC", "SOL_K", "SOL_CBN", "CLAY", "SILT", "SAND", "ROCK", 
                         "SOL_ALB", "USLE_K", "SOL_EC", "SOL_CAL", "SOL_PH"), i)
    soilpf[sel_cols]  <- soilp[sel_cols] 
  }
  return(soilpf)
}

#' Function to get soil hydrologic groups
#'
#' @param d_imp character for depth to impervious layer. Only three entry options possible: "<50cm", "50-100cm", ">100cm".
#' @param d_wtr character for water table high. Only three entry options possible: "<60cm", "60-100cm", ">100cm".
#' @param drn character for tile drainage status. Only two entry options possible: "Y" for drained areas, "N" for areas without working tile drains.
#' @param t one row dataframe with all SOL_Z and SOL_K values for soil type.
#' @return character of soil hydrologic group A, B, C or D
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df <- data.frame(SOL_K1 = 10, SOL_K2 = 1, SOL_K3 = 2, 
#'                  SOL_Z1 = 250, SOL_Z2 = 700, SOL_Z3 = 1000)
#' get_hsg(d_imp = ">100cm", d_wtr = "<60cm", drn = "Y", df)
#' }

get_hsg <- function(d_imp, d_wtr, drn, t){
  r <- NULL
  ##Case Depth to water impermeable layer <50cm
  if (d_imp == "<50cm"){
    r <- "D"
    ##Case Depth to water impermeable layer 50-100cm
  } else if (d_imp == "50-100cm"){
    ##Water table <60cm
    if (d_wtr == "<60cm"){
      if (min_ks(t, 600) > 40){
        if (drn == "Y"){
          r <- "A"
        } else if (drn == "N"){
          r <- "D"
        }
      } else if (min_ks(t, 600) > 10 & min_ks(t, 600) <= 40){
        if (drn == "Y"){
          r <- "B"
        } else if (drn == "N"){
          r <- "D"
        }
      } else if (min_ks(t, 600) > 1 & min_ks(t, 600) <= 10){
        if (drn == "Y"){
          r <- "C"
        } else if (drn == "N"){
          r <- "D"
        }
      } else if (min_ks(t, 600) <= 1){
        r <- "D"
      }
      ##Water table 60-100cm
    } else {
      if (min_ks(t, 500) > 40){
        r <- "A"
      } else if (min_ks(t, 500) > 10 & min_ks(t, 500) <= 40){
        r <- "B"
      } else if (min_ks(t, 500) > 1 & min_ks(t, 500) <= 10){
        r <- "C"
      } else if (min_ks(t, 500) <= 1){
        r <- "D"
      }
    }
    ##Case Depth to water impermeable layer >100cm
  } else if (d_imp == ">100cm"){
    ##Water table <60cm
    if (d_wtr == "<60cm"){
      if (min_ks(t, 1000) > 10){
        if (drn == "Y"){
          r <- "A"
        } else if (drn == "N"){
          r <- "D"
        }
      } else if (min_ks(t, 1000) > 4 & min_ks(t, 1000) <= 10){
        if (drn == "Y"){
          r <- "B"
        } else if (drn == "N"){
          r <- "D"
        }
      } else if (min_ks(t, 1000) > .4 & min_ks(t, 1000) <= 4){
        if (drn == "Y"){
          r <- "C"
        } else if (drn == "N"){
          r <- "D"
        }
      } else if (min_ks(t, 1000) <= .4){
        r <- "D"
      }
      #Water table 60-100cm
    } else if (d_wtr == "60-100cm"){
      if (min_ks(t, 500) > 40){
        r <- "A"
      } else if (min_ks(t, 500) > 10 & min_ks(t, 500) <= 40){
        r <- "B"
      } else if (min_ks(t, 500) > 1 & min_ks(t, 500) <= 10){
        r <- "C"
      } else if (min_ks(t, 500) <= 1){
        r <- "D"
      }
      #Water table >100cm
    } else if (d_wtr == ">100cm"){
      if (min_ks(t, 1000) > 10){
        r <- "A"
      } else if (min_ks(t, 1000) > 4 & min_ks(t, 1000) <= 10){
        r <- "B"
      } else if (min_ks(t, 1000) > .4 & min_ks(t, 1000) <= 4){
        r <- "C"
      } else if (min_ks(t, 1000) <= .4){
        r <- "D"
      }
    }
  }
  return(r)
}

#' Convert 'usersoil.csv' to 'soils.sol'
#' 
#' This function converts a user-defined soil CSV file to the soils.sol file 
#' required for SWAT+ model input.
#'
#' @param csv_path Character, path to CSV file containing user-defined soil 
#' information (example "usersoil_lrew.csv"). 
#' The CSV file should have the following columns (* indicates not required, yet 
#' column should be present in the CSV file):
#'   - OBJECTID*: Identifier for each record.
#'   - MUID*: Unique identifier.
#'   - SEQN*: Sequence number.
#'   - SNAM: Soil name.
#'   - S5ID*: Soil ID.
#'   - CMPPCT*: Component percentage.
#'   - NLAYERS: Number of layers.
#'   - HYDGRP: Hydrologic group.
#'   - SOL_ZMX: Maximum soil depth.
#'   - ANION_EXCL: Anion exclusion.
#'   - SOL_CRK: Soil cracking.
#'   - TEXTURE: Soil texture. \cr\cr
#' **For each available layer (up to 10 layers)**:
#'   - SOL_Z1 - SOL_Z10: Depth of each layer. Units: mm;
#'   - SOL_BD1 - SOL_BD10: Bulk density of each layer. Units: Mg/m3 or g/cm3;
#'   - SOL_AWC1 - SOL_AWC10: Available water capacity of each layer. Units: mm H2O/mm soil;
#'   - SOL_K1 - SOL_K10: Saturated hydraulic conductivity of each layer. Units: mm/hr;
#'   - SOL_CBN1 - SOL_CBN10: Carbon content of each layer. Units: % soil weight;
#'   - CLAY1 - CLAY10: Clay  (particles <0.002 mm) content of each layer. 
#'   Units: % soil weight;
#'   - SILT1 - SILT10: Silt (particles between 0.002 and 0.05 mm) 
#'   content of each layer. Units: % soil weight;
#'   - SAND1 - SAND10: Sand (particles between 0.05 and 2 mm) 
#'   content of each layer. Units: % soil weight;
#'   - ROCK1 - ROCK10: Rock (particles >2 mm) content of each layer. Units: % total weight;
#'   - SOL_ALB1 - SOL_ALB10: Soil albedo of each layer. Units: ratio (values 0-1);
#'   - USLE_K1 - USLE_K10: USLE K factor of each layer. Units: 0.013 (metric ton m2 hr)/(m3-metric ton cm);
#'   - SOL_EC1 - SOL_EC10: Soil electrical conductivity of each layer. Units: dS/m;
#'   - SOL_CAL1 - SOL_CAL10: Soil CaCO3 content of each layer. Units: % (values 0 - 50%);
#'   - SOL_PH1 - SOL_PH10: Soil pH of each layer. Units: pH (values 3-10). \cr\cr
#'     Soil properties data can be prepared using \code{\link{get_usersoil_table}} 
#'     function and saved into the CSV file using 
#'     \code{write.csv(usertable, ".my_file.csv", row.names=FALSE, quote=FALSE)}.
#' @param db_path (optional) Character path to SQLite project database (example 
#' "output/project.sqlite"). Default \code{db_path = NULL}, which means SWAT+ model setup
#' .sqlite database will not be used to reduce the size of the soils.sol file by 
#' leveraging information from an SQLite database if there are fewer soil types 
#' in the database compared to the user's soil CSV file.
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom dplyr mutate_all %>% 
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom stringr str_extract
#' @importFrom purrr map2_chr
#' @importFrom readr write_lines parse_number
#' @importFrom utils read.csv2
#' @return Soils.sol file for SWAT+ model input.
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert user-defined soil CSV to soils.sol
#' usersoil_to_sol("output/usersoil_lrew.csv")
#'
#' # Convert using an SQLite project database to reduce the size of the soils.sol 
#' file
#' usersoil_to_sol("output/usersoil_lrew.csv", "output/project.sqlite")
#' }
#'
#' @seealso \code{\link{get_usersoil_table}}
#' For details on 'soil.sol' file read [SWAT+ Soils Input](https://swatplus.gitbook.io/io-docs/introduction/soils/soils.sol)
#' @keywords writing

usersoil_to_sol <- function(csv_path, db_path = NULL){
  ##Reading usersoil table
  df <- read.csv2(csv_path, sep=",")
  ##Reading db, in case if not successful, just continue without it
  if(!is.null(db_path)){
    tryCatch({
      db <- dbConnect(RSQLite::SQLite(), db_path)
      soils_sol <- dbReadTable(db, 'soils_sol')
      dbDisconnect(db)
      df <- df[df$SNAM %in% soils_sol$name,]
    },
    error = function(e) {
      warning("Your database could not be read!!! Please check if path is correct or soil_sol table exists.")
    })
  }
  ##Settings to function
  path <- sub("[^/]+$", "", csv_path)
  c_names <- c("SOL_Z", "SOL_BD", "SOL_AWC", "SOL_K", "SOL_CBN", "CLAY", "SILT", "SAND", "ROCK", 
               "SOL_ALB", "USLE_K", "SOL_EC", "SOL_CAL", "SOL_PH")
  c_write_names <- c("name", "nly", "hyd_grp", "dp_tot", "anion_excl", "perc_crk", "texture", "dp", "bd", "awc", 
                     "soil_k", "carbon", "clay", "silt", "sand", "rock", "alb", "usle_k", "ec", "caco3", "ph")
  ##Spacing in output file 
  sol_nam <- c('%-34s', rep('%15s', 6), '%25s', rep('%15s', 13))
  sol_val1 <- c('%-34s', rep('%15s', 6))
  sol_val2 <- c('%156s', rep('%15s', 13))
  nfile <- paste0(path, 'soils.sol')
  ##Getting max number of layers available
  max_lyr <- max(as.numeric(str_extract(names(df)[c(13:dim(df)[2])], "[[:digit:]]+")))
  ##Getting rid of empty layers
  while (all(df[paste0(c_names, max_lyr)] == 0)) {
    df <- df[,!names(df) %in% paste0(c_names, max_lyr)]
    max_lyr <- max_lyr - 1
  }
  ##Converting to characters to numeric and fixing decimal places
  df[c(9:11,13:dim(df)[2])] <- mutate_all(mutate_all(df[c(9:11,13:dim(df)[2])], 
                                                     function(x) as.numeric(as.character(x))), ~sprintf(., fmt = '%#.5f'))
  ##First line to be printed into file
  text_l <- paste0("soils.sol: written by SWATprepR R package ", Sys.time(), " for SWAT+ rev.60.5.4")
  ##Heading to be printed into file
  sol_names <- c_write_names %>%
    map2_chr(., sol_nam, ~sprintf(.y, .x)) %>%
    paste(., collapse = ' ')
  print("Writing soils.sol started.")
  write_lines(c(text_l, sol_names), nfile, append = FALSE)
  ##df separation to df1 common parameters per profile
  df1 <- df[c("SNAM", "NLAYERS", "HYDGRP", "SOL_ZMX", "ANION_EXCL", "SOL_CRK", "TEXTURE")] %>% 
    mutate(TEXTURE = ifelse(is.na(TEXTURE), "null", TEXTURE))
  ##df2 parameters different in each layer
  df2 <- df[c(4,13:dim(df)[2])] %>% 
    pivot_longer(c(-SNAM), names_to = "param", values_to = "values") %>% 
    mutate(n_lyr = parse_number(param),
           param = gsub('[[:digit:]]+', '', param)) %>% 
    pivot_wider(names_from = param, values_from = values)
  ##Writing loop into file for each soil type 
  for (i in seq_len(nrow(df1))){
    ##Filtering
    s1 <- df1[i,]
    s2 <- df2[df2$SNAM == s1$SNAM, c_names]
    ##Parameters for profile
    df_to_txt(path, 'soils.sol',  s1, sol_val1)
    ##Parameters for layer
    df_to_txt(path, 'soils.sol',  s2, sol_val2)
  }
  print("File conversion from usersoil csv to soils.sol finished successfully.")
  print(paste0("Prepared result in ", nfile))
  print("Please copy file to your setup folder.")
}

# Updating SWAT+ SQLite database -----------------------------------------------

#' Update SWAT+ SQLite database with weather data
#'
#' This function updates an SWAT+ SQLite database with weather data, including 
#' meteorological and weather generator data.
#'
#' @param db_path A character string representing the path to the SWAT+ SQLite database 
#' (e.g., "./output/project.sqlite").
#' @param meteo_lst A nested list of lists with dataframes. 
#'   Nested structure: \code{meteo_lst -> data -> Station ID -> Parameter -> 
#'   Dataframe (DATE, PARAMETER)}, 
#'   \code{meteo_lst -> stations -> Dataframe (ID, Name, Elevation, Source, 
#'   geometry, Long, Lat)}. \cr\cr
#'   meteo_lst can be created using \code{\link{load_template}} function using 
#'   'xlsx' template file or it could to be created with \code{\link{load_swat_weather}}
#'   function loading information from SWAT+ model setup weather files.
#' @param wgn_lst A list of two dataframes: wgn_st - weather generator station 
#' data, wgn_data - weather generator data (prepared by function).
#' @param fill_missing (optional) Boolean, TRUE - fill data for missing stations with data 
#' from closest stations with available data. FALSE - leave stations without 
#' data. Weather generator will be used to fill missing variables for a model.
#' Default \code{fill_missing = TRUE}.
#' @importFrom sf st_transform st_coordinates st_drop_geometry
#' @importFrom dplyr select full_join mutate %>% rename
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom lubridate yday interval years
#' @importFrom RSQLite SQLite
#' @importFrom utils write.table
#' @return Updated SWAT+ SQLite database with weather data.
#'
#' @examples
#' \dontrun{
#'   # Getting meteorological data from template
#'   met_lst <- load_template(temp_path, 3035)
#'   
#'   # Calculating weather generator parameters
#'   wgn <- prepare_wgn(met_lst)
#'   
#'   # Writing weather input into the model database
#'   db_path <- "./output/test/project.sqlite"
#'   add_weather(db_path, met_lst, wgn)
#' }
#'
#' @export
#' @seealso \code{\link{load_template}}, \code{\link{prepare_wgn}}, 
#' \code{\link{prepare_climate}}
#' @keywords writing

add_weather <- function(db_path, meteo_lst, wgn_lst, fill_missing = TRUE){
  ##Path to the folder to write weather files (same as sql db)
  write_path <- sub("[^/]+$", "", db_path)
  ##Dictionary for parameters
  p_lst <- list("PCP" = c("pcp", "Precipitation"), 
                "SLR" = c("slr", "Solar radiation"), 
                "RELHUM" = c("hmd", "Relative humidity"), 
                "TMP_MAX" = c("tmp", "Temperature"), 
                "TMP_MIN" =  c("tmp", "Temperature"), 
                "WNDSPD" = c("wnd", "Wind speed"), 
                "WND_DIR" = c("wnd_dir", "Wind direction"), 
                "ATMO_DEP" = c("atmo_dep", "Atmospheric deposition"))
  ##Filling data missing at stations with closest station data
  if (fill_missing){
    print("Closest stations are used to fill missing variables.")
    meteo_lst$data <- fill_with_closest(meteo_lst)
  }
  ##Converting station coordinates (if not correct already) 
  st <-  meteo_lst[["stations"]]
  if (!grepl("4326", st_crs(st)$input)){
    st <- st_transform(st, 4326)
    st$Long = st_coordinates(st)[,1]
    st$Lat = st_coordinates(st)[,2]
    print("Coordinate system checked and transformed to EPSG:4326.")
  }
  st <- st_drop_geometry(st)
  ##Initiating tables to be filled in loop
  weather_file <- data.frame(id=integer(), filename=character(), type=character(), lat=numeric(), lon=numeric())
  weather_sta_cli <- data.frame(id=integer(), name=character(), wgn_id=integer(), 
                                pcp=character(), tmp=character(), slr = character(), hmd = character(),
                                wnd = character(), wnd_dir = character(), atmo_dep = character(),
                                lat=numeric(), lon=numeric())
  ##Setting up counters for ids
  id <- 1
  id_st <- 1
  ##Main loop to write weather files and fill 'weather_file' and 'weather_sta_cli' tables
  for (n in sort(names(meteo_lst[["data"]]))){
    ##Initial information to weather file 2 and 3 lines
    df1 <- data.frame(nbyr = 0, 
                      tstep = 0, 
                      lat = round(as.numeric(st[st$ID == n,"Lat"]), 3), 
                      lon = round(as.numeric(st[st$ID == n,"Long"]), 3), 
                      elev = round(as.numeric(st[st$ID == n,"Elevation"]), 3))
    pars <- names(meteo_lst[["data"]][[n]])
    pars <- pars[pars %in% names(p_lst)[c(1:6)]]
    ##Initial information to 'weather_sta_cli'
    weather_sta_cli[id_st, c("id", "name", "wgn_id", "lat", "lon")] <- 
      list(as.integer(id_st), paste0("s", gsub("\\.", "", as.character(df1$lat)), "n", gsub("\\.", "", as.character(df1$lon)), "e"),
           wgn_lst$wgn_st$ID[wgn_lst$wgn_st$NAME == st$Name[st$ID == n]],
           df1$lat, df1$lon)
    ##Writing and filling data for temperature
    if(all(c("TMP_MAX", "TMP_MIN") %in% pars)){
      df <- meteo_lst[["data"]][[n]][["TMP_MAX"]] %>% 
        full_join(meteo_lst[["data"]][[n]][["TMP_MIN"]], by = "DATE") %>% 
        mutate(year = year(DATE), day = yday(DATE)) %>% 
        select(year, day, TMP_MAX, TMP_MIN, DATE)
      df1$nbyr <- ceiling(interval(df[[1,"DATE"]], df[[nrow(df),"DATE"]]) / years(1))
      file_n <- paste0("sta_", tolower(n), ".", p_lst[["TMP_MAX"]][[1]])
      ##Head line in file
      text_l <- paste0(file_n, ": ", p_lst[["TMP_MAX"]][[2]], " data - file written by SWATprepR R package ", Sys.time())
      ##Writing temperature file per station
      write.table(text_l, paste0(write_path, file_n), append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      suppressWarnings(write.table(df1, paste0(write_path, file_n), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE, quote = FALSE))
      write.table(df[c(1:4)], paste0(write_path, file_n), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      ##Writing .cli files
      if(!file.exists(paste0(write_path, p_lst[["TMP_MAX"]][[1]], ".cli"))){
        text_l <- paste0(p_lst[["TMP_MAX"]][[1]], ".cli", ": ", p_lst[["TMP_MAX"]][[2]], " file names - file written by SWATprepR R package ", Sys.time())
        write.table(text_l, paste0(write_path, p_lst[["TMP_MAX"]][[1]], ".cli"), append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
        write.table("filename", paste0(write_path, p_lst[["TMP_MAX"]][[1]], ".cli"), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      }
      write.table(file_n, paste0(write_path, p_lst[["TMP_MAX"]][[1]], ".cli"), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      ##Taking out temperature from parameter
      pars <- pars[which(!pars %in% c("TMP_MAX", "TMP_MIN"))]
      weather_file[id,] <- list(id, file_n, p_lst[["TMP_MAX"]][[1]], as.numeric(df1$lat), as.numeric(df1$lon))
      weather_sta_cli[id_st, "tmp"] <- file_n
      id <- id + 1
    } 
    ##Looping for all other parameters (except of temperature)
    for(p in pars){
      df <- meteo_lst[["data"]][[n]][[p]] %>% 
        mutate(year = year(DATE), day = yday(DATE)) 
      df1$nbyr <- ceiling(interval(df[[1,"DATE"]], df[[nrow(df),"DATE"]]) / years(1))
      file_n <- paste0("sta_", tolower(n), ".", p_lst[[p]][[1]])
      text_l <- paste0(file_n, ": ", p_lst[[p]][[2]], " data - file written by SWATprepR R package ", Sys.time())
      ##Writing file per parameter for each station
      write.table(text_l, paste0(write_path, file_n), append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      suppressWarnings(write.table(df1, paste0(write_path, file_n), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE, quote = FALSE))
      write.table(df[c("year", "day", p)], paste0(write_path, file_n), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      ##Writing .cli files
      if(!file.exists(paste0(write_path, p_lst[[p]][[1]], ".cli"))){
        text_l <- paste0(p_lst[[p]][[1]], ".cli", ": ", p_lst[[p]][[2]], " file names - file written by SWATprepR R package ", Sys.time())
        write.table(text_l, paste0(write_path, p_lst[[p]][[1]], ".cli"), append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
        write.table("filename", paste0(write_path, p_lst[[p]][[1]], ".cli"), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      }
      write.table(file_n, paste0(write_path, p_lst[[p]][[1]], ".cli"), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      ##Filling sqlite tables
      weather_file[nrow(weather_file)+1,] <- c(id, file_n, p_lst[[p]][[1]], df1$lat, df1$lon)
      weather_sta_cli[id_st, p_lst[[p]][[1]]] <- file_n
      id <- id + 1
    }
    print(paste0("Finished writing data for station ", n))
    id_st <- id_st + 1
  }
  ##Preparing weather generator tables:'weather_wgn_cli', 'weather_wgn_cli_mon'
  weather_wgn_cli <- wgn_lst$wgn_st %>% 
    rename(id = ID, name = NAME, lat = LAT, lon = LONG, elev = ELEVATION, rain_yrs = RAIN_YRS) %>% 
    mutate(lat = round(as.numeric(lat), 2),
           lon = round(as.numeric(lon), 2),
           elev = round(as.numeric(elev), 1),
           rain_yrs = as.integer(rain_yrs))
  weather_wgn_cli_mon <- wgn_lst$wgn_data %>% 
    rename(weather_wgn_cli_id = wgn_id)
  ##Database part
  ##Opening database to write 
  db <- dbConnect(RSQLite::SQLite(), db_path)
  ##Appending by prepared table (db tables should be empty)
  tryCatch({
    dbWriteTable(db, 'weather_file', weather_file, append = TRUE)
  },
  error = function(e) {
    stop("Your database could not be updated. This is probably due to that it already have or had weather data written in before. Please use .sqlite database in which weather data were not written before!!!")
  })
  dbWriteTable(db, 'weather_sta_cli', weather_sta_cli, append = TRUE)
  dbWriteTable(db, 'weather_wgn_cli', weather_wgn_cli, append = TRUE)
  dbWriteTable(db, 'weather_wgn_cli_mon', weather_wgn_cli_mon, append = TRUE)
  dbDisconnect(db)
  ##Updating tables with nearest weather station id (wst_id)
  ##Reading tables to update
  for (t in c("hru_con", "rout_unit_con", "chandeg_con", "aquifer_con", "reservoir_con")){
    update_wst_id(t, db_path, weather_sta_cli)
  }
  return(print(paste("Weather data was successfuly added to", gsub(".*/","",db_path))))
}

#' Update SWAT+ SQLite database with atmospheric deposition data
#'
#' This function updates an SWAT+ SQLite database with atmospheric deposition data.
#'
#' @param df A data frame containing columns "DATE," "NH4_RF," "NO3_RF," 
#' "NH4_DRY," and "NO3_DRY" obtained from the \code{\link{get_atmo_dep}} function.
#' @param db_path A character string representing the path to the SQLite 
#' database (e.g., "./output/project.sqlite").
#' @param t_ext (optional) A string indicating the type of time aggregation: 'year' for 
#' yearly averages, 'month' for monthly averages, and 'annual' for the average 
#' of the entire period. Default default \code{t_ext = "year"}. 
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbReadTable
#' @importFrom RSQLite SQLite
#' @importFrom utils write.table
#' @return Writes data to 'atmodep.cli' file and updates the SWAT+ SQLite database 
#' with codes and connections to atmospheric deposition data.
#' @export 
#' @examples
#' \dontrun{
#'   basin_path <- system.file("extdata", "GIS/basin.shp", package = "SWATprepR")
#'   df <- get_atmo_dep(basin_path)
#'   db_path <- "./output/test/project.sqlite"
#'   add_atmo_dep(df, db_path)
#' }
#' @seealso \code{\link{get_atmo_dep}}
#' @keywords writing

add_atmo_dep <- function(df, db_path, t_ext = "year"){
  ##Path to the folder to write weather files (same as sql db)
  write_path <- sub("[^/]+$", "", db_path)
  f_name <- "atmodep.cli"
  ##Rounding input data
  df <- mutate_if(df, is.numeric, ~round(.,3))
  d <- as.data.frame(t(df))
  ##Setting file name and initial parameters to write
  f_path <- paste0(write_path, f_name)
  mo_init <- 0 
  yr_init <- as.numeric(substr(d[1,1], 1, 4))
  num_aa <- dim(d)[2]
  ##Cases depending on time step
  if(t_ext == "year"){
    ts <- "yr"
    atmo_dep <- 'y'
    d <- d[-1,]
  } else if(t_ext == "month"){
    ts <- "mo"
    atmo_dep <- 'm'
    mo_init <- as.numeric(substr(d[1,1], 6, 7))
    d <- d[-1,]
  } else if(t_ext == "annual"){
    ts <- "aa"
    atmo_dep <- 'a'
    yr_init <- 0
    d <- mutate_if(as.data.frame(colMeans(df[,-1])), is.numeric, ~round(.,3))
    num_aa <- 0
  } else {
    stop("Wrong input t_ext should be 'year', 'month' or 'annual'")
  }
  ##Combining all parameters in a dataframe
  df <- data.frame(NUM_STA = 1, TIMESTEP = ts, MO_INIT = mo_init, 
                   YR_INIT = yr_init, NUM_AA = num_aa)
  ##Adding parameter names in the end
  d$par <- rownames(d)
  ##Writing file
  write.table(paste0("'atmodep.cli' file was written by SWATprepR R package ", Sys.time()), f_path, append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  suppressWarnings(write.table(df, f_path, append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE, quote = FALSE))
  write.table(f_name, f_path, append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.table(d, f_path, append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  ##Info from writing
  print(paste("Atmospheric deposition data were written into ", f_path))
  
  ##Adding info to database tables
  db <- dbConnect(RSQLite::SQLite(), db_path)
  weather_sta_cli <- dbReadTable(db, 'weather_sta_cli')
  codes_bsn <- dbReadTable(db, 'codes_bsn')
  weather_sta_cli$atmo_dep <- f_name
  codes_bsn$atmo_dep <- atmo_dep
  dbWriteTable(db, 'weather_sta_cli', weather_sta_cli, overwrite = TRUE)
  dbWriteTable(db, 'codes_bsn', codes_bsn, overwrite = TRUE)
  ##Table for atmo_cli 
  df <- data.frame(id = 1, filename = f_name, timestep = ts, mo_init = mo_init, 
                   yr_init = yr_init, num_aa = num_aa)
  dbWriteTable(db, 'atmo_cli', df, overwrite = TRUE)
  dbDisconnect(db)
  return(print(paste("Atmospheric deposition data was successfuly added to", gsub(".*/","",db_path))))
}

# Land use and management -----------------------------------------------

#' Prepare training points for remote sensing algorithm
#'
#' This function prepares training points for a remote sensing algorithm based on
#' land use and crop classes.
#'
#' @param df An sf data.frame with land use. The "type" column should be present.
#' @param year A numeric value representing the year of land use.
#' @param lookup A dataframe with a "lc1" column for numeric codes and a "type" 
#'   column for text.
#' @param lu_constant (optional) A vector of strings with land uses to be kept constant in 
#'   land use (e.g., water, urban areas, etc.). Default \code{lu_constant = c()}.
#' @param nb_pts (optional) A numeric value representing the number of points per land use/crop 
#'   class. Default \code{nb_pts = 100}.
#' @param col_name A string with the name of the column to be used representing 
#'   the type of crops/land use. Default \code{col_name = "type"}.
#' @importFrom sf st_as_sf st_join st_transform st_sample
#' @importFrom dplyr rename mutate left_join select filter sample_n ungroup
#' @return An sf data.frame with point input for a remote sensing training algorithm.
#' @export
#' @examples
#' \dontrun{
#'   library(sf)
#'   
#'   # Loading land use/crop layer
#'   lu_path <- system.file("extdata", "GIS/lu_layer.shp", package = "SWATprepR")
#'   lu <- st_read(lu_path, quiet = TRUE)
#'   
#'   # Preparing lookup table
#'   lookup <- data.frame(lc1 = seq(1:length(unique(c(lu$type)))), 
#'                        type = unique(c(lu$type)))
#'                        
#'   # Setting land uses to be kept constant
#'   lu_constant <- c("fesc", "orch", "frst", "frse", "frsd", "urld", "urhd", 
#'                    "wetl", "past", "watr", "agrl")
#'   
#'   # Getting training points
#'   pts <- get_lu_points(lu, 2021, lookup, lu_constant)
#' }
#' @references
#' Mészáros, J., & Szabó, B. (2022). Script to derive and apply crop 
#' classification based on Sentinel 1 satellite radar images in Google Earth 
#' Engine platform. \url{https://doi.org/10.5281/zenodo.6700122}
#' @keywords remote-sensing

get_lu_points <- function(df, year, lookup, lu_constant = c(),  nb_pts = 100, col_name = "type"){
  df <- df[col_name] %>%
    rename(type = 1) %>%
    mutate(year = year) %>%
    left_join(lookup, by = "type") %>%
    select(lc1, year) %>% 
    filter(lc1 %in% c(lookup[!lookup$type %in% lu_constant,"lc1"]))
  pts <- st_as_sf(st_sample(df, length(c(lookup[!lookup$type %in% lu_constant,"lc1"]))*2*nb_pts))
  pts <- st_join(pts, left = FALSE, df[c("lc1","year")]) %>%
    group_by(lc1, year) %>%
    sample_n(size = nb_pts, replace = TRUE) %>%
    ungroup %>%
    filter(!is.na(lc1)) %>%
    st_transform(4326)
  return(pts)
}

#' Extract rotation information from raster file
#'
#' This function extracts crop rotation information from a raster file and 
#' amends the land use data accordingly.
#'
#' @param df An sf data.frame with land use. Columns "id" and "type" should be 
#' present. 
#' @param start_year A numeric value representing the year from which data begins.
#' @param tif_name A string for the name of the .tif raster file.
#' @param r_path A string for the path to the .tif file.
#' @param lookup A dataframe with a "lc1" column for numeric codes and a "type" 
#' column for text.
#' @param lu_constant (optional) A vector of strings with land uses to be kept constant in 
#' land use (e.g., water, urban areas). Default \code{lu_constant = c()}.
#' @importFrom raster raster nbands extract
#' @importFrom dplyr left_join mutate_at all_of mutate select vars starts_with
#' @importFrom sf st_point_on_surface st_transform st_drop_geometry st_crs
#' @return An sf data.frame with land use amended with crop rotation information.
#' @export
#' @examples
#' \dontrun{
#'   library(sf)
#'   
#'   # Loading land use/crop layer
#'   lu_path <- system.file("extdata", "GIS/lu_layer.shp", package = "SWATprepR")
#'   lu <- st_read(lu_path, quiet = TRUE) %>% mutate(id = row_number())
#'   
#'   # Preparing lookup table
#'   lookup <- data.frame(lc1 = seq(1:length(unique(c(lu$type)))), 
#'   type = unique(c(lu$type)))
#'   lu_constant <- c("fesc", "orch", "frst", "frse", "frsd", "urld", "urhd", 
#'   "wetl", "past", "watr", "agrl")
#' 
#'   # Extracting rotation information from raster
#'   # Raster information should have been prepared with remote sensing 
#'   lu_rot <- extract_rotation(lu, 2015, "cropmaps.tif", "./output/", lookup, 
#'   lu_constant)
#' }
#' @references 
#' Mészáros, J., & Szabó, B. (2022). Script to derive and apply crop 
#' classification based on Sentinel 1 satellite radar images in Google Earth 
#' Engine platform. \url{https://doi.org/10.5281/zenodo.6700122}
#' @keywords remote-sensing

extract_rotation <- function(df, start_year, tif_name, r_path, lookup, lu_constant = c()){
  r <- raster(paste0(r_path, tif_name), band = 1)
  bn <- nbands(r)
  ##Centroids for each field in land use data created
  suppressWarnings(centroid <- df["id"] %>% 
                     st_point_on_surface() %>% 
                     st_transform(st_crs(r)))
  c <- c()
  for (i in seq(1:bn)){
    if(i>1){r <- raster(paste0(r_path, tif_name), band = i)}
    n <- paste0("y_", start_year)
    centroid[n] <- raster::extract(r, centroid)
    print(paste0("Finished working on year ", start_year))
    i <- i + 1
    start_year <- start_year + 1
    c <- c(c, n)
  }
  ##Preparing GIS field layer with rotations in attributes
  lu_rot <- df[c("id", "type")] %>% 
    left_join(st_drop_geometry(centroid), by = "id") %>% 
    mutate_at(vars(all_of(c)), ~lookup$type[match(., lookup$lc1)]) %>% 
    mutate(lu = ifelse(type %in% lu_constant, type, paste0("field_", id))) %>% 
    dplyr::select(lu, type, starts_with("y_"), geometry)
  
  ##Removing rotations for constant land uses/crops
  lu_rot[!startsWith(lu_rot$lu, "field_"), c] <- NA
  print("Extraction finished succesfully")
  return(lu_rot)
}

# Point source data -----------------------------------------------

#' Prepare Point Source Data Model Text Files
#'
#' This function prepares text files for a SWAT+ model to represent point source 
#' data. It is designed for simple cases with yearly values,
#' where point sources discharge to a single channel.
#'
#' @param pt_lst Nested list with dataframes. 
#'   Nested structure: \code{pt_lst -> data -> Dataframe (name, DATE, flo, ...)}, 
#'   \code{pt_lst -> st -> Dataframe (name, Lat, Long)}.
#'   Additional information on the input variables, which could be used in the template 
#'   files can be found in the SWAT+ documentation: ['filename'.rec](https://swatplus.gitbook.io/io-docs/introduction/point-sources-and-inlets/filename.rec)
#'   Point source data can be loaded with \code{\link{load_template}} function 
#'   using 'xlsx' template file. 
#' @param project_path Character, path to the SWAT+ project folder (example "my_model"). 
#' @param write_path (optional) Character, path to SWAT+ txtinout folder (example "my_model"). 
#'   Default \code{write_path = NULL}, which is the same as \code{project_path}.
#' @param cha_shape_path (optional) Character, path to SWAT+ channel shapefile. 
#'   'id' column should be present in attributes with numeric values representing channel ids, 
#'   the same as in 'chandeg.con' file. Default \code{cha_shape_path = FALSE}, 
#'   which assigns point sources to the will be assigned based on nearest center 
#'   point in 'chandeg.con'
#' @importFrom sf st_as_sf st_nearest_feature st_crs st_drop_geometry st_transform read_sf
#' @importFrom lubridate year
#' @importFrom dplyr select left_join
#'
#' @return Text files for SWAT+ model in write_path.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Example usage:
#'   temp_path <- system.file("extdata", "pnt_data.xls", package = "SWATprepR")
#'   pnt_data <- load_template(temp_path)
#'   prepare_pt_source(pnt_data, "my_model")
#' }
#' @keywords writing

prepare_ps <- function(pt_lst, project_path, write_path = NULL, cha_shape_path = FALSE){
  if(is.null(write_path)){
    write_path <- project_path
  }
  if(project_path == write_path){
    print(paste0("Files in ", project_path, " will be overwritten!!!"))
  }
  id <- 0
  for(i in unique(pt_lst$data$ob_name)){
    ri <- pt_lst$data[pt_lst$data$ob_name == i,]
    t <- find_time_step(ri[2, "DATE"], ri[1, "DATE"])
    fname <- paste0(i, ".rec")
    ri$ob_typ <- paste0(ri$ob_typ, "_", t$typ)
    f_path <- paste0(write_path, "/", fname)
    f_rec_path <- paste0(write_path, "/", "recall.rec")
    f_con_path <- paste0(write_path, "/", "recall.con")
    s <- c(rep('%8s', 2), rep('%9s', 3), rep('%10s', 19))
    text_l <- paste0(": file was written by SWATprepR R package ", Sys.time())
    ri <- ri[, !(colnames(ri) == "DATE")]
    write.table(paste0(fname, text_l), f_path, append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, 
                quote = FALSE)
    write.table(dim(ri)[1], f_path, append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    write.table(paste(sprintf(s, names(ri)), collapse = ' '), f_path, append = TRUE, sep = "\t", dec = ".", row.names = FALSE, 
                col.names = FALSE, quote = FALSE)
    df_to_txt(write_path, fname, ri, s)
    print(paste0(fname, " file was successfully written."))
    if(id == 0){
      write.table(paste0("recall.rec", text_l), f_rec_path, append = FALSE, sep = "\t", dec = ".", row.names = FALSE, 
                  col.names = FALSE, quote = FALSE)
      write.table(paste(sprintf(c(rep('%10s', 4)), c("id", "name", "rec_typ", "file")), collapse = ' '), f_rec_path, 
                  append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      write.table(paste0("recall.con", text_l), f_con_path, append = FALSE, sep = "\t", dec = ".", row.names = FALSE, 
                  col.names = FALSE, quote = FALSE)
      write.table(paste(sprintf(c(rep('%10s', 17)), c('id', 'name', 'gis_id', 'area', 'lat', 'lon', 'elev', 'rec', 'wst', 'cst', 
                                                      'ovfl', 'rule', 'out_tot', 'obj_typ', 'obj_id', 'hyd_typ', 'frac')), 
                        collapse = ' '), f_con_path, append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, 
                  quote = FALSE)
      cha_table <- read_tbl("chandeg.con", project_path) 
      if(!is.character(cha_shape_path)){
        warning("No channel shapefile provided. Channel IDs will be assigned based on nearest center point in 'chandeg.con'.")
        cha_table <- cha_table %>% 
          st_as_sf(coords = c("lon", "lat"), crs = st_crs(pt_lst$st)$input) %>% 
          .[st_nearest_feature(pt_lst$st, .),] %>% 
          st_drop_geometry
      } else {
        cha <- read_sf(cha_shape_path) %>% 
          st_transform(4326) %>% 
          .[st_nearest_feature(pt_lst$st, .),] %>% 
          st_drop_geometry %>% 
          select(id) %>% 
          left_join(cha_table, by = "id")
      }
    } 
    id <- id + 1
    write.table(paste(sprintf(c(rep('%10s', 4)), c(id, i, t$rec_typ, fname)), collapse = ' '), f_rec_path, append = TRUE, 
                sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    write.table(paste(sprintf(c(rep('%10s', 17)), c(id, i, cha_table[id, "gis_id"][[1]], "0.00001", pt_lst$st[pt_lst$st$name == i, "Lat"][[1]], 
                                                    pt_lst$st[pt_lst$st$name == i, "Long"][[1]], cha_table[id, "elev"][[1]], id, 
                                                    "null", cha_table[id, "cst"][[1]], cha_table[id, "ovfl"][[1]],
                                                    cha_table[id, "rule"][[1]], 1, "sdc", 
                                                    cha_table[id, "id"][[1]], "tot", 1)), 
                      collapse = ' '), f_con_path, append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
  print(paste0("recall.rec", " file was successfully written."))
  print(paste0("recall.con", " file was successfully written."))
  
  file_cio <- readLines(paste0(project_path, "/", "file.cio"))
  if(!grepl("recall.rec", file_cio[11], fixed = TRUE)){
    file_cio[11] <- "recall            recall.rec        "
    writeLines(file_cio, paste0(project_path, "/", "file.cio"))
    print(paste0("file.cio", " file was successfully updated."))
  }
  print(paste0("Point source files for model have been successfully prepared and written in ", write_path, " folder."))
}

