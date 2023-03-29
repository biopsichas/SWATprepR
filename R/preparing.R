
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
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "svatools")
#' DEM_path <- system.file("extdata", "GIS/DEM.tif", package = "svatools")
#' basin_path <- system.file("extdata", "GIS/basin.shp", package = "svatools")
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

#' Interpolating and writing results into model input files
#'
#' @param meteo_lst nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' @param catchment_boundary_path path to basin boundary shape file.
#' @param dem_data_path path to DEM raster data in same projection as weather station.
#' @param grid_spacing numeric value for distance in grid. Units of coordinate system should be used.
#' @param p_vector character vector representing weather variables to interpolate (optional, default all variables selected 
#' c("PCP", "SLR", "RELHUM", "WNDSPD", "TMP_MAX", "TMP_MIN" ).
#' @param idw_exponent numeric value for exponent parameter to be used in interpolation 
#' (optional, default value is 2).
#' @importFrom sf st_zm st_bbox st_read st_crs st_transform
#' @return nested list of lists with dataframes for interpolation results.
#' Nested structure lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER). 
#' Function also writes all SWAT weather text input files from the interpolation results.
#' @export
#' @examples
#' \dontrun{
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "svatools")
#' DEM_path <- system.file("extdata", "GIS/DEM.tif", package = "svatools")
#' basin_path <- system.file("extdata", "GIS/basin.shp", package = "svatools")
#' met_lst <- load_template(temp_path, 3035)
#' interpolate(met_lst, basin_path, DEM_path, 2000) 
#' }

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
  return(results)
}

# Weather data -----------------------------------------------

#' Filling missing variables from the closest stations, which has data. 
#'
#' @param meteo_lst meteo_lst nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' Nested meteo_lst -> stations Dataframe (ID, Name, Elevation, Source, geometry, Long, Lat).
#' @param par_fill vector of variables to be filled. Optional (default is c("TMP_MAX", "TMP_MIN","PCP", "RELHUM", "WNDSPD", "SLR")).
#' @importFrom sf st_distance
#' @importFrom dplyr filter %>% 
#' @return list of dataframe with filled data. Returned list is for meteo_lst$data.
#' @export
#'
#' @examples
#' \dontrun{
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "svatools")
#' met_lst <- load_template(temp_path, 3035)
#' met_lst$data <- fill_with_closest(met_lst, c("TMP_MAX", "TMP_MIN"))
#' }

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

#' Function to generate wgn data for the model
#'
#' @param meteo_lst meteo_lst nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' Nested meteo_lst -> stations Dataframe (ID, Name, Elevation, Source, geometry, Long, Lat).
#' @param TMP_MAX dataframe with two columns: DATE : POSIXct, TMP_MAX : num. Optional (default NULL).
#' This parameter refers to data, which should be used instead, if TMP_MAX variable is missing for a station.  
#' @param TMP_MIN dataframe with two columns: DATE : POSIXct, TMP_MIN : num. Optional (default NULL).
#' This parameter refers to data, which should be used instead, if TMP_MIN is missing for a station.
#' @param PCP dataframe with two columns: DATE : POSIXct, PCP : num. Optional (default NULL).
#' This parameter refers to data, which should be used instead, if PCP is missing for a station.
#' @param RELHUM dataframe with two columns: DATE : POSIXct, RELHUM : num. Optional (default NULL).
#' This parameter refers to data, which should be used instead, if RELHUM is missing for a station.
#' @param WNDSPD dataframe with two columns: DATE : POSIXct, WNDSPD : num. Optional (default NULL).
#' This parameter refers to data, which should be used instead, if WNDSPD is missing for a station.
#' @param MAXHHR dataframe with two columns: DATE : POSIXct, MAXHHR : num. Optional (default NULL).
#' This parameter refers to data, which should be used instead, if MAXHHR is missing for a station.
#' @param SLR dataframe with two columns: DATE : POSIXct, SLR : num. Optional (default NULL).
#' This parameter refers to data, which should be used instead, if SLR is missing for a station.
#' @importFrom stats aggregate sd
#' @importFrom sf st_coordinates st_transform st_crs st_drop_geometry
#' @importFrom dplyr %>% rename mutate bind_rows select
#' @importFrom lubridate month
#' @return list of two dataframes: wgn_st - wgn station data, wgn_data - wgn data
#' @export
#' @examples
#' \dontrun{
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "svatools")
#' met_lst <- load_template(temp_path, 3035)
#' TMP_MAX <- met_lst$data$ID10$TMP_MAX
#' TMP_MIN <- met_lst$data$ID10$TMP_MIN
#' PCP <- met_lst$data$ID9$PCP
#' RELHUM = met_lst$data$ID9$RELHUM
#' WNDSPD <- met_lst$data$ID12$WNDSPD
#' MAXHHR <- met_lst$data$ID11$MAXHHR
#' SLR <- met_lst$data$ID9$SLR
#' ##Does the thing
#' wgn <- prepare_wgn(met_lst, TMP_MAX, TMP_MIN, PCP, RELHUM, WNDSPD, MAXHHR, SLR)
#' }

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
    wgn_stat$ID <- j
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
    wgn_mon$wgn_id <- j
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

#' Extract EMEP atmospheric deposition data for a catchment
#'
#' @param catchment_boundary_path path to basin boundary shape file.
#' @param t_ext string, which EMEP data to access 'year' for yearly averages, 'month' - monthly averages,
#''day' for daily averages and 'hour' - hourly. Optional (default - "year").
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
#' basin_path <- system.file("extdata", "GIS/basin.shp", package = "svatools")
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
    } else if(t_ext %in% c("month", "day")){
      prec <- var.get.nc(r, "WDEP_PREC")[ilon, ilat,]
      dry_oxn <- apply(var.get.nc(r, "DDEP_OXN_m2Grid")[ilon, ilat,]*4.4268/100, 3, mean)
      wet_oxn <- apply(var.get.nc(r, "WDEP_OXN")[ilon, ilat,]*4.4268/prec, 3, mean)
      dry_rdn <- apply(var.get.nc(r, "DDEP_RDN_m2Grid")[ilon, ilat,]*1.2878/100, 3, mean)
      wet_rdn <- apply(var.get.nc(r, "WDEP_RDN")[ilon, ilat,]*1.2878/prec, 3, mean)
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
      stop("Wrong t_ext!!! Should be one of these strings: 'year', 'month' or 'day'.")
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

# Preparing soils -----------------------------------------------

#' Function to fill soil parameters
#'
#' @param soilp dataframe with at least SNAM	NLAYERS	columns and SOL_Z	CLAY	
#' SILT	SAND	OC columns for each available soil layer.
#' @importFrom euptf2 euptfFun 
#' @importFrom readxl read_excel
#' @importFrom dplyr rename_at vars
#' @importFrom tidyselect all_of
#' @importFrom Rdpack reprompt
#' @return dataframe with fully formatted and filled table of soil parameters for SWAT model.
#' @export
#'
#' @examples
#' \dontrun{
#' temp_path <- system.file("extdata", "soil_parameters.xlsx", package = "svatools")
#' library(euptf2)
#' soil <- get_soil_parameters(temp_path)
#' str(soil)
#' }

get_soil_parameters <- function(soilp){
  soilp$rownum <- 1:nrow(soilp)
  sol_z <- names(soilp)[grep("SOL_Z.*", names(soilp))]
  sol_oc <- names(soilp)[grep("OC>*", names(soilp))]
  sol_cbn <- sub("OC", "SOL_CBN", sol_oc)
  soilp <- rename_at(soilp, vars(all_of(sol_oc)), ~sol_cbn)
  soilp["SOL_ZMX"] <- do.call(pmax, c(soilp[sol_z], list(na.rm=TRUE)))
  ##Loop to fill parameters for each layer
  for(i in seq_along(sol_z)){
    # soilp[paste0("BD", i)] <- 1.72 - 0.294*( soilp[paste0("SOL_CBN", i)] ^ 0.5)
    soilp[paste0("BD", i)] <- ifelse(soilp[paste0("SOL_CBN", i)] < 12, 
                                      1.72 - 0.294 * (soilp[paste0("SOL_CBN", i)] ^ 0.5), 
                                     0.074 + 2.632 * exp(-0.076*( soilp[paste0("SOL_CBN", i)])))
    soilp[paste0("SOL_BD", i)] <- ifelse(soilp[paste0("SOL_CBN", i)] > 0.58,  soilp[paste0("BD", i)] + 0.009 * soilp[paste0("CLAY", i)], 
                                         soilp[paste0("BD", i)] + 0.005 * soilp[paste0("CLAY", i)]+ 0.001 * soilp[paste0("SILT", i)])
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
    pred_VG1 <- euptfFun(ptf = "PTF07", predictor = input, target = "VG", query = "predictions")
    names(pred_VG1)[2:6] <- c("THS","THR", "ALP", "N", "M")
    input <- input[c(1:nrow(input)-d),]
    pred_VG <- merge(pred_VG1[c(1:nrow(pred_VG1)-d), c(1:6,8,10:14)], input[,c(1,2)], by="rownum", all.y=TRUE)
    # correct VG parameters for organic soils if organic soils are present in the data
    pred_VG$THR <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M <= 30), 0.111, pred_VG$THR)
    pred_VG$THS <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M <= 30), 0.697, pred_VG$THS)
    pred_VG$ALP <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M <= 30), 0.0069, pred_VG$ALP)
    pred_VG$N <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M <= 30), 1.4688, pred_VG$N)
    
    pred_VG$THR <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M > 30), 0.001, pred_VG$THR)
    pred_VG$THS <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M > 30), 0.835, pred_VG$THS)
    pred_VG$ALP <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M > 30), 0.0113, pred_VG$ALP)
    pred_VG$N <- ifelse((((pred_VG$OC > 12 & is.na(pred_VG$USCLAY)) | (pred_VG$OC >= (12+pred_VG$USCLAY*0.1) & pred_VG$USCLAY < 60) | (pred_VG$OC >= 18 & pred_VG$USCLAY>=60)) & pred_VG$DEPTH_M > 30), 1.2256, pred_VG$N)

    FC <- pred_VG$THR+(pred_VG$THS-pred_VG$THR)*((1+(((pred_VG$N-1)/pred_VG$N)^(1-2*pred_VG$N)))^((1-pred_VG$N)/pred_VG$N))
    WP <- pred_VG$THR+((pred_VG$THS-pred_VG$THR)/((1+pred_VG$THR*(15000^pred_VG$N))^(1-(1/pred_VG$N))))

    soilp[paste0("SOL_AWC", i)] <- FC-WP
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
#' @export
#'
#' @examples
#' df <- data.frame(SOL_K1 = 10, SOL_K2 = 1, SOL_K3 = 2, 
#'                  SOL_Z1 = 250, SOL_Z2 = 700, SOL_Z3 = 1000)
#' get_hsg(d_imp = ">100cm", d_wtr = "<60cm", drn = "Y", df)

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

#' Convert usersoil.csv to soils.sol
#'
#' @param csv_path character path to csv file (example "usersoil_lrew.csv"")
#' @param db_path character to sqlite project database (example "output/project.sqlite"). 
#' Optional, default NULL. Could be used to reduce soils.sol file, if there are less soil 
#' types in sqlite database than in usersoil csv file.
#' @importFrom DBI dbConnect dbReadTable
#' @importFrom RSQLite SQLite
#' @importFrom dplyr mutate_all %>% 
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom stringr str_extract
#' @importFrom purrr map2_chr map2_df
#' @importFrom readr write_lines parse_number
#' @importFrom utils read.csv2
#' @return soils.sol SWAT+ model input file
#' @export
#'
#' @examples
#' \dontrun{
#' usersoil_to_sol("output/usersoil_lrew.csv")
#' ##Or
#' usersoil_to_sol("output/usersoil_lrew.csv", "output/project.sqlite")
#' }

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
  text_l <- paste0("soils.sol: written by svatools R package ", Sys.time(), " for SWAT+ rev.60.5.4")
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
    s1 <- s1 %>%
      map2_df(., sol_val1, ~sprintf(.y, .x)) %>%
      apply(., 1, paste, collapse = ' ')
    write_lines(s1, nfile, append = TRUE)
    ##Parameters for layer
    s2 <- s2 %>%
      map2_df(., sol_val2, ~sprintf(.y, .x)) %>%
      apply(., 1, paste, collapse = ' ')
    write_lines(s2, nfile, append = TRUE)
  }
  print("File conversion from usersoil csv to soils.sol finished successfully.")
  print(paste0("Prepared result in ", nfile))
  print("Please copy file to your setup folder.")
}

# Updating .sqlite database -----------------------------------------------

#' Update sqlite database with weather data
#'
#' @param db_path character to sqlite database (example "./output/project.sqlite")
#' @param meteo_lst meteo_lst nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' Nested meteo_lst -> stations Dataframe (ID, Name, Elevation, Source, geometry, Long, Lat).
#' @param wgn_lst list of two dataframes: wgn_st - wgn station data, wgn_data - wgn data (prepared by \code{prepare_wgn()} function).
#' @param fill_missing Boolean, TRUE - fill data for missing stations with data from closest stations with available data.
#' FALSE - leave stations without data. Weather generator will be used to fill missing variables for a model. Optional (\code{Default = TRUE}).
#' @importFrom sf st_transform st_coordinates st_drop_geometry
#' @importFrom dplyr select full_join mutate %>%  rename
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect
#' @importFrom lubridate yday interval years
#' @importFrom RSQLite SQLite
#' @importFrom utils write.table
#' @return updated sqlite database with weather data
#' @export
#' @examples
#' \dontrun{
#' ##Getting meteo data from template
#' met_lst <- load_template(temp_path, 3035)
#' ##Calculating wgn parameters
#' wgn <- prepare_wgn(met_lst, MAXHHR = met_lst$data$ID11$MAXHHR)
#' ##Writing weather input into model database
#' db_path <- "./output/test/project.sqlite"
#' add_weather(db_path, met_lst, wgn)
#' }

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
      text_l <- paste0(file_n, ": ", p_lst[["TMP_MAX"]][[2]], " data - file written by svatools R package ", Sys.time())
      ##Writing temperature file per station
      write.table(text_l, paste0(write_path, file_n), append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      suppressWarnings(write.table(df1, paste0(write_path, file_n), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE, quote = FALSE))
      write.table(df[c(1:4)], paste0(write_path, file_n), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      ##Writing .cli files
      if(!file.exists(paste0(write_path, p_lst[["TMP_MAX"]][[1]], ".cli"))){
        text_l <- paste0(p_lst[["TMP_MAX"]][[1]], ".cli", ": ", p_lst[["TMP_MAX"]][[2]], " file names - file written by svatools R package ", Sys.time())
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
      df1$nbyr <- round(interval(df[[1,"DATE"]], df[[nrow(df),"DATE"]]) / years(1), 0)
      file_n <- paste0("sta_", tolower(n), ".", p_lst[[p]][[1]])
      text_l <- paste0(file_n, ": ", p_lst[[p]][[2]], " data - file written by svatools R package ", Sys.time())
      ##Writing file per parameter for each station
      write.table(text_l, paste0(write_path, file_n), append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      suppressWarnings(write.table(df1, paste0(write_path, file_n), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = TRUE, quote = FALSE))
      write.table(df[c("year", "day", p)], paste0(write_path, file_n), append = TRUE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      ##Writing .cli files
      if(!file.exists(paste0(write_path, p_lst[[p]][[1]], ".cli"))){
        text_l <- paste0(p_lst[[p]][[1]], ".cli", ": ", p_lst[[p]][[2]], " file names - file written by svatools R package ", Sys.time())
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

#' Update sqlite database with atmospheric deposition data
#'
#' @param df dafaframe with "DATE", "NH4_RF", "NO3_RF" , "NH4_DRY"  and "NO3_DRY" columns obtained from \code{\link{get_atmo_dep}}) function.
#' @param db_path character to sqlite database (example "./output/project.sqlite")
#' @param t_ext string, 'year' for yearly averages, 'month' - monthly averages
#' and 'annual' for average of all period. Optional (default - "year"). 
#' @importFrom DBI dbConnect dbWriteTable dbDisconnect dbReadTable
#' @importFrom RSQLite SQLite
#' @importFrom utils write.table
#' @return write data in 'atmodep.cli' file and updated sqlite database with codes and connection 
#' to atmospheric deposition data 
#' @export 
#' @examples
#' \dontrun{
#' basin_path <- system.file("extdata", "GIS/basin.shp", package = "svatools")
#' df <- get_atmo_dep(basin_path)
#' db_path <- "./output/test/project.sqlite"
#' add_atmo_dep(df, db_path)
#' }

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
  write.table(paste0("'atmodep.cli' file was written by svatools R package ", Sys.time()), f_path, append = FALSE, sep = "\t", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
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

#' Preparing training points for remote sensing algorithm
#'
#' @param df sf data.frame with land use. "type" column should be present. 
#' @param year numeric value, year of land use.
#' @param lookup dataframe with "lc1" column for numeric codes and "type" column 
#' for text.
#' @param lu_constant vector of strings with land uses to be kept constant in 
#' land use (i.e. water, urban areas, etc.)
#' @param nb_pts numeric, number of points per land use/crop class. Optional, default 100. 
#' @param col_name string with name of column to be used representing type of crops/land use. 
#' Optional, default "type".
#' @importFrom sf st_as_sf st_join st_transform st_sample
#' @importFrom dplyr rename mutate left_join select filter sample_n ungroup
#' @return sf data.frame with point input for remote sensing training algorithm.
#' @export
#' @examples
#' \dontrun{
#' library(sf)
#' ##Loading land use/crop layer
#' lu_path <- system.file("extdata", "GIS/lu_layer.shp", package = "svatools")
#' lu <- st_read(lu_path,  quiet = TRUE)
#' ##Preparing lookup table
#' lookup <- data.frame(lc1 = seq(1:length(unique(c(lu$type)))), 
#' type = unique(c(lu$type)))
#' lu_constant <- c("fesc", "orch", "frst", "frse", "frsd", "urld", "urhd", 
#' "wetl", "past", "watr", "agrl")
#' ##Getting training points
#' pts <- get_lu_points(lu, 2021, lookup, lu_constant)
#' }

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
#' @param df sf data.frame with land use. "id" and "type" columns should be present. 
#' @param start_year numeric, representing a year from which data begins.
#' @param tif_name string for name of .tif raster file.
#' @param r_path string for path to .tif file.
#' @param lookup dataframe with "lc1" column for numeric codes and "type" column 
#' for text.
#' @param lu_constant vector of strings with land uses to be kept constant in 
#' land use (i.e. water, urban areas, etc.)
#' @importFrom raster raster nbands extract
#' @importFrom dplyr left_join mutate_at all_of mutate select vars starts_with
#' @importFrom sf st_point_on_surface st_transform st_drop_geometry st_crs
#' @return sf data.frame with land use amended with crop rotation information
#' @export
#' @examples
#' \dontrun{
#' library(sf)
#' ##Loading land use/crop layer
#' lu_path <- system.file("extdata", "GIS/lu_layer.shp", package = "svatools")
#' lu <- st_read(lu_path,  quiet = TRUE) %>% mutate(id = row_number())
#' ##Preparing lookup table
#' lookup <- data.frame(lc1 = seq(1:length(unique(c(lu$type)))), 
#' type = unique(c(lu$type)))
#' lu_constant <- c("fesc", "orch", "frst", "frse", "frsd", "urld", "urhd", 
#' "wetl", "past", "watr", "agrl")
#' 
#' ##Extracting rotation information from raster
#' ##Raster information should have been prepared with remote sensing 
#' lu_rot <- extract_rotation(lu, 2015, "cropmaps.tif", "./output/", lookup, lu_constant)
#' }

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
    mutate(lu = ifelse(type %in% lu_constant, type, paste0("f", id))) %>% 
    dplyr::select(lu, type, starts_with("y_"), geometry)
  
  ##Removing rotations for constant land uses/crops
  lu_rot[!startsWith(lu_rot$lu, "f"), c] <- NA
  print("Extraction finished succesfully")
  return(lu_rot)
}


