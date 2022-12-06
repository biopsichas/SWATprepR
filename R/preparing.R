
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
#' @param par character representing weather variable to extract (i.e. "PCP", "SLR", etc).
#' @param catchment_boundary_path path to basin boundary shape file.
#' @param dem_data_path path to DEM raster data in same projection as weather station.
#' @param grid_spacing numeric value for distance in grid. Units of coordinate system should be used.
#' @param idw_exponent numeric value for exponent parameter to be used in interpolation. 
#' (optional, default value is 2).
#' @importFrom sp coordinates<- proj4string<- CRS over
#' @importFrom methods as
#' @importFrom raster extract raster
#' @importFrom sf st_crs st_transform read_sf
#' @return SpatialPointsDataFrame with interpolated data.
#' @export
#'
#' @examples
#' \dontrun{
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "svatools")
#' DEM_path <- system.file("extdata", "GIS/DEM.tif", package = "svatools")
#' basin_path <- system.file("extdata", "GIS/basin.shp", package = "svatools")
#' met_lst <- load_template(temp_path, 3035)
#' get_interpolated_data(met_lst, "PCP", basin_path, DEM_path, 2000, 2)
#' }

get_interpolated_data <- function(meteo_lst, par, catchment_boundary_path, dem_data_path, grid_spacing, idw_exponent = 2){
  ##Preparing data for interpolation and grid
  df <- get_data_to_interpolate(meteo_lst, par)
  grd <- get_grid(df, grid_spacing)
  ##Loading data
  DEM <- raster(dem_data_path)
  shp <- read_sf(catchment_boundary_path)
  ##Defining coordinate system
  m_proj <- st_crs(meteo_lst$stations)$input
  if (m_proj != st_crs(shp)$input){
    shp <- st_transform(shp, m_proj)
  }
  suppressWarnings(proj4string(DEM) <-  CRS(SRS_string = m_proj))
  ##extracting points from the grid for the catchment and saving results
  meteo_pts <- as(as(grd, "SpatialPoints")[!is.na(over(as(grd, "SpatialPoints"), 
                                                       as(shp, 'Spatial'))[1]),], "SpatialPointsDataFrame")
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
#' @param write_path path to folder where results should be written.
#' @param catchment_boundary_path path to basin boundary shape file.
#' @param dem_data_path path to DEM raster data in same projection as weather station.
#' @param grid_spacing numeric value for distance in grid. Units of coordinate system should be used.
#' @param p_vector character vector representing weather variables to interpolate (optional, default all variables selected 
#' c("PCP", "SLR", "RELHUM", "WNDSPD", "TMP_MAX", "TMP_MIN" ).
#' @param idw_exponent numeric value for exponent parameter to be used in interpolation 
#' (optional, default value is 2).
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
#' interpolate(met_lst, "./output/",  basin_path, DEM_path, 2000) 
#' }

interpolate <- function(meteo_lst, write_path, catchment_boundary_path, dem_data_path, grid_spacing, 
                        p_vector = c("PCP", "SLR", "RELHUM", "WNDSPD", "TMP_MAX", "TMP_MIN"), idw_exponent = 2){
  ##List to save interpolation results for examining
  results <- list()
  p_lst <- list("PCP" = "pcp", "SLR" = "solar", "RELHUM" = "rh", "TMP_MAX" = "tmp", 
                "TMP_MIN" = "tmp", "WNDSPD" = "wind")
  ##Loop for all parameter
  for (p in p_vector){
    ##Interpolation case for which has data at more than 1 station and not TMP
    if(get_nb_st_with_data(meteo_lst, p)>1 & !startsWith(p, 'TMP')){
      r <- get_interpolated_data(meteo_lst, p, catchment_boundary_path, dem_data_path, grid_spacing, idw_exponent)
      write_ref_file(paste0(write_path, p_lst[[p]], ".txt"), r, p)
      write_input_files(write_path, r, meteo_lst, p)
      results[[p]] <- r
      ##Interpolation for TMP data
    } else if(p == 'TMP_MAX'){
      r_tmx <- get_interpolated_data(meteo_lst, "TMP_MAX", catchment_boundary_path, dem_data_path, grid_spacing, idw_exponent)
      r_tmn <- get_interpolated_data(meteo_lst, "TMP_MIN", catchment_boundary_path, dem_data_path, grid_spacing, idw_exponent)
      results[['TMP_MAX']] <- r_tmx
      results[['TMP_MIN']] <- r_tmn
      write_input_files_tmp(write_path, r_tmx, r_tmn, meteo_lst)
      write_ref_file(paste0(write_path, p_lst[[p]], ".txt"), r_tmx, "TMP")
    }
  }
  cat("\014") 
  print("Interpolation is finished. Results are in results dataframe.")
  return(results)
}

#' Function to generate wgn data for the model
#'
#' @param meteo_lst meteo_lst nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' Nested meteo_lst -> stations Dataframe (ID, Name, Elevation, Source, geometry, Long, Lat).
#' @param TMP_MAX dataframe with two columns: DATE : POSIXct, TMP_MAX : num. Optional (default NULL).
#' @param TMP_MIN dataframe with two columns: DATE : POSIXct, TMP_MIN : num. Optional (default NULL).
#' @param PCP dataframe with two columns: DATE : POSIXct, PCP : num. Optional (default NULL).
#' @param RELHUM dataframe with two columns: DATE : POSIXct, RELHUM : num. Optional (default NULL).
#' @param WNDSPD dataframe with two columns: DATE : POSIXct, WNDSPD : num. Optional (default NULL).
#' @param MAXHHR dataframe with two columns: DATE : POSIXct, MAXHHR : num. Optional (default NULL).
#' @param SLR dataframe with two columns: DATE : POSIXct, SLR : num. Optional (default NULL).
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
  if (length(c_f) != 0){
    stop(paste("These variables", paste(as.character(c_f), sep="' '", collapse=", "), "are missing for some of the stations. 
             Please add data on these variables to function, which should be used in case station is missing variable."))
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
    wgn_mon$pcp_hhr <- aggregate(MAXHHR~mon, df, max)[,2]
    wgn_mon$pcp_days <- aggregate(PCP~mon, df, my.pcpd, nyears)[,2]
    wgn_mon$pcp_sd <- aggregate(PCP~mon, df, sd)[,2]
    wgn_mon$pcp_skew <- aggregate(PCP~mon, df, my.skew)[,2]
    wgn_mon$wet_dry <- aggregate(PCP~mon, df, my.pwd)[,2]
    wgn_mon$wet_wet <- aggregate(PCP~mon, df, my.pww)[,2]
    wgn_mon$slr_ave <- aggregate(SLR~mon, df, mean)[,2]
    wgn_mon$dew_ave <- aggregate(RELHUM~mon, df, mean)[,2]/100
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
    soilp[paste0("SOL_BD", i)] <- 1.72 - 0.294*( soilp[paste0("SOL_CBN", i)] ^ 0.5)
    input <- soilp[c("rownum", paste0("SOL_Z", i), paste0("SOL_BD", i), paste0("SOL_CBN", i), paste0("CLAY", i), 
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
  }
  ##Formating table
  soilpf <- data.frame(OBJECTID = soilp$rownum, MUID = "", SEQN = 1, SNAM = soilp$SNAM, S5ID = "", CMPPCT = 1, NLAYERS = soilp$NLAYERS, 
                       HYDGRP = "", SOL_ZMX = soilp$SOL_ZMX, ANION_EXCL = 0.5, SOL_CRK = 0.5, TEXTURE = "")
  
  for(i in seq_along(sol_z)){
    sel_cols <- c(paste0("SOL_Z", i), paste0("SOL_BD", i), paste0("SOL_AWC", i), paste0("SOL_K", i), paste0("SOL_CBN", i), paste0("CLAY", i),
                  paste0("SILT", i), paste0("SAND", i), paste0("ROCK", i), paste0("SOL_ALB", i), paste0("USLE_K", i), paste0("SOL_EC", i))
    soilpf[sel_cols]  <- soilp[sel_cols] 
  }
  return(soilpf)
}

#' Function to get soil hydrologic groups
#'
#' @param d_imp character for depth to impervious layer. Only three entry options possible: "<50cm", "50-100cm", ">100cm".
#' @param d_wtr character for water table high. Only three entry options possible: "<60cm", "60-100cm", ">100cm".
#' @param drn character for tile drainage status. Only two entry options possible: "Y" for drained areas, "N" for areas without working tile drains., ">100cm".
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



