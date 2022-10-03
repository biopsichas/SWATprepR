
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
#' ##get_interpolated_data(weather_data, "PCP", "basin.shp", "DEM.tif", 2000, 2)

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


#' Function to fill soil parameters
#'
#' @param template_path path to *.xlsx file with at least SNAM	NLAYERS	columns and SOL_Z	CLAY	
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
#' ##get_soil_pars("templates/soil_parameters.xlsx")

get_soil_parameters <- function(template_path){
  soilp <- read_excel(template_path)
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
                       HYDGRP = "B", SOL_ZMX = soilp$SOL_ZMX, ANION_EXCL = 0.5, SOL_CRK = 0.5, TEXTURE = "")
  
  for(i in seq_along(sol_z)){
    sel_cols <- c(paste0("SOL_Z", i), paste0("SOL_BD", i), paste0("SOL_AWC", i), paste0("SOL_K", i), paste0("SOL_CBN", i), paste0("CLAY", i),
                  paste0("SILT", i), paste0("SAND", i), paste0("ROCK", i), paste0("SOL_ALB", i), paste0("USLE_K", i), paste0("SOL_EC", i))
    soilpf[sel_cols]  <- soilp[sel_cols] 
  }
  return(soilpf)
}


