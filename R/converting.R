
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
#' @export
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
