
# Miscellaneous --------------------------------------------------------------------

#' Function to put option remove hide or show all lines in chart and this function also print chart.
#'
#' @param graph plotly graph object
#' @importFrom plotly plotly_build layout
#' @return plotly graph object with option to remove or show all lines
#' @keywords internal
#'
#' @examples
#' ##hide_show_print_graph(fig)

hide_show <- function(graph){
  plotly_build(graph) %>%
    layout(updatemenus = list(
      list(type = "buttons", direction = "right", xanchor = "center", yanchor = "top", 
           showactive = FALSE, x = 0.3, y = 1.0,
           buttons = list(
             list(method = "restyle",
                  args = list("visible", "all"),
                  label = "show all"),
             list(method = "restyle",
                  args = list("visible", "legendonly"),
                  label = "hide all")))))
}

#' Function to get maximum min and max dates available for all time series data for all stations for all parameters. 
#'
#' @param meteo_lst nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' @importFrom lubridate now
#' @return list with 2 values: minimum and maximum value for all available data.
#' @export
#'
#' @examples
#' ##get_nx_date_full(meteo_lst)

get_dates <- function(meteo_lst){
  meteo_lst <- meteo_lst$data
  min_date <- as.POSIXct(as.Date(now()), "%Y-%m-%d", tz = "UTC")
  max_date <- as.POSIXct("1900-01-01", "%Y-%m-%d", tz = "UTC")
  for (n in names(meteo_lst)){
    for(p in names(meteo_lst[[n]])){
      if(min_date >= min(meteo_lst[[n]][[p]]$DATE)){
        min_date <-  min(meteo_lst[[n]][[p]]$DATE)
      }
      if(max_date <= max(meteo_lst[[n]][[p]]$DATE)){
        max_date <-  max(meteo_lst[[n]][[p]]$DATE)
      }
    }
  }
  ##Returning a list of two dates
  return(list(min_date = min_date, max_date = max_date))
}

# Interpolation helpers ---------------------------------------------------

#' Prepare grid for interpolation
#'
#' @param sp_df SpatialPointsDataFrame with columns as days and rows as stations. Lat and Long columns should be included.
#' @param grid_spacing Grid spacing. Depending on coordinate system could be in meters, or degrees.
#' @importFrom sf st_crs
#' @importFrom sp coordinates<- proj4string<- gridded<- fullgrid<- CRS
#' @return Grid needed for the interpolation. 
#' @keywords internal
#'
#' @examples
#' ##get_grid(sp_sf, 2000)

get_grid <- function(sp_df, grid_spacing){
  x.range <- c(min(round(sp_df$Long,2)*0.999),max(round(sp_df$Long,2)*1.001))
  y.range <- c(min(round(sp_df$Lat,2)*0.999),max(round(sp_df$Lat,2)*1.001))
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = grid_spacing), y = seq(from = y.range[1], to = y.range[2], by = grid_spacing)) 
  coordinates(grd) <- c("x", "y")
  gridded(grd)     <- TRUE
  fullgrid(grd)    <- TRUE
  suppressWarnings(proj4string(grd) <-  CRS(SRS_string = st_crs(sp_df)$wkt))
  return(grd)
}

#' Get interpolation values for catchment
#'
#' @param sp_df SpatialPointsDataFrame with one column for one day and rows as stations.
#' @param st SpatialPointsDataFrame with virtual stations from a grid. Should contain
#' dataframe in data for station elevation from DEM  
#' @param i integer representing day number.
#' @importFrom gstat idw
#' @importFrom raster raster extract
#' @importFrom spatialEco sp.na.omit
#' @return SpatialPointsDataFrame with virtual stations and interpolated data in data dataframe
#' @keywords internal
#'
#' @examples
#' ##get_interpolation(sp_df, meteo_pts, 2, 2)

get_interpolation <- function(sp_df, st, i, idw_exponent){
  if(sum(is.na(sp_df@data[1])) != dim(sp_df@data[1])[[1]]){
    if(sum(is.na(sp_df@data[1])) != 0){
      sp_df <- sp.na.omit(sp_df)
    }
    colnames(sp_df@data) <- "v"
    idw <- gstat::idw(v ~ 1, sp_df, newdata=grd, idp=idw_exponent, debug.level = 0)
    r <- raster(idw)
    r <- raster::extract(r, st)
    st@data[as.character(i)] <- r
  } else {
    st@data[as.character(i)] <- NA
  }
  return(st)
}


