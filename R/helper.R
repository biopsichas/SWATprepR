
# Miscellaneous --------------------------------------------------------------------

#' Function to put option remove hide or show all lines in chart and this function also print chart.
#'
#' @param graph plotly graph object
#' @importFrom plotly plotly_build layout
#' @return plotly graph object with option to remove or show all lines
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' hide_show_print_graph(fig)
#' }

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
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_nx_date_full(meteo_lst)
#' }

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


#' Getting number of stations with data for selected parameter
#' 
#' @param meteo_lst nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' @param par is weather variable to extract (i.e. "PCP", "SLR", etc)
#' @return number of stations with data for this parameter. 
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_nb_st_with_data(meteo_lst, "PCP")
#' }

get_nb_st_with_data <- function(meteo_lst, par){
  meteo_lst <- meteo_lst$data
  i <- 0
  for (n in names(meteo_lst)){
    if(par %in% names(meteo_lst[n][[1]])){
      i <- i+1
    }
  }
  return(i)
}

#' Count number of years data available for variable
#'
#' @param df dataframe with DATE column and at least one variable column.
#' @param col variable column name
#' @importFrom lubridate year
#' @return numeric number on year for which data is available. 
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' nyears(df, "PCP")
#' }

nyears <- function(df, col){
  ts <- df$DATE[!is.na(df[[col]])]
  max(year(ts))-min(year(ts))+1
}

#' Get min Ks within depth range from SOL_K and SOL_Z
#'
#' @param df one row dataframe of soil parameters with all SOL_K and SOL_Z columns
#' @param max_depth numeric value for depth range from 0.  
#' @return numeric min Ks parameter value within depth range
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df <- data.frame(SOL_K1 = 10, SOL_K2 = 1, SOL_K3 = 2, 
#'                  SOL_Z1 = 250, SOL_Z2 = 700, SOL_Z3 = 1000)
#' min_ks(df, 800)
#' }

min_ks <- function(df, max_depth){
  ind <- as.vector(grep("SOL_K", colnames(df))[df[,grep("SOL_Z", colnames(df))] < max_depth])
  if(length(ind) > 1){
    ind <- c(ind, ind[-1] + 1)
  } else if (length(ind) == 1){
    ind <- c(ind, ind + 1)
  } else if (max_depth > 0){
    ind <- 1
  }
  return(min(df[,ind])/0.4167)
}

# Interpolation --------------------------------------------------------------------

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
#' \dontrun{
#' get_grid(sp_sf, 2000)
#' }

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
#' @param grd Grid for interpolation.
#' @param i integer representing day number.
#' @param idw_exponent numeric value for exponent parameter to be used in interpolation. 
#' @importFrom gstat idw
#' @importFrom raster raster extract
#' @importFrom spatialEco sp.na.omit
#' @return SpatialPointsDataFrame with virtual stations and interpolated data in data dataframe
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_interpolation(sp_df, meteo_pts, 2, 2)
#' }

get_interpolation <- function(sp_df, st, grd, i, idw_exponent){
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


# Writing -----------------------------------------------------------------

#' Transforming sp dataframe to dataframe
#'
#' @param sp_df sp dataframe
#' @importFrom dplyr %>% mutate_if
#' @return Dataframe prepared for being written out. 
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df_t(sp_df)
#' }

df_t <- function(sp_df){
  ##Preparing time series files and writing them into output folder
  df <- sp_df@data[,-1]
  df[is.na(df)] <- -99 ##Changing NA to -99 for weather generator
  as.data.frame(t(df)) %>%
    mutate_if(is.numeric, ~round(., 3))
}

#' Transforming list of sp dataframes to list of list 
#'
#' @param list_sp a list of SpatialPointsDataFrames (for each weather variable) 
#' prepared with  \code{\link{interpolate}} function.
#' @param start_date time series starting date string. Example "1998-01-01".
#' @param end_date time series ending date string. Example "2021-12-31".
#' @importFrom dplyr %>% bind_cols
#' @importFrom tibble as_tibble
#' @importFrom sf st_as_sf
#' @return meteo_lst nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' Nested meteo_lst -> stations Dataframe (ID, Name, Elevation, Source, geometry, Long, Lat).
#' @export
#' @examples
#' \dontrun{
#' ###First step to get interpolation results
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "svatools")
#' DEM_path <- system.file("extdata", "GIS/DEM.tif", package = "svatools")
#' basin_path <- system.file("extdata", "GIS/basin.shp", package = "svatools")
#' met_lst <- load_template(temp_path, 3035)
#' result <- interpolate(met_lst, "./output/",  basin_path, DEM_path, 2000) 
#' 
#' ###Second step converting interpolation results to list of list format.
#' start_date <- svatools:::get_dates(met_lst)$min_date
#' end_date <- svatools:::get_dates(met_lst)$max_date
#' int_met_lst <- transform_to_list(result, start_date, end_date)
#' }

transform_to_list <- function(list_sp, start_date, end_date){
  df <- data.frame(DATE = seq(as.POSIXct(start_date, "%Y-%m-%d", tz = "UTC"),
                              as.POSIXct(end_date, "%Y-%m-%d", tz = "UTC"), by = "day"))
  r <- list()
  r[["stations"]] <- data.frame(ID = paste0("ID", 1:dim(list_sp[[1]]@data)[[1]]),
                                Name = paste0("ID", 1:dim(list_sp[[1]]@data)[[1]]),
                                Elevation = list_sp[[1]]$DEM,
                                Source = "Interpolation",
                                geometry = st_as_sf(list_sp[[1]]@coords %>% 
                                                      as.data.frame, 
                                                    coords = c("x", "y"), 
                                                    crs = list_sp[[1]]@proj4string@projargs),
                                Long = list_sp[[1]]@coords[,1],
                                Lat = list_sp[[1]]@coords[,2]) %>% 
    as_tibble() %>% 
    st_as_sf()
  for (p in c("PCP", "SLR", "RELHUM", "WNDSPD", "TMP_MAX", "TMP_MIN")){
    if(p %in% names(list_sp)){
      y <- df_t(list_sp[[p]])
      names(y) <- rep(p, dim(y)[2])
      for(i in 1:ncol(y)){
        r[["data"]][[paste0("ID", i)]][[p]] <- bind_cols(df["DATE"], y[1])
      }
    } else {
      warning(paste(p, "variable is missing. Please add it later."))
    }
  }
  return(r)
}

#' Transforming list to dataframe
#'
#' @param lst nested list dataframes (for each weather variable)
#' @importFrom dplyr full_join
#' @return dataframe with all collected dataframes in single dataframe. 
#' @keywords internal
#' @examples
#' \dontrun{
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "svatools")
#' met_lst <- load_template(temp_path, 3035)
#' df <- list_to_df(met_lst$data$ID1)
#' }

list_to_df <- function(lst){
  df <- NULL
  for (p in names(lst)){
    if(!is.null(df)){
      df <- full_join(df, lst[[p]], by = "DATE")
    } else {
      df <- lst[[p]]
    }
  }
  arrange(df, DATE)
}

# WGN helpers -----------------------------------------------------------------

#' Function to calculate pcp_skew parameter
#'
#' @param x vector of numbers 
#' @param na.rm logical, if NA should be cleaned
#' @return numeric result
#' @keywords internal
#' @examples
#' \dontrun{
#' my.skew(x)
#' }

my.skew <- function(x, na.rm = FALSE){
  if(na.rm)
    x <- x[!is.na(x)]
  else if (any(is.na(x)))
    return(NA)
  n <- length(x)
  if(n == 0)
    return(NA)
  skew <- (n * sum((x - mean(x))^3)) / ((n-1) * (n-2) * sd(x)^3)
  return(skew)
}

#' Helper for my.pwd function
#'
#' @param x vector of numbers 
#' @return logical, if wet day after dry
#' @keywords internal
#' @examples
#' \dontrun{
#' my.check.pwd(x)
#' }

my.check.pwd <- function(x){
  nwd <- sum(if(x[1] == 0 & x[2] > 0){TRUE} else{FALSE})
  return(nwd)
}

#' Helper for my.pww function
#'
#' @param x vector of numbers 
#' @return logical, if wet day after wet
#' @keywords internal
#' @examples
#' \dontrun{
#' my.check.pww(x)
#' }

my.check.pww <- function(x){
  nww <- sum(if(x[1] > 0 & x[2] > 0){TRUE} else{FALSE})
  return(nww)
}

#' Function to calculate wet_dry parameter
#'
#' @param x vector of numbers 
#' @param na.rm logical, if NA should be cleaned
#' @importFrom zoo rollapply
#' @return numeric result
#' @keywords internal
#' @examples
#' \dontrun{
#' my.pwd(x)
#' }

my.pwd <- function(x, na.rm = FALSE){
  if(na.rm)
    x <- x[!is.na(x)]
  else if (any(is.na(x)))
    return(NA)
  if(sum(x == 0) == 0)
    return(0)
  pwd <- sum(rollapply(data = x, width = 2, my.check.pwd)) / sum(x == 0)
  return(pwd)
}

#' Function to calculate wet_wet parameter
#'
#' @param x vector of numbers 
#' @param na.rm logical, if NA should be cleaned
#' @importFrom zoo rollapply
#' @return numeric result
#' @keywords internal
#' @examples
#' \dontrun{
#' my.pww(x)
#' }

my.pww <- function(x, na.rm = FALSE){
  if(na.rm)
    x <- x[!is.na(x)]
  else if (any(is.na(x)))
    return(NA)
  if(sum(x > 0) == 0)
    return(NA)
  pww <- sum(rollapply(data = x, width = 2, my.check.pww)) / sum(x > 0)
  return(pww)
}

#' Function to calculate pcp_days parameter
#'
#' @param x vector of numbers 
#' @param nyears numeric number of years available for PCP variable
#' @param na.rm logical, if NA should be cleaned
#' @return numeric result
#' @keywords internal
#' @examples
#' \dontrun{
#' my.pcpd(x)
#' }

my.pcpd <- function(x, nyears, na.rm = FALSE){
  if(na.rm)
    x <- x[!is.na(x)]
  else if (any(is.na(x)))
    return(NA)
  if(nyears == 0)
    return(NA)
  pcpd <- sum(x > 0) / nyears
  return(pcpd)
}

