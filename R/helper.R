
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
#' hide_show(fig)
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
#' @param b  vector of border c(xmin, ymin, xmax, ymax) coordinates to define extent of the grid.
#' @param grid_spacing Grid spacing. Depending on coordinate system could be in meters, or degrees.
#' @param wkt_str projection string
#' @importFrom sf st_crs
#' @importFrom sp coordinates<- proj4string<- gridded<- fullgrid<- CRS
#' @return Grid needed for the interpolation. 
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_grid(shp, 2000, wkt_str)
#' }

get_grid <- function(b, grid_spacing, wkt_str){
  x.range <- c(b[1]*0.999,b[3]*1.001)
  y.range <- c(b[2]*0.999,b[4]*1.001)
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = grid_spacing), y = seq(from = y.range[1], to = y.range[2], by = grid_spacing)) 
  coordinates(grd) <- c("x", "y")
  gridded(grd)     <- TRUE
  fullgrid(grd)    <- TRUE
  suppressWarnings(proj4string(grd) <-  CRS(SRS_string = wkt_str))
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

#' Function to remove NAs in sp dataframe
#'
#' @param x sp dataframe
#' @param margin numeric. Optional, default 1 for remove rows, 2 for removing columns.
#'
#' @return sp dataframe without NAs in data 
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' sp_df <- sp.na.omit(sp_df)
#' }

sp.na.omit <- function(x, margin=1) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
  ##Deleting rows
  if(margin == 1) {  
    return( x[-na.index,]  ) 
  }
  ##Deleting columns
  if(margin == 2) {  
    return( x[,-na.index]  ) 
  }
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
  # df[is.na(df)] <- -99 ##Changing NA to -99 for weather generator
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
        r[["data"]][[paste0("ID", i)]][[p]] <- bind_cols(df["DATE"], y[i])
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


#' Updating sqlite database table with wst_id information
#'
#' @param tname character of table name (example "hru_con")
#' @param db_path character to sqlite database (example "./output/project.sqlite")
#' @param wst_cli data.frame weather_sta_cli table
#' @importFrom DBI dbConnect dbReadTable dbWriteTable dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom sf st_as_sf st_nearest_feature
#' @return updates sql database with table amended with wst_id (id of closest weather station)
#' @keywords internal
#' @examples
#' \dontrun{
#' update_wst_id("hru_con", "./output/project.sqlite", weather_sta_cli)
#' }

update_wst_id <- function(tname, db_path, wst_cli){
  db <- dbConnect(RSQLite::SQLite(), db_path)
  t <- dbReadTable(db, tname)
  t_sf <- st_as_sf(t, coords = c("lon", "lat"), crs = 4326)
  wst_sf <- st_as_sf(wst_cli, coords = c("lon", "lat"), crs = 4326)
  ##Finding wst_id of nearest station
  t$wst_id <- wst_cli[st_nearest_feature(t_sf, wst_sf),"id"]
  dbWriteTable(db, tname, t, overwrite = TRUE)
  dbDisconnect(db)
  return(print(paste(tname, "updated with wst_id and rewritten into database.")))
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


# Cleaning data ----------------------------------------------------------------

#' Function to clean outliers outside by some standard deviations
#'
#' @param df dataframe with columns c("Station", "DATE", "Variables", "Values", "Source").
#' @param times_sd numeric value representing multiplication factor for standard deviation. 
#' Values outside mean - sd X times_sd, mean + sd X times_sd are identified as outliers. 
#' Optional with default value 3.  
#' @return list of two dataframes. *newdf* dataframe contains dataframe cleaned from outliers.
#' *dropped*  dataframe contains data, which was removed from *newdf* dataframe.
#' @importFrom dplyr mutate %>% group_by summarise left_join
#' @importFrom lubridate month 
#' @export
#'
#' @examples
#' temp_path <- system.file("extdata", "calibration_data.xlsx", package = "svatools")
#' cal_data <- load_template(temp_path)
#' lst <- clean_outliers(cal_data$data)
#' ##Looking at data to be removed
#' print(head(lst$dropped))
#' ##Updating data
#' cal_data$data <- lst$newdf

clean_outliers <- function(df, times_sd = 3){
  ##Calculating monthly min, max values
  ref_values <- df %>% 
    mutate(Month = month(DATE)) %>% 
    group_by(Station, Variables, Month) %>% 
    summarise(mean = mean(Values), sd = sd(Values), .groups = 'drop') %>% 
    mutate(min_value = ifelse(mean - sd * times_sd < 0, 0, mean - sd * times_sd),
           max_value = mean + sd * times_sd)
  ##Identifying outliers and splitting into two dfs
  df <- df %>% 
    mutate(Month = month(DATE)) %>% 
    left_join(ref_values, by = c("Station", "Variables", "Month")) %>% 
    mutate(P = ifelse(Values >= min_value & Values <= max_value, T, F))
  
  return(list(newdf =  df[df$P == T, c("Station", "DATE", "Variables", "Values", "Source")], 
              dropped = df[df$P == F, c("Station", "DATE", "Variables", "Values", "Source")]))
}

#' Clean water quality data from most typical issues
#'
#' @param df dataframe with water quality data with  with columns c("Station", "DATE", "Variables", "Values", "Source").
#' @param zero_to_min numeric coefficient to zeros by min variable value X zero_to_min. Optional, default 1. 
#' @return cleaned dataframe
#' @export
#' @importFrom dplyr mutate left_join distinct filter summarise select group_by
#' @examples
#' temp_path <- system.file("extdata", "calibration_data.xlsx", package = "svatools")
#' cal_data <- load_template(temp_path)
#' cal_data$data <- clean_wq(cal_data$data)

clean_wq <- function(df, zero_to_min = 1){
  ##Cleaning common problems
  if(inherits(df$Values, "character")){
    df <- df %>% 
      mutate(Values = gsub(",", ".", Values)) %>% 
      mutate(Values = ifelse(grepl("<", Values), as.numeric(gsub("<", "", Values))/2, Values)) %>%  ##Below LOD/LOQ, leaving half value.
      mutate(Values = ifelse(grepl("^[0-9]", Values), Values, NA)) %>%  ##From here should be starting with number, if not - NA
      mutate(Values = as.numeric(Values)) 
  } else if (!inherits(df$Values, "numeric")){
    stop("'Values' column data type should be 'numeric' or 'character'!!!")
  }
  ##Negative to positive
  df[df$Values < 0, c("Values")] <- abs(df[df$Values < 0, c("Values")])
  ##Fixing N and P units
  df[df$Variables == "NH4", c("Variables", "Values")] <- c("N-NH4", df[df$Variables == "NH4", c("Values")] * 0.776490)
  df[df$Variables == "NO3", c("Variables", "Values")] <- c("N-NO3", df[df$Variables == "NO3", c("Values")] * 0.225897)
  df[df$Variables == "NO2", c("Variables", "Values")] <- c("N-NO2", df[df$Variables == "NO2", c("Values")] * 0.304457)
  df[df$Variables == "PO4", c("Variables", "Values")] <- c("P-PO4", df[df$Variables == "PO4", c("Values")] * 0.326138)
  ##Replacing zeros with min values, dropping NAs and duplicates
  df <- df %>%
    left_join(df %>%
                filter(Values > 0) %>%
                group_by(Variables) %>%
                summarise(min_value = min(Values), .groups = 'drop'), by = c("Variables")) %>%
    mutate(Values = ifelse(Values == 0 & !grepl("^Q", Variables), min_value * zero_to_min, Values)) %>%
    select(Station, DATE, Variables, Values, Source) %>%
    filter(!is.na(Values)) %>%
    distinct()
  return(df)
}

#' Replace empty PCP entry with 0
#'
#' @param df_to_correct dataframe to correct for PCP variable with "DATE" and "PCP" columns.
#' @param df_valid (optional) dataframe with valid data for PCP variable with "DATE" and "PCP" columns.
#'  Also should overlap with df_to_correct data.
#' @param value_to_zero (optional) numeric value of daily PCP. Only NA values can be set to 0 when df_valid PCP
#' values are below or equal to value_to_zero (default 0.2).
#' @importFrom dplyr left_join mutate case_when filter select
#' @return updated dataframe 
#' @export
#'
#' @examples
#' \dontrun{
#' met_lst$data$ID8$PCP <- add_missing_pcp_zero(met_lst$data$ID8$PCP, met_lst$data$ID9$PCP)
#' }

add_missing_pcp_zero <- function(df_to_correct, df_valid = NULL, value_to_zero = .2){
  df <- data.frame(DATE = seq.POSIXt(df_to_correct[[1,"DATE"]], df_to_correct[[nrow(df_to_correct),"DATE"]], by="day"))
  df <- left_join(df, df_to_correct, by = "DATE") 
  if(is.null(df_valid)){
    df[is.na(df)] <- 0
  } else {
    df <- df %>% 
      left_join(rename(df_valid, P = 2), by = "DATE") %>%
      mutate(PCP = case_when(is.na(PCP) & P <= value_to_zero ~ 0,
                             !is.na(PCP) ~ PCP)) %>% 
      filter(!is.na(PCP)) %>% 
      select(DATE, PCP)
  }
  return(df)
}
