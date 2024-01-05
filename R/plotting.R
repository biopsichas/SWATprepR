
# Plotting time series ----------------------------------------------------

#' Plot Calibration Data
#'
#' This function generates an interactive plot of calibration data based on the 
#' provided data.
#'
#' @param df Dataframe with formatted data (requires "Station", "DATE", "
#' Variables", and "Values" columns). Data can be loaded with 
#' \code{\link{load_template}} function using 'xlsx' template file. 
#' @param stations Character vector listing stations to be selected for the figure.
#' @param variables (Optional) Character vector specifying which parameters 
#' should be included in the figure. Default \code{variables = NULL}, all available
#' variables will be plotted.
#' @importFrom dplyr filter mutate group_by %>% group_map
#' @importFrom ggplot2 ggplot aes facet_wrap geom_line geom_point
#' @importFrom plotly plot_ly subplot ggplotly 
#' @importFrom comprehenr to_vec
#' @return Plotly object of an interactive figure.
#' @export
#'
#' @examples
#' # Example using calibration data
#' temp_path <- system.file("extdata", "calibration_data.xlsx", package = "SWATprepR")
#' cal_data <- load_template(temp_path)
#' plot_cal_data(cal_data$data, stations = c("1", "2", "3", "10"), variables = c("PT", "NT"))
#' @keywords plotting

plot_cal_data <- function(df, stations, variables = NULL) {
  if (is.null(variables)) variables = unique(df$Variables)
  df = subset(df, Station %in% stations & Variables %in% variables)
  if (nrow(df)==0) stop("Non existing station or variable")
  df_gaps = data.frame (matrix(nrow = 0, ncol = length(colnames (df))))
  colnames(df_gaps) = colnames (df)
  
  for (i in 1:length(stations)) {
    ss = df %>% 
      subset (Station == stations[i]) %>% 
      group_by(Variables) %>% 
      summarise(max = max(DATE), min = min (DATE), 
                d_amount = max(DATE)-min(DATE)+1)
    
    hh = data.frame (
      DATE = to_vec(for (j in 1:length(ss$Variables))  as.character(seq(ss$min[j], ss$max[j], by="days"))) %>%
        as.Date() %>%
        as.POSIXlt(), 
      Station = stations[i], 
      Variables = rep (ss$Variables,ss$d_amount))
    
    df_gaps = rbind(df_gaps, hh)
  }
  
  df = merge(df,df_gaps, by.x = c ("DATE", "Station", "Variables"), 
             by.y = c("DATE",  "Station", "Variables"), all.x=T,all.y=T)
  
  if(length(stations) == 1){
    num_rows = ifelse (length(ss$Variables)< 3, length(ss$Variables), 3)
    fig <- df %>%
      filter(Station %in% stations) %>% 
      mutate(Variables = as.factor(Variables)) %>% 
      group_by(Variables) %>%
      group_map(~ plot_ly(data=., x = ~DATE, y = ~Values, color = ~Variables, colors = "Set2", type = "scatter", mode =  "lines+markers"), .keep=TRUE) %>%
      subplot(nrows = num_rows, shareX = FALSE, shareY=FALSE) %>% 
      hide_show()
  } else if (length(stations) > 1){
    fig <- ggplotly(ggplot(df %>% filter(Station %in% stations), aes(x = DATE, y = Values, color = Station))+
                      geom_line()+
                      geom_point()+
                      facet_wrap(~Variables, scales = "free_y")+
                      theme_bw()) %>% 
      subplot(shareX = FALSE, shareY=FALSE) %>% 
      hide_show()
  } else {
    stop("No stations selected!!!")
  }
  return(fig)
}

#' Function to prepare static time series figure for single station
#'
#' @param station character indicating station, which should be selected for figure. 
#' @param df dataframe with formatted data (Station, DATE, Variables and Values columns are needed)
#' @return ggplot object of static figure.
#' @importFrom dplyr filter group_by %>% n
#' @importFrom ggplot2 ggplot aes geom_line geom_point ggtitle facet_wrap theme_minimal theme 
#' theme_void geom_text xlab element_text element_blank
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' temp_path <- system.file("extdata", "calibration_data.xlsx", package = "SWATprepR")
#' cal_data <- load_template(temp_path)
#' plot_ts_fig("4", cal_data$data)
#' }

plot_ts_fig <- function(station, df){
  if(station %in% df$Station){
    ggplot(df %>% filter(Station == station) %>% group_by(Variables) %>% filter(n() > 1), aes(x = DATE, y = Values))+
      geom_point()+
      geom_line(color="gray50", size=0.1, linetype = "dotted")+
      ggtitle(paste0("Station ID: ", station))+
      facet_wrap(~Variables, scales="free_y")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 35, vjust = 0.5, hjust=1),
            axis.title.x = element_blank())
  } else {
    ggplot() +
      theme_void() +
      geom_text(aes(0,0,label='No data for this point')) +
      xlab(NULL) 
  }
}

# Plotting monthly averages -----------------------------------------------

#' Aggregating values of single station to interactive monthly box plots 
#'
#' @param df Dataframe with formatted data (Station, DATE, Variables and Values columns are needed).
#' Data can be loaded with \code{\link{load_template}} function using 'xlsx' template file. 
#' @param station Character, indicating station ID, which should be selected for figure. 
#' @param variables (optional) Character vector, variables/s, which should be in figure. 
#' Default is \code{variables = NULL}, which means all variables will be in figure.
#' @return plotly object of interactive figure
#' @importFrom dplyr filter mutate group_by %>% group_map
#' @importFrom plotly plot_ly subplot 
#' @importFrom plotly layout
#' @importFrom lubridate month
#' @export
#'
#' @examples
#' temp_path <- system.file("extdata", "calibration_data.xlsx", package = "SWATprepR")
#' cal_data <- load_template(temp_path)
#' plot_monthly(cal_data$data, station = "4")
#' @keywords plotting

plot_monthly <- function(df, station, 
                         variables = NULL){
  if (is.null(variables)) variables = levels(as.factor(df$Variables))
  if (nrow(subset(df, Station %in% station & Variables %in% variables))==0) stop("Non existing station or variable")
  df = subset(df, Station == station & Variables %in% variables)
   ss = df %>% 
    group_by(Variables)%>%
     summarise()
  num_rows = ifelse (length(ss$Variables)< 3, length(ss$Variables), 3)
  n_colors = ifelse (length(ss$Variables)< 3, 3, length(ss$Variables))
  df %>% 
    mutate(Variables = as.factor(Variables),
           Month = month(DATE)) %>% 
    group_by(Variables) %>% 
    group_map(~ plot_ly(data=., x = ~Month, y = ~Values, 
                        color = ~Variables, 
                        colors = RColorBrewer::brewer.pal(n_colors, "Set2")[1:length(ss$Variables)],
                        type = "box") %>%
          layout(xaxis = list(
          tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
          tickangle = 0)
                ), 
          .keep=TRUE)  %>%
    subplot(nrows = num_rows, shareX = FALSE, shareY=FALSE) %>% 
    hide_show()
}

#' Plot Regression and Fractions for Each Month
#'
#' This function generates two sets of plots for monthly regression and fractions
#' between nutrient parts and the total.
#'
#' @param df Dataframe with formatted data (requires "Station", "DATE", 
#' "Variables", and "Values" columns). Data can be loaded with 
#' \code{\link{load_template}} function using 'xlsx' template file. 
#' @param station Character vector indicating the station/s to be selected for 
#' the figure.
#' @param total_var Character vector for the variable selected to represent the 
#' total of a certain nutrient.
#' @param min_vars Character vector for the variable/s selected to represent the 
#' mineral or organic fraction of a certain nutrient.
#' @importFrom dplyr filter select mutate group_by %>% summarise_all ungroup arrange
#' @importFrom tidyr drop_na pivot_wider
#' @importFrom lubridate month
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth facet_wrap theme_bw theme
#' element_text geom_boxplot xlab ylab after_stat
#' @importFrom ggpmisc stat_poly_eq
#' @return A list of two ggplot objects: "regression" for monthly regression 
#' plots and "fraction" for monthly fraction values.
#' @export
#'
#' @examples
#' # Example using calibration data
#' temp_path <- system.file("extdata", "calibration_data.xlsx", package = "SWATprepR")
#' cal_data <- load_template(temp_path)
#' plot_fractions(cal_data$data, c("4"), c("PT"), c("P-PO4"))
#' @keywords plotting

plot_fractions <- function(df, station, total_var, min_vars){
  ##Preparing df for regression
  df <- df %>%
    filter(Station %in% c(station) & Variables %in% c(total_var, min_vars)) %>% 
    select(DATE, Variables, Values) %>% 
    group_by(DATE, Variables) %>% 
    summarise_all(mean) %>% 
    pivot_wider(names_from = "Variables", values_from = "Values") %>% 
    mutate(Month = month(DATE, label = TRUE, abbr = FALSE, locale = "C"))%>% 
    drop_na() %>% 
    ungroup() %>% 
    select(-DATE) %>%
    mutate(Min = rowSums(.[min_vars]),
           Tot = rowSums(.[total_var])) %>% 
    mutate(Frac = Min/Tot) %>% 
    select(-all_of(c(total_var, min_vars))) %>% 
    arrange(Month)
  
  
  ##Making regression plots
  plot_reg <- ggplot(df, aes(x = Min, y = Tot)) +
    geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
    stat_poly_eq(formula = y~x, 
                 aes(label = paste(ggplot2::after_stat(eq.label),ggplot2::after_stat(rr.label), sep = "~~~")), 
                 parse = TRUE, 
                 size = 2.4) +         
    geom_point()+
    facet_wrap(~Month) +
    xlab(paste(min_vars, collapse = ' + ')) +
    ylab(total_var) +
    theme_bw()
  
  ##Fraction plot
  plot_frac <- ggplot(df , aes(x = Month, y = Frac))+
    geom_boxplot()+
    xlab("Month")+
    ylab(paste0(paste(min_vars, collapse = ' + '), " fraction to ", total_var))+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  return(list(regression = plot_reg, fraction = plot_frac))
}

# Plotting maps -----------------------------------------------------------

#' Prepare Interactive Map of Monitoring Points
#'
#' This function generates an interactive map with monitoring points, 
#' allowing users to click on points to view data.
#'
#' @param df Dataframe with formatted data (requires "Station", "DATE", 
#' "Variables", and "Values" columns). Data can be loaded with 
#' \code{\link{load_template}} function using 'xlsx' template file. 
#' @param df_station Dataframe with formatted station data (requires "ID" and 
#' "geometry" columns).
#' @param rch sf dataframe for reaches (requires "id" and "geometry" columns).
#' @param shp sf dataframe for basin (requires "name" and "geometry" columns).
#' @importFrom leaflet leaflet addProviderTiles addPolygons addPolylines addMarkers  
#' addLayersControl layersControlOptions
#' @importFrom leafpop addPopupGraphs
#' @return Leaflet object of an interactive map with monitoring data displayed 
#' when points are clicked.
#' @export
#'
#' @examples
#' library(sf)
#' 
#' # Example using calibration data
#' temp_path <- system.file("extdata", "calibration_data.xlsx", package = "SWATprepR")
#' reach_path <- system.file("extdata", "GIS/reaches.shp", package = "SWATprepR")
#' basin_path <- system.file("extdata", "GIS/basin.shp", package = "SWATprepR")
#' cal_data <- load_template(temp_path, 4326)
#' reach <- st_transform(st_read(reach_path), 4326)
#' basin <- st_transform(st_read(basin_path), 4326)
#' plot_map(cal_data$data, cal_data$stations, reach, basin)
#' @keywords plotting

plot_map <- function(df, df_station, rch, shp){
  if (st_crs(rch)$input == "EPSG:4326" & 
      st_crs(shp)$input == "EPSG:4326" & 
      st_crs(df_station)$input == "EPSG:4326"){
    if(!"id" %in% colnames(rch)){rch$id <- NA}
    if(!"type" %in% colnames(rch)){rch$type <- NA}
    p_all <- lapply(df_station$ID, plot_ts_fig, df = df)
    leaflet() %>%
      addProviderTiles("OpenStreetMap", group = "OSM") %>%
      addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
      addProviderTiles("OpenTopoMap", group = "Topography") %>%
      addPolygons(data = shp, color = "blue", weight = 1,
                  opacity = 1.0, fillOpacity = 0.1, group = "Basin") %>%
      addPolylines(data = rch, color = "black", weight = 2, opacity = 0.5, group = "Reaches", 
                   label = ~paste0("Reach ID: ", rch$id, ", type: ", rch$type)) %>%
      addMarkers(
        data = df_station,
        layerId=~ID,
        label = ~as.character(paste("Station ID:", ID)),
        group = 'Stations') %>%
      addPopupGraphs(p_all, group = 'Stations', width = 600, height = 400) %>%
      addLayersControl(baseGroups = c("OSM", "Imagery", "Topography"),
                       overlayGroups = c("Basin", "Reaches", "Stations"),
                       position = "bottomleft",
                       options = layersControlOptions(collapsed = TRUE))
  } else {
    stop("Coordinate system of station, reach and basin data should be EPSG:4326!!!. Now it is not. Please correct it. ")
  }
}


# Plotting weather data ---------------------------------------------------

#' Prepare Plotly Figure for Weather Data
#'
#' This function generates a Plotly figure for weather data visualization based 
#' on user-defined parameters.
#'
#' @param meteo_lst A nested list with dataframes. 
#'   Nested structure: \code{meteo_lst -> data -> Station ID -> Parameter -> 
#'   Dataframe (DATE, PARAMETER)}, 
#'   \code{meteo_lst -> stations -> Dataframe (ID, Name, Elevation, Source, 
#'   geometry, Long, Lat)}. \cr\cr
#'   meteo_lst can be created using \code{\link{load_template}} function using 
#'   'xlsx' template file or it could to be created with \code{\link{load_swat_weather}}
#'   function loading information from SWAT+ model setup weather files.
#' @param par Character, the weather variable to extract (e.g., "PCP", "SLR").
#' @param period (optional) Character describing the time interval to display. 
#' Default \code{period = "day"}, other examples are "week", "month", "year".
#' See [lubridate::floor_date](https://www.rdocumentation.org/packages/lubridate/versions/1.3.3/topics/floor_date) for details.
#' @param fn_summarize (optional) Function to recalculate to the specified time interval. 
#' Default \code{fn_summarize ="mean"}, other examples are "median", "sum".
#' See [dplyr::summarise](https://dplyr.tidyverse.org/reference/summarise.html) for details.
#' @importFrom lubridate floor_date
#' @importFrom plotly plot_ly layout
#' @importFrom dplyr bind_rows %>% rename summarize mutate left_join arrange
#' @importFrom sf st_drop_geometry
#' @return Plotly figure object with displayed weather data.
#' @export
#'
#' @examples 
#' library(SWATprepR)
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "SWATprepR")
#' met_lst <- load_template(temp_path, 4326)
#' plot_weather(met_lst, "PCP", "month", "sum")
#' @keywords plotting

plot_weather <- function(meteo_lst, par, period = "day", fn_summarize = "mean"){
  station <- meteo_lst$stations %>% 
    st_drop_geometry() %>% 
    mutate(Name = paste0(str_to_title(Name), " (", Source, ")")) %>% 
    select(ID, Name, Lat, Long) 
  meteo_lst <- meteo_lst$data
  ##Extracting data for the stations from the list 
  df_r <- NULL
  for (n in names(meteo_lst)){
    if(par %in% names(meteo_lst[n][[1]])){
      df <- meteo_lst[n][[1]][par][[1]] %>% mutate(Stations = n)
      if (is.null(df_r)){
        df_r <- df
      } else {
        df_r <- bind_rows(df_r, df)
      }
    }
  }
  ##Aggregating data by time step
  df_r$DATE<- floor_date(df_r$DATE, period)
  df_r <- df_r %>%
    rename(Values = 2) %>% 
    group_by(Stations, DATE) %>%
    summarize(Values = get(fn_summarize)(Values))
  if(length(meteo_lst) == 1){
    df_r <- df_r %>%
      mutate(Name = Stations)
    legend_name <- "Names"
  } else {
    df_r <- df_r %>%
      left_join(station, by = c("Stations" = "ID"))
    legend_name <- "Stations"
  }
  ##Plotting
  if (length(unique(df_r$Stations))>1){
    fig <- plot_ly(df_r, x=~DATE, y=~Values, color=~Name, type = 'scatter', mode = 'lines', connectgaps = FALSE) 
  } else {
    fig <- plot_ly(df_r %>% arrange(DATE), x=~DATE, y=~Values, name = unique(df_r$Name), type = 'scatter', mode = 'lines', connectgaps = FALSE) %>% 
      layout(showlegend = TRUE) 
  }
  
  return(fig %>% layout(title = paste(par, "parameter"), yaxis = list(title = "Values"), 
                        legend = list(title=list(text=paste('<b>', legend_name, '</b>')))) %>%
           hide_show())
}

#' Plotting Figure to Compare Two Datasets with Weather Data
#'
#' This function generates a Plotly figure to compare weather data between two datasets based on user-defined parameters.
#'
#' @param meteo_lst1 First nested list with dataframes. 
#'   Nested structure: \code{meteo_lst -> data -> Station ID -> Parameter -> 
#'   Dataframe (DATE, PARAMETER)}, 
#'   \code{meteo_lst -> stations -> Dataframe (ID, Name, Elevation, Source, 
#'   geometry, Long, Lat)}. \cr\cr
#'   meteo_lst can be created using \code{\link{load_template}} function using 
#'   'xlsx' template file or it could to be created with \code{\link{load_swat_weather}}
#'   function loading information from SWAT+ model setup weather files.
#' @param meteo_lst2 Second nested list with dataframes. Same structure as \code{meteo_lst1}.
#' @param par Character vector, weather variable to extract (e.g., "PCP", "SLR").
#' @param period (optional) Character, the time interval to display. Default 
#' \code{period = "day"}, , other examples are "week", "month", "year".
#' See [lubridate::floor_date](https://www.rdocumentation.org/packages/lubridate/versions/1.3.3/topics/floor_date) for details.
#' @param fn_summarize (optional) Function to recalculate to the specified time interval. 
#' Default \code{fn_summarize ="mean"}, other examples are "median", "sum".
#' See [dplyr::summarise](https://dplyr.tidyverse.org/reference/summarise.html) for details.
#' @param name_set1 (optional) Character, to name the first dataset. Default 
#' \code{name_set1 = "dataset 1"}.
#' @param name_set2 (optional) Character, to name the second dataset. Default 
#' \code{name_set2 = "dataset 2"}.
#' @importFrom plotly layout subplot
#' @return Plotly figure object with displayed weather data for two datasets.
#' @export
#'
#' @examples
#' ##Loading data
#' temp_path1 <- system.file("extdata", "weather_data.xlsx", package = "SWATprepR")
#' met_lst1 <- load_template(temp_path1)
#' temp_path2 <- system.file("extdata", "weather_data_raw.xlsx", package = "SWATprepR")
#' met_lst2 <- load_template(temp_path2)
#' ##Plotting
#' plot_weather_compare(met_lst1, met_lst2, "PCP", "month", "mean", "clean", "raw")
#' @keywords plotting

plot_weather_compare <- function(meteo_lst1, meteo_lst2, par, period = "day", fn_summarize = "mean", 
                                 name_set1 = "dataset 1", name_set2 = "dataset 2"){
  ##Getting figure for set1 data
  fig1 <- plot_weather(meteo_lst1, par, period, fn_summarize) %>% 
    layout(showlegend = TRUE,
           annotations = list( 
             list(x = 0.52 , y = 1.05, text = paste(str_to_title(name_set1), "data"), showarrow = F, xref='paper', yref='paper')))
  ##Getting figure for set2 data
  fig2 <- plot_weather(meteo_lst2, par, period, fn_summarize) %>% 
    layout(showlegend = TRUE,
           annotations = list( 
             list(x = 0.52 , y = 1.05, text = paste(str_to_title(name_set2), "data"), showarrow = F, xref='paper', yref='paper')))
  
  ##Adding both figures
  fig <- subplot(fig1, fig2, shareX = TRUE, nrows = 2, margin = 0.07) %>% 
    layout(title = paste(par, period, fn_summarize)) %>% 
    hide_show()
  return(fig)
}

#' Plot WGN Parameters Comparison
#'
#' This function generates a ggplot figure for comparing WGN parameters between two datasets for specified stations.
#'
#' @param meteo_lst1 First nested list with dataframes. 
#'   Nested structure: \code{meteo_lst -> data -> Station ID -> Parameter -> 
#'   Dataframe (DATE, PARAMETER)}, 
#'   \code{meteo_lst -> stations -> Dataframe (ID, Name, Elevation, Source, 
#'   geometry, Long, Lat)}. \cr\cr
#'   meteo_lst can be created using \code{\link{load_template}} function using 
#'   'xlsx' template file or it could to be created with \code{\link{load_swat_weather}}
#'   function loading information from SWAT+ model setup weather files.
#' @param meteo_lst2 Second nested list with dataframes. Same structure as 
#' \code{meteo_lst1}. 
#' @param station1 Character, ID of one station in the first list selected for 
#' comparison (example "ID1").
#' @param station2 Character, ID of one station in the second list selected for 
#' comparison (example "ID1").
#' @param type1 (optional) Character, naming of the first dataset 
#' (example "measured"). Default \code{type1 = "set 1"}.
#' @param type2 (optional) Character, naming of the second dataset 
#' (example "netCDF"). Default \code{type2 = "set 2"}.
#' @param title (optional) Character, information to be added in the figure title.
#' Default \code{title = "comparison"}.
#' @importFrom dplyr %>% filter select mutate bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_bar facet_wrap ggtitle aes theme_minimal labs
#' @return ggplot figure of bar plots.
#' @export
#'
#' @examples
#' ##Loading data
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "SWATprepR")
#' met_lst <- load_template(temp_path)
#' ##Plotting
#' plot_wgn_comparison(met_lst, met_lst, "ID9", "ID2", "Samszyce", "Glebokie", "comparison")
#' @keywords plotting

plot_wgn_comparison <- function(meteo_lst1, meteo_lst2, station1, station2, type1 = "set 1", type2 = "set 2", title = "comparison"){
  ##Checking inputs
  stopifnot(is.list(meteo_lst1))
  stopifnot(is.list(meteo_lst2))
  stopifnot(is.character(station1))
  stopifnot(is.character(station2))
  stopifnot(is.character(type1))
  stopifnot(is.character(type2))
  stopifnot(is.character(title))
  ##Filter second based on min and max dates of first list
  min_date <- as.POSIXct(get_dates(meteo_lst1)$min_date, "%Y-%m-%d", tz = "UTC")
  max_date <- as.POSIXct(get_dates(meteo_lst1)$max_date, "%Y-%m-%d", tz = "UTC")
  for (id in names(meteo_lst2$data)){
    for (p in names(meteo_lst2$data[[id]])){
      meteo_lst2$data[[id]][[p]] <- meteo_lst2$data[[id]][[p]] %>% 
        filter(DATE >= min_date & DATE <= max_date)
    }
  }
  ##In case no variable, fill with same variable from the closest station
  meteo_lst1$data <- fill_with_closest(meteo_lst1)
  meteo_lst2$data <- fill_with_closest(meteo_lst2)
  ##Filtering for different stations
  meteo_lst1$data <-  meteo_lst1$data[names(meteo_lst1$data) %in% c(station1)]
  meteo_lst1$stations <- meteo_lst1$stations[meteo_lst1$stations$ID %in% c(station1),]
  
  meteo_lst2$data <- meteo_lst2$data[names(meteo_lst2$data) %in% c(station2)]
  meteo_lst2$stations <- meteo_lst2$stations[meteo_lst2$stations$ID %in% c(station2),]
  ##Preparing wng value dataframes
  df1 <- prepare_wgn(meteo_lst1)$wgn_data %>% 
    select(-c("id", "wgn_id")) %>% 
    pivot_longer(-month, names_to = "par", values_to = "values") %>% 
    mutate(source = type1)
  
  df2 <- prepare_wgn(meteo_lst2)$wgn_data %>% 
    select(-c("id", "wgn_id")) %>% 
    pivot_longer(-month, names_to = "par", values_to = "values") %>% 
    mutate(source = type2)
  
  ##Bar plot
  fig <- ggplot(bind_rows(df1,df2), aes(x = as.factor(month), y = values, fill = source))+
    geom_bar(stat='identity', position = "dodge")+
    facet_wrap(~par, scales = "free")+
    labs(title=paste(type1, "vs.", type2,  title), x ="Months", y = "Values")+
    theme_minimal()
  
  return(fig)
}
