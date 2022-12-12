
# Plotting time series ----------------------------------------------------

#' Plotting calibration data
#'
#' @param df dataframe with formatted data (Station, DATE, Variables and Values columns are needed)
#' @param stations character vector listing stations, which should be selected for figure. 
#' @return plotly object of interactive figure.
#' @importFrom dplyr filter mutate group_by %>% group_map
#' @importFrom ggplot2 ggplot aes facet_wrap geom_line geom_point
#' @importFrom plotly plot_ly subplot ggplotly 
#' @export
#'
#' @examples
#' temp_path <- system.file("extdata", "calibration_data.xlsx", package = "svatools")
#' cal_data <- load_template(temp_path)
#' plot_cal_data(cal_data$data, c("1", "2", "3","10"))

plot_cal_data <- function(df, stations){
  if(length(stations) == 1){
    fig <- df %>%
      filter(Station %in% stations) %>% 
      mutate(Variables = as.factor(Variables)) %>% 
      group_by(Variables) %>%
      group_map(~ plot_ly(data=., x = ~DATE, y = ~Values, color = ~Variables, colors = "Set2", type = "scatter", mode =  "lines"), keep=TRUE) %>%
      subplot(nrows = 3, shareX = FALSE, shareY=FALSE) %>% 
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
#' temp_path <- system.file("extdata", "calibration_data.xlsx", package = "svatools")
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
#' @param df dataframe with formatted data (Station, DATE, Variables and Values columns are needed)
#' @param station character indicating station, which should be selected for figure. 
#' @param drop_variables optional parameter of character vector, which parameters should be excluded from figure. 
#' @return plotly object of interactive figure
#' @importFrom dplyr filter mutate group_by %>% group_map
#' @importFrom plotly plot_ly subplot 
#' @importFrom lubridate month
#' @export
#'
#' @examples
#' temp_path <- system.file("extdata", "calibration_data.xlsx", package = "svatools")
#' cal_data <- load_template(temp_path)
#' plot_monthly(cal_data$data, station = "4", drop_variables = c("Q"))
#' 

plot_monthly <- function(df, station, drop_variables = c()){
  df %>%
    filter(Station == station & !Variables %in% drop_variables) %>% 
    mutate(Variables = as.factor(Variables),
           Month = month(DATE)) %>% 
    group_by(Variables) %>% 
    group_map(~ plot_ly(data=., x = ~Month, y = ~Values, color = ~Variables,  type = "box"), keep=TRUE) %>%
    subplot(nrows = 3, shareX = FALSE, shareY=FALSE) %>% 
    hide_show()
}

#' Plot regression and fractions for each month between parts of nutrient and total
#'
#' @param df dataframe with formatted data (Station, DATE, Variables and Values columns are needed).
#' @param station character vector indicating station/s, which should be selected for figure.
#' @param total_var character vector for variable selected to represent total of certain variable.
#' @param min_vars character vector for variable/s selected to represent mineral or organic of certain variable.
#' @importFrom dplyr filter select mutate group_by %>% summarise_all ungroup arrange
#' @importFrom tidyr drop_na pivot_wider
#' @importFrom lubridate month
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth facet_wrap theme_bw theme
#' element_text geom_boxplot xlab ylab
#' @importFrom ggpmisc stat_poly_eq
#' @return list of two ggplot objects: regression - monthly regression plots for total vs some part 
#' (i.e. mineral or organic), fraction - monthly fraction values.
#' @export
#'
#' @examples
#' temp_path <- system.file("extdata", "calibration_data.xlsx", package = "svatools")
#' cal_data <- load_template(temp_path)
#' plot_fractions(cal_data$data, c("4"), c("NT"), c("N-NO3", "N-NH4", "N-NO2"))
#' plot_fractions(cal_data$data, c("4"), c("PT"), c("P-PO4"))

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
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
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
  
  return(list(regretion = plot_reg, fraction = plot_frac))
}

# Plotting maps -----------------------------------------------------------

#' Preparing interactive map of monitoring points
#'
#' @param df dataframe with formatted data (Station, DATE, Variables and Values columns are needed).
#' @param df_station dataframe with formatted station data (ID, geometry columns are needed).
#' @param rch sf dataframe for reaches (id, geometry columns are needed).
#' @param shp sf dataframe for basin (name, geometry columns are needed).
#' @return leaflet object of interactive map with monitoring data opening while pressing on points. 
#' @importFrom leaflet leaflet addProviderTiles addPolygons addPolylines addMarkers  
#' addLayersControl layersControlOptions
#' @importFrom leafpop addPopupGraphs
#' @export
#'
#' @examples
#' library(sf)
#' temp_path <- system.file("extdata", "calibration_data.xlsx", package = "svatools")
#' reach_path <- system.file("extdata", "GIS/reaches.shp", package = "svatools")
#' basin_path <- system.file("extdata", "GIS/basin.shp", package = "svatools")
#' cal_data <- load_template(temp_path, 4326)
#' reach <- st_transform(st_read(reach_path), 4326)
#' basin <-st_transform(st_read(basin_path), 4326)
#' plot_map(cal_data$data, cal_data$stations, reach, basin)


plot_map <- function(df, df_station, rch, shp){
  if (st_crs(rch)$input == "EPSG:4326" & 
      st_crs(shp)$input == "EPSG:4326" & 
      st_crs(df_station)$input == "EPSG:4326"){
    p_all <- lapply(df_station$ID, plot_ts_fig, df = df)
    leaflet() %>%
      addProviderTiles("OpenStreetMap", group = "OSM") %>%
      addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
      addProviderTiles("OpenTopoMap", group = "Topography") %>%
      addPolygons(data = shp, color = "blue", weight = 1,
                  opacity = 1.0, fillOpacity = 0.1, group = "Basin") %>%
      addPolylines(data = rch, color = "black", weight = 1, group = "Reaches") %>%
      addMarkers(
        data = df_station,
        layerId=~ID,
        label = ~as.character(paste("Station ID:", ID)),
        group = 'Stations') %>%
      addPopupGraphs(p_all, group = 'Stations', width = 600, height = 400) %>%
      addLayersControl(baseGroups = c("OSM", "Imagery", "Topography"),
                       overlayGroups = c("Basin", "Reaches", "Stations"),
                       options = layersControlOptions(collapsed = FALSE))
  } else {
    stop("Coordinate system of station, reach and basin data should be EPSG:4326!!!. Now it is not. Please correct it. ")
  }
}


# Plotting weather data ---------------------------------------------------

#' Prepare the plotly figure for weather data
#'
#' @param meteo_lst nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' @param par character marking weather variable to extract (i.e. "PCP", "SLR", etc).
#' @param period character describing, which time interval to display (default is "day", 
#' other examples are "week", "month", etc).
#' @param fn_summarize function to recalculate to time interval (default is "mean", other examples 
#' are "median", "sum", etc).
#' @importFrom lubridate floor_date
#' @importFrom plotly plot_ly layout
#' @importFrom dplyr bind_rows %>% rename summarize mutate left_join arrange
#' @importFrom sf st_drop_geometry
#' @return plotly figure object with displayed weather data
#' @export
#'
#' @examples 
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "svatools")
#' met_lst <- load_template(temp_path, 4326)
#' plot_weather(met_lst, "PCP", "month", "sum")

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

#' Plotting figure to comparing two datasets with weather data
#'
#' @param meteo_lst1 first nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' @param meteo_lst2 second nested list of lists with dataframes. 
#' Nested structure meteo_lst -> data -> Station ID -> Parameter -> Dataframe (DATE, PARAMETER).
#' @param par character marking weather variable to extract (i.e. "PCP", "SLR", etc).
#' @param period character describing, which time interval to display (default is "day", 
#' other examples are "week", "month", etc).
#' @param fn_summarize function to recalculate to time interval (default is "mean", other examples 
#' are "median", "sum", etc).
#' @param name_set1 character to name first dataset.
#' @param name_set2 character to name second dataset.
#' @importFrom plotly layout subplot
#' @return plotly figure object with displayed weather data for two datasets.
#' @export
#'
#' @examples
#' temp_path <- system.file("extdata", "weather_data.xlsx", package = "svatools")
#' met_lst1 <- load_template(temp_path, 4326)
#' temp_path <- system.file("extdata", "weather_data_raw.xlsx", package = "svatools")
#' met_lst2 <- load_template(temp_path, 4326)
#' plot_weather_compare(met_lst1, met_lst2, "PCP", "month", "mean", "clean", "raw")

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
