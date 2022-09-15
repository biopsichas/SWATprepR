
# Plotting time series ----------------------------------------------------

#' Plotting stations with few data points
#'
#' @param df dataframe with formatted data (Station, DATE, Variables and Values columns are needed)
#' @param drop_st optional parameter of character vector, which stations should be excluded for figure. 
#' Stations with a lot of data should be plotted with plot_one function. 
#' @return plotly object of interactive figure
#' @importFrom dplyr filter %>% 
#' @importFrom ggplot2 ggplot aes facet_wrap geom_line geom_point
#' @importFrom plotly ggplotly subplot 
#' @export
#'
#' @examples
#' ## plot_many(df, drop_st = c("4"))

plot_many <- function(df, drop_st = c()){
  ggplotly(ggplot(df %>% filter(!Station %in% drop_st), aes(x = DATE, y = Values, color = Station))+
                     geom_line()+
                     geom_point()+
                     facet_wrap(~Variables, scales = "free_y")) %>% 
    subplot(shareX = FALSE, shareY=FALSE) %>% 
    hide_show()
}

#' Plotting single, data rich station
#'
#' @param df dataframe with formatted data (Station, DATE, Variables and Values columns are needed)
#' @param station character indicating station, which should be selected for figure. 
#' @return plotly object of interactive figure.
#' @importFrom dplyr filter mutate group_by %>% group_map
#' @importFrom plotly plot_ly subplot 
#' @export
#'
#' @examples
#' ## plot_one(df, station = "4")

plot_one <- function(df, station){
  df %>%
    filter(Station == station) %>% 
    mutate(Variables = as.factor(Variables)) %>% 
    group_by(Variables) %>%
    group_map(~ plot_ly(data=., x = ~DATE, y = ~Values, color = ~Variables,  type = "scatter", mode =  "lines"), keep=TRUE) %>%
    subplot(nrows = 3, shareX = FALSE, shareY=FALSE) %>% 
    hide_show()
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
#' ##get_ts_fig("4", df)

get_ts_fig <- function(station, df){
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
#' ## plot_monthly_one(df, station = "4", drop_variables = c("Q", "Q1h", "Q2h"))
#' 

plot_monthly_one <- function(df, station, drop_variables = c()){
  df %>%
    filter(Station == station & !Variables %in% drop_variables) %>% 
    mutate(Variables = as.factor(Variables),
           Month = month(DATE)) %>% 
    group_by(Variables) %>% 
    group_map(~ plot_ly(data=., x = ~Month, y = ~Values, color = ~Variables,  type = "box"), keep=TRUE) %>%
    subplot(nrows = 3, shareX = FALSE, shareY=FALSE) %>% 
    hide_show()
}

#' Plot N min to NT regression in each month and fraction Nmin of NT
#'
#' @param df dataframe with formatted data (Station, DATE, Variables and Values columns are needed)
#' @param station character vector indicating station/s, which should be selected for figure.
#' @return list of two ggplot objects: regression - monthly regression plots for NT vs Nmin, 
#' fraction - monthly fraction values Nmin/NT
#' @importFrom dplyr filter select mutate group_by %>% summarise_all ungroup arrange
#' @importFrom tidyr drop_na pivot_wider
#' @importFrom lubridate month
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth facet_wrap theme_bw theme
#' element_text geom_bar
#' @importFrom ggpmisc stat_poly_eq
#' @export
#'
#' @examples
#' ## plot_Nmin_NT(df, station = c("4"))

plot_Nmin_NT <- function(df, station){
  ##Preparing df for regression
  df <- df %>%
    filter(Station %in% station & Variables %in% c("NT", "N-NO3", "N-NH4", "N-NO2")) %>% 
    select(DATE, Variables, Values) %>% 
    group_by(DATE, Variables) %>% 
    summarise_all(mean) %>% 
    pivot_wider(names_from = "Variables", values_from = "Values") %>% 
    mutate(Month = month(DATE, label = TRUE, abbr = FALSE, locale = "C"))%>% 
    drop_na() %>% 
    ungroup() %>% 
    select(-DATE) %>%
    mutate(Nmin = `N-NO3` + `N-NH4` + `N-NO2`) %>% 
    arrange(Month)
  
  ##Making regression plots
  plot_reg <- ggplot(df, aes(x = `Nmin`, y = NT)) +
    geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
    stat_poly_eq(formula = y~x, 
                 aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                 parse = TRUE) +         
    geom_point()+
    facet_wrap(~Month) +
    theme_bw()
  
  ##Fraction plots
  plot_frac <- ggplot(df %>% 
                        group_by(Month) %>% 
                        summarise_all(mean) %>% 
                        mutate(`Nmin fraction` = Nmin/NT), 
                      aes(x = Month, y = `Nmin fraction`)) +
    geom_bar(stat='identity')+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  return(list(regretion = plot_reg, fraction = plot_frac))
}

#' Plot Pmin fraction of PT 
#'
#' @param df dataframe with formatted data (Station, DATE, Variables and Values columns are needed)
#' @param station character vector indicating station/s, which should be selected for figure.
#' @return ggplot object for monthly Pmin/PT fraction box plot 
#' @importFrom dplyr filter select mutate group_by %>% summarise_all ungroup arrange
#' @importFrom tidyr drop_na pivot_wider
#' @importFrom lubridate month
#' @importFrom ggplot2 ggplot aes geom_boxplot xlab ylab theme_classic
#' @export
#'
#' @examples
#' ## plot_Pmin_PT(df, station = unique(station$ID))

plot_Pmin_PT <- function(df, station){
  df <- df %>% 
    filter(Station %in% station & Variables %in% c("P-PO4", "PT")) %>% 
    select(DATE, Variables, Values) %>% 
    group_by(DATE, Variables) %>% 
    summarise_all(mean) %>% 
    pivot_wider(names_from = "Variables", values_from = "Values") %>% 
    mutate(Pmin_TP_ratio = `P-PO4`/PT) %>% 
    mutate(Month = month(DATE)) %>% 
    drop_na(Pmin_TP_ratio) %>% 
    ungroup %>% 
    select(Month, Pmin_TP_ratio)
  
  ggplot(df , aes(x = as.factor(Month), y = Pmin_TP_ratio))+
    geom_boxplot()+
    xlab("Month")+
    ylab("Pmin to TP ratio")+
    theme_classic()
}

# Plotting maps -----------------------------------------------------------

#' Preparing interactive map of monitoring points with data inside
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
#' ##get_map(df, df_station, rch, shp)

get_map <- function(df, df_station, rch, shp){
  p_all <- lapply(df_station$ID, get_ts_fig, df = df)
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
}
