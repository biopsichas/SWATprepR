
# Plotting time series ----------------------------------------------------

#' Plotting stations with few data points
#'
#' @param df dataframe with formatted data (Station, DATE, Variables and Values columns are needed)
#' @param drop_st option parameter of character vector, which stations should be excluded for figure. 
#' Stations with a lot of data should be plotted with plot_one function. 
#' @return plotly object of interactive figure
#' @importFrom dplyr filter %>% 
#' @importFrom ggplot2 ggplot aes facet_wrap geom_line geom_point
#' @importFrom plotly ggplotly subplot 
#' @export
#'
#' @examples
#' ## plot_many(df)

plot_many <- function(df, drop_st = c()){
  ggplotly(ggplot(df %>% filter(!Station %in% drop_st), aes(x = DATE, y = Values, color = Station))+
                     geom_line()+
                     geom_point()+
                     facet_wrap(~Variables, scales = "free_y")) %>% 
    subplot(shareX = FALSE, shareY=FALSE) %>% 
    hide_show_print_graph()
}