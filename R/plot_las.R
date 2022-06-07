#' Produces chart for bulletin
#'
#' @description Produce chart for bulletin.
#'
#' @param df Dataframe
#' @param yr_or_qtr Variable from x-axis can either be year or quarter
#' @param volval Variable from y-axis, which is usually with volume or value
#' @param ylabel Labelling the y-axis
#' @param ymax Maximum value on the y-axis
#' @param intervals Space between y-axis labels
#' @param legend.rows Number of rows in the legend
#' @param label_position Position of label
#' @param nudge_x Where to nudge the graph horizontally
#' @param nudge_y Where to nudge the graph vertically
#
#' @examples
# plot_las(df = OE, yr_or_qtr = "fin_yr", volval = "value", ylabel = "Expenditure (£m)", ymax = 3500, intervals = 500,
# label_position = "2010-11", nudge_x = 3, nudge_y = 1000) 
# produces the chart for Overall Expenditure (figure 1 in the bulletin)
# 
# plot_las(df = lgfs, volval = "value", ylabel = "Expenditure (£m)", ymax = 80, intervals = 10,
# label_position = "2016q1", nudge_x = 3, nudge_y = 10)
# produces the chart for LGFS expenditure (figure 4c)
#' 
#' @return Chart
#'
#' @export

plot_las <- function(df, yr_or_qtr = "qtr", volval, ylabel, ymax, intervals, legend.rows = dplyr::n_distinct(df$category),
                     label_position, nudge_x = 5, nudge_y = 50){
  
  # Colour scheme
  las_colour <- unname(c(mojchart::moj_palette(6, "muted2"), mojchart::moj_palette(6, "vibrant2")), 
                       mojchart::moj_colours("mojblack", "mojpurple", "mojbrightpurple", "mojdarkpurple"))
  
  # Format y-axis labels
  y_format <- if(deparse(substitute(df)) == "ecf_outcome"){
    scales::label_percent(accuracy = 1)
  }
  else{scales::label_comma(accuracy = 1)}
  
  # ECF stacked bar chart or other graphs
  graph <- if(deparse(substitute(df)) == "ecf_category"){
    
    df$category <- factor(df$category, levels = c("Other", "Family", "Immigration", "Inquest"))
    
    ggplot2::ggplot(data = df,
           aes(x = get(yr_or_qtr), y = get(volval), fill=category)) + # Select the variables
      
      ggplot2::geom_bar(stat = 'identity', width = 0.5) + # Set line colours
      
      ggplot2::scale_fill_manual(values = las_colour[1:dplyr::n_distinct(df$category)],
                        guide=guide_legend(nrow=legend.rows, byrow= T, title = ""))
    
  }
  
  ## Label lines of Overall Expenditure graphs
  else if (deparse(substitute(df)) == "OE"){
    ggplot2::ggplot(data = df,
           aes(x = get(yr_or_qtr), y = get(volval), color=category, group = category, label = category)) + # Select the variables
      
      ggplot2::geom_line(size = 0.7) +
      
      ggplot2::scale_color_manual(values = las_colour[1:dplyr::n_distinct(df$category)])  + 
      
      ggrepel::geom_text_repel(data = filter(df, fin_yr == label_position), 
                      aes(label = category),
                      #color = "black",
                      fontface = "bold",
                      size = 5,
                      nudge_x = nudge_x,
                      nudge_y = nudge_y,
                      segment.linetype = 2,
                      na.rm = TRUE)
  }
  
  ## Label lines on all other graphs
  else{
    ggplot2::ggplot(data = df,
           aes(x = get(yr_or_qtr), y = get(volval), color=category, group = category, label = category)) + # Select the variables
      
      ggplot2::geom_line(size = 0.7) +
      
      ggplot2::scale_color_manual(values = las_colour[1:dplyr::n_distinct(df$category)]) +
      
      ggrepel::geom_text_repel(data = filter(df, qtr == label_position), 
                      aes(label = category),
                      #color = "black",
                      fontface = "bold",
                      size = 5,
                      nudge_x = nudge_x,
                      nudge_y = nudge_y,
                      segment.linetype = 2,
                      na.rm = TRUE)
  }
  
  # Customize graph
  graph <- graph +
    ggplot2::ggtitle(ylabel) +                                      # Modify formatting of axis
    ggplot2::coord_cartesian(ylim = c(0, ymax), expand = FALSE, clip = "off") +
    ggplot2::scale_y_continuous(labels = y_format, breaks = seq(0, ymax, by = intervals)) +
    ggplot2::guides(colour = guide_legend(nrow = legend.rows))
  
  # Produces either Overall Expenditure graph or other graphs
  if(deparse(substitute(df)) == "OE"){
    
    # Produces Overall Expenditure graph
    graph +
      ggplot2::geom_segment(
        data = tibble(),
        mapping = aes(x= c(1:dplyr::n_distinct(OE$fin_yr)),
                      y=rep(30, dplyr::n_distinct(OE$fin_yr)),
                      xend= c(1:dplyr::n_distinct(OE$fin_yr)),
                      yend=rep(-30, dplyr::n_distinct(OE$fin_yr))),
        colour = "grey80",
        inherit.aes = FALSE
      )
  }
  
  else if(deparse(substitute(df)) == "ecf_category"){
    # Create x-axis labels for quarters
    xaxis <- df %>%
      dplyr::select(fin_yr, qtr) %>%
      dplyr::distinct() %>%
      dplyr::arrange(fin_yr, qtr) %>%
      dplyr::mutate(xlabel = dplyr::case_when(substring(qtr, 6) == 2 ~ fin_yr,
                                              TRUE ~ ""))
    
    all_qtrs <- c("", "", "2011-12", "", "", "", "2012-13", "", "", "", "2013-14", "", "", "", "2014-15", "",       
                  "", "", "2015-16", "", "", "", "2016-17", "", "", "", "2017-18", "", "", "", "2018-19",
                  "", "", "", "2019-20", "", "", "", "2020-21", "", "", "2021-22")
    
    xlabel <- all_qtrs[(length(all_qtrs) + 1 - dplyr::n_distinct(df$qtr)) : length(all_qtrs)]
    
    data_ends <- df %>% dplyr::filter(qtr == "2021q2")
    
    # Produces other graphs
    graph +
      
      ggplot2::annotate(geom = "text", x = c(1:dplyr::n_distinct(xaxis$qtr)), y = -ymax/6.25, 
               label = xlabel, 
               size = 4) +
      
      ggplot2::scale_x_discrete(labels = qtr_label(unique(xaxis$qtr))) +
      
      ggplot2::geom_segment(
        data = tibble(),
        mapping = aes(x=seq(0, dplyr::n_distinct(xaxis$qtr), 4),
                      y=rep(0, dplyr::n_distinct(xaxis$fin_yr)),
                      xend=seq(0, dplyr::n_distinct(xaxis$qtr), 4),
                      yend=rep(-ymax/5, dplyr::n_distinct(xaxis$fin_yr))),
        colour = "grey80",
        inherit.aes = FALSE
      )
  }
  
  else{
    # Create x-axis labels for quarters
    xaxis <- df %>%
      dplyr::select(fin_yr, qtr) %>%
      dplyr::distinct() %>%
      dplyr::arrange(fin_yr, qtr) %>%
      dplyr::mutate(xlabel = dplyr::case_when(substring(qtr, 6) == 2 ~ fin_yr,
                                              TRUE ~ ""))
    
    all_qtrs <- c("", "", "2011-12", "", "", "", "2012-13", "", "", "", "2013-14", "", "", "", "2014-15", "",       
                  "", "", "2015-16", "", "", "", "2016-17", "", "", "", "2017-18", "", "", "", "2018-19",
                  "", "", "", "2019-20", "", "", "", "2020-21", "", "", "2021-22", "")
    
    xlabel <- all_qtrs[(length(all_qtrs) + 1 - dplyr::n_distinct(df$qtr)) : length(all_qtrs)]
    
    data_ends <- df %>% dplyr::filter(qtr == "2021q3")
    
    # Produces other graphs
    graph +
      ggplot2::annotate(geom = "text", x = c(1:dplyr::n_distinct(xaxis$qtr)), y = -ymax/6.25, 
               label = xlabel, 
               size = 4) +
      ggplot2::scale_x_discrete(labels = qtr_label(unique(xaxis$qtr))) +
      ggplot2::geom_segment(
        data = tibble(),
        mapping = aes(x=seq(1, dplyr::n_distinct(xaxis$qtr), 4),
                      y=rep(0, dplyr::n_distinct(xaxis$fin_yr)),
                      xend=seq(1, dplyr::n_distinct(xaxis$qtr), 4),
                      yend=rep(-ymax/5, dplyr::n_distinct(xaxis$fin_yr))),
        colour = "grey80",
        inherit.aes = FALSE
      )
  }
  
}