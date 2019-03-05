# library(scales)
# library(zoo)
# library(ggplot2)
# library(dplyr)
# library(knitr)
# library(stringr)
# library(tidyverse)
# library(lubridate)
# library(scales)
# 
# las_data <- s3tools::s3_path_to_full_df("alpha-legal-aid-statistics-team/lasq118r.csv")
# 
# las_data <- tibble::as_tibble(las_data) 
# 
# filt <- c("ch - Litigator Graduated Fee Scheme","ch - Advocate Graduated Fee Scheme","ch - High Cost Crime")
# #think about counting this vector to use as divisor below
# 
# las2 <- filter(las_data,scheme=="Crime Higher",category %in% filt) %>%
#   mutate(period = dmy(dte)) %>%
#   filter(period > lubridate::dmy('01/04/2014'))
# 
# las4 <- las2 %>% 
#   mutate(period = dmy(dte)-days(10))
# 
# 
# grp2 = dplyr::group_by(las4, period,fin_yr,fin_qtr, category)
# 
# chartit <- dplyr::summarize(grp2, volval = sum(value, na.rm = TRUE)/1000000)
# 
# 
# 
# las3 <- las4 %>% 
#   mutate(year = as.numeric(year(period)))
# las3 <- transform(las3, period = as.yearqtr(period))
# grp3 = dplyr::group_by(las3, period,fin_yr,fin_qtr, category,year)
# chartit3 <- dplyr::summarize(grp3, volval = sum(value, na.rm = TRUE)/1000000)
# 
# df <- chartit3
# 
# 
# 
# chartit2 <- dplyr::summarize(grp2, volval = sum(volume, na.rm = TRUE)/1000000)
# 
# 
# 
# 
# las13 <- transform(las3, period = as.yearqtr(period))
# grp13 = dplyr::group_by(las13, period,fin_yr,fin_qtr, category,year)
# dfvol <- dplyr::summarize(grp13, volval = sum(volume, na.rm = TRUE))
# 
# 
# 
# #create variables for use below
# title <- "Test graph"                                                         # chart title (delete if no title needed)
# ylabel <- "y axis"                                      # label for y-axis
# ybreak <- 25                                                            # spacing between breaks for y-axis
# ymax <- max(df$volval) + ybreak                                  # highest y-axis value (currently highest charted value + the width between each break)
# vyearlocation <- -0.04*ymax                                               # vertical location for year lables (currently 3% of ymax below zero)
# numyears <- length(unique(df$year))                                   # number of years given in chart
# hyearlocation <- unique(df$year)+((as.numeric(table(df$year))/3)-1)/8 # horizontal locations for year labels (currently set to centre of the year regardless of number of quarters elapsed)
# hquarterlocation <- seq(min(df$year), 
#                         max(as.numeric(unique(df$period))), 0.25) # horizontal locations for quarter labels
# vquarterlocation <- -0.01*ymax                                            # vertical location for quarter lables (currently 1% of ymax below zero)
# quarterlabels <- c("Q1","Q2","Q3","Q4","Q1","Q2","Q3","Q4","Q1","Q2","Q3","Q4","Q1","Q2","Q3","Q4","Q1")                               # list of quarters to be labelled on x-axis
# yearbreaks <- seq(min(df$year)+7/8, 
#                   max(as.numeric(unique(df$period))), 1)          # list of numeric values half way between Q4 and Q1 for each year charted
# fylabel <- unique(df$fin_yr)
# 
# 
# 
# 
# 
# graphtestval <- 
#   
#   ggplot(data = df,
#        aes(x = period, y = volval, variable=category, color=category)) + # Select the variables
#   geom_line(size = 1.0) +
#   labs(colour = "") + # Remove legend header
#   scale_color_manual(values = c("#2C486E","#789AC8", "#DDE5F1")) + # Set line colours
#   ylab("Crime Expenditure (£m)") +
#   #geom_vline(xintercept = as.numeric(df$period[yday(df$period) == 1]), color = "lightgrey") +
#   #geom_segment(x=2014, xend=2014, y=0, yend=100, colour="lightgrey", show.legend = FALSE) +
#   #scale_y_continuous(expand = c(0,0), labels = scales::comma) +
#   scale_y_continuous(minor_breaks = seq(0, ymax, ybreak),
#                      breaks = seq(0, ymax, ybreak), 
#                      expand = c(0, 0),
#                      limits = c(vyearlocation, ymax)) +                                             # y-axis breaks and limits
#   #scale_x_yearqtr(breaks = seq(from = min(df$period), to = max(df$period), by = 0.25),
#      #             format = "%YQ%q") +
#   #scale_x_date(date_breaks = "3 months", date_labels = "%b-%y" ) +
#   scale_x_yearqtr(expand = c(0.01,0))+ 
#   #scale_x_yearqtr(format = "%Y", n = numyears, expand = c(0.01,0),
#   #                minor_breaks = hyearlocation,
#   #                breaks = hyearlocation)+ # year placement on x-axis
#   annotate(geom = "text", x = hquarterlocation, y = vquarterlocation, label = quarterlabels, 
#            size = 3, vjust=1) +
#   annotate(geom = "text", x = hyearlocation, y = vyearlocation, label = fylabel, 
#            size = 3, vjust=1) +
#   # quarter placement on x-axis
#   expand_limits(y = c(0, 100)) +
#   theme_bw() + # Change theme to get rid of grey background
#   theme(#axis.text.x = element_text(angle = 0),
#         axis.text.x = element_blank(),
#         panel.border = element_blank(),
#         panel.grid.major.y = element_line(color = "lightgrey", linetype = "solid"),
#         #panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
#         panel.grid.major.x = element_blank(),
#         text = element_text(size = 10, family = "Arial"),
#         panel.grid.minor.x = element_blank(),
#         #axis.line = element_line(color = "grey", linetype = "solid"),
#         axis.title.x = element_blank(),
#         axis.ticks = element_blank(),
#         legend.position = "bottom")
# 
# # Plot lines for year seperators
# for (i in yearbreaks) {
#   graphtestval <- graphtestval + geom_segment(x=i, xend=i, y=vyearlocation, yend=0, colour="lightgrey", show.legend = FALSE)
# }
# 
# 
# # Allow for annotation outside of plot area (prevent Qs from cutting off) and plot chart
# fig2 <- ggplot_gtable(ggplot_build(graphtestval))
# fig2$layout$clip[fig2$layout$name == "panel"] <- "off"
# grid::grid.draw(fig2)
# 
# 
# 
# 
# graphtestvol <- 
#   
#   ggplot(data = dfvol,
#          aes(x = period, y = volval, variable=category, color=category)) + # Select the variables
#   geom_line(size = 1.0) +
#   labs(colour = "") + # Remove legend header
#   scale_color_manual(values = c("#2C486E","#789AC8", "#DDE5F1")) + # Set line colours
#   ylab("Crime Volume") +
#   #geom_vline(xintercept = as.numeric(df$period[yday(df$period) == 1]), color = "lightgrey") +
#   #geom_segment(x=2014, xend=2014, y=0, yend=100, colour="lightgrey", show.legend = FALSE) +
#   #scale_y_continuous(expand = c(0,0), labels = scales::comma) +
#   scale_y_continuous(minor_breaks = seq(0, ymax, ybreak),
#                      breaks = seq(0, ymax, ybreak), 
#                      expand = c(0, 0),
#                      limits = c(vyearlocation, ymax)) +                                             # y-axis breaks and limits
#   #scale_x_yearqtr(breaks = seq(from = min(df$period), to = max(df$period), by = 0.25),
#   #             format = "%YQ%q") +
#   #scale_x_date(date_breaks = "3 months", date_labels = "%b-%y" ) +
#   scale_x_yearqtr(expand = c(0.01,0))+ 
#   #scale_x_yearqtr(format = "%Y", n = numyears, expand = c(0.01,0),
#   #                minor_breaks = hyearlocation,
#   #                breaks = hyearlocation)+ # year placement on x-axis
#   annotate(geom = "text", x = hquarterlocation, y = vquarterlocation, label = quarterlabels, 
#            size = 3, vjust=1) +
#   annotate(geom = "text", x = hyearlocation, y = vyearlocation, label = fylabel, 
#            size = 3, vjust=1) +
#   # quarter placement on x-axis
#   expand_limits(y = c(0, 100)) +
#   theme_bw() + # Change theme to get rid of grey background
#   theme(#axis.text.x = element_text(angle = 0),
#     axis.text.x = element_blank(),
#     panel.border = element_blank(),
#     panel.grid.major.y = element_line(color = "lightgrey", linetype = "solid"),
#     #panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
#     panel.grid.major.x = element_blank(),
#     text = element_text(size = 10, family = "Arial"),
#     panel.grid.minor.x = element_blank(),
#     #axis.line = element_line(color = "grey", linetype = "solid"),
#     axis.title.x = element_blank(),
#     axis.ticks = element_blank(),
#     legend.position = "bottom")
# 
# # Plot lines for year seperators
# for (i in yearbreaks) {
#   graphtestvol <- graphtestvol + geom_segment(x=i, xend=i, y=vyearlocation, yend=0, colour="lightgrey", show.legend = FALSE)
# }
# 
# 
# # Allow for annotation outside of plot area (prevent Qs from cutting off) and plot chart
# fig1 <- ggplot_gtable(ggplot_build(graphtestvol))
# fig1$layout$clip[fig1$layout$name == "panel"] <- "off"
# grid::grid.draw(fig1)
# 
# 
# 
