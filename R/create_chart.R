# figure1
#
#' This function plots time series data using MoJ styling.
#' @param timeseries A dataframe
#' @seealso \code{\link{update_pop_series}}


#
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



figure1 <- function(timeseries){

timeseries <- timeseries %>% filter(Date >= (max(Date) - lubridate::years(20)))

ggplot2::ggplot(data = timeseries,
         ggplot2::aes(x = Date, y = Total, group = Variable,
                      colour = Variable)) + # Select the variables
         ggplot2::geom_line(size = 1.2) + # Makes it a line chart
         ggplot2::labs(colour = "") + # Remove legend header
         ggplot2::scale_color_manual(values = c("#0000cc", "#00ccff",
                                                "#0066ff")) + # Set line colours
         ggplot2::ylab("Prison population") +
         ggplot2::scale_y_continuous(minor_breaks = seq(0, 100000, 100000),
         breaks = seq(0, 100000, 10000), expand = c(0, 0),
                  labels = scales::comma) + # Format y-axis scale and numbers
         ggplot2::scale_x_date(breaks = timeseries$Date[seq(1, 400, 24)],
                 labels = format(timeseries$Date[seq(1, 400, 24)], "%b %Y"),
                 expand = c(0, 0),
                 limits = c(min(timeseries$Date - 1),
                            as.Date(max(timeseries$Date + 400)))) + # CHANGE THIS IF X AXIS CUTS OFF
         ggplot2::expand_limits(y = c(0, 90000)) +
         ggplot2::theme_bw() + # Change theme to get rid of grey background
         ggplot2::theme(axis.text.x = element_text(angle = 0),
         panel.border = element_blank(),
         panel.grid.major.y = element_line(color = "grey", linetype = "dashed"),
         panel.grid.major.x = element_blank(),
                              text = element_text(size = 12, family = "Arial"),
         panel.grid.minor.x = element_blank(),
         axis.line = element_line(color = "grey", linetype = "solid"),
         axis.title.x = element_blank(),
         legend.position = "bottom")

}
