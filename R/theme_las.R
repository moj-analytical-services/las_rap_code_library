#' Theme of LAS bulletin graphs
#'
#' @description Set theme of LAS bulletin graphs
#'
#' @param base_size Size of text
#' @param base_line_size Size of line
#' @param flipped Whether to set flipped orientation
#' @param xticks Whether to include x-axis ticks
#' @param xlabel Whether to include title of x-axis
#' @param xangle Angle of x-axis labels
#' @param legend.position Position of legend in the graph
#
#' @examples
#' theme_las() gives the default theme of LAS graphs
#' theme_las(xangle = -45) rotates x-axis text -45 degrees
#' theme_las(legend.position = c(0.75,0.95)) places the legend at the top-right of the graph
#' 
#' @return Chart
#'
#' @export

theme_las <- function(base_size = 15, base_line_size = 0.5, flipped = FALSE, xticks = FALSE, xlabel = FALSE,
                      xangle = 90, legend.position = "none")
{
  
  # Base theme object
  base_elements <- theme_grey(base_size = base_size, base_line_size = base_line_size) +
    theme(line = element_line(colour = "grey80"),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.ticks.length = unit(0.5, "lines"),
          axis.title = element_blank(), 
          panel.background = element_blank(),
          axis.text.x = element_text(angle = xangle, size = 13, family = "Arial"),
          axis.text.y = element_text(angle = 0, size = 12, family = "Arial"),
          legend.direction = "horizontal",
          legend.position = legend.position,
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.key.size = unit(1.5, "lines"),
          legend.text=element_text(size=15),
          plot.title = element_text(face = "bold", angle = 0, vjust = 2, size = base_size),
          plot.subtitle = element_text(margin = margin(b = 10)),
          plot.caption = element_text(hjust = 0, margin = margin(t = 10)),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          panel.spacing = unit(1, "lines"),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold")
    )
  
  
  # To add for regular orientation
  regular_elements <- theme(
    plot.margin = unit(c(1, 1, 4, 1), "lines"),
    panel.grid.major.y = element_line(),
    axis.line.x = element_line()
  )
  
  # To add for flipped orientation
  flipped_elements <- theme(
    panel.grid.major.x = element_line(),
    axis.line.y = element_line(),
    axis.title = element_blank(),
    axis.text.y = element_text(margin = margin(r = 10))
  )
  
  # To add x axis ticks
  xticks_element <- theme(
    axis.ticks.x = element_line()
  )
  
  # To add x axis label
  xlabel_element <- theme(
    axis.title.x = element_text(margin = margin(t = 10))
  )
  
  # Build final theme
  if (!flipped) {
    t <- base_elements +
      regular_elements
  } else {
    t <- base_elements +
      flipped_elements
  }
  
  if (!flipped && xticks)
    t <- t +
    xticks_element
  
  if (xlabel)
    t <- t +
    xlabel_element
  
  t
}