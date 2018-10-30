# chartit
#
#' Takes a  dataset and returns a chart from legal aid data
#
#' @param 

#
# This function filters a dataset and returns totals based on a given criteria

library(dplyr)

chartit <- function (tibble, volval, variable, swi, schF,catF,sub1F,sub2F,dteStart,xlab,ylab) {
  
  
  volval2 <- dplyr::enquo(volval)
  schF2 <- dplyr::enquo(schF)
  catF2 <- dplyr::enquo(catF)
  sub1F2 <- dplyr::enquo(sub1F)
  sub2F2 <- dplyr::enquo(sub2F)
  dteStart2 <- dplyr::enquo(dteStart)
  variable2 <- dplyr::enquo(variable)
  
  
  las2 <- filter(las_data,scheme %in% !!schF2,category %in% !!catF2,str_detect(sub_cat1,!!sub1F2),str_detect(sub_cat2,!!sub2F2)) %>%
    mutate(period = dmy(dte)) %>%
    filter(period > lubridate::dmy(!!dteStart2))
  las3 <- las2 %>%
    mutate(value = value/1000000)
  las4 <- las3 %>% 
    mutate(period = dmy(dte)-days(10))
  
  grp2 = dplyr::group_by(las4, period,fin_yr,fin_qtr, !!variable2) 
  chartit <- dplyr::summarize(grp2, volval3 = sum(!!volval2, na.rm = TRUE)) 
  

  ymax <- ceiling(1.1 * max(chartit$volval3, na.rm = TRUE))
  
    ggplot2::ggplot(data = chartit,
                  ggplot2::aes_string(x = "period", y = "volval3", variable=switch(swi, 'ca' = "category",'su' = "sub_cat1",'sc'="scheme"), color=switch(swi, 'ca' = "category",'su' = "sub_cat1",'sc'="scheme"))) + # Select the variables
    ggplot2::geom_line(size = 1.0) +
    ggplot2::labs(colour = "") + # Remove legend header
    ggplot2::scale_color_manual(values = c("#2C486E", "#DDE5F1",
                                           "#789AC8", "#3D6496", "#BCCDE4", "#5782BB", "#96B1D4", "#000000")) + # Set line colours
    ggplot2::ylab(ylab) +
    ggplot2::xlab(xlab) +
    ggplot2::scale_y_continuous(expand = c(0,0), labels = scales::comma) +
    #ggplot2::scale_x_yearqtr(format = "%YQ%q") +
    #ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b-%y" ) +
    ggplot2::scale_x_date(date_labels = "%Y" ) +
    ggplot2::expand_limits(y = c(0, ymax)) +
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


#TESTING
# library(dplyr)
# library(stringr)
# library(lubridate)
# library(ggplot2)
# library(mojrap)
# library(lasrap)
# 
# las_data <- s3tools::s3_path_to_full_df("alpha-legal-aid-statistics-team/lasq118r.csv")
# las_data <- tibble::as_tibble(las_data) 
# 
# filtCat <- c("ch - Litigator Graduated Fee Scheme","ch - Advocate Graduated Fee Scheme","ch - High Cost Crime")
# filtScheme <- c("Crime Higher")
# 
# filtCat <- c("lh - Matters completed","cr - Closed - cost met by LAA","cr - Closed - cost met by opponent")
# filtScheme <- c("Legal Help","Civil Representation")
# 
# filtSubCat1 <- c(" - ")
# filtSubCat2 <- c(" - ")
# startDte <- '01/04/2015'
# labelX <- "Period"
# labelY <- "Expenditure"
# plotit <- "sub_cat1"
# 
# chartit(las_data,value,sub_cat1,'su',filtScheme,filtCat,filtSubCat1,filtSubCat2,startDte,labelX,labelY)
# 
# chartit(las_data,volume,category,'ca',filtScheme,filtCat,filtSubCat1,filtSubCat2,startDte,labelX,labelY)
# 
# chartit(las_data,value,scheme,'sc',filtScheme,filtCat,filtSubCat1,filtSubCat2,startDte,labelX,labelY)
# 
# chartit(las_data,value,c("Civil Representation","Legal Help"),c("cr - Closed - cost met by LAA","cr - Closed - cost met by opponent","lh - Matters completed"),filtSubCat1,filtSubCat2,startDte,labelX,labelY)
# 
# 

