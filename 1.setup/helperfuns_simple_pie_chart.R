library(webr)
library(ggplot2)
library(ggtext)
library(lemon)

### Simple pie chart

simple_piefun = function(df, x_var, col_values=c(GREEN3, RED3), donut_label_size = 9, pie_label_size = 9 ) {
  pie <- webr::PieDonut(df, aes_string(x_var)
                        , explode = 1
                        , explodeDonut = TRUE
                        , start = 120
                        , showRatioThreshold = 0
                        , ratioByGroup=FALSE
                        , showPieName=FALSE
                        , donutLabelSize = donut_label_size
                        , pieLabelSize = pie_label_size
  ) + 
    scale_fill_manual(values=col_values)
  
  pie
}