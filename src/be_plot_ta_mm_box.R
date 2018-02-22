library(ggplot2)
library(RColorBrewer)

be_plot_ta_mm_box <- function(data, title){
  ggplot(data, aes(x = g_m, y = Ta_200)) + 
    geom_boxplot(position = "dodge", fill = "#e31a1c") +
    geom_vline(xintercept = seq(1.5, 12, 1), linetype = "dotted") +
    labs(list(title = title, 
              x = "Month", y = "Mean air temperature (°C) 2009 to 2015")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
