library(ggplot2)
library(RColorBrewer)

be_plot_pr_mm_box <- function(data, title){
    ggplot(data, aes(x = g_m, y = P_RT_NRT)) + 
      geom_boxplot(position = "dodge", fill = "#e31a1c") +
      geom_vline(xintercept = seq(1.5, 12, 1), linetype = "dotted") +
      labs(list(title = title, 
                x = "Month", y = "Precipitation (mm, mean) 2009 to 2017")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


