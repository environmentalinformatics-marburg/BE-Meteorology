library(ggplot2)
library(RColorBrewer)

be_plot_p_dmltm <- function(data, title){
  ggplot(data, aes(x = month, y = P_RT_NRT_ds, fill = year)) + 
    geom_boxplot(position = "dodge") +
    geom_vline(xintercept = seq(1.5, 12, 1), linetype = "dotted") +
    scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
                                 "#fb9a99", "#e31a1c")) + 
    #   geom_vline(xintercept = seq(6.5, 6*12, 6), linetype = "dotted") +
    #   stat_summary(fun.y=median, geom="line", aes(group =  year,  colour  = year))  + 
    labs(list(title = title, 
              x = "Month", y = "Precipitation (mm, deseasoned)", 
              fill = "Year")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

