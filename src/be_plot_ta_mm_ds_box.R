library(ggplot2)
library(RColorBrewer)

# Boxplot of deseasoned monthly mean Ta_200 values
be_plot_ta_mm_ds_box <- function(data, title){
  ggplot(data, aes(x = g_m, y = Ta_200_mm_ds, fill = g_a)) + 
    geom_boxplot(position = "dodge") +
    geom_vline(xintercept = seq(1.5, 12, 1), linetype = "dotted") +
    scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
                                 "#fb9a99", "#e31a1c","#DBD413", "#49E9FF", "#BA49FF", "#d1b5b5")) + 
    #   geom_vline(xintercept = seq(6.5, 6*12, 6), linetype = "dotted") +
    #   stat_summary(fun.y=median, geom="line", aes(group =  year,  colour  = year))  + 
    labs(list(title = title, 
              x = "Month", y = "Air temperature (C, deseasoned)", 
              fill = "Year")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
