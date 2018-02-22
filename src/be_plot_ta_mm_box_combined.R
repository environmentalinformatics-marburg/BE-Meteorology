be_plot_ta_mm_box_combined <- function(data, notch = FALSE, title){
  data <- data[!data$g_belc %in% c("AET", "HET", "SET"), ]
  ggplot(data, aes(x = g_m, y = Ta_200, fill = g_belc)) + 
    geom_boxplot(position = "dodge", notch = notch) +
    geom_vline(xintercept = seq(1.5, 12, 1), linetype = "dotted") +
    scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", 
                                 "#33a02c", "#fb9a99", "#e31a1c")) + 
    #geom_vline(xintercept = seq(6.5, 6*12, 6), linetype = "dotted") +
    #stat_summary(fun.y=median, geom="line", aes(group =  year,  colour  = year))  + 
    labs(list(title = title, 
              x = "Month", y = "Air temperature (°C, monthly mean)", 
              fill = "Exploratory")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=20))
}