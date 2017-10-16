be_plot_ta_mm_ds_box_combined <- function(data, notch = False, title){
  data <- data[!data$g_belc %in% c("AET", "HET", "SET"), ]
  ggplot(data, aes(x = g_a, y = Ta_200_mm_ds, fill = g_belc)) + 
    geom_boxplot(position = "dodge", notch = notch) +
    geom_vline(xintercept = seq(1.5, 7, 1), linetype = "dotted") +
    scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
                                 "#fb9a99", "#e31a1c","#DBD413", "#49E9FF", "#BA49FF", "#d1b5b5")) + 
    #geom_vline(xintercept = seq(6.5, 6*12, 6), linetype = "dotted") +
    #stat_summary(fun.y=median, geom="line", aes(group =  year,  colour  = year))  + 
    labs(list(title = title, 
              x = "Year", y = "Air temperature (C, deseasoned monthly mean)", 
              fill = "Exploratory")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=20))
}

be_plot_ta_mm_ds_box_combined_png <- function(path_output, genData, title){
  png(paste0(path_output, "temperature/be_plot_ta_mm_ds_box_combined.png"),
      width     = 3880,
      height    = 4808,
      units     = "px",
      res       = 200,
      pointsize = 1
  )
  print({
    p <- be_plot_ta_mm_ds_box_combined(genData, notch = TRUE, title )  
  })
  dev.off()
}