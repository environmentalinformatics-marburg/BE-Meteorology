be_plot_ta_mm_ds_box_combined_indv <- function(data, notch = False, title, plotIDs, belcs){
  data <- data[data$g_belc %in% belcs, ]
  plots <- data[data$plotID %in% plotIDs, ]
  for(i in plotIDs){
    plots$g_belc[plots$plotID == i] <- i
  }
  data <- rbind(data, plots)
  ggplot(data, aes(x =g_a, y = Ta_200_mm_ds, fill = g_belc)) + 
    geom_boxplot(position = "dodge", notch = notch) +
    geom_vline(xintercept = seq(1.5, 7, 1), linetype = "dotted") +
    scale_fill_manual(values = c("#cccccc", "#b2df8a", 
                                 "#969696", "#33a02c")) + 
    #geom_vline(xintercept = seq(6.5, 6*12, 6), linetype = "dotted") +
    #stat_summary(fun.y=median, geom="line", aes(group =  year,  colour  = year))  + 
    labs(list(title = title, 
              x = "Year", y = "Air temperature (C, deseasoned monthly mean)", 
              fill = "Plots")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=20))
}


