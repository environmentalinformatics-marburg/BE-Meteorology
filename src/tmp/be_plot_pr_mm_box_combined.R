be_plot_pr_mm_box_combined <- function(data, notch = FALSE, title){
  ggplot(data, aes(x = g_m, y = P_RT_NRT, fill = g_belc)) + 
    coord_cartesian(ylim = c(0, 200)) + 
    geom_boxplot(position = "dodge", notch = notch) +
    geom_vline(xintercept = seq(1.5, 12, 1), linetype = "dotted") +
    scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
                                 "#fb9a99", "#e31a1c","#DBD413")) + 
    
    #geom_vline(xintercept = seq(6.5, 6*12, 6), linetype = "dotted") +
    # stat_summary(fun.y=median, geom="line", aes(group =  g_belc,  colour  = g_belc))  + 
    labs(list(title = title, 
              x = "Month", y = "Precipitation (mm, monthly sum)", 
              fill = "Exploratory")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=20))
}