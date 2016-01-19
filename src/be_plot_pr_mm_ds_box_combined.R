be_plot_pr_mm_ds_box_combined <- function(data, title){
  ggplot(data, aes(x = g_a, y = P_RT_NRT_ms_ds, fill = g_belc)) + 
    geom_boxplot(position = "dodge") +
    geom_vline(xintercept = seq(1.5, 7, 1), linetype = "dotted") +
    scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", 
                                 "#33a02c", "#fb9a99", "#e31a1c")) + 
    #geom_vline(xintercept = seq(6.5, 6*12, 6), linetype = "dotted") +
    #stat_summary(fun.y=median, geom="line", aes(group =  year,  colour  = year))  + 
    labs(list(title = title, 
              x = "Year", y = "Precipitation (mm, deseasoned)", 
              fill = "Exploratory")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}