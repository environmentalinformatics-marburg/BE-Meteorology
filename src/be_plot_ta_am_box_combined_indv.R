be_plot_ta_am_box_combined_indv <- function(data, notch = False, title, plotIDs, belcs){
  data <- data[data$g_belc %in% belcs, ]
#   plots <- data[data$plotID %in% plotIDs, ]
#   for(i in plotIDs){
#     plots$g_belc[plots$plotID == i] <- i
#   }
  # data <- rbind(data, plots)
  ggplot(data = data, aes(x = g_a, y = Ta_200, fill = g_belc)) + 
    geom_boxplot(position = "dodge", notch = notch) +
    geom_vline(xintercept = seq(1.5, 7, 1), linetype = "dotted") +
    scale_fill_manual(values = c("#cccccc", "#969696")) + 
    geom_point(data = data[data$plotID %in% plotIDs, ], 
              aes(x = g_a, y = Ta_200, group = plotID, color = plotID),
              size = 8) + 
    scale_color_manual(values = c("#b2df8a", "#33a02c")) +
    labs(list(title = title, 
              x = "Year", y = "Air temperature (°C, annual means)", 
              fill = "Plots")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          text = element_text(size=16))
}
