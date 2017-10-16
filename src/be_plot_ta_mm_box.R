library(ggplot2)
library(RColorBrewer)

be_plot_ta_mm_box <- function(data, title){
  ggplot(data, aes(x = g_m, y = Ta_200)) + 
    geom_boxplot(position = "dodge", fill = "#e31a1c") +
    geom_vline(xintercept = seq(1.5, 12, 1), linetype = "dotted") +
    labs(list(title = title, 
              x = "Month", y = "Mean air temperature (C) 2009 to 2017")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

be_plot_ta_mm_box_png <- function(path_output, genData, station){
  title = paste0("Mean monthly air temperature over all years-",station)
  png(paste0(path_output, "temperature/be_plot_ta_mm_box/be_plot_ta_mm_box_", station,".png"),
      width     = 3880,
      height    = 4808,
      units     = "px",
      res       = 200,
      pointsize = 1
  )
  print({
    p <- be_plot_ta_mm_box(data = genData[genData$g_belc == station,], title )
  })
  dev.off()
}