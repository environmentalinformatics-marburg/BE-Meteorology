
library(ggplot2)
library(RColorBrewer)


be_plot_ta_pr_mmltm <- function(data, station, year, path_output){
  png(paste0(path_output, paste0("be_plot_ta_200_PR_", station,"-", year ,".png")),
      width     = 3880,
      height    = 4808,
      units     = "px",
      res       = 200,
      # pointsize = 1
  )
  df <- subset(data,  substr( as.character(data$plotID),  1, 3)==station)
  df_ta_mm <- aggregate(df$Ta_200, by = list(df$aggid,substr(as.character(df$datetime), 1, 4)), FUN = mean)
  df_pr_mm <- aggregate(df$P_RT_NRT, by = list(df$aggid,substr(as.character(df$datetime), 1, 4)), FUN = mean)
  df_ta_mm <- cbind(df_ta_mm,df_pr_mm$x)
  colnames(df_ta_mm ) <- c ("month", "year","Ta_200", "PR")
  df_ta_mm$month<-substr(as.character(df_ta_mm$month),7,8)
  if(year != "")
    df_ta_mm<-subset(df_ta_mm, df_ta_mm$year==year)
  df_ta_mm.mlt_t <- melt(df_ta_mm)
  p1 <-   ggplot(df_ta_mm.mlt_t, aes(x = month, y = value, fill = variable)) + 
    geom_boxplot(position = "dodge") +
    geom_vline(xintercept = seq(1.5, 12, 1), linetype = "dotted") +
    scale_fill_manual(values = c( "#e31a1c","#a6cee3")) + 
    labs(list(title = year, 
              x = "Month", y = "Air temperature (C, deseasoned) & Precipitation 2009 to 2015", 
              fill = "Parameter")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(p1)
  dev.off()
}