# PREPROCESS METEOROLOGICAL READ DATA
be_io_met_monthly <- function(filepath, ...){
  df_met <- read.table(filepath, header = TRUE, sep = ",", dec = ".")
  
  # Define grouping by month-year
  df_met$g_ma <- paste0(substr(as.character(df_met$datetime), 6, 7), "-", 
                       substr(as.character(df_met$datetime), 1, 4))
  
  # Define grouping by year
  df_met$g_a <- substr(as.character(df_met$datetime), 1, 4)
  
  # Define grouping by month
  df_met$g_m <- substr(df_met$datetime, 6, 7)
  
  # Define grouping by exploratory and land cover type
  df_met$g_belc <- substr(as.character(df_met$plotID), 1, 3)
  
  # Define grouping by exploratory, land cover type and month
  df_met$g_belcm <- paste0(substr(as.character(df_met$plotID), 1, 3),
                          "_", substr(as.character(df_met$datetime), 6, 7))
  
  # Define grouping by plot and month
  df_met$g_pm <- paste0(as.character(df_met$plotID), "_",
                      substr(as.character(df_met$datetime), 6, 7))
  
  # Define grouping by plot and year
  df_met$g_pa <- paste0(as.character(df_met$plotID), "_",
                      substr(as.character(df_met$datetime), 1, 4))
  return(df_met)
}
