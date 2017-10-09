# PREPROCESS METEOROLOGICAL READ DATA
be_io_met_annual <- function(filepath, ...){
  df_met <- read.table(filepath, header = TRUE, sep = ",", dec = ".")
  
  # Define grouping by year
  df_met$g_a <- substr(as.character(df_met$datetime), 1, 4)
  
  # Define grouping by exploratory and land cover type
  df_met$g_belc <- substr(as.character(df_met$plot), 1, 3)
  
  # Define grouping by plot and year
  df_met$g_pa <- paste0(as.character(df_met$plot), "_",
                      substr(as.character(df_met$datetime), 1, 4))
  return(df_met)
}

# Define this function for a tubeDB data, wich is download per url
be_io_met_annual_urlData <- function(df_met){
  
  # Define col timestamp
  df_met$timestamp <- paste0(substr(as.character(df_met$datetime), 1, 4), substr(as.character(df_met$datetime), 6, 7))
  
  # Define grouping by year
  df_met$g_a <- substr(as.character(df_met$datetime), 1, 4)
  
  # Define grouping by exploratory and land cover type
  df_met$g_belc <- substr(as.character(df_met$plot), 1, 3)
  
  # Define grouping by plot and year
  df_met$g_pa <- paste0(as.character(df_met$plot), "_",
                        substr(as.character(df_met$datetime), 1, 4))
  return(df_met)
}