# PREPROCESS LUI
be_io_lui <- function(filepath, ...){
  df_lui <- read.table(filepath, header = TRUE, sep = ";", dec = ",")
  df_lui$Std_procedure.exploratory.<-NULL
  colnames(df_lui)[colnames(df_lui) == "Std_procedure.year."] <- "year"
  df_lui$year <- substr(df_lui$year,12,15)
  return(df_lui)
}
