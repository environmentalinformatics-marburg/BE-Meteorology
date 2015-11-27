
path_source <- "D:/active/exploratorien/paper_be_meteorology/src/"
path_data <- "D:/active/exploratorien/data/"
path_output <- "D:/active/exploratorien/output/"

source(paste0(path_source, "be_plot_ta_mmltm.R"))
source(paste0(path_source, "be_plot_ta_dmltm.R"))

# READ DATA
df <- read.table(paste0(path_data, "plots.csv"), 
                 header = TRUE, sep = ",", dec = ".")


# DESEASON
# Deseason monthly air temperature data
df$aggid <- paste0(as.character(df$plotID), "_",
                   substr(as.character(df$datetime), 6, 7))
df_ta_mm <- aggregate(df$Ta_200, by = list(df$aggid), FUN = mean)
colnames(df_ta_mm) <- c("aggid", "df_ta_mm")
df <- merge(df, df_ta_mm, by = "aggid")
df$Ta_200_ds <- df$Ta_200 - df$df_ta_mm


# Deseason monthly rainfall data
df_p_mm <- aggregate(df$P_RT_NRT, by = list(df$aggid), FUN = mean, 
                     na.rm=TRUE)
colnames(df_p_mm) <- c("aggid", "df_p_mm")
df <- merge(df, df_p_mm, by = "aggid")
df$P_RT_NRT_ds <- df$P_RT_NRT - df$df_p_mm


# GROUPING
# Define grouping by month-year
df$gma <- paste0(substr(as.character(df$datetime), 6, 7), "-", 
                 substr(as.character(df$datetime), 1, 4))

# Define grouping by year
df$year <- substr(as.character(df$datetime), 1, 4)

# Define grouping by year
df$month <- substr(df$datetime, 6, 7)

# Define grouping by exploratory and land cover type
df$belc <- substr(as.character(df$plotID), 1, 3)


# PLOTS
# Create plots per exploratory and land cover type
belc_ta <- unique(df$belc[df$belc != "AET" & df$belc != "SET"])
belc_p <- c("AEG", "HEG", "SEG")

# Mean monthly air temperature over all years
lapply(belc_ta, function(x){
  be_plot_ta_mmltm(data = df[df$belc == x,], title = x)  
})

# Air temperature deviations from long term mean per month and year
lapply(belc_ta, function(x){
  be_plot_ta_dmltm(data = df[df$belc == x,], title = x)  
})

# Mean monthly air temperature and rainfall over all years (in one plot)
# Also so, dass immer die beiden Boxplots für T und R nebeneinander für jeden 
# Monat sind, praktisch wie die monatlichen Abweichungen, wo wir die einzelnen
# Jahre pro Monat immer nebeneinander gepackt haben.