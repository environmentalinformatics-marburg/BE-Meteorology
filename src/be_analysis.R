require(grid)
library(ggplot2)
library(reshape2)

path_source <- "D:/active/exploratorien/paper_be_meteorology/src/"
path_data <- "D:/active/exploratorien/data/"
path_output <- "D:/active/exploratorien/output/"

source(paste0(path_source, "be_plot_ta_ltmm.R"))
source(paste0(path_source, "be_plot_ta_mmds.R"))

# PREPROCESS METEOROLOGICAL READ DATA
df_met <- read.table(paste0(path_data, "plots.csv"), 
                     header = TRUE, sep = ",", dec = ".")

# GROUPING
# Define grouping by month-year
df_met$gma <- paste0(substr(as.character(df_met$datetime), 6, 7), "-", 
                     substr(as.character(df_met$datetime), 1, 4))

# Define grouping by year
df_met$year <- substr(as.character(df_met$datetime), 1, 4)

# Define grouping by month
df_met$month <- substr(df_met$datetime, 6, 7)

# Define grouping by exploratory and land cover type
df_met$belc <- substr(as.character(df_met$plotID), 1, 3)

# Define grouping by plot and month
df_met$pm <- paste0(as.character(df_met$plotID), "_",
                    substr(as.character(df_met$datetime), 6, 7))

# Define grouping by plot and month
df_met$pa <- paste0(as.character(df_met$plotID), "_",
                    substr(as.character(df_met$datetime), 1, 4))


# DESEASON
# Deseason monthly temperature data
df_ta_mm <- aggregate(df_met$Ta_200, by = list(df_met$pm), 
                      FUN = mean, na.rm = TRUE)
colnames(df_ta_mm) <- c("pm", "df_ta_mm")
df_met <- merge(df_met, df_ta_mm, by = "pm")
df_met$Ta_200_mm_ds <- df_met$Ta_200 - df_met$df_ta_mm

# Deseason annual air temperature
df_ta_am <- aggregate(df_met$Ta_200, by=list(df_met$pa), 
                      FUN=mean, na.rm = TRUE)
colnames(df_ta_am) <- c("pa", "df_ta_am")
df_met <- merge(df_met, df_ta_am, by = "pa")
df_met$Ta_200_am_ds <- df_met$Ta_200 - df_met$df_ta_am

# Deseason monthly rainfall data
df_p_mm <- aggregate(df_met$P_RT_NRT, by = list(df_met$pm), FUN = mean, 
                     na.rm=TRUE)
colnames(df_p_mm) <- c("pm", "df_p_mm")
df_met <- merge(df_met, df_p_mm, by = "pm")
df_met$P_RT_NRT_ds <- df_met$P_RT_NRT - df_met$df_p_mm


# PREPROCESS LUI
df_lui <- read.table(paste0(path_data, "lui.csv"), 
                     header = TRUE, sep = ";", dec = ",")
df_lui$Std_procedure.exploratory.<-NULL
colnames(df_lui)[colnames(df_lui) == "Std_procedure.year."] <- "year"
df_lui$year <- substr(df_lui$year,12,15)


# PREPROCESS LAND-USE DATA
df_lut <- read.table(paste0(path_data, "landuse_type.csv"), header = TRUE, sep = ";", dec = ",")
df_lut$notes<-NULL
df_lut <- melt(df_lut, id=(c("EP_Plotid","Explo", "initial_landuse_type", "new_landuse_type")))
colnames(df_lut) <- c("plotID", "initial_landuse_type","Explo","new_landuse_type","year","landuse")
df_lut$year <- substr(df_lut$year, 2,5)
df_lui_lut <- merge(df_lui, df_lut, by=c("plotID","year"))


# COMBINE DATASETS
df_lui_lut <- merge(df_lui, df_lut, by=c("plotID","year"), all.x = TRUE)
df <- merge(df_met, df_lui_lut, by=c("plotID","year"), all.x = TRUE)

df$year <- as.numeric(df$year)
df$month <- as.numeric(df$month)

head(df_met)



# PLOTS
# Create plots per exploratory and land cover type
belc_ta <- unique(df_met$belc[df_met$belc != "AET" & df_met$belc != "SET"])
belc_p <- c("AEG", "HEG", "SEG")

# Mean monthly air temperature over all years
lapply(belc_ta, function(x){
  be_plot_ta_ltmm(data = df_met[df_met$belc == x,], title = x)  
})

# Air temperature deviations from long term mean per month and year
lapply(belc_ta, function(x){
  be_plot_ta_mmds(data = df_met[df_met$belc == x,], title = x)  
})








library(ggplot2)
library(RColorBrewer)

be_plot_ta_mmds <- function(data, title){
  ggplot(data, aes(x = LUI_cut, y = Ta_200_mm_ds)) + geom_boxplot(notch=TRUE)
  ggplot(data, aes(x = M_std_cut, y = Ta_200_mm_ds)) + geom_boxplot(notch=TRUE)
  ggplot(data, aes(x = G_std_cut, y = Ta_200_mm_ds)) + geom_boxplot(notch=TRUE)
  ggplot(data, aes(x = F_std_cut, y = Ta_200_mm_ds)) + geom_boxplot(notch=TRUE)
  
  
  ggplot(data, aes(x = LUI, y = M_std, color = as.factor(year))) + geom_point()
  
  ggplot(data[data$plotID == "SEG20" & data$year < 2013,], aes(x = timestamp, y = Ta_200_mm_ds, group =1)) + geom_point() + geom_smooth(method=lm)
  
  
  
    geom_boxplot(position = "dodge") +
    geom_vline(xintercept = seq(1.5, 12, 1), linetype = "dotted") +
    scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
                                 "#fb9a99", "#e31a1c")) + 
    #   geom_vline(xintercept = seq(6.5, 6*12, 6), linetype = "dotted") +
    #   stat_summary(fun.y=median, geom="line", aes(group =  year,  colour  = year))  + 
    labs(list(title = title, 
              x = "Month", y = "Air temperature (°C, deseasoned)", 
              fill = "Year")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

data <- df[df_met$belc == "SEG",]
title <- "test"
data$LUI_cut <- cut(data$LUI, quantile(data$LUI, probs = seq(0, 1, 0.1), na.rm = TRUE))
data$LUI_cut <- cut(data$LUI, seq(0, 5, 1))
data$M_std_cut <- cut(data$M_std, seq(0, 5, 1))
data$G_std_cut <- cut(data$G_std, seq(0, 10, 1))
data$F_std_cut <- cut(data$F_std, seq(0, 7, 1))


summary(lm(Ta_200_mm_ds ~ timestamp, data = data[data$year < 2014,]))
