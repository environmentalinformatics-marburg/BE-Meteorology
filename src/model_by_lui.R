#!/usr/bin/Rscript
#note: needs to be run from parent directory. e.g. ./src/model_by_lui.R

source("src/config.R")

library(CAST)
library(caret)

#prepare vegetation
vegetation_df <- read.csv(paste0(path_input, "Vegetation_HeaderData_2008-2016.csv"))

#prepare LUI
lui_csv = read.csv(paste0(path_input, "LUI_glob_sep_11.02.2019+185450.txt"), sep = "\t", dec=',')
year <- as.character(lui_csv$Std_procedure.year.)
year[year == "separately(2008)"] <- "2008"
year[year == "separately(2009)"] <- "2009"
year[year == "separately(2010)"] <- "2010"
year[year == "separately(2011)"] <- "2011"
year[year == "separately(2012)"] <- "2012"
year[year == "separately(2013)"] <- "2013"
year[year == "separately(2014)"] <- "2014"
year[year == "separately(2015)"] <- "2015"
year[year == "separately(2016)"] <- "2016"
lui_df = data.frame(EpPlotID = lui_csv$EP.Plotid, Year = year, lui_g = lui_csv$G_std, lui_m = lui_csv$M_std, lui_f = lui_csv$F_std, lui = lui_csv$LUI)
lui_df = lui_df[complete.cases(lui_df), ]

#!! filter by year !!
lui_df = lui_df[lui_df$Year == 2015, ]

write.csv(lui_df, paste0(path_output, "lui.csv"))


#prepare temperature
temperature_csv = read.csv(paste0(path_input, "temperature.csv"))
plotid = substring(temperature_csv$plotID, 1, 5)
temperature_df = data.frame(Useful_EP_PlotID = plotid, Year=temperature_csv$datetime, Ta_200 = temperature_csv$Ta_200, Ta_200_DTR = temperature_csv$Ta_200_DTR, Ta_200_growing_degree_days_10 = temperature_csv$Ta_200_growing_degree_days_10)
temperature_df = temperature_df[complete.cases(temperature_df), ]
write.csv(temperature_df, paste0(path_output, "temperature.csv"))

#prepare precipitation
precipitation_csv = read.csv(paste0(path_input, "precipitation_radolan.csv"))
precipitation_df = data.frame(Useful_EP_PlotID = precipitation_csv$plotID, Year=precipitation_csv$datetime, precipitation_radolan = precipitation_csv$precipitation_radolan, precipitation_radolan_rain_days = precipitation_csv$precipitation_radolan_rain_days)
precipitation_df = precipitation_df[complete.cases(precipitation_df), ]
write.csv(precipitation_df, paste0(path_output, "precipitation.csv"))

#prepare climate
climate_df <- merge(temperature_df, precipitation_df, by=c("Useful_EP_PlotID", "Year"))
write.csv(climate_df, paste0(path_output, "climate.csv"))

#merge data.frames
org_df <- merge(vegetation_df, lui_df, by=c("EpPlotID", "Year"))
org_df <- merge(org_df, climate_df, by=c("Useful_EP_PlotID", "Year"))
write.csv(org_df, paste0(path_output, "vegetation_lui_climate.csv"))
#str(org_df) # types of columns

pdf(paste0(path_output, "plots.pdf"), width=20, height=20)
pch =  "."
par(mfrow=c(4,4))

#par(mfrow=c(2,2))
plot(org_df$biomass_g ~ org_df$lui_g, pch = pch)
plot(org_df$biomass_g ~ org_df$lui_m, pch = pch)
plot(org_df$biomass_g ~ org_df$lui_f, pch = pch)
plot(org_df$biomass_g ~ org_df$lui, pch = pch)

#par(mfrow=c(2,2))
plot(org_df$doy_biomass ~ org_df$lui_g, pch = pch)
plot(org_df$doy_biomass ~ org_df$lui_m, pch = pch)
plot(org_df$doy_biomass ~ org_df$lui_f, pch = pch)
plot(org_df$doy_biomass ~ org_df$lui, pch = pch)

#par(mfrow=c(2,2))
plot(org_df$shannon ~ org_df$lui_g, pch = pch)
plot(org_df$shannon ~ org_df$lui_m, pch = pch)
plot(org_df$shannon ~ org_df$lui_f, pch = pch)
plot(org_df$shannon ~ org_df$lui, pch = pch)

#par(mfrow=c(2,2))
plot(org_df$number.herbs.with.shrubs ~ org_df$lui_g, pch = pch)
plot(org_df$number.herbs.with.shrubs ~ org_df$lui_m, pch = pch)
plot(org_df$number.herbs.with.shrubs ~ org_df$lui_f, pch = pch)
plot(org_df$number.herbs.with.shrubs ~ org_df$lui, pch = pch)

dev.off()

# LUI predictors
pred_lui_indices <- c("lui_g", "lui_m", "lui_f", "lui")
print(names(org_df[,pred_lui_indices]))

#climate predictors
pred_climate_indices <- c("Ta_200", "Ta_200_DTR", "Ta_200_growing_degree_days_10", "precipitation_radolan", "precipitation_radolan_rain_days")
print(names(org_df[,pred_climate_indices]))


# variables (number.herbs.with.shrubs to shannon)
rspvars = c(5:24)
# ! FOR TESTING !
#rspvars = c("biomass_g") # ! FOR TESTING !
rspvars = names(org_df[,rspvars]) # replace index numbers by names
print(names(org_df[,rspvars]))


k_fold = 10
kt_fold = 9
# ! FOR TESTING !
k_fold = 2   # ! FOR TESTING !
kt_fold = 10  # ! FOR TESTING !

# Loop over all variables (number.herbs to shannon)
for(rv in rspvars){
  print(rv)
  
  # remove NA rows in rv column
  df <- org_df[!is.na(org_df[rv]),]  
  
  # k-fold Leave-Location-Out cross validation (over non NA data)
  indp_cv = CAST::CreateSpacetimeFolds(df, spacevar = "Useful_EP_PlotID", timevar = NA, k = k_fold, seed = 31051974)
  
  # cross validation loop
  for(icv in seq(length(indp_cv$index))){
    print(icv)
    
    act_df = df[-indp_cv$indexOut[[icv]],]
    
    # kt-fold Leave-Location-Out cross validation for training
    cv_indicies = CAST::CreateSpacetimeFolds(act_df, spacevar = "Useful_EP_PlotID", timevar = NA, k = kt_fold, seed = 65451994)
    
    # Set train control
    trCntr = caret::trainControl(method="cv",
                                index = cv_indicies$index, 
                                indexOut = cv_indicies$indexOut,
                                returnResamp = "all",
                                #repeats = 1, 
                                verbose = FALSE)
    
    model_lui = CAST::ffs(predictors = act_df[, pred_lui_indices], 
                          response = act_df[, rv],  
                          metric = "RMSE",
                          method = "rf",
                          #method = "xgbLinear",
                          trControl = trCntr,
                          #tuneLength = tune_length,
                          #tuneGrid = lut$MTHD_DEF_LST[["rf"]]$tunegr
                          importance = TRUE)
    saveRDS(model_lui, file = paste0(path_output, "model_lui_", rv, "_", icv, ".rds"))
    print(model_lui)
    
    test_df = df[indp_cv$indexOut[[icv]],]
    m_lui_prediction = predict(model_lui, test_df)
    m_lui_res = test_df[, rv] - m_lui_prediction
    
    m_lui_df = data.frame(response = test_df[rv], m_lui_prediction = m_lui_prediction, m_lui_res = m_lui_res)
    saveRDS(m_lui_df, file = paste0(path_output, "m_lui_", rv, "_", icv, ".rds"))
    write.csv(m_lui_df, file = paste0(path_output, "m_lui_", rv, "_", icv, ".csv"))
    
    stat_lui_df = data.frame(rss = sum((test_df[, rv] - m_lui_prediction)^2), mse = ModelMetrics::mse(test_df[, rv], m_lui_prediction), rmse = ModelMetrics::rmse(test_df[, rv], m_lui_prediction), mae = ModelMetrics::mae(test_df[, rv], m_lui_prediction))
    write.csv(stat_lui_df, file = paste0(path_output, "stat_lui_", rv, "_", icv, ".csv"))
    
    data.frame(RSS=sum(m_lui_res^2), RSS=sum(m_lui_res^2))
    

    pred_df = act_df[, pred_climate_indices]
    pred_df$lui_pred = predict(model_lui, act_df)
    
    model_climate = CAST::ffs(predictors = pred_df, 
                          response = act_df[, rv],  
                          metric = "RMSE", 
                          method = "rf",
                          trControl = trCntr,
                          #tuneLength = tune_length,
                          #tuneGrid = lut$MTHD_DEF_LST[["rf"]]$tunegr
                          importance = TRUE)
    saveRDS(model_climate, file = paste0(path_output, "model_climate_", rv, "_", icv, ".rds"))
    print(model_climate)
    
    test_climate_df = test_df[, pred_climate_indices]
    test_climate_df$lui_pred = m_lui_prediction
    
    m_climate_prediction = predict(model_climate, test_climate_df)
    m_climate_res = test_df[, rv] - m_climate_prediction
    
    m_climate_df = data.frame(response = test_df[rv], m_lui_prediction = m_lui_prediction, m_lui_res = m_lui_res, m_climate_prediction = m_climate_prediction, m_climate_res = m_climate_res)
    saveRDS(m_climate_df, file = paste0(path_output, "m_climate_", rv, "_", icv, ".rds"))
    write.csv(m_climate_df, file = paste0(path_output, "m_climate_", rv, "_", icv, ".csv"))
    
    stat_climate_df = data.frame(rss = sum((test_df[, rv] - m_climate_prediction)^2), mse = ModelMetrics::mse(test_df[, rv], m_climate_prediction), rmse = ModelMetrics::rmse(test_df[, rv], m_climate_prediction), mae = ModelMetrics::mae(test_df[, rv], m_climate_prediction))
    write.csv(stat_climate_df, file = paste0(path_output, "stat_climate_", rv, "_", icv, ".csv"))
  }
}


