#!/usr/bin/Rscript
# note: needs to be run from parent directory. e.g. ./src/model_by_lui.R

source("src/config.R")

library(CAST)
library(caret)
library(doParallel)
library(ModelMetrics)

# https://stackoverflow.com/questions/40901445/function-to-calculate-r2-r-squared-in-r
rsq <- function (x, y) cor(x, y) ^ 2

cl <- makeCluster(3)
registerDoParallel(cl) # start parallel

# prepare vegetation
vegetation_df <- read.csv(paste0(path_input, "Vegetation_HeaderData_2008-2016.csv"))
vegetation_df$ambID <- substring(vegetation_df$Useful_EP_PlotID, 4, 6) # ambiguous plotID
vegetation_df$lA <- ifelse(substring(vegetation_df$PlotID, 1, 1) == 'A', 1, 0)
vegetation_df$lH <- ifelse(substring(vegetation_df$PlotID, 1, 1) == 'H', 1, 0)
vegetation_df$lS <- ifelse(substring(vegetation_df$PlotID, 1, 1) == 'S', 1, 0)

# prepare LUI
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

# !! filter by year !!
lui_df = lui_df[lui_df$Year == 2015, ]

write.csv(lui_df, paste0(path_output, "lui.csv"))


# prepare temperature full year
#temperature_csv = read.csv(paste0(path_input, "temperature.csv"))
#plotid = substring(temperature_csv$plotID, 1, 5)
#temperature_df = data.frame(Useful_EP_PlotID = plotid, Year=temperature_csv$datetime, Ta_200 = temperature_csv$Ta_200, Ta_200_DTR = temperature_csv$Ta_200_DTR, Ta_200_growing_degree_days_10 = temperature_csv$Ta_200_growing_degree_days_10)
#temperature_df = temperature_df[complete.cases(temperature_df), ]
#write.csv(temperature_df, paste0(path_output, "temperature.csv"))

# prepare temperature growth months
temperature_month_csv = read.csv(paste0(path_input, "temperature_month.csv"))
temperature_month_csv$plot = substring(temperature_month_csv$plotID, 1, 5)
temperature_month_csv$year = as.numeric(substring(temperature_month_csv$datetime, 1, 4))
temperature_month_csv$month = as.numeric(substring(temperature_month_csv$datetime, 6, 7))
temperature_month_csv$growth =  temperature_month_csv$month >= 1 & temperature_month_csv$month <= 6
temperature_month_csv = temperature_month_csv[temperature_month_csv$growth,]
temperature_df = data.frame(Useful_EP_PlotID = temperature_month_csv$plot, Year=temperature_month_csv$year, Ta_200 = temperature_month_csv$Ta_200, Ta_200_DTR = temperature_month_csv$Ta_200_DTR, Ta_200_growing_degree_days_10 = temperature_month_csv$Ta_200_growing_degree_days_10)
temperature_df = temperature_df[complete.cases(temperature_df), ]
temperature_df <- aggregate(temperature_df[,c("Ta_200", "Ta_200_DTR", "Ta_200_growing_degree_days_10")], by = list(Useful_EP_PlotID = temperature_df$Useful_EP_PlotID, Year = temperature_df$Year), FUN = mean, na.rm = TRUE)
write.csv(temperature_df, paste0(path_output, "temperature.csv"))


# prepare precipitation full year
#precipitation_csv = read.csv(paste0(path_input, "precipitation_radolan.csv"))
#precipitation_df = data.frame(Useful_EP_PlotID = precipitation_csv$plotID, Year=precipitation_csv$datetime, precipitation_radolan = precipitation_csv$precipitation_radolan, precipitation_radolan_rain_days = precipitation_csv$precipitation_radolan_rain_days)
#precipitation_df = precipitation_df[complete.cases(precipitation_df), ]
#write.csv(precipitation_df, paste0(path_output, "precipitation.csv"))

# prepare precipitation growth months
precipitation_month_csv = read.csv(paste0(path_input, "precipitation_radolan_month.csv"))
precipitation_month_csv$year = as.numeric(substring(precipitation_month_csv$datetime, 1, 4))
precipitation_month_csv$month = as.numeric(substring(precipitation_month_csv$datetime, 6, 7))
precipitation_month_csv$growth =  precipitation_month_csv$month >= 1 & precipitation_month_csv$month <= 6
precipitation_month_csv = precipitation_month_csv[precipitation_month_csv$growth,]
precipitation_df = data.frame(Useful_EP_PlotID = precipitation_month_csv$plotID, Year=precipitation_month_csv$year, precipitation_radolan = precipitation_month_csv$precipitation_radolan, precipitation_radolan_rain_days = precipitation_month_csv$precipitation_radolan_rain_days)
precipitation_df = precipitation_df[complete.cases(precipitation_df), ]
precipitation_df <- aggregate(precipitation_df[,c("precipitation_radolan", "precipitation_radolan_rain_days")], by = list(Useful_EP_PlotID = precipitation_df$Useful_EP_PlotID, Year = precipitation_df$Year), FUN = sum, na.rm = TRUE)
write.csv(precipitation_df, paste0(path_output, "precipitation.csv"))


# prepare climate
climate_df <- merge(temperature_df, precipitation_df, by=c("Useful_EP_PlotID", "Year"))
write.csv(climate_df, paste0(path_output, "climate.csv"))

# merge data.frames
org_df <- merge(vegetation_df, lui_df, by=c("EpPlotID", "Year"))
org_df <- merge(org_df, climate_df, by=c("Useful_EP_PlotID", "Year"))
write.csv(org_df, paste0(path_output, "vegetation_lui_climate.csv"))
#str(org_df) # types of columns

pdf(paste0(path_output, "plots.pdf"), width=20, height=40)
pch =  "."
par(mfrow=c(8,4))

#par(mfrow=c(2,2))
plot(org_df$biomass_g ~ org_df$lui_g, pch = pch)
plot(org_df$biomass_g ~ org_df$lui_m, pch = pch)
plot(org_df$biomass_g ~ org_df$lui_f, pch = pch)
plot(org_df$biomass_g ~ org_df$lui, pch = pch)

#par(mfrow=c(2,2))
plot(org_df$total.cover.cum ~ org_df$lui_g, pch = pch)
plot(org_df$total.cover.cum ~ org_df$lui_m, pch = pch)
plot(org_df$total.cover.cum ~ org_df$lui_f, pch = pch)
plot(org_df$total.cover.cum ~ org_df$lui, pch = pch)

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

#par(mfrow=c(2,2))
plot(org_df$biomass_g ~ org_df$Ta_200, pch = pch)
plot(org_df$biomass_g ~ org_df$Ta_200_growing_degree_days_10, pch = pch)
plot(org_df$biomass_g ~ org_df$precipitation_radolan, pch = pch)
plot(org_df$biomass_g ~ org_df$precipitation_radolan_rain_days, pch = pch)

#par(mfrow=c(2,2))
plot(org_df$total.cover.cum ~ org_df$Ta_200, pch = pch)
plot(org_df$total.cover.cum ~ org_df$Ta_200_growing_degree_days_10, pch = pch)
plot(org_df$total.cover.cum ~ org_df$precipitation_radolan, pch = pch)
plot(org_df$total.cover.cum ~ org_df$precipitation_radolan_rain_days, pch = pch)

#par(mfrow=c(2,2))
plot(org_df$shannon ~ org_df$Ta_200, pch = pch)
plot(org_df$shannon ~ org_df$Ta_200_growing_degree_days_10, pch = pch)
plot(org_df$shannon ~ org_df$precipitation_radolan, pch = pch)
plot(org_df$shannon ~ org_df$precipitation_radolan_rain_days, pch = pch)

#par(mfrow=c(2,2))
plot(org_df$number.herbs.with.shrubs ~ org_df$Ta_200, pch = pch)
plot(org_df$number.herbs.with.shrubs ~ org_df$Ta_200_growing_degree_days_10, pch = pch)
plot(org_df$number.herbs.with.shrubs ~ org_df$precipitation_radolan, pch = pch)
plot(org_df$number.herbs.with.shrubs ~ org_df$precipitation_radolan_rain_days, pch = pch)

dev.off()

# LUI predictors
pred_lui_indices <- c("lui_g", "lui_m", "lui_f", "lui")
print(names(org_df[,pred_lui_indices]))

# climate predictors
#pred_climate_indices <- c("Ta_200", "Ta_200_DTR", "Ta_200_growing_degree_days_10", "precipitation_radolan", "precipitation_radolan_rain_days")
pred_climate_indices <- c("Ta_200", "Ta_200_DTR", "Ta_200_growing_degree_days_10", "precipitation_radolan", "precipitation_radolan_rain_days", "lA", "lH", "lS") # with region
print(names(org_df[,pred_climate_indices]))

# variables (number.herbs.with.shrubs to shannon)
rspvars = c(5:24)
# ! FOR TESTING !
#rspvars = c("total.cover.cum", "biomass_g") # ! FOR TESTING !
rspvars = names(org_df[,rspvars]) # replace index numbers by names
print(names(org_df[,rspvars]))


k_fold = 10
kt_fold = 9
# ! FOR TESTING !
#k_fold = 2   # ! FOR TESTING !
#kt_fold = 2  # ! FOR TESTING !

# Loop over all variables (number.herbs to shannon)
statistic_df = data.frame()
for(rv in rspvars){
  print(paste0("---------- processing ", rv, "    model 1 ----------"))
  
  # remove NA rows in rv column
  df <- org_df[!is.na(org_df[rv]),]  
  
  # k-fold Leave-Location-Out cross validation (over non NA data)
  indp_cv = CAST::CreateSpacetimeFolds(df, spacevar = "ambID", timevar = NA, k = k_fold, seed = 31051974)
  
  # cross validation loop
  lui_prediction = rep(NA, nrow(df))
  fold_index = rep(NA, nrow(df))
  stat_lui_folds_df = data.frame()
  for(icv in seq(length(indp_cv$index))){
    act_df = df[-indp_cv$indexOut[[icv]],]
    print(paste0("*** processing ", rv, "    model 1   fold ", icv, " ***   train ", nrow(act_df), "  test ", length(indp_cv$indexOut[[icv]])))
    
    # kt-fold Leave-Location-Out cross validation for training
    cv_indicies = CAST::CreateSpacetimeFolds(act_df, spacevar = "Useful_EP_PlotID", timevar = NA, k = kt_fold, seed = 65451994)
    
    # Set train control
    trCntr = caret::trainControl(method="cv",
                                index = cv_indicies$index, 
                                indexOut = cv_indicies$indexOut,
                                returnResamp = "all",
                                verbose = FALSE)
    
    model_lui = CAST::ffs(predictors = act_df[, pred_lui_indices], 
                          response = act_df[, rv],  
                          metric = "RMSE",
                          method = "rf",
                          trControl = trCntr,
                          importance = TRUE)
    saveRDS(model_lui, file = paste0(path_output, "model_lui_", rv, "_", icv, ".rds"))
    print(model_lui)
    
    test_df = df[indp_cv$indexOut[[icv]],]
    m_lui_prediction = predict(model_lui, test_df)

    #global lui prediction scatter
    lui_prediction[indp_cv$indexOut[[icv]]] = m_lui_prediction
    fold_index[indp_cv$indexOut[[icv]]] = icv
    print(lui_prediction)
    x = test_df[, rv]
    res = model_lui$results[model_lui$results$mtry == model_lui$bestTune$mtry, ]
    stat_lui_fold_df = data.frame(RMSE_lui_pred = rmse(x, m_lui_prediction), RMSE_lui_mod = res$RMSE, Rsquared_lui_pred = rsq(x, m_lui_prediction), Rsquared_lui_mod = res$Rsquared, MAE_lui_pred = mae(x, m_lui_prediction), MAE_lui_mod = res$MAE, selectedVars_lui = paste0(model_lui$selectedvars, collapse = "  "),fold_index_lui = icv)
    stat_lui_folds_df = rbind(stat_lui_folds_df, stat_lui_fold_df)
  }
  
  print(paste0("---------- processing ", rv, "    model 2 ----------"))
  climate_prediction = rep(NA, nrow(df))
  stat_climate_folds_df = data.frame()
  for(icv in seq(length(indp_cv$index))){
    act_df = df[-indp_cv$indexOut[[icv]],]
    act_lui_prediction = lui_prediction[-indp_cv$indexOut[[icv]]]
    print(paste0("*** processing ", rv, "    model 2   fold ", icv, " ***   train ", nrow(act_df), "  test ", length(indp_cv$indexOut[[icv]])))
    
    # kt-fold Leave-Location-Out cross validation for training, same seed as in model 1
    cv_indicies = CAST::CreateSpacetimeFolds(act_df, spacevar = "Useful_EP_PlotID", timevar = NA, k = kt_fold, seed = 65451994)
    
    # Set train control
    trCntr = caret::trainControl(method="cv",
                                 index = cv_indicies$index, 
                                 indexOut = cv_indicies$indexOut,
                                 returnResamp = "all",
                                 verbose = FALSE)
    

    pred_df = act_df[, pred_climate_indices]
    pred_df$lui_prediction = act_lui_prediction
    
    model_climate = CAST::ffs(predictors = pred_df, 
                          response = act_df[, rv],  
                          metric = "RMSE", 
                          method = "rf",
                          trControl = trCntr,
                          importance = TRUE)
    saveRDS(model_climate, file = paste0(path_output, "model_climate_", rv, "_", icv, ".rds"))
    print(model_climate)
    
    test_df = df[indp_cv$indexOut[[icv]],]
    test_lui_prediction = lui_prediction[indp_cv$indexOut[[icv]]]
    pred_test_df = test_df[, pred_climate_indices]
    print(nrow(pred_test_df))
    print(length(test_lui_prediction))
    pred_test_df$lui_prediction = test_lui_prediction
    m_climate_prediction = predict(model_climate, pred_test_df)

    #global climate prediction scatter
    climate_prediction[indp_cv$indexOut[[icv]]] = m_climate_prediction
    print(climate_prediction)
    x = test_df[, rv]
    res = model_climate$results[model_climate$results$mtry == model_climate$bestTune$mtry, ]
    stat_climate_fold_df = data.frame(RMSE_climate_pred = rmse(x, m_climate_prediction), RMSE_climate_mod = res$RMSE, Rsquared_climate_pred = rsq(x, m_climate_prediction), Rsquared_climate_mod = res$Rsquared, MAE_climate_pred = mae(x, m_climate_prediction), MAE_climate_mod = res$MAE, selectedVars_climate = paste0(model_climate$selectedvars, collapse = "  "), fold_index_climate = icv)
    stat_climate_folds_df = rbind(stat_climate_folds_df, stat_climate_fold_df)
  }
  write.csv(cbind(stat_lui_folds_df, stat_climate_folds_df), file = paste0(path_output, "statistic_folds__", rv, ".csv"))
  
  x = df[, rv]
  write.csv(data.frame(value = x, lui_prediction = lui_prediction, climate_prediction = climate_prediction, fold_index = fold_index), file = paste0(path_output, "prediction__", rv, ".csv"))
  stat_df = data.frame(RMSE_lui = rmse(x, lui_prediction), RMSE_climate = rmse(x, climate_prediction),  Rsquared_lui = rsq(x, lui_prediction), Rsquared_climate = rsq(x, climate_prediction), MAE_lui = mae(x, lui_prediction), MAE_climate = mae(x, climate_prediction))  
  write.csv(stat_df, file = paste0(path_output, "statistic__", rv, ".csv"))
  stat_df$var = rv
  statistic_df = rbind(statistic_df, stat_df)
  
  pdf(paste0(path_output, "residuals__", rv, ".pdf"), width = 20, height = 20)
  y1 = x - lui_prediction
  y2 = x - climate_prediction
  plot(y1~x, ylim = c(min(y1, y2), max(y1, y2)), xlab = rv, ylab = "residuals", pch='o', col="blue")
  points(y2~x, pch='x', col="red")
  abline(h = 0)
  dev.off()
}
write.csv(statistic_df, file = paste0(path_output, "statistic.csv"))

stopCluster(cl) # end parallel


