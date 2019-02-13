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
write.csv(lui_df, paste0(path_output, "lui.csv"))

#prepare climate
climate_csv = read.csv(paste0(path_input, "plots.csv"))
plotid = substring(climate_csv$plotID, 1, 5)
climate_df = data.frame(Useful_EP_PlotID = plotid, Year=climate_csv$datetime, Ta_200 = climate_csv$Ta_200, Ta_200_DTR = climate_csv$Ta_200_DTR, a = climate_csv$Ta_200^1.1, b = climate_csv$Ta_200_DTR^1.5)
climate_df = climate_df[complete.cases(climate_df), ]
write.csv(climate_df, paste0(path_output, "climate.csv"))

#merge data.frames
org_df <- merge(vegetation_df, lui_df, by=c("EpPlotID", "Year"))
org_df <- merge(org_df, climate_df, by=c("Useful_EP_PlotID", "Year"))
write.csv(org_df, paste0(path_output, "vegetation_lui_climate.csv"))
#str(org_df) # types of columns

# LUI predictors
pred_lui_indices <- c("lui_g", "lui_m", "lui_f", "lui")
print(names(org_df[,pred_lui_indices]))

#climate predictors
pred_climate_indices <- c("Ta_200", "Ta_200_DTR", "a", "b")
print(names(org_df[,pred_climate_indices]))


# variables (number.herbs.with.shrubs to shannon)
rspvars = c("number.herbs.with.shrubs", "number.grasses.with.shrubs")
# ! FOR TESTING !
rspvars = c("shannon")

k_fold = 2  # 10
kt_fold = 2 # ? (k_fold - 1)

# Loop over all variables (number.herbs to shannon)
for(rv in rspvars){
  print(rv)
  
  # remove NA rows in rv column
  df <- org_df[!is.na(org_df[rv]),]  
  
  # k-fold Leave-Location-Out cross validation (over non NA data)
  indp_cv = CreateSpacetimeFolds(df, spacevar = "Useful_EP_PlotID", timevar = NA, k = k_fold, seed = 31051974)
  
  # cross validation loop
  for(icv in seq(length(indp_cv$index))){
    print(icv)
    
    act_df = df[-indp_cv$indexOut[[icv]],]
    
    # kt-fold Leave-Location-Out cross validation for training
    cv_indicies = CreateSpacetimeFolds(act_df, spacevar = "Useful_EP_PlotID", timevar = NA, k = kt_fold, seed = 65451994)
    
    # Set train control
    trCntr = trainControl(method="cv",
                          index = cv_indicies$index, 
                          indexOut = cv_indicies$indexOut,
                          returnResamp = "all",
                          #repeats = 1, 
                          verbose = FALSE)
    
    model_lui = ffs(predictors = act_df[, pred_lui_indices], 
                response = act_df[, rv],  
                metric = "RMSE", 
                method = "rf",
                trControl = trCntr,
                #tuneLength = tune_length,
                #tuneGrid = lut$MTHD_DEF_LST[["rf"]]$tunegr
                importance = TRUE)
    saveRDS(model_lui, file = paste0(path_output, "model_lui_", rv, "_", icv, ".rds"))
    print(model_lui)
    
    test_df = df[indp_cv$indexOut[[icv]],]
    m_lui_prediction = predict(model_lui, test_df)
    m_lui_res = test_df[, rv] - m_lui_prediction
    
    m_lui_df = data.frame(m_lui_prediction = m_lui_prediction, m_lui_res = m_lui_res)
    saveRDS(m_lui_df, file = paste0(path_output, "m_lui_", rv, "_", icv, ".rds"))
    write.csv(m_lui_df, file = paste0(path_output, "m_lui_", rv, "_", icv, ".csv"))
    
    #prepare response of residuals in same order as in act_df
    m_climate_response = act_df[, rv] - predict(model_lui, act_df)
    print(m_climate_response)
    
    model_climate = ffs(predictors = act_df[, pred_climate_indices], 
                        response = m_climate_response,  
                        metric = "RMSE", 
                        method = "rf",
                        trControl = trCntr,
                        #tuneLength = tune_length,
                        #tuneGrid = lut$MTHD_DEF_LST[["rf"]]$tunegr
                        importance = TRUE)
    saveRDS(model_climate, file = paste0(path_output, "model_climate_", rv, "_", icv, ".rds"))
    print(model_climate)
    
    m_climate_prediction = predict(model_climate, test_df)
    m_climate_res = m_lui_res - m_climate_prediction
    
    m_climate_df = data.frame(m_lui_prediction = m_lui_prediction, m_lui_res = m_lui_res, m_climate_prediction = m_climate_prediction, m_climate_res = m_climate_res)
    saveRDS(m_climate_df, file = paste0(path_output, "m_climate_", rv, "_", icv, ".rds"))
    write.csv(m_climate_df, file = paste0(path_output, "m_climate_", rv, "_", icv, ".csv"))    
  }
}


