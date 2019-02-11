library(CAST)
library(caret)

# df = unser dataframe a la
# EpPlotID,	PlotID,	Year,	number.herbs.with.shrubs, number.grasses.with.shrubs, ..., shannon,
# LUI, LUI_K1, LUI_K2, LUI_K3, Klimaparameter...



# 10-fold Leave-Location-Out cross validation
cv_indicies = CreateSpacetimeFolds(df, spacevar = "PlotID", timevar = NA, 
                                   k = 10, seed = 31051984)


# Set train control
trCntr = trainControl(method="cv",
                      index = cv_indicies$index, 
                      indexOut = cv_indicies$indexOut,
                      returnResamp = "all",
                      repeats = 1, verbose = FALSE)

# Loop over all variables (number.herbs to shannon)
rspvars = c(5:20, 22:24)
for(rv in rspvars){
  model = ffs(predictors = df[, #LUI-Spaltennummern], 
                              response = df[, rv],  
                              metric = "RMSE", 
                              method = "rf",
                              trControl = trCntr,
                              tuneLength = tune_length,
                              tuneGrid = lut$MTHD_DEF_LST[["rf"]]$tunegr
                              importance = TRUE) 
  filename = paste0("model_", rv, "rds")
  saveRDS(model, file=...)
  
  # Nochmal jeden Response-Variable 10 mal vorhersagen
  # Laufen über Folds, jeweils index ins Training, index out in den Test
  m = tune(rf, df[cv_indicies$index, optimum_predictors_aus_ffs], df[cv_indicies$index, rv])
  m_prediction = prediction(m, df[cv_indicies$indexOut,])
  m_res = df[cv_indicies$indexOut, rv] - m_prediction
  saveRDS(m_res etc.)
  
}







# 10-fold Leave-Location-Out cross validation
indp_cv = CreateSpacetimeFolds(df, spacevar = "PlotID", timevar = NA, 
                               k = 10, seed = 31051984)

rspvars = c(5:20, 22:24)


# Loop over all variables (number.herbs to shannon)
for(rv in rspvars){
  
  
  for(icv in seq(length(indp_cv$index)){
    
    act_df = df[-indp_cv$indexOut[[icv]],]
    
    cv_indicies = CreateSpacetimeFolds(act_df, spacevar = "PlotID", timevar = NA, 
                                       k = 9, seed = 65451984)
    
    # Set train control
    trCntr = trainControl(method="cv",
                          index = cv_indicies$index, 
                          indexOut = cv_indicies$indexOut,
                          returnResamp = "all",
                          repeats = 1, verbose = FALSE)
    
    model = ffs(predictors = act_df[, #LUI-Spaltennummern], 
                                    response = act_df[, rv],  
                                    metric = "RMSE", 
                                    method = "rf",
                                    trControl = trCntr,
                                    tuneLength = tune_length,
                                    tuneGrid = lut$MTHD_DEF_LST[["rf"]]$tunegr
                                    importance = TRUE) 
    filename = paste0("model_", rv, "rds")
    saveRDS(model, file=...)
    
    m_prediction = prediction(model, df[indp_cv$indexOut[[icv]],])
    m_res = df[indp_cv$indexOut[[icv]],rv] - m_prediction
    saveRDS(m_res etc.)
    
  } 
}

