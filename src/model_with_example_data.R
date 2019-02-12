library(CAST)
library(caret)

path_output = "output/"
if(!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)

df <- data.frame(
  Useful_EP_PlotID=c("AEG01","AEG02","AEG03","AEG04","AEG01","AEG02","AEG03","AEG04","AEG01","AEG02","AEG03","AEG04","AEG01","AEG02","AEG03","AEG04"),
  Year=c(2008,2008,2008,2008,2009,2009,2009,2009,2010,2010,2010,2010,2011,2011,2011,2011),
  number.herbs.with.shrubs=c(10,11.5,10.2,10.5,20,22,19,21,12,11.1,12.1,10.12,9,8,9.5,9.8),
  number.grasses.with.shrubs=c(10,11.5,10.2,10.5,20,22,19,21,12,11.1,12.1,10.12,9,8,9.5,9.8)^0.9,
  LUI=c(1.01,1.1,0.9,0.95,2.1,2.04,1.8,1.85,1.2,1.05,1.4,1.02,0.8,0.85,0.79,0.91),
  LUIa=c(1.01,1.1,0.9,0.95,2.1,2.04,1.8,1.85,1.2,1.05,1.4,1.02,0.8,0.85,0.79,0.91)^1.1,
  LUIb=c(1.01,1.1,0.9,0.95,2.1,2.04,1.8,1.85,1.2,1.05,1.4,1.02,0.8,0.85,0.79,0.91)^1.2,
  LUIc=c(1.01,1.1,0.9,0.95,2.1,2.04,1.8,1.85,1.2,1.05,1.4,1.02,0.8,0.85,0.79,0.91)^1.3
)

pred_indices <- c(5:8)

k_fold = 2  # 10
kt_fold = 2 # ? (k_fold - 1)

# k-fold Leave-Location-Out cross validation
indp_cv = CreateSpacetimeFolds(df, spacevar = "Useful_EP_PlotID", timevar = NA, k = k_fold, seed = 31051984)

rspvars = c("number.herbs.with.shrubs", "number.grasses.with.shrubs")

# Loop over all variables (number.herbs to shannon)
for(rv in rspvars){
  print(rv)
  
  # cross validation loop
  for(icv in seq(length(indp_cv$index))){
    print(icv)
    
    act_df = df[-indp_cv$indexOut[[icv]],]
    
    # (k - 1)-fold Leave-Location-Out cross validation for training
    cv_indicies = CreateSpacetimeFolds(act_df, spacevar = "Useful_EP_PlotID", timevar = NA, k = kt_fold, seed = 65451984)
    
    # Set train control
    trCntr = trainControl(method="cv",
                          index = cv_indicies$index, 
                          indexOut = cv_indicies$indexOut,
                          returnResamp = "all",
                          #repeats = 1, 
                          verbose = FALSE)
    
    model = ffs(predictors = act_df[, pred_indices], 
                response = act_df[, rv],  
                metric = "RMSE", 
                method = "rf",
                trControl = trCntr,
                #tuneLength = tune_length,
                #tuneGrid = lut$MTHD_DEF_LST[["rf"]]$tunegr
                importance = TRUE)
    saveRDS(model, file = paste0(path_output, "model_", rv, "_", icv, ".rds"))
    
    test_df = df[indp_cv$indexOut[[icv]],]
    m_prediction = predict(model, test_df)
    m_res = test_df[, rv] - m_prediction
    
    m_df = data.frame(m_prediction = m_prediction, m_res = m_res)
    saveRDS(m_df, file = paste0(path_output, "m_", rv, "_", icv, ".rds"))
    write.csv(m_df, file = paste0(path_output, "m_", rv, "_", icv, ".csv"))
  }
}

