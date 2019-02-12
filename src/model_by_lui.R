source("src/config.R")

library(CAST)
library(caret)

lui_csv = read.csv(paste0(path_input, "LUI_glob_sep_11.02.2019+185450.txt"), sep = "\t", dec=',')
lui_df = data.frame(EpPlotID = lui_csv$EP.Plotid, lui_g = lui_csv$G_std, lui_m = lui_csv$M_std, lui_f = lui_csv$F_std, lui = lui_csv$LUI)
write.csv(lui_df, paste0(path_output, "lui.csv"))

vegetation_df <- read.csv(paste0(path_input, "Vegetation_HeaderData_2008-2016.csv"))

org_df <- merge(vegetation_df, lui_df, by="EpPlotID")
write.csv(org_df, paste0(path_output, "vegetation_lui.csv"))
#str(org_df) # types of columns

# LUI
pred_indices <- c("lui_g", "lui_m", "lui_f", "lui")
print(names(org_df[,pred_indices]))

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


