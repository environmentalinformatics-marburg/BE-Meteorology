source("C:/Users/tnauss/permanent/plygrnd/exploratorien/BE-Meteorology/src/000_set_environment.R")
if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

# Read datasets
df_met_h_AE = readRDS(paste0(path_rdata, "/df_met_h_AE.rds"))
df_met_h_dwd = readRDS(paste0(path_rdata, "/df_met_dwd_h.rds"))
# df_met_m= readRDS(paste0(path_rdata, "/df_met_h.rds"))
# df_met_m_meta = readRDS(paste0(path_rdata, "/df_met_h_meta.rds"))
# df_met_m = readRDS(paste0(path_rdata, "/df_met_m.rds"))
# df_met_m_meta = readRDS(paste0(path_rdata, "/df_met_m_meta.rds"))

dt_range = data.frame(datetime = as.POSIXct(df_met_h_AE$datetime[df_met_h_AE$EPID == "AEG01"]))
# dt_range = data.frame(datetime = as.character(dt_range))

df_met_h_dwd$Ta_200[df_met_h_dwd$Ta_200 == -999] = NA
df_met_h_dwd$rH_200[df_met_h_dwd$rH_200 == -999] = NA

df_met_h_dwd$datetime = as.POSIXct(df_met_h_dwd$datetime)
# df_met_h_dwd$datetime = as.character(df_met_h_dwd$datetime)

prm = c("Ta_200")

v_na = lapply(prm, function(v){
  df_met_h_dwd_wide = dcast(melt(df_met_h_dwd[, colnames(df_met_h_dwd) %in% c("STATIONS_ID", "datetime", v)], 
                                 id.vars = c("STATIONS_ID", "datetime")), 
                            datetime~variable+STATIONS_ID)
  
  df_met_h_dwd_wide_dt_range = merge(dt_range, df_met_h_dwd_wide, by = "datetime", all.x = TRUE)
  # summary(df_met_h_dwd_wide_dt_range)
  
  p_na = lapply(colnames(df_met_h_dwd_wide_dt_range[, -1]), function(p){
    act_station = df_met_h_dwd_wide_dt_range[, c("datetime", p)]
    act_na = which(is.na(act_station[, 2]))
    if(length(act_na) > 0){
      response_id = substr(p, nchar(v)+2, nchar(p))
      timespan_fill = act_station$datetime[act_na]
      
      predictor_set = df_met_h_dwd[df_met_h_dwd$STATIONS_ID != p &
                                     df_met_h_dwd$datetime %in% timespan_fill, 
                                   c("STATIONS_ID", "datetime", v)]
      predictor_set_wide = dcast(predictor_set, datetime ~ STATIONS_ID, value.var=v)
      predictor_set_wide = predictor_set_wide[ , colnames(predictor_set_wide) == "datetime" |
                                                 colSums(is.na(predictor_set_wide)) == 0]
      
      training_set = df_met_h_dwd[!df_met_h_dwd$datetime %in% timespan_fill &
                                    df_met_h_dwd$STATIONS_ID %in% c(response_id, colnames(predictor_set_wide[, -1])), 
                                  c("STATIONS_ID", "datetime", v)]
      training_set_wide = dcast(training_set, datetime ~ STATIONS_ID, value.var=v)
      training_set_wide = training_set_wide[complete.cases(training_set_wide),]
      
      # Create training folds
      stf = CreateSpacetimeFolds(training_set_wide, spacevar = NA, timevar = "datetime", k = 10, 
                                 seed = 10)
      
      
      trCntr <- trainControl(method="cv",
                             index = stf$training_index,
                             indexOut = stf$training_indexOut,
                             returnResamp = "all",
                             repeats = 1, verbose = FALSE)
      
      # Compute model and predict values
      
      model = train(training_set_wide[, !colnames(training_set_wide) %in% c("datetime", response_id)], 
                    training_set_wide[, colnames(training_set_wide) %in% c(response_id)], 
                    method = "lm",
                    trControl = trCntr)  
      
      fillvalues = data.frame(act_na = act_na,
                              predicting_set_wide$datetime, 
                              predict(model, predicting_set_wide[, -1]))
      colnames(fillvalues) = c("datetime", v)
      g_fill = list(fillvalues = fillvalues, model = model)
      
      saveRDS(g_fill, file = paste0(path_temp, "g_fill_", v, "_", as.character(p), ".rds"))
      
      return(g_fill)
    }
  })
  
})

