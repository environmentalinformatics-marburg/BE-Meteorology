source("C:/Users/tnauss/permanent/plygrnd/exploratorien/BE-Meteorology/src/000_set_environment.R")
if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

# Read datasets
# df_met_h_AE = readRDS(paste0(path_rdata, "/df_met_h_AE.rds"))
# df_met_h_dwd = readRDS(paste0(path_rdata, "/df_met_dwd_h.rds"))
# df_met_m= readRDS(paste0(path_rdata, "/df_met_h.rds"))
# df_met_m_meta = readRDS(paste0(path_rdata, "/df_met_h_meta.rds"))
# df_met_m = readRDS(paste0(path_rdata, "/df_met_m.rds"))
# df_met_m_meta = readRDS(paste0(path_rdata, "/df_met_m_meta.rds"))

dwd_station_groups = data.frame(EP=rep(c("AE", "HE", "SE"),each=5),
                                stid = c("3278", "3402", "2814", "2074", "4887",
                                         "6305", "7368", "1297", "0896", "1270", 
                                         "0164", "1869", "7351", "5745", "7389"))

be_files = list.files(path_rdata, pattern = glob2rx("df_met_be_h_*.rds"), full.names = TRUE)

lapply(be_files, function(e){
  df_met_h = readRDS(e)
  df_met_h$datetime = as.POSIXct(df_met_h$datetime)
  
  prm = c("Ta_200", "rH_200")
  
  v_na = lapply(prm, function(v){
    
    act_ep = dwd_station_groups[dwd_station_groups$EP == (substr(df_met_h$EPID[1], 1, 2)),]
    act_dwd_files = list.files(path_rdata, pattern = glob2rx(paste0("*dwd_h_", v, "*.rds")), full.names = TRUE)
    act_dwd_files = act_dwd_files[(substr(basename(act_dwd_files), nchar(basename(act_dwd_files))-7, nchar(basename(act_dwd_files))-4) %in% act_ep$stid)]
    
    dwd = lapply(act_dwd_files, function(f){
      af = readRDS(f)
      af = af[, c("datetime", "STATIONS_ID", v)]
      return(af)
    })
    dwd = do.call("rbind", dwd)
    
    dwd_wide = dcast(dwd, datetime ~ STATIONS_ID, value.var = v)
    colnames(dwd_wide)[-1] = paste0(v, "_", colnames(dwd_wide)[-1])
    
    p_na = lapply(unique(df_met_h$EPID), function(p){
      act_station = df_met_h[df_met_h$EPID == p, c("EPID", "datetime", v, "qualitycounter")]
      
      act_na = which(is.na(act_station[, v]))
      if(length(act_na) > 0){
        timespan_fill = act_station$datetime[act_na]
        
        predictor_set_wide = dwd_wide[dwd_wide$datetime %in% timespan_fill,]
        
        training_set_wide = dwd_wide[!dwd_wide$datetime %in% timespan_fill,]
        training_set_wide[, as.character(p)] = act_station[!act_station$datetime %in% timespan_fill,v]
        
        # Create training folds
        stf = CreateSpacetimeFolds(training_set_wide, spacevar = NA, timevar = "datetime", k = 10, 
                                   seed = 10)
        
        
        trCntr <- trainControl(method="cv",
                               index = stf$training_index,
                               indexOut = stf$training_indexOut,
                               returnResamp = "all",
                               repeats = 1, verbose = FALSE)
        
        # Compute model and predict values
        model = ffs(training_set_wide[, !colnames(training_set_wide) %in% c("datetime", as.character(p))], 
                    training_set_wide[, colnames(training_set_wide) %in% as.character(p)], 
                    method = "lm",
                    trControl = trCntr)  
        # gam = model
        # pls = model
        fillvalues = data.frame(act_na = act_na,
                                datetime = predictor_set_wide$datetime, 
                                predict(model, predictor_set_wide))
        colnames(fillvalues)[3] = c(v)
      }
      act_station$RMSE = NA
      act_station$Rsquared = NA
      act_station$MAE = NA
      act_station$RMSESD = NA
      act_station$RsquaredSD = NA
      act_station$MAESD = NA
      
      if(length(act_na) > 0){
        act_station[fillvalues$act_na, v] = fillvalues[, v]
        
        # act_station$RMSE[fillvalues$act_na] = model$results[model$bestTune$ncomp, "RMSE"]
        # act_station$Rsquared[fillvalues$act_na] = model$results[model$bestTune$ncomp, "Rsquared"]
        # act_station$MAE[fillvalues$act_na] = model$results[model$bestTune$ncomp, "MAE"]
        # act_station$RMSESD[fillvalues$act_na] = model$results[model$bestTune$ncomp, "RMSESD"]
        # act_station$RsquaredSD[fillvalues$act_na] = model$results[model$bestTune$ncomp, "RsquaredSD"]
        # act_station$MAESD[fillvalues$act_na] = model$results[model$bestTune$ncomp, "MAESD"]
        
        act_station$RMSE[fillvalues$act_na] = model$results$RMSE
        act_station$Rsquared[fillvalues$act_na] = model$results$Rsquared
        act_station$MAE[fillvalues$act_na] = model$results$MAE
        act_station$RMSESD[fillvalues$act_na] = model$results$RMSESD
        act_station$RsquaredSD[fillvalues$act_na] = model$results$RsquaredSD
        act_station$MAESD[fillvalues$act_na] = model$results$MAESD
        
        saveRDS(model, file = paste0(path_rdata, "df_met_be_h_model_", v, "_", as.character(p), ".rds"))
        saveRDS(act_station, file = paste0(path_rdata, "df_met_be_h_", v, "_", as.character(p), ".rds"))
      } else {
        saveRDS(act_station, file = paste0(path_rdata, "df_met_be_h_", v, "_", as.character(p), ".rds"))
      }
      return(NULL)
    })
  })
})



