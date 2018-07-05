source("C:/Users/tnauss/permanent/plygrnd/exploratorien/BE-Meteorology/src/000_set_environment.R")

# Read datasets
df_met_d= readRDS(paste0(path_rdata, "/df_met_d.rds"))
df_met_d_meta = readRDS(paste0(path_rdata, "/df_met_d_meta.rds"))
# df_met_m = readRDS(paste0(path_rdata, "/df_met_m.rds"))
# df_met_m_meta = readRDS(paste0(path_rdata, "/df_met_m_meta.rds"))


# Check for gaps
if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

v_na = lapply(colnames(df_met_d)[3:51], function(v){
  p_na = lapply(unique(df_met_d$EPID), function(p){
    act = df_met_d[df_met_d$EPID == p, ]
    act_na = which(is.na(act[, which(colnames(df_met_d) == v)]))
    if(length(act_na) > 0){
      p_type = substr(p, 1, 3)
      act_na_grps = tapply(act_na, cumsum(c(TRUE, diff(act_na) != 1)), identity)    
      
      # For each consecutive gap, look for all other stations which have no gap for this time span
      g_fill = lapply(act_na_grps, function(g){
        
        # Get stations which cover gap time span
        timespan_fill = act$datetime[g]
        predicting_set = df_met_d[df_met_d$EPID != p & 
                                    grepl(p_type, df_met_d$EPID) &
                                    df_met_d$datetime %in% timespan_fill & 
                                    !is.na(df_met_d[, which(colnames(df_met_d) == v)]), 
                                  which(colnames(df_met_d) %in% c("EPID", "datetime", v))]
        predicting_set_wide = dcast(predicting_set, datetime ~ EPID, value.var=v)
        predicting_set_wide = predicting_set_wide[ , colSums(is.na(predicting_set_wide)) == 0]
        
        # Get training dataset
        training_set = df_met_d[df_met_d$EPID %in% c(p, colnames(predicting_set_wide)[-1]) & 
                                  !is.na(df_met_d[, which(colnames(df_met_d) == v)]), 
                                which(colnames(df_met_d) %in% c("EPID", "datetime", v))]
        
        
        training_set_wide = dcast(training_set, datetime ~ EPID, value.var=v)
        training_set_wide = training_set_wide[complete.cases(training_set_wide[, -1]), ]
        
        # Compute model and predict values
        model = ffs(training_set_wide[, !colnames(training_set_wide) %in% c("datetime", p)], 
                    training_set_wide[, colnames(training_set_wide) == p], 
                    method = "pls")  
        fillvalues = predict(model, predicting_set_wide[, -1])
        
        g_fill = list(fillvalues = fillvalues, quality = model$results[model$bestTune$ncomp, ])
        return(g_fill)
      })
    }
  })
})


# Write dataset
df_met_d_filled = readRDS(paste0(path_rdata, "/df_met_d_filled.rds"))


