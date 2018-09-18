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

df_met_h = df_met_h_AE

# Define baseline for internal filling
# bl = data.frame(EP=c("AE", "HE", "SE"),
#                 dt = c(as.POSIXlt("2009-05-01 00:00:00", "UTC"),
#                        as.POSIXlt("2009-05-01 00:00:00", "UTC"),
#                        as.POSIXlt("2009-05-01 00:00:00", "UTC")))
# 
# df_met_h = df_met_h[df_met_h$datetime >= bl$dt[bl$EP %in% substr(df_met_h$EPID[1],1,2)], ]


dt_range = df_met_h_AE$datetime
df_met_h_dwd_wide = dcast(melt(df_met_h_dwd[, colnames(df_met_h_dwd) %in% c("STATIONS_ID", "datetime", "Ta_200", "EP")], 
                               id.vars = c("STATIONS_ID", "datetime")), 
                          datetime~variable+STATIONS_ID)

head(df_met_h_dwd)

merge(dt_range, df_met_h_dwd, all.x = TRUE)

prm = c("Ta_200")
v_na = lapply(prm, function(v){
  p_na = lapply(unique(df_met_h_dwd$STATIONS_ID), function(p){
    act_station = df_met_h_dwd[df_met_h_dwd$STATIONS_ID == p, ]
    act_na = which(is.na(act_station[, which(colnames(df_met_h_dwd) == v)]))
    if(length(act_na) > 0){
      print(p)
    }
  })
})




head(df_met_h_dwd)
df_met_h_dwd$datetime = as.character(df_met_h_dwd$datetime)
df_met_h_dwd_wide = dcast(melt(df_met_h_dwd[, colnames(df_met_h_dwd) %in% c(""STATIONS_ID" "datetime"    "QN_9"        "Ta_200"      "rH_200"      "eor"         "EP" ")], id.vars = c("STATIONS_ID", "datetime")), 
                          datetime~variable+STATIONS_ID)
head(df_met_h_dwd_wide)

# Check for gaps
prm = c("Ta_200")

v_na = lapply(prm, function(v){
  p_na = lapply(unique(df_met_h$EPID), function(p){
    act_station = df_met_h[df_met_h$EPID == p, ]
    act_na = which(is.na(act_station[, which(colnames(df_met_h) == v)]))
    
    if(length(act_na) > 0){
      p_type = substr(p, 1, 2)
      timespan_fill = act_station$datetime[act_na]
      
      modelling_set = df_met_h_dwd[df_met_h_dwd$EP == substr(p, 1,2),]
      modelling_set_wide = dcast(modelling_set, datetime ~ STATIONS_ID, value.var=v)
      
      predicting_set = modelling_set_wide[modelling_set_wide$datetime %in% timespan_fill, ]
      
      str(modelling_set_wide)
      str(act_station[-act_na, colnames(act_station) %in% c("datetime", v)])
      
      modelling_set_wide$datetime = as.character(modelling_set_wide$datetime)
      act_station$datetime = as.character(act_station$datetime)
      training_set = merge(modelling_set_wide, act_station[-act_na, colnames(act_station) %in% c("datetime", v)],
                           by = "datetime")
      
      head(training_set)
      
      nrow(predicting_set)
      length(timespan_fill)
      
    }
  })
})








# v_na = lapply(colnames(df_met_h)[3:51], function(v){
v_na = lapply(prm, function(v){
  p_na = lapply(unique(df_met_h$EPID), function(p){
    act = df_met_h[df_met_h$EPID == p, ]
    act_na = which(is.na(act[, which(colnames(df_met_h) == v)]))
    
    if(length(act_na) > 0){
      p_type = substr(p, 1, 2)
      act_na_grps = tapply(act_na, cumsum(c(TRUE, diff(act_na) != 1)), identity)    
      # act_na_grps = act_na
      # For each consecutive gap, look for all other stations which have no gap for this time span
      
      agg_na_grps = list()
      act_grp = 0
      for(e in act_na_grps[order(sapply(act_na_grps,length),decreasing=T)]){
        if(act_grp == 0){
          act_grp = act_grp + 1
          agg_na_grps[[act_grp]] = e
        } else  if(length(agg_na_grps[[act_grp]]) < 1000){
          agg_na_grps[[act_grp]] = c(agg_na_grps[[act_grp]], e)
        } else {
          act_grp = act_grp + 1
          agg_na_grps[[act_grp]] = e
        }
      }
      
      g_fill = lapply(agg_na_grps, function(g){
        
        # Get stations which cover gap time span
        print(g)
        timespan_fill = act$datetime[g]
        predicting_set = df_met_h[df_met_h$EPID != p & 
                                    grepl(p_type, df_met_h$EPID) &
                                    df_met_h$datetime %in% timespan_fill & 
                                    !is.na(df_met_h[, which(colnames(df_met_h) == v)]), 
                                  which(colnames(df_met_h) %in% c("EPID", "datetime", v))]
        predicting_set_wide = dcast(predicting_set, datetime ~ EPID, value.var=v)
        predicting_set_wide = predicting_set_wide[ , colnames(predicting_set_wide) == "datetime" |
                                                     colSums(is.na(predicting_set_wide)) == 0]
        
        # Get training dataset
        training_set = df_met_h[df_met_h$EPID %in% c(as.character(p), colnames(predicting_set_wide)[-1]) & 
                                  !is.na(df_met_h[, which(colnames(df_met_h) == v)]), 
                                which(colnames(df_met_h) %in% c("EPID", "datetime", v))]
        training_set = training_set[!is.na(training_set$datetime),]
        
        training_set_wide = dcast(training_set, datetime ~ EPID, value.var=v)
        training_set_wide = training_set_wide[complete.cases(training_set_wide[, -1]), ]
        
        # Remove near zero variation variables
        nzv = nearZeroVar(training_set_wide, saveMetrics = TRUE)
        rm = which(nzv$zeroVar == TRUE | 
                     nzv$nzv == TRUE)
        if(length(rm) > 0){
          training_set_wide = training_set_wide[, -rm]
        }
        
        # Remove highly correlated variables
        if(length(colnames(training_set_wide)) > 5){
          cin = which(colnames(training_set_wide) %in% c("datetime", as.character(p)))
          hc = findCorrelation(cor(
            training_set_wide[, -cin],  
            use = "pairwise.complete.obs"),  
            cutoff = 0.98)
          training_set_wide = cbind(training_set_wide[, cin], training_set_wide[, -hc])
        }
        
        head(training_set_wide)
        
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
                    training_set_wide[, colnames(training_set_wide) %in% c(as.character(p))], 
                    method = "gam",
                    trControl = trCntr)  
        fillvalues = data.frame(na_groups = g,
                                predicting_set_wide$datetime, 
                                predict(model, predicting_set_wide[, -1]))
        colnames(fillvalues) = c("datetime", v)
        g_fill = list(fillvalues = fillvalues, model = model)
        
        saveRDS(g_fill, file = paste0(path_temp, "g_fill_", v, "_", as.character(p), ".rds"))
        
        return(g_fill)
      })
    }
  })
})


# Write dataset
v_na = saveRDS(v_na, paste0(path_rdata, "/v_na.rds"))
# df_met_h_filled = readRDS(paste0(path_rdata, "/df_met_h_filled.rds"))


