#!/usr/bin/Rscript
# note: needs to be run from parent directory.
source("src/000_set_environment.R")

if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

# Read datasets
df_met_h_dwd = readRDS(paste0(path_rdata, "/df_met_dwd_h.rds"))

dt_range = data.frame(datetime = unique(df_met_h_dwd$datetime))

# testing dataset
#df_met_h_dwd = data.frame(datetime=c("a","a","b","b","c","c","d","d","e","e"), STATIONS_ID=c("n","m","x","x","y","y","y","y","z","z"), Ta_200=c(7,8,10,20,NA,1,2,NA,NA,NA))

#df_met_h_dwd = head(df_met_h_dwd)

# remove duplicates (and NA)
df_met_h_dwd = aggregate(Ta_200 ~ datetime + STATIONS_ID, df_met_h_dwd, mean)

# prm = c("Ta_200", "rH_200")
prm = c("Ta_200")

valueSelect = function(v) {
  x = mean(v, na.rm=TRUE)
  return(is.finite(x))
}

v = prm[1]
#v_na = lapply(prm, function(v){
  df_met_h_dwd_wide = dcast(melt(df_met_h_dwd[!is.na(df_met_h_dwd[, v]), 
                                              colnames(df_met_h_dwd) %in% c("STATIONS_ID", "datetime", v)], 
                                 id.vars = c("STATIONS_ID", "datetime")),
                            #fun.aggregate = valueSelect,
                            datetime~variable+STATIONS_ID)
  df_met_h_dwd_wide_dt_range = merge(dt_range, df_met_h_dwd_wide, by = "datetime", all.x = TRUE)
  # summary(df_met_h_dwd_wide_dt_range)
  
  plots = colnames(df_met_h_dwd_wide_dt_range)[grepl(v, colnames(df_met_h_dwd_wide_dt_range))]
  plots = substr(plots, nchar(v)+2, nchar(plots))
  
  #p = 3278
  p_na = lapply(plots, function(p){
    act_station = df_met_h_dwd_wide_dt_range[, c(1, grep(p, colnames(df_met_h_dwd_wide_dt_range)))]

    act_na = which(is.na(act_station[, grep(v, colnames(act_station))]))
    #act_na = which(act_station[, grep(v, colnames(act_station))] == 0)
    #act_na = which(!act_station[, grep(v, colnames(act_station))])
    
    if(length(act_na) > 0){
      response_id = p
      timespan_fill = act_station$datetime[act_na]
      
      predictor_set = df_met_h_dwd[df_met_h_dwd$STATIONS_ID != p &
                                     df_met_h_dwd$datetime %in% timespan_fill, 
                                   c("STATIONS_ID", "datetime",v)]
      predictor_set_wide = dcast(predictor_set, datetime ~ STATIONS_ID, value.var=v)
      #predictor_set_wide = dcast(predictor_set, datetime ~ STATIONS_ID, value.var=v, fun.aggregate=valueSelect)
      predictor_set_wide = predictor_set_wide[ , colnames(predictor_set_wide) == "datetime" |
                                                 colSums(is.na(predictor_set_wide)) == 0]
      
      training_set = df_met_h_dwd[!df_met_h_dwd$datetime %in% timespan_fill &
                                    df_met_h_dwd$STATIONS_ID %in% c(response_id, colnames(predictor_set_wide[, -1])), 
                                  c("STATIONS_ID", "datetime", v)]
      training_set_wide = dcast(training_set, datetime ~ STATIONS_ID, value.var=v)
      #training_set_wide = dcast(training_set, datetime ~ STATIONS_ID, value.var=v, fun.aggregate=valueSelect)
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
      if(ncol(training_set_wide[, !colnames(training_set_wide) %in% c("datetime", response_id)]) > 2){
        model = ffs(training_set_wide[, !colnames(training_set_wide) %in% c("datetime", response_id)], 
                    training_set_wide[, colnames(training_set_wide) %in% c(response_id)], 
                    method = "lm",
                    trControl = trCntr)  
      } else {
        model = train(training_set_wide[, !colnames(training_set_wide) %in% c("datetime", response_id)], 
                      training_set_wide[, colnames(training_set_wide) %in% c(response_id)], 
                      method = "lm",
                      trControl = trCntr)  
      }
      
      fillvalues = data.frame(act_na = act_na,
                              predictor_set_wide$datetime, 
                              predict(model, predictor_set_wide))
      colnames(fillvalues) = c("act_na", "datetime", v)
    }
      
    act_station = df_met_h_dwd[df_met_h_dwd$STATIONS_ID == p,
                               c(which((colnames(df_met_h_dwd) %in% c("datetime", "STATIONS_ID"))), grep(v, colnames(df_met_h_dwd)))]
    print(paste0("station: ", p, " ", v, " entries: ", length(act_station$datetime), "   NA: ", length(act_na), " real: ", (length(act_station$datetime) - length(act_na)) ))
    print(paste0("station: ", p, " ", v, " entries: ", length(act_station$datetime), "   NA: ", sum(is.na(act_station[[v]])), " real: ", sum(!is.na(act_station[[v]])) ))
    
    if(length(act_na) > 0){
      act_station = data.frame(act_station, model$results[,-1])
      
      fv = data.frame(datetime=fillvalues$datetime, STATIONS_ID=p)
      fv[v] = fillvalues[v]
      fv[, c("RMSE", "Rsquared", "MAE", "RMSESD", "RsquaredSD", "MAESD")] = NA
      act_station = rbind(act_station, fv)
      act_station <- act_station[order(act_station$datetime),]
      
      #act_station[-fillvalues$act_na, colnames(act_station) %in% c("RMSE", "Rsquared", "MAE", "RMSESD", "RsquaredSD", "MAESD")] = NA
      #act_station[fillvalues$act_na, v] = fillvalues[, v]
      
      if(v == "rH_200"){
        if(nrow(act_station[!is.na(act_station$RMSE) & act_station[, v] > 100, ])>0){
          act_station[!is.na(act_station$RMSE) & act_station[, v] > 100, v] = 100
        }
        if(nrow(act_station[!is.na(act_station$RMSE) & act_station[, v] < 0, ])){
          act_station[!is.na(act_station$RMSE) & act_station[, v] < 0, v] = 0
        }
      }
      print(paste0("station: ", p, " ", v, " entries: ", length(act_station$datetime), "   NA: ", sum(is.na(act_station[[v]])), " real: ", sum(!is.na(act_station[[v]])) ))
      
      
      saveRDS(model, file = paste0(path_rdata, "df_met_dwd_h_model_", v, "_", as.character(p), ".rds"))
      saveRDS(act_station, file = paste0(path_rdata, "df_met_dwd_h_", v, "_", as.character(p), ".rds"))
    } else {
      act_station$RMSE = NA
      act_station$Rsquared = NA
      act_station$MAE = NA
      act_station$RMSESD = NA
      act_station$RsquaredSD = NA
      act_station$MAESD = NA
      saveRDS(act_station, file = paste0(path_rdata, "df_met_dwd_h_", v, "_", as.character(p), ".rds"))
    }
    return(NULL)
  })
  #})


# Cross check
dwd_files = list.files(path_rdata, pattern = glob2rx("*dwd_h_*.rds"), full.names = TRUE)
dwd_files = dwd_files[!grepl("model", dwd_files)]


# prm = c("Ta_200", "rH_200")
prm = c("Ta_200")
df_met_h_dwd_filled = lapply(prm, function(v){
  lapply(dwd_files[grep(v, dwd_files)], function(f){
    dat = readRDS(f)
    sid = colnames(dat)[2]
    dat$STATION_ID = substr(sid, 8, nchar(sid))
    colnames(dat)[2] = substr(sid, 1, 6)
    return(dat)
  })
})

df_met_h_dwd_filled_Ta = do.call("rbind", df_met_h_dwd_filled[[1]])
#df_met_h_dwd_filled_rH = do.call("rbind", df_met_h_dwd_filled[[2]])

round(quantile(df_met_h_dwd_filled_Ta$Ta_200, probs = seq(0,1,0.1)), 2)
round(quantile(df_met_h_dwd_filled_Ta$Ta_200[is.na(df_met_h_dwd_filled_Ta$RMSE)], probs = seq(0,1,0.1)), 2)
round(quantile(df_met_h_dwd_filled_Ta$Ta_200[!is.na(df_met_h_dwd_filled_Ta$RMSE)], probs = seq(0,1,0.1)), 2)

#round(quantile(df_met_h_dwd_filled_rH$rH_200, probs = seq(0,1,0.1)), 2)
#round(quantile(df_met_h_dwd_filled_rH$rH_200[is.na(df_met_h_dwd_filled_Ta$RMSE)], probs = seq(0,1,0.1)), 2)
#round(quantile(df_met_h_dwd_filled_rH$rH_200[!is.na(df_met_h_dwd_filled_Ta$RMSE)], probs = seq(0,1,0.1)), 2)

#boxplot(unique(df_met_h_dwd_filled_rH$RMSE[!is.na(df_met_h_dwd_filled_rH$RMSE)]))
boxplot(unique(df_met_h_dwd_filled_Ta$RMSE[!is.na(df_met_h_dwd_filled_Ta$RMSE)]))
