source("C:/Users/tnauss/permanent/plygrnd/exploratorien/BE-Meteorology/src/000_set_environment.R")
if(length(showConnections()) == 0){
  cores = 3
  cl = parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)
}

# Model data
be_model_files = list.files(path_rdata, 
                            pattern = glob2rx("df_met_*_h_model_*.rds"), 
                            full.names = TRUE)
be_model_files = be_model_files[-grep("dwd", be_model_files)]
length(be_model_files)

be_fill_results = lapply(be_model_files, function(e){
  model = readRDS(e)
  df = data.frame(EPID = substr(basename(e), 8, 12),
                  var = substr(basename(e), nchar(basename(e))-9, nchar(basename(e))-4),
                  model$results,
                  dwdvars = length(model$finalModel$coefficients)-1,
                  dwd01 = NA,
                  dwd02 = NA,
                  dwd03 = NA,
                  dwd04 = NA,
                  dwd05 = NA)
  df[, seq(9, 8+df$dwdvars)] = model$selectedvars
  return(df)
})

be_fill_results = do.call("rbind", be_fill_results)
summary(be_fill_results)
be_fill_results$EP = substr(be_fill_results$EPID, 1, 3)


# Observation data
be_obs_files = list.files(path_rdata, 
                          pattern = glob2rx("df_met_*_h_*.rds"), 
                          full.names = TRUE)
be_obs_files = be_obs_files[-grep("model", be_obs_files)]
be_obs_files = be_obs_files[-grep("dwd", be_obs_files)]


be_obs = lapply(be_obs_files, function(f){
  obs = readRDS(f)
  obs$agg = substr(obs$datetime, 6, 13)
  obs_mean = aggregate(obs[, 2], by = list(obs$agg), FUN=mean)
  head(obs_mean)
  colnames(obs_mean) = c("agg", "mean")
  obs = merge(obs, obs_mean)
  obs$ds = obs[, 3] - obs$mean
  data.frame(EPID = substr(basename(f), 8,12),
             var = colnames(obs)[3],
             nas =  any(is.na(obs[, 4])),
             sd = sd(obs[! is.na(obs[, 4]), 3]),
             sdds = sd(obs[! is.na(obs[, 4]), "ds"]))
})
be_obs = do.call("rbind", be_obs)


# Combine model and observed data
be_merged = merge(be_fill_results, be_obs, by = c("EPID", "var"))
be_merged$nsdRMSE = be_merged$RMSE/be_merged$sd
be_merged$nsddsRMSE = be_merged$RMSE/be_merged$sdds
saveRDS(be_merged, file=paste0(path_rdata, "be_merged.rds"))

ggplot(data = be_merged, aes(x = var, y = nsdRMSE, fill = EP)) + 
  geom_boxplot() + 
  ggtitle("Gap fill RMSE normalized to SD of observations") + 
  theme_bw()

ggplot(data = be_merged, aes(x = var, y = nsddsRMSE, fill = EP)) + 
  geom_boxplot() + 
  ggtitle("Gap fill RMSE normalized to SD of observations") + 
  theme_bw()


be_files = list.files(path_rdata, pattern = glob2rx("df_met_h_*.rds"), full.names = TRUE)
lapply(be_files, function(f){
  obs = readRDS(f)
  data.frame(EPID = substr(basename(f), 8,12),
             var = "precipitation_radolan",
             nas =  sum(is.na(obs[, "precipitation_radolan"])))
  
})


t = obs[is.na(obs[, "precipitation_radolan"]), "datetime"]
t1 = unique(t)
summary(t1)


radolan = be_io_met_hourly(paste0(path_met_h, "/precipitation_radolan_2008_2017_5ea12bb559ea2a64/plots.csv"))
head(radolan)
saveRDS(radolan, file=paste0(path_temp, "radolan.rds"))
t = radolan[is.na(radolan$precipitation_radolan), ]
head(t)
radolan[2139,]

summary(df_met$precipitation_radolan)
summary(radolan$precipitation_radolan)
