source("C:/Users/tnauss/permanent/plygrnd/exploratorien/BE-Meteorology/src/000_set_environment.R")

# Read datasets
df_met_d_filled = readRDS(paste0(path_rdata, "/df_met_d_filled.rds"))
df_met_d_meta = readRDS(paste0(path_rdata, "/df_met_d_meta.rds"))


# Aggregate variables to monthly values
df_met_d_filled$datetime_month = format(df_met_d_filled$datetime, "%Y-%m")
vars = colnames(df_met_d)[c(3:9, 11:48)]
aggyaml = yaml.load_file(paste0(path_met_d, "/sensor.yaml"))

vars_agg = lapply(vars, function(v){
  
  agg = aggyaml[v == names(aggyaml)][[1]]$aggregation
  if(agg == "average"){
    agg = mean
  } else if(agg == "maximum"){
    agg = max
  } else if(agg == "minimum"){
    agg = min
  } else if(agg == "sum"){
    agg = sum
  }
  
  ma = aggregate(df_met_d_filled[, v == colnames(df_met_d_filled)],
                 by = list(df_met_d_filled$EPID, df_met_d_filled$datetime_month),
                 FUN = agg, na.rm = TRUE)
  colnames(ma) = c("EPID", "datetime", "value")
  ma$var = v
  return(ma)
})

vars_agg = do.call("rbind", vars_agg)
vars_agg <- dcast(vars_agg, EPID + datetime ~ var, value.var="value")

# Add grouping variable for months, years and exploratory landcovers
vars_agg$grp_months = substr(vars_agg$datetime, 6, 7)
vars_agg$grp_years = substr(vars_agg$datetime, 1, 4)
vars_agg$grp_belc = substr(vars_agg$EPID, 1, 3)
vars_agg$grp_lc = substr(vars_agg$EPID, 3, 3)

# Write dataset
saveRDS(vars_agg, paste0(path_rdata, "/df_met_m_from_d.rds"))


