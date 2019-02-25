#!/usr/bin/Rscript
# note: needs to be run from parent directory. e.g. ./src/model_by_lui.R

source("src/config.R")

data = read.csv(paste0(path_output, "vegetation_lui_climate.csv"))

rv = "biomass_g"
#rv = "total.cover.cum"
#rv = "nr_sp"
#rv = "evenness"
#rv = "shannon"
#rv = "number.herbs"
#rv = "number.herbs.with.shrubs"
#rv = "number.grasses"
#rv = "number.legumes"

df = read.csv(paste0("output_with_region/", "statistic_folds__", rv, ".csv"))
#df = read.csv(paste0("output_without_region/", "statistic_folds__", rv, ".csv"))

rv_norm = mean(data[ ,rv], na.rm = TRUE)
#rv_norm = sd(data[ ,rv], na.rm = TRUE)

df$"LUI model" = df$RMSE_lui_mod / rv_norm
df$"LUI prediction" = df$RMSE_lui_pred / rv_norm
df$"climate model" = df$RMSE_climate_mod / rv_norm
df$"climate prediction" = df$RMSE_climate_pred / rv_norm

boxplot(df[ , c("LUI model", "LUI prediction", "climate model", "climate prediction")])


