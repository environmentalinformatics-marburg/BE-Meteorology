source("C:/Users/tnauss/permanent/plygrnd/exploratorien/BE-Meteorology/src/000_set_environment.R")

# Read datasets
df_met_d = be_io_met_daily(paste0(path_met_d, "/plots.csv"))
df_met_d = df_met_d[df_met_d$datetime >= as.POSIXct("2009-01-01") & df_met_d$datetime <= as.POSIXct("2016-12-31"), ]
df_met_d = df_met_d[!df_met_d$g_belc %in% c("AET", "HET", "SET"), ]
df_met_d$g_belc = factor(df_met_d$g_belc, levels = c("AEG", "HEG", "SEG",
                                                     "AEW", "HEW", "SEW"))
df_met_d_meta = read.table(paste0(path_data, "/met_d/sensor_description.csv"),
                           sep = ",", header = TRUE)

df_met_m = be_io_met_monthly(paste0(path_met_m, "/2008_2018_a28b5cd71110d9b5/plots.csv"))
# df_met_a = be_io_met_annual(paste0(path_data, "met_a/plots.csv"))
df_met_m_meta = read.table(paste0(path_met_m, "/2008_2018_a28b5cd71110d9b5/sensor_description.csv"),
                           sep = ",", header = TRUE)



# Write datasets
saveRDS(df_met_d, paste0(path_rdata, "/df_met_d.rds"))
saveRDS(df_met_d_meta, paste0(path_rdata, "/df_met_d_meta.rds"))
saveRDS(df_met_m, paste0(path_rdata, "/df_met_m.rds"))
saveRDS(df_met_m_meta, paste0(path_rdata, "/df_met_m_meta.rds"))
