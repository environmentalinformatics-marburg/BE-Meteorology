source("C:/Users/tnauss/permanent/plygrnd/exploratorien/BE-Meteorology/src/000_set_environment.R")

# Read datasets
df_met_h = be_io_met_hourly(paste0(path_met_h, "/plots.csv"))
df_met_h$g_belc = factor(df_met_h$g_belc, levels = c("AEG", "HEG", "SEG",
                                                     "AEW", "HEW", "SEW", 
                                                     "AET", "HET", "SET"))

# df_met_h_uf = be_io_met_hourly(paste0(path_met_h_uf, "/plots.csv"))
# df_met_h_uf$g_belc = factor(df_met_h_uf$g_belc, levels = c("AEG", "HEG", "SEG",
#                                                      "AEW", "HEW", "SEW", 
#                                                      "AET", "HET", "SET"))


# df_met_d = be_io_met_daily(paste0(path_met_d, "/plots.csv"))
# df_met_d = df_met_d[df_met_d$datetime >= as.POSIXct("2009-01-01") & df_met_d$datetime <= as.POSIXct("2017-12-31"), ]
# df_met_d = df_met_d[!df_met_d$g_belc %in% c("AET", "HET", "SET"), ]
# df_met_d$g_belc = factor(df_met_d$g_belc, levels = c("AEG", "HEG", "SEG",
#                                                      "AEW", "HEW", "SEW"))
# df_met_d_meta = read.table(paste0(path_data, "/met_d/sensor_description.csv"),
#                            sep = ",", header = TRUE)

# df_met_m = be_io_met_monthly(paste0(path_met_m, "/2008_2018_a28b5cd71110d9b5/plots.csv"))
# df_met_a = be_io_met_annual(paste0(path_data, "met_a/plots.csv"))
# df_met_m_meta = read.table(paste0(path_met_m, "/2008_2018_a28b5cd71110d9b5/sensor_description.csv"),
#                            sep = ",", header = TRUE)

# Select stations from DWD
# polyAlb = readBExpPoly(paste0(path_plots, "polyAlbEp_latlon.shp"))
# polyHai = readBExpPoly(paste0(path_plots, "polyHaiEp_latlon.shp"))
# polySch = readBExpPoly(paste0(path_plots, "polySchEp_latlon.shp"))
# mapview(dwd_stat_spdf) + polyAlb
# mapview(dwd_stat_spdf) + polyHai
# mapview(dwd_stat_spdf) + polySch

# Alb
# 3278 Metzingen
# 3402 Münsingen-Apfelstetten
# 2814 Merklingen
# 2074 Hechingen
# 4887 Stötten
# 
# Hai
# 6305 Mühlhausen
# 7368 Eisenach
# 1297 Eschwege
# 896 Dachwig
# 1270 Erfurt-Weimar
# 
# Sch
# 164 Angermünde
# 1869 Grünow
# 7351 Feldberg
# 5745 Zehdenick
# 7389 Heckelberg
dwd_station_list = list.files(path_dwd, recursive = TRUE, 
                              pattern = glob2rx("*produkt_tu*.txt"), 
                              full.names = TRUE)
dwd_station_groups = data.frame(EP=rep(c("AE", "HE", "SE"),each=5),
                                stid = c("3278", "3402", "2814", "2074", "4887",
                                         "6305", "7368", "1297", "0896", "1270", 
                                         "0164", "1869", "7351", "5745", "7389"))

df_met_h_dwd = lapply(dwd_station_list, function(s){
  act_dat = dwd_io_stations_hourly(s)
  act_dat$EP = dwd_station_groups$EP[grepl(substr(basename(s),38,41), dwd_station_groups$stid)]
  return(act_dat)
})
df_met_h_dwd = do.call("rbind", df_met_h_dwd)
df_met_h_dwd = df_met_h_dwd[df_met_h_dwd$datetime >= as.POSIXlt("2008-01-01 00:00:00", "UTC") &
                              df_met_h_dwd$datetime <= as.POSIXlt("2017-12-31 23:00:00", "UTC"),]

# Write datasets
saveRDS(df_met_h, paste0(path_rdata, "/df_met_h.rds"))
saveRDS(df_met_h[substr(df_met_h$EPID, 1, 2) == "AE",], paste0(path_rdata, "/df_met_h_AE.rds"))
saveRDS(df_met_h[substr(df_met_h$EPID, 1, 2) == "HE",], paste0(path_rdata, "/df_met_h_HE.rds"))
saveRDS(df_met_h[substr(df_met_h$EPID, 1, 2) == "SE",], paste0(path_rdata, "/df_met_h_SE.rds"))

saveRDS(df_met_h_dwd, paste0(path_rdata, "/df_met_dwd_h.rds"))

# saveRDS(df_met_h_uf, paste0(path_rdata, "/df_met_h_uf.rds"))
# saveRDS(df_met_h_uf[substr(df_met_h_uf$EPID, 1, 2) == "AE",], paste0(path_rdata, "/df_met_h_uf_AE.rds"))
# saveRDS(df_met_h_uf[substr(df_met_h_uf$EPID, 1, 2) == "HE",], paste0(path_rdata, "/df_met_h_uf_HE.rds"))
# saveRDS(df_met_h_uf[substr(df_met_h_uf$EPID, 1, 2) == "SE",], paste0(path_rdata, "/df_met_h_uf_SE.rds"))

# saveRDS(df_met_d, paste0(path_rdata, "/df_met_d.rds"))
# saveRDS(df_met_d_meta, paste0(path_rdata, "/df_met_d_meta.rds"))
# saveRDS(df_met_m, paste0(path_rdata, "/df_met_m.rds"))
# saveRDS(df_met_m_meta, paste0(path_rdata, "/df_met_m_meta.rds"))
