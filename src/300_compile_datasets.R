source("C:/Users/tnauss/permanent/plygrnd/exploratorien/BE-Meteorology/src/000_set_environment.R")

#### Compile data
# Read datasets
df_met_d = be_io_met_daily(paste0(path_data, "/met_d/plots.csv"))
df_met_d = df_met_d[df_met_d$datetime >= as.POSIXct("2009-01-01") & df_met_d$datetime <= as.POSIXct("2016-12-31"), ]
df_met_d = df_met_d[!df_met_d$g_belc %in% c("AET", "HET", "SET"), ]
df_met_d$g_belc = factor(df_met_d$g_belc, levels = c("AEG", "HEG", "SEG",
                                                     "AEW", "HEW", "SEW"))
df_met_d_meta = read.table(paste0(path_data, "/met_d/sensor_description.csv"),
                           sep = ",", header = TRUE)

df_met_m = be_io_met_monthly(paste0(path_met_m, "/2008_2018_a28b5cd71110d9b5/plots.csv"))
df_met_a = be_io_met_annual(paste0(path_data, "met_a/plots.csv"))
df_met_m_meta = read.table(paste0(path_met_m, "/2008_2018_a28b5cd71110d9b5/sensor_description.csv"),
                           sep = ",", header = TRUE)

df_lut = readBExplLUT(paste0(path_lui, "lut.csv"))
df_lui = readBExpLUI(paste0(path_lui, "/LUI_glob_sep_22.02.2018+224004.txt"))
df_smi = readBExpSMI(paste0(path_smi, "/17746_Forest_EP_SMI_Silvicultural_management_intensity_index_1.2.2/17746.txt"))
df_sst = readBExpStandStruc(paste0(path_smi, "/20106_Forest_EP_Stand_structural_attributes_core_SSA_1.2.2/20106.txt"))

# df_vegrel = readBExpVegReleves(paste0(path_releves, "/19686_Vegetation Records for Grassland EPs, 2008 - 2016_1.7.13/19686.txt"))
# df_vegrel = compSpecRichBExpVegReleves(df_vegrel)
df_veghead = readBExpVegHeaderData(paste0(path_releves, "Vegetation_Header_Data_2008-2016.csv"))
df_vegforest = readRDS(paste0(path_rdata, "/forest_diversity.rds"))


# Compile grassland data
df_met_d_g = merge(df_met_d, df_lui, by.x = c("EPID", "g_a"), by.y = c("EPID", "Year"))
df_met_d_g = merge(df_met_d_g, df_veghead, by.x = c("EPID", "g_a"), by.y = c("EPID", "Year"))
df_met_d_g = merge(df_met_d_g, df_lut, by.x = c("EPID", "g_a"), by.y = c("EPID", "year"), all.x = TRUE)
df_met_d_g$EPID = droplevels(df_met_d_g$EPID)
length(unique(df_met_d_g$EPID)) == 150

df_met_m_g = merge(df_met_m, df_lui, by.x = c("EPID", "g_a"), by.y = c("EPID", "Year"))
df_met_m_g = merge(df_met_m_g, df_veghead, by.x = c("EPID", "g_a"), by.y = c("EPID", "Year"))
df_met_m_g = merge(df_met_m_g, df_lut, by.x = c("EPID", "g_a"), by.y = c("EPID", "year"), all.x = TRUE)
df_met_m_g$EPID = droplevels(df_met_m_g$EPID)
length(unique(df_met_m_g$EPID)) == 150


# Compile forest data
df_met_d_w = merge(df_met_d, df_smi, by.x = c("EPID"), by.y = c("EPID"))
df_met_d_w = merge(df_met_d_w, df_sst, by.x = c("EPID"), by.y = c("EPID"))
df_met_d_w = merge(df_met_d_w, df_vegforest, by.x = c("EPID"), by.y = c("EPID"))
df_met_d_w$EPID = droplevels(df_met_d_w$EPID)
length(unique(df_met_d_w$EPID)) == 150

df_met_m_w = merge(df_met_m, df_smi, by.x = c("EPID"), by.y = c("EPID"))
df_met_m_w = merge(df_met_m_w, df_sst, by.x = c("EPID"), by.y = c("EPID"))
df_met_m_w = merge(df_met_m_w, df_vegforest, by.x = c("EPID"), by.y = c("EPID"))
df_met_m_w$EPID = droplevels(df_met_m_w$EPID)
length(unique(df_met_m_w$EPID)) == 150


# Save data
saveRDS(df_met_d_g, paste0(path_rdata, "/df_met_d_g.rds"))
saveRDS(df_met_d_w, paste0(path_rdata, "/df_met_d_w.rds"))
saveRDS(df_met_m_g, paste0(path_rdata, "/df_met_m_g.rds"))
saveRDS(df_met_m_w, paste0(path_rdata, "/df_met_m_w.rds"))
saveRDS(df_met_d_meta, paste0(path_rdata, "/df_met_d_meta.rds"))
saveRDS(df_met_m_meta, paste0(path_rdata, "/df_met_m_meta.rds"))
