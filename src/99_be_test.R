source("C:/Users/tnauss/permanent/plygrnd/exploratorien/paper_be_meteorology/src/00_set_environment.R")

#### Read data
df_met_d = readRDS(paste0(path_rdata, "/df_met_d.rds"))
df_met_m = readRDS(paste0(path_rdata, "/df_met_m.rds"))
df_met_d_meta = readRDS(paste0(path_rdata, "/df_met_d_meta.rds"))
df_met_m_meta = readRDS(paste0(path_rdata, "/df_met_m_meta.rds"))


#### Compute LUI residuals
b = "AEG"
met = df_met_m[df_met_m$g_belc == b,]
lmod_lui = lm(shannon ~ LUI, data = met)
summary(lmod_lui)
plot(lmod_lui)


