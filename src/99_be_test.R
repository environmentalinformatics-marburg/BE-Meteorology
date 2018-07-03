source("C:/Users/tnauss/permanent/plygrnd/exploratorien/paper_be_meteorology/src/00_set_environment.R")

#### Read data
df_met_d = readRDS(paste0(path_rdata, "/df_met_d.rds"))
df_met_m_g = readRDS(paste0(path_rdata, "/df_met_m_g.rds"))
df_met_m_w = readRDS(paste0(path_rdata, "/df_met_m_w.rds"))
df_met_d_meta = readRDS(paste0(path_rdata, "/df_met_d_meta.rds"))
df_met_m_meta = readRDS(paste0(path_rdata, "/df_met_m_meta.rds"))



#### Temperature regulation
colnames(met[grep("Ta", names(met))])

nph = c("HEW10", "HEW11", "HEW12", "HEW34", "HEW35", "HEW36", "HEW37", "HEW38", "HEW39", "HEW40",
        "HEW41", "HEW42", "HEW50", "HEG17", "HEG19", "HEG41")

b = "HEW"
met = df_met_m_w[df_met_m_w$g_belc == b,]
met$inside = "outside NP"
met$inside[met$EPID %in% nph] = "inside NP"

ggplot(data = met, aes(x = inside, y = Ta_200_DTR, fill = inside)) +
  geom_boxplot(notch = TRUE) +
  scale_fill_manual(values = c("#e31a1c", "#fb9a99")) +
  labs(list(x = "Subregion", y = "Daily temperature range (°C)")) +
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), text = element_text(size=20))

t.test(met[met$inside == "Yes", "Ta_200_DTR"], met[met$inside == "No", "Ta_200_DTR"])




ggplot(data = met, aes(x = inside, y = Ta_200_cold_days)) +
  geom_boxplot(notch = TRUE) 
t.test(met[met$inside == "Yes", "Ta_200_cold_days"], met[met$inside == "No", "Ta_200_cold_days"])

ggplot(data = met, aes(x = inside, y = Ta_200_humidex)) +
  geom_boxplot(notch = TRUE) 
t.test(met[met$inside == "Yes", "Ta_200_humidex"], met[met$inside == "No", "Ta_200_humidex"])

ggplot(data = met, aes(x = inside, y = Ta_200_dew_point)) +
  geom_boxplot(notch = TRUE) 
t.test(met[met$inside == "Yes", "Ta_200_dew_point"], met[met$inside == "No", "Ta_200_dew_point"])

ggplot(data = met, aes(x = inside, y = rH_200)) +
  geom_boxplot(notch = TRUE) 
t.test(met[met$inside == "Yes", "rH_200"], met[met$inside == "No", "rH_200"])

ggplot(data = met, aes(x = inside, y = Ta_200)) +
  geom_boxplot(notch = TRUE) 
t.test(met[met$inside == "Yes", "Ta_200"], met[met$inside == "No", "Ta_200"])



ggplot(data = met, aes(x = SMI, y = Ta_200_DTR)) +
  geom_point() + 
  geom_smooth(method = "lm")
lmod = lm(Ta_200_DTR ~ SMI, data = met)
summary(lmod)
anova(lmod)

ggplot(data = met, aes(x = spat_SCI, y = Ta_200_cold_days)) +
  geom_point() + 
  geom_smooth(method = "lm")
lmod = lm(Ta_200_cold_days ~ spat_SCI, data = met)
summary(lmod)
anova(lmod)

ggplot(data = met, aes(x = spat_SCI, y = Ta_200_dew_point)) +
  geom_point() + 
  geom_smooth(method = "lm")
lmod = lm(Ta_200_dew_point ~ spat_SCI, data = met)
summary(lmod)
anova(lmod)



ggplot(data = met, aes(x = spat_SCI, y = Ta_200_DTR)) +
  geom_point() + 
  geom_smooth(method = "lm")
lmod = lm(Ta_200_DTR ~ spat_SCI, data = met)
summary(lmod)
anova(lmod)

meta = aggregate(met$Ta_200_DTR, by = list(met$EPID, met$g_a), FUN = sum)
colnames(meta) = c("EPID", "g_a", "Ta_200_DTR")
meta = merge(meta, met[, c("EPID", "g_a", "spat_SCI")], by = c("EPID", "g_a"))
ggplot(data = meta, aes(x = spat_SCI, y = Ta_200_DTR)) +
  geom_point() + 
  geom_smooth(method = "lm")
lmod = lm(Ta_200_DTR ~ spat_SCI, data = met)
summary(lmod)
anova(lmod)


