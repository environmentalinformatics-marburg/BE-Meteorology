source("C:/Users/tnauss/permanent/plygrnd/exploratorien/BE-Meteorology/src/000_set_environment.R")

# Read datasets
df_met_m_from_d = readRDS(paste0(path_rdata, "/df_met_m_from_d.rds"))
df_met_d_meta = readRDS(paste0(path_rdata, "/df_met_d_meta.rds"))


# Mean monthly temperature by exploratory and landcover
ggplot(df_met_m_from_d, aes(x = grp_months, y = Ta_200, fill = grp_belc)) + 
  geom_boxplot(position = "dodge") +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
                               "#fb9a99", "#e31a1c","#DBD413")) + 
  geom_vline(xintercept = seq(1.5, 12, 1), linetype = "dotted") +
  labs(list(x = "Month", y = "Mean air temperature (°C) 2009 to 2016")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw()


# Mean monthly precipitation by exploratory and landcover
ggplot(df_met_m_from_d, aes(x = grp_months, y = precipitation_radolan, fill = grp_belc)) + 
  geom_boxplot(position = "dodge") +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
                               "#fb9a99", "#e31a1c","#DBD413")) + 
  geom_vline(xintercept = seq(1.5, 12, 1), linetype = "dotted") +
  labs(list(x = "Month", y = "Mean monthly precipitation (mm) 2009 to 2016")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw()


# Mean monthly precipitation by landcover
tmp = melt(df_met_m_from_d[, c(19, 36, 48, 51)], id.vars = c("grp_lc", "grp_months"))
head(tmp)
tmp$grp = paste(tmp$variable, tmp$grp_lc, sep = "_")

ggplot(tmp, aes(x = grp_months, y = value, fill = grp)) + 
  geom_boxplot(position = "dodge") +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#fb9a99", "#e31a1c"),
                    breaks=c("Ta_200_cold_days_G", "Ta_200_cold_days_W", 
                             "Ta_200_summer_days_G", "Ta_200_summer_days_W"),
                    labels=c("Cold days grassland", "Cold days forest",
                             "Summer days grassland", "Summer days forest")) + 
  geom_vline(xintercept = seq(1.5, 12, 1), linetype = "dotted") +
  labs(list(x = "Month", y = "Number of days 2009 to 2016")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme_bw()


summary(df_met_m_from_d$Ta_200[substr(df_met_m_from_d$EPID, 1, 1) == "A"])
summary(df_met_m_from_d$Ta_200[substr(df_met_m_from_d$EPID, 1, 1) == "H"])
summary(df_met_m_from_d$Ta_200[substr(df_met_m_from_d$EPID, 1, 1) == "S"])




############# Testing ######################
df_met_m_from_d$aggYear = substr(df_met_m_from_d$datetime, 1, 4)
ta200_stat = df_met_m_from_d[, colnames(df_met_m_from_d) %in% c("EPID", "Ta_200", "aggYear")]
ta200_stat = aggregate(ta200_stat$Ta_200, by=list(ta200_stat$EPID, ta200_stat$aggYear), FUN=mean)
colnames(ta200_stat) = c("EPID", "Year", "Ta_200")
ta200_stat$Explo = substr(ta200_stat$EPID, 1, 3)
ggplot(ta200_stat, aes(x = Year, y = Ta_200, fill = Explo)) + 
  geom_boxplot(position = "dodge")

ta200_stat = ta200_stat[ta200_stat$Year == "2010", ]

tapply(ta200_stat$Ta_200, ta200_stat$Explo, summary)

df_met_a = read.table("C:/Users/tnauss/permanent/plygrnd/exploratorien/data/met_a/plots.csv", sep = ",", dec = ".", header = TRUE)
head(df_met_a)
ta_stat_a = aggregate(df_met_a$Ta_200, by = list(df_met_a$plotID, df_met_a$datetime), FUN=mean)
colnames(ta_stat_a) = c("EPID", "Year", "Ta_200")
head(ta_stat_a)
ta_stat_a$Explo = substr(ta_stat_a$EPID, 1, 3)
ggplot(ta_stat_a[!ta_stat_a$Explo %in% c("AET", "HET", "SET"), ], aes(x = Year, y = Ta_200, fill = Explo)) + 
  geom_boxplot()

