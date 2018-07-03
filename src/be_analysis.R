source("C:/Users/tnauss/permanent/plygrnd/exploratorien/paper_be_meteorology/src/00_set_environment.R")


source(paste0(path_source, "be_io_lut.R"))
source(paste0(path_source, "be_io_met_annual.R"))
source(paste0(path_source, "be_io_met_monthly.R"))
source(paste0(path_source, "be_plot_multi.R"))
source(paste0(path_source, "be_plot_pr_mm_box.R"))
source(paste0(path_source, "be_plot_pr_mm_box_combined.R"))
source(paste0(path_source, "be_plot_pr_mm_ds_box.R"))
source(paste0(path_source, "be_plot_pr_mm_ds_box_combined.R"))
source(paste0(path_source, "be_plot_ta_mm_box.R"))
source(paste0(path_source, "be_plot_ta_mm_box_combined.R"))
source(paste0(path_source, "be_plot_ta_mm_box_combined_indv.R"))
source(paste0(path_source, "be_plot_ta_mm_ds_box.R"))
source(paste0(path_source, "be_plot_ta_mm_ds_box_combined.R"))
source(paste0(path_source, "be_plot_ta_mm_ds_box_combined_indv.R"))
source(paste0(path_source, "000_be_plot_pr_mmltm.R"))

#### Read data
df_met_d = readRDS(paste0(path_rdata, "/df_met_d.rds"))
df_met_m = readRDS(paste0(path_rdata, "/df_met_m.rds"))
df_met_d_meta = readRDS(paste0(path_rdata, "/df_met_d_meta.rds"))
df_met_m_meta = readRDS(paste0(path_rdata, "/df_met_m_meta.rds"))



#### Humidity related parameters
hmp = c("rH_200", "rH_200_max", "rH_200_min", "Ta_200_dew_point", "Ta_200_dew_point_approx",
        "Ta_200_humidex", "Ta_200_heat_index", "SM_10",
        "evaporation", "precipitation_radolan")
df_met_d_meta[df_met_d_meta$name %in% hmp, ]

hmp_plots = lapply(hmp, function(i){

  p = ggplot(data = df_met_d, aes_string(x = "plotID", y = i, fill = "g_belc")) +
    geom_boxplot(notch = TRUE) +
    theme_bw() +
    scale_fill_manual(values = c("#a6cee3", "#b2df8a", "#fb9a99",
                                 "#1f78b4", "#33a02c", "#e31a1c")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=20))

  if(i == "precipitation_radolan"){
    p = p + scale_y_log10()
  }

  return(p)
})
names(hmp_plots) = hmp

for(i in seq(length(hmp_plots))){
  png(file = paste0(path_vis, names(hmp_plots[i]), "_box_vs_plotId.png"),
      width = 1200, height = 800)
  plot(hmp_plots[[i]])
  dev.off()
}



df_met_d_w = df_met_d[df_met_d$g_belc %in% c("AEW", "HEW", "SEW"),]
df_met_d_w_h = df_met_d_w[, colnames(df_met_d_w) %in% hmp]
df_met_d_w_h_cor = cor(df_met_d_w_h, use = "pairwise.complete.obs")

df_met_d_w = df_met_d_w[df_met_d_w$datetime >= as.POSIXct("2014-03-01") & df_met_d_w$datetime <= as.POSIXct("2014-09-30"), ]

hmp_plots = lapply(hmp, function(i){

  p = ggplot(data = df_met_d_w, aes_string(x = "spat_SCIround", y = i, fill = "g_belc")) +
    geom_boxplot(notch = TRUE) +
    theme_bw() +
    scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=20))

  if(i == "precipitation_radolan"){
    p = p + scale_y_log10()
  }

  return(p)
})
names(hmp_plots) = hmp

for(i in seq(length(hmp_plots))){
  png(file = paste0(path_vis, "spat_SCI_", names(hmp_plots[i]), "_box.png"),
      width = 1200, height = 800)
  plot(hmp_plots[[i]])
  dev.off()
}



d_m
hmp_plots = lapply(hmp, function(i){
  agg = aggregate(df_met_d_w[, i], by = list(df_met_d_w$plotID), FUN = mean, na.rm = TRUE)
  agg = merge(agg, df_sst, by.x = "Group.1", by.y = "EPID")
  agg = merge(agg, unique(df_met_d_w[,c("plotID", "g_belc")]), by.x = "Group.1", by.y = "plotID")

  p = ggplot(data = agg, aes_string(x = "spat_SCI", y = "x", color = "g_belc")) +
    geom_point() +
    theme_bw() +
    scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c")) +
    geom_smooth(method = "lm") +
    labs(list(y = i)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=20))
  return(p)
})
names(hmp_plots) = hmp

for(i in seq(length(hmp_plots))){
  png(file = paste0(path_vis, "spat_SCI_", names(hmp_plots[i]), "_box.png"),
      width = 1200, height = 800)
  plot(hmp_plots[[i]])
  dev.off()
}





#### Standard meteorological parameters
smp = c("Ta_200", "rH_200", "precipitation_radolan", "SM_10")

smp_plots = lapply(smp, function(i){

  p = ggplot(data = df_met_d, aes_string(x = "plotID", y = i, fill = "g_belc")) +
    geom_boxplot(notch = TRUE) +
    theme_bw() +
    scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a",
                                 "#33a02c", "#fb9a99", "#e31a1c")) +
    +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size=20))
  if(i == "precipitation_radolan"){
    p = p + scale_y_log10()
  }

  return(p)
})
names(smp_plots) = smp

for(i in seq(length(smp_plots))){
  png(file = paste0(path_vis, names(smp_plots[i]), "_box_vs_plotId.png"),
      width = 1200, height = 800)
  plot(smp_plots[[i]])
  dev.off()
}




test = df_met_d[df_met_d$plotID %in% c("SEG27", "SEG40", "SEG41", "SEG50", "SEW07"), ]
ggplot(test, aes(x = datetime, y = precipitation_radolan, color = plotID)) +
  geom_line()

sew07 = test[test$plotID == "SEW07",]
seg27 = test[test$plotID == "SEG27",]
seg40 = test[test$plotID == "SEG40",]
seg41 = test[test$plotID == "SEG41",]
seg50 = test[test$plotID == "SEG50",]


summary(df_met_d[df_met_d$g_belc == "SEG",]$precipitation_radolan)
summary(df_met_d[df_met_d$plotID == "SEG50",]$precipitation_radolan)


test = df_met_d[df_met_d$g_belc == "SEG", ]
test$year = substr(as.character(test$datetime), 1, 4)

a = aggregate(test$precipitation_radolan, by = list(test$plotID, test$year), FUN = sum)
a2010 = a[a$Group.2 == "2010",]
a2010[order(a2010$x),]

a2015 = a[a$Group.2 == "2015",]
a2015[order(a2015$x),]


plot(seg50$datetime, seg50$precipitation_radolan, type = "l", col = "blue", ylim = c(-30, 30))
lines(seg50$datetime, -seg41$precipitation_radolan, col = "red")

summary(lm(seg40$precipitation_radolan ~ seg50$precipitation_radolan))
summary(lm(seg40$precipitation_radolan ~ seg41$precipitation_radolan))
summary(lm(seg41$precipitation_radolan ~ seg50$precipitation_radolan))
summary(lm(seg27$precipitation_radolan ~ seg41$precipitation_radolan))
summary(lm(seg27$precipitation_radolan ~ seg50$precipitation_radolan))
summary(lm(seg27$precipitation_radolan ~ sew07$precipitation_radolan))



# Ta_x plots per exploratory
ta_params = colnames(df_met_d)[grep("Ta", colnames(df_met_d))]
ta_params = data.frame(df_met_d_meta[df_met_d_meta$name %in% ta_params,])

ta_plots = lapply(seq(nrow(ta_params)), function(i){

  p = ggplot(data = df_met_d, aes_string(x = "g_belc", y = as.character(ta_params$name[i]))) +
    geom_boxplot(notch = TRUE) +
    theme_bw()

  if(any(grepl(ta_params$unit[i], c("day", "amount")))){

    p = p + scale_y_log10()
  }

  return(p)
})
names(ta_plots) = ta_params$name

for(i in seq(length(ta_plots))){
  png(file = paste0(path_vis, names(ta_plots[i]), ".png"),
      width = 1200, height = 1200)
  plot(ta_plots[[i]])
  dev.off()
}



df_met_m




# Hainich
nph = c("HEW10", "HEW11", "HEW12", "HEW34", "HEW35", "HEW36", "HEW37", "HEW38", "HEW39", "HEW40",
        "HEW41", "HEW42", "HEW50", "HEG17", "HEG19", "HEG41")

s = df_met_m[grepl("HEG", df_met_m$plotID), ]
s$nph = "outside NP"
s$nph[s$plotID %in% nph] = "inside NP"

p1 = ggplot(data = s, aes(x = nph, y = Ta_200_warm_days, fill = nph)) +
  geom_boxplot(notch=TRUE) +
  scale_fill_manual(values = c("#fb9a99", "#e31a1c")) +
  labs(list(title = title,
            x = "Subregion", y = "Number of days with Tmax >= 20 ?C",
            fill = "Exploratory")) +
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), text = element_text(size=20))


p2 = ggplot(data = s, aes(x = nph, y = Ta_200_cool_days, fill = nph)) +
  geom_boxplot(notch=TRUE) +
  scale_fill_manual(values = c("#1f78b4", "#a6cee3")) +
  labs(list(title = title,
            x = "Subregion", y = "Number of days with Tmax =< 10 ?C",
            fill = "Exploratory")) +
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), text = element_text(size=20))













str(df_met_d)













# Deseason annual air temperature
df_met_m = be_deseason_m(df_met_m)
df_met_a = be_deseason_a(df_met_a)


# COMBINE DATASETS
df_lui_lut = merge(df_lui, df_lut, by=c("plotID","year"), all.x = TRUE)
df_met_m = merge(df_met_m, df_lui_lut,
                  by.x=c("plotID","g_a"), by.y=c("plotID","year"),
                  all.x = TRUE)
df_met_a = merge(df_met_a, df_lui_lut,
                  by.x=c("plotID","g_pa"), by.y=c("plotID","year"),
                  all.x = TRUE)
df_bio = merge(df_met_a[df_met_a$g_a== "2009",], df_bio,by.x=c("plotID"),
                all.x = TRUE)

head(df_met_m)
head(df_met_a)
head(df_bio)


# PLOTS
# Create plots per exploratory and land cover type
belc_ta = unique(df_met_m$g_belc[df_met_m$g_belc != "AET" &
                                    df_met_m$g_belc != "SET"])
belc_p = c("AEG", "HEG", "SEG")

# Temperature
# Mean monthly air temperature over all years per Exploratory (single plots)
# lapply(belc_ta, function(x){
#   be_plot_ta_mm_box(data = df_met_m[df_met_m$g_belc == x,], title = x)
#   })

# Mean monthly air temperature over all years per Exploratory (combined plot)
be_plot_ta_mm_box_combined(data = df_met_m, notch = TRUE, title = NULL)

# Air temperature deviations from long term mean per month, year and Exploratory
# lapply(belc_ta, function(x){
#   be_plot_ta_mm_ds_box(data = df_met_m[df_met_m$g_belc == x,], title = x)
# })

# Monthly air temperature deviations from long term mean per year and Exploratory
be_plot_ta_mm_ds_box_combined(data = df_met_m, notch = TRUE, title = NULL)

head(df_met_a)

be_plot_ta_mm_ds_box_combined_indv(data = df_met_m, notch = TRUE, title = NULL,
                                   plotIDs = c("HEG42", "HEW12"),
                                   belcs = c("HEG", "HEW"))

be_plot_ta_mm_box_combined_indv(data = df_met_m, notch = TRUE, title = NULL,
                                plotIDs = c("HEG42", "HEW12"),
                                belcs = c("HEG", "HEW"))

for(i in unique(df_met_a$g_belc)){
  print(i)
  print(summary(df_met_a[df_met_a$g_belc == i,
                         c("g_belc", "Ta_200", "Ta_200_min",
                           "Ta_200_max", "P_RT_NRT")]))
}


# Rainfall
# Mean monthly rainfall over all years per Exploratory (single plots)
# lapply(belc_p, function(x){
#   be_plot_pr_mm_box(data = df_met_m[df_met_m$g_belc == x,], title = x)
# })

# Mean monthly rainfall over all years per Exploratory (combined plot)
be_plot_pr_mm_box_combined(data = df_met_m, title = NULL)

# Rainfall deviations from long term mean per month, year and Exploratory
# lapply(belc_p, function(x){
#   be_plot_pr_mm_ds_box (data = df_met_m[df_met_m$g_belc == x,], title = x)
# })

# Monthly rainfall total deviations from long term mean per year and Exploratory
be_plot_pr_mm_ds_box_combined(data = df_met_m, title = NULL)


#Test tnauss
data = df_met_m[df_met_m$g_belc == "HEG",]
data = df_met_m
title = "test"

data$LUI_cut = cut(data$LUI, c(0, median(data$LUI, na.rm = TRUE),
                                max(data$LUI, na.rm = TRUE)))



data$LUI_cut = cut(data$LUI, quantile(data$LUI, probs = seq(0, 1, 0.1), na.rm = TRUE))
data$LUI_cut = cut(data$LUI, seq(0, 5, 1))
data$M_std_cut = cut(data$M_std, seq(0, 5, 1))
data$G_std_cut = cut(data$G_std, seq(0, 10, 1))
data$F_std_cut = cut(data$F_std, seq(0, 7, 1))

ggplot(data[!is.na(data$LUI_cut),], aes(x = LUI_cut, y = Ta_200_mm_ds, fill = g_belc)) + geom_boxplot(notch=TRUE)
ggplot(data[!is.na(data$LUI_cut),], aes(x = LUI_cut, y = Ta_200_min_mm_ds, fill = g_belc)) + geom_boxplot(notch=TRUE)
ggplot(data[!is.na(data$LUI_cut),], aes(x = LUI_cut, y = Ta_200_max_mm_ds, fill = g_belc)) + geom_boxplot(notch=TRUE)

ggplot(data[!is.na(data$LUI),], aes(x = LUI, y = Ta_200_mm_ds)) + geom_point()
ggplot(data[!is.na(data$LUI),], aes(x = LUI, y = Ta_200_min_mm_ds)) + geom_point()

summary(lm(Ta_200_max_mm_ds ~ LUI, data = data))

ggplot(data, aes(x = M_std_cut, y = Ta_200_mm_ds)) + geom_boxplot(notch=TRUE)
ggplot(data, aes(x = G_std_cut, y = Ta_200_mm_ds)) + geom_boxplot(notch=TRUE)
ggplot(data, aes(x = F_std_cut, y = Ta_200_mm_ds)) + geom_boxplot(notch=TRUE)
ggplot(data, aes(x = LUI, y = M_std, color = as.factor(g_a))) + geom_point()
ggplot(data[data$plotID == "SEG20" & data$g_a < 2013,], aes(x = timestamp, y = Ta_200_mm_ds, group =1)) + geom_point() + geom_smooth(method=lm)


ggplot(df_bio, aes(x = BM, y = Ta_200_am_ds)) + geom_point()
summary(lm(Ta_200_am_ds ~ BM, data = df_bio))

#Test sforteva bio
dataBM = df_bio[df_bio$g_belc == "SEG",]
title = "test BM"
dataBM$BM_cut = cut(dataBM$BM, quantile(dataBM$BM, probs = seq(0, 1, 0.1), na.rm = TRUE))
#dataBM$BM_cut = cut(dataBM$BM, seq(0, 5, 1))
ggplot(dataBM[!is.na(dataBM$BM_cut),], aes(x = BM_cut, y = Ta_200_am_ds)) + geom_boxplot(notch=TRUE)
ggplot(dataBM[!is.na(dataBM$BM_cut),], aes(x = BM, y = Ta_200)) + geom_point()
plot(Ta_200 ~ BM, dataBM[!is.na(dataBM$BM_cut),])

#ggplot(dataBM, aes(x = BM, y = M_std, color = as.factor(g_a))) + geom_point()



# Mean monthly air temperature one year (in multiplot)
# png(paste0(path_output, "be_plot_multiplot_ta_200_ta_mm_box_multplot.png"),
#     width     = 3880,
#     height    = 4808,
#     units     = "px",
#     res       = 200,
#     # pointsize = 1
# )
# p1 = try(be_plot_ta_mm_box(data = df_met_m[df_met_m$g_belc == "HEG",], title = "HEG") )
# p2 = try(be_plot_ta_mm_box(data = df_met_m[df_met_m$g_belc == "HEW",], title = "HEW") )
# p3 = try(be_plot_ta_mm_box(data = df_met_m[df_met_m$g_belc == "SEG",], title = "SEG") )
# p4 = try(be_plot_ta_mm_box(data = df_met_m[df_met_m$g_belc == "SEW",], title = "SEW") )
# p5 = try(be_plot_ta_mm_box(data = df_met_m[df_met_m$g_belc == "AEG",], title = "AEG") )
# p6 = try(be_plot_ta_mm_box(data = df_met_m[df_met_m$g_belc == "AEW",], title = "AEW") )
# try(be_plot_multi(p1, p2, p3, p4, p5, p6))
# dev.off()

# Mean monthly deseasoned air temperature one year (in multiplot)
# png(paste0(path_output, "be_plot_multiplot_ta_200_ta_mm_ds_box_multplot.png"),
#     width     = 3880,
#     height    = 4808,
#     units     = "px",
#     res       = 200,
#     # pointsize = 1
# )
# p1 = try(be_plot_ta_mm_ds_box(data = df_met_m[df_met_m$g_belc == "HEG",], title = "HEG") )
# p2 = try(be_plot_ta_mm_ds_box(data = df_met_m[df_met_m$g_belc == "HEW",], title = "HEW") )
# p3 = try(be_plot_ta_mm_ds_box(data = df_met_m[df_met_m$g_belc == "SEG",], title = "SEG") )
# p4 = try(be_plot_ta_mm_ds_box(data = df_met_m[df_met_m$g_belc == "SEW",], title = "SEW") )
# p5 = try(be_plot_ta_mm_ds_box(data = df_met_m[df_met_m$g_belc == "AEG",], title = "AEG") )
# p6 = try(be_plot_ta_mm_ds_box(data = df_met_m[df_met_m$g_belc == "AEW",], title = "AEW") )
# try(be_plot_multi(p1, p2, p3, p4, p5, p6))
# dev.off()


nph = c("HEW10", "HEW11", "HEW12", "HEW34", "HEW35", "HEW36", "HEW37", "HEW38", "HEW39", "HEW40",
        "HEW41", "HEW42", "HEW50", "HEG17", "HEG19", "HEG41")

s = df_met_m[grepl("HEG", df_met_m$plotID), ]
s$nph = "outside NP"
s$nph[s$plotID %in% nph] = "inside NP"

p1 = ggplot(data = s, aes(x = nph, y = Ta_200_warm_days, fill = nph)) +
  geom_boxplot(notch=TRUE) +
  scale_fill_manual(values = c("#fb9a99", "#e31a1c")) +
  labs(list(title = title,
            x = "Subregion", y = "Number of days with Tmax >= 20 ?C",
            fill = "Exploratory")) +
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), text = element_text(size=20))


p2 = ggplot(data = s, aes(x = nph, y = Ta_200_cool_days, fill = nph)) +
  geom_boxplot(notch=TRUE) +
  scale_fill_manual(values = c("#1f78b4", "#a6cee3")) +
  labs(list(title = title,
            x = "Subregion", y = "Number of days with Tmax =< 10 ?C",
            fill = "Exploratory")) +
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), text = element_text(size=20))

multiplot(p1, p2, cols=2)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot = function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots = c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout = matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx = as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
