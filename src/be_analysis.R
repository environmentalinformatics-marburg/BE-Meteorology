require(grid)
library(ggplot2)
library(reshape2)

path_source <- "/home/dogbert/paper_be_meteorology/src/"
path_data <- "/home/dogbert/data/"
path_output <- "/home/dogbert/output/"

source(paste0(path_source, "be_deseason.R"))
source(paste0(path_source, "be_io_lui.R"))
source(paste0(path_source, "be_io_lut.R"))
source(paste0(path_source, "be_io_met_annual.R"))
source(paste0(path_source, "be_io_met_monthly.R"))
source(paste0(path_source, "be_plot_multi.R"))
source(paste0(path_source, "be_plot_pr_mm_box.R"))
source(paste0(path_source, "be_plot_pr_mm_ds_box.R"))
source(paste0(path_source, "be_plot_ta_mm_box.R"))
source(paste0(path_source, "be_plot_ta_mm_ds_box.R"))
source(paste0(path_source, "000_be_plot_pr_mmltm.R"))

# Read data
df_met_m <- be_io_met_monthly(paste0(path_data, "met_m/plots.csv"))
df_met_a <- be_io_met_annual(paste0(path_data, "met_a/plots.csv"))
df_lui <- be_io_lui(paste0(path_data, "lui.csv"))
df_lut <- be_io_lut(paste0(path_data, "lut.csv"))
df_bio <- read.table(paste0(path_data, "biomasse.csv"), header = TRUE, sep = ";", dec = ",")

# Deseason annual air temperature
df_met_m <- be_deseason_m(df_met_m)
df_met_a <- be_deseason_a(df_met_a)


# COMBINE DATASETS
df_lui_lut <- merge(df_lui, df_lut, by=c("plotID","year"), all.x = TRUE)
df_met_m <- merge(df_met_m, df_lui_lut, 
                  by.x=c("plotID","g_a"), by.y=c("plotID","year"),
                  all.x = TRUE)
df_met_a <- merge(df_met_a, df_lui_lut, 
                  by.x=c("plotID","g_pa"), by.y=c("plotID","year"),
                  all.x = TRUE)
df_bio <- merge(df_met_a[df_met_a$g_a== "2009",], df_bio,by.x=c("plotID"),
                all.x = TRUE)

head(df_met_m)
head(df_met_a)


# PLOTS
# Create plots per exploratory and land cover type
belc_ta <- unique(df_met_m$g_belc[df_met_m$g_belc != "AET" & 
                                    df_met_m$g_belc != "SET"])
belc_p <- c("AEG", "HEG", "SEG")

# Temperature
# Mean monthly air temperature over all years
lapply(belc_ta, function(x){
  be_plot_ta_mm_box(data = df_met_m[df_met_m$g_belc == x,], title = x)  
  })

# Air temperature deviations from long term mean per month and year
lapply(belc_ta, function(x){
  be_plot_ta_mm_ds_box(data = df_met_m[df_met_m$g_belc == x,], title = x)  
})

# Rainfall
# Mean monthly rainfall over all years
lapply(belc_p, function(x){
  be_plot_pr_mm_box(data = df_met_m[df_met_m$g_belc == x,], title = x)  
})

# Mean monthly rainfall over all years and all plots
be_plot_p_mm_sumary(data = df_met_m,"test") 

# Mean monthly rainfall per month and year
lapply(belc_p, function(x){
  be_plot_p_mm(data = df_met_m[df_met_m$g_belc == x,], title = x)  
})

# Rainfall deviations from long term mean per month and year
lapply(belc_p, function(x){
  be_plot_pr_mm_ds_box (data = df_met_m[df_met_m$g_belc == x,], title = x)  
})


#Test tnauss
data <- df_met_m[df_met_m$g_belc == "SEG",]
title <- "test"
data$LUI_cut <- cut(data$LUI, quantile(data$LUI, probs = seq(0, 1, 0.1), na.rm = TRUE))
data$LUI_cut <- cut(data$LUI, seq(0, 5, 1))
data$M_std_cut <- cut(data$M_std, seq(0, 5, 1))
data$G_std_cut <- cut(data$G_std, seq(0, 10, 1))
data$F_std_cut <- cut(data$F_std, seq(0, 7, 1))
summary(lm(Ta_200_mm_ds ~ timestamp, data = data[data$g_a < 2014,]))
ggplot(data[!is.na(data$LUI_cut),], aes(x = LUI_cut, y = Ta_200_mm_ds)) + geom_boxplot(notch=TRUE)
ggplot(data[!is.na(data$LUI_cut),], aes(x = LUI, y = Ta_200_mm_ds)) + geom_point()
ggplot(data, aes(x = M_std_cut, y = Ta_200_mm_ds)) + geom_boxplot(notch=TRUE)
ggplot(data, aes(x = G_std_cut, y = Ta_200_mm_ds)) + geom_boxplot(notch=TRUE)
ggplot(data, aes(x = F_std_cut, y = Ta_200_mm_ds)) + geom_boxplot(notch=TRUE)
ggplot(data, aes(x = LUI, y = M_std, color = as.factor(g_a))) + geom_point()
ggplot(data[data$plotID == "SEG20" & data$g_a < 2013,], aes(x = timestamp, y = Ta_200_mm_ds, group =1)) + geom_point() + geom_smooth(method=lm)


#Test sforteva bio
dataBM <- df_bio[df_bio$g_belc == "SEG",]
title <- "test BM"
dataBM$BM_cut <- cut(dataBM$BM, quantile(dataBM$BM, probs = seq(0, 1, 0.1), na.rm = TRUE))
#dataBM$BM_cut <- cut(dataBM$BM, seq(0, 5, 1))
ggplot(dataBM[!is.na(dataBM$BM_cut),], aes(x = BM_cut, y = Ta_200_mm_ds)) + geom_boxplot(notch=TRUE)
ggplot(dataBM[!is.na(dataBM$BM_cut),], aes(x = BM, y = Ta_200)) + geom_point()
#ggplot(dataBM, aes(x = BM, y = M_std, color = as.factor(g_a))) + geom_point()



# Mean monthly air temperature one year (in multiplot)
# png(paste0(path_output, "be_plot_multiplot_ta_200_ta_mm_box_multplot.png"),
#     width     = 3880,
#     height    = 4808,
#     units     = "px",
#     res       = 200,
#     # pointsize = 1
# )
# p1 <- try(be_plot_ta_mm_box(data = df_met_m[df_met_m$g_belc == "HEG",], title = "HEG") )
# p2 <- try(be_plot_ta_mm_box(data = df_met_m[df_met_m$g_belc == "HEW",], title = "HEW") )
# p3 <- try(be_plot_ta_mm_box(data = df_met_m[df_met_m$g_belc == "SEG",], title = "SEG") )
# p4 <- try(be_plot_ta_mm_box(data = df_met_m[df_met_m$g_belc == "SEW",], title = "SEW") )
# p5 <- try(be_plot_ta_mm_box(data = df_met_m[df_met_m$g_belc == "AEG",], title = "AEG") )
# p6 <- try(be_plot_ta_mm_box(data = df_met_m[df_met_m$g_belc == "AEW",], title = "AEW") )
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
# p1 <- try(be_plot_ta_mm_ds_box(data = df_met_m[df_met_m$g_belc == "HEG",], title = "HEG") )
# p2 <- try(be_plot_ta_mm_ds_box(data = df_met_m[df_met_m$g_belc == "HEW",], title = "HEW") )
# p3 <- try(be_plot_ta_mm_ds_box(data = df_met_m[df_met_m$g_belc == "SEG",], title = "SEG") )
# p4 <- try(be_plot_ta_mm_ds_box(data = df_met_m[df_met_m$g_belc == "SEW",], title = "SEW") )
# p5 <- try(be_plot_ta_mm_ds_box(data = df_met_m[df_met_m$g_belc == "AEG",], title = "AEG") )
# p6 <- try(be_plot_ta_mm_ds_box(data = df_met_m[df_met_m$g_belc == "AEW",], title = "AEW") )
# try(be_plot_multi(p1, p2, p3, p4, p5, p6))
# dev.off()
