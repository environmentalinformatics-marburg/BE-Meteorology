require(grid)
library(ggplot2)
library(reshape2)


path_source <- ".../paper_be_meteorology/src/"
path_data <- ".../paper_be_meteorology/data/"
path_output <- ".../paper_be_meteorology/output/"

source(paste0(path_source, "be_deseason.R"))
source(paste0(path_source, "be_io_lui.R"))
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

# Read data lokal
#df_met_m <- be_io_met_monthly(paste0(path_data, "met_m/plots.csv"))
#df_met_a <- be_io_met_annual(paste0(path_data, "met_a/plots.csv"))


# Dynamical data of tubeDB no plot=HET38&plot=HET44&
path_H_m = "http://137.248.191.219:8080/tsdb/query_csv?plot=HEG01&plot=HEG02&plot=HEG03&plot=HEG04&plot=HEG05&plot=HEG06&plot=HEG07&plot=HEG08&plot=HEG09&plot=HEG10&plot=HEG11&plot=HEG12&plot=HEG13&plot=HEG14&plot=HEG15&plot=HEG16&plot=HEG17&plot=HEG18&plot=HEG19&plot=HEG20&plot=HEG21&plot=HEG22&plot=HEG23&plot=HEG24&plot=HEG25&plot=HEG26&plot=HEG27&plot=HEG28&plot=HEG29&plot=HEG30&plot=HEG31&plot=HEG32&plot=HEG33&plot=HEG34&plot=HEG35&plot=HEG36&plot=HEG37&plot=HEG38&plot=HEG39&plot=HEG40&plot=HEG41&plot=HEG42&plot=HEG43&plot=HEG44&plot=HEG45&plot=HEG46&plot=HEG47&plot=HEG48&plot=HEG49&plot=HEG50&plot=HEW01&plot=HEW02&plot=HEW03&plot=HEW04&plot=HEW05&plot=HEW06&plot=HEW07&plot=HEW08&plot=HEW09&plot=HEW10&plot=HEW11&plot=HEW12&plot=HEW13&plot=HEW14&plot=HEW15&plot=HEW16&plot=HEW17&plot=HEW18&plot=HEW19&plot=HEW20&plot=HEW21&plot=HEW22&plot=HEW23&plot=HEW24&plot=HEW25&plot=HEW26&plot=HEW27&plot=HEW28&plot=HEW29&plot=HEW30&plot=HEW31&plot=HEW32&plot=HEW33&plot=HEW34&plot=HEW35&plot=HEW36&plot=HEW37&plot=HEW38&plot=HEW39&plot=HEW40&plot=HEW41&plot=HEW42&plot=HEW43&plot=HEW44&plot=HEW45&plot=HEW46&plot=HEW47&plot=HEW48&plot=HEW49&plot=HEW50&sensor=P_RT_NRT&sensor=P_RT_NRT_01&sensor=P_RT_NRT_02&sensor=rH_200&sensor=Ta_200&sensor=Ta_200_max&sensor=Ta_200_min&aggregation=month&interpolated=false&quality=physical&width=1692&height=100&by_month=true&col_plot=true"
path_H_a = "http://137.248.191.219:8080/tsdb/query_csv?plot=HEG01&plot=HEG02&plot=HEG03&plot=HEG04&plot=HEG05&plot=HEG06&plot=HEG07&plot=HEG08&plot=HEG09&plot=HEG10&plot=HEG11&plot=HEG12&plot=HEG13&plot=HEG14&plot=HEG15&plot=HEG16&plot=HEG17&plot=HEG18&plot=HEG19&plot=HEG20&plot=HEG21&plot=HEG22&plot=HEG23&plot=HEG24&plot=HEG25&plot=HEG26&plot=HEG27&plot=HEG28&plot=HEG29&plot=HEG30&plot=HEG31&plot=HEG32&plot=HEG33&plot=HEG34&plot=HEG35&plot=HEG36&plot=HEG37&plot=HEG38&plot=HEG39&plot=HEG40&plot=HEG41&plot=HEG42&plot=HEG43&plot=HEG44&plot=HEG45&plot=HEG46&plot=HEG47&plot=HEG48&plot=HEG49&plot=HEG50&plot=HEW01&plot=HEW02&plot=HEW03&plot=HEW04&plot=HEW05&plot=HEW06&plot=HEW07&plot=HEW08&plot=HEW09&plot=HEW10&plot=HEW11&plot=HEW12&plot=HEW13&plot=HEW14&plot=HEW15&plot=HEW16&plot=HEW17&plot=HEW18&plot=HEW19&plot=HEW20&plot=HEW21&plot=HEW22&plot=HEW23&plot=HEW24&plot=HEW25&plot=HEW26&plot=HEW27&plot=HEW28&plot=HEW29&plot=HEW30&plot=HEW31&plot=HEW32&plot=HEW33&plot=HEW34&plot=HEW35&plot=HEW36&plot=HEW37&plot=HEW38&plot=HEW39&plot=HEW40&plot=HEW41&plot=HEW42&plot=HEW43&plot=HEW44&plot=HEW45&plot=HEW46&plot=HEW47&plot=HEW48&plot=HEW49&plot=HEW50&sensor=P_RT_NRT&sensor=P_RT_NRT_01&sensor=P_RT_NRT_02&sensor=rH_200&sensor=Ta_200&sensor=Ta_200_max&sensor=Ta_200_min&aggregation=month&interpolated=false&quality=physical&width=1692&height=100&by_year=true&col_plot=true"

path_A_m = "http://137.248.191.219:8080/tsdb/query_csv?plot=AEG01&plot=AEG02&plot=AEG03&plot=AEG04&plot=AEG05&plot=AEG06&plot=AEG07&plot=AEG08&plot=AEG09&plot=AEG10&plot=AEG11&plot=AEG12&plot=AEG13&plot=AEG14&plot=AEG15&plot=AEG16&plot=AEG17&plot=AEG18&plot=AEG19&plot=AEG20&plot=AEG21&plot=AEG22&plot=AEG23&plot=AEG24&plot=AEG25&plot=AEG26&plot=AEG27&plot=AEG28&plot=AEG29&plot=AEG30&plot=AEG31&plot=AEG32&plot=AEG33&plot=AEG34&plot=AEG35&plot=AEG36&plot=AEG37&plot=AEG38&plot=AEG39&plot=AEG40&plot=AEG41&plot=AEG42&plot=AEG43&plot=AEG44&plot=AEG45&plot=AEG46&plot=AEG47&plot=AEG48&plot=AEG49&plot=AEG50&plot=AEW01&plot=AEW02&plot=AEW03&plot=AEW04&plot=AEW05&plot=AEW06&plot=AEW07&plot=AEW08&plot=AEW09&plot=AEW10&plot=AEW11&plot=AEW12&plot=AEW13&plot=AEW14&plot=AEW15&plot=AEW16&plot=AEW17&plot=AEW18&plot=AEW19&plot=AEW20&plot=AEW21&plot=AEW22&plot=AEW23&plot=AEW24&plot=AEW25&plot=AEW26&plot=AEW27&plot=AEW28&plot=AEW29&plot=AEW30&plot=AEW31&plot=AEW32&plot=AEW33&plot=AEW34&plot=AEW35&plot=AEW36&plot=AEW37&plot=AEW38&plot=AEW39&plot=AEW40&plot=AEW41&plot=AEW42&plot=AEW43&plot=AEW44&plot=AEW45&plot=AEW46&plot=AEW47&plot=AEW48&plot=AEW49&plot=AEW50&sensor=P_RT_NRT&sensor=P_RT_NRT_01&sensor=P_RT_NRT_02&sensor=rH_200&sensor=Ta_200&sensor=Ta_200_max&sensor=Ta_200_min&aggregation=month&interpolated=false&quality=physical&width=1692&height=100&by_month=true&col_plot=true"
path_A_a = "http://137.248.191.219:8080/tsdb/query_csv?plot=AEG01&plot=AEG02&plot=AEG03&plot=AEG04&plot=AEG05&plot=AEG06&plot=AEG07&plot=AEG08&plot=AEG09&plot=AEG10&plot=AEG11&plot=AEG12&plot=AEG13&plot=AEG14&plot=AEG15&plot=AEG16&plot=AEG17&plot=AEG18&plot=AEG19&plot=AEG20&plot=AEG21&plot=AEG22&plot=AEG23&plot=AEG24&plot=AEG25&plot=AEG26&plot=AEG27&plot=AEG28&plot=AEG29&plot=AEG30&plot=AEG31&plot=AEG32&plot=AEG33&plot=AEG34&plot=AEG35&plot=AEG36&plot=AEG37&plot=AEG38&plot=AEG39&plot=AEG40&plot=AEG41&plot=AEG42&plot=AEG43&plot=AEG44&plot=AEG45&plot=AEG46&plot=AEG47&plot=AEG48&plot=AEG49&plot=AEG50&plot=AEW01&plot=AEW02&plot=AEW03&plot=AEW04&plot=AEW05&plot=AEW06&plot=AEW07&plot=AEW08&plot=AEW09&plot=AEW10&plot=AEW11&plot=AEW12&plot=AEW13&plot=AEW14&plot=AEW15&plot=AEW16&plot=AEW17&plot=AEW18&plot=AEW19&plot=AEW20&plot=AEW21&plot=AEW22&plot=AEW23&plot=AEW24&plot=AEW25&plot=AEW26&plot=AEW27&plot=AEW28&plot=AEW29&plot=AEW30&plot=AEW31&plot=AEW32&plot=AEW33&plot=AEW34&plot=AEW35&plot=AEW36&plot=AEW37&plot=AEW38&plot=AEW39&plot=AEW40&plot=AEW41&plot=AEW42&plot=AEW43&plot=AEW44&plot=AEW45&plot=AEW46&plot=AEW47&plot=AEW48&plot=AEW49&plot=AEW50&sensor=P_RT_NRT&sensor=P_RT_NRT_01&sensor=P_RT_NRT_02&sensor=rH_200&sensor=Ta_200&sensor=Ta_200_max&sensor=Ta_200_min&aggregation=month&interpolated=false&quality=physical&width=1692&height=100&by_year=true&col_plot=true"

path_S_m = "http://137.248.191.219:8080/tsdb/query_csv?plot=SEG01&plot=SEG02&plot=SEG03&plot=SEG04&plot=SEG05&plot=SEG06&plot=SEG07&plot=SEG08&plot=SEG09&plot=SEG10&plot=SEG11&plot=SEG12&plot=SEG13&plot=SEG14&plot=SEG15&plot=SEG16&plot=SEG17&plot=SEG18&plot=SEG19&plot=SEG20&plot=SEG21&plot=SEG22&plot=SEG23&plot=SEG24&plot=SEG25&plot=SEG26&plot=SEG27&plot=SEG28&plot=SEG29&plot=SEG30&plot=SEG31&plot=SEG32&plot=SEG33&plot=SEG34&plot=SEG35&plot=SEG36&plot=SEG37&plot=SEG38&plot=SEG39&plot=SEG40&plot=SEG41&plot=SEG42&plot=SEG43&plot=SEG44&plot=SEG45&plot=SEG46&plot=SEG47&plot=SEG48&plot=SEG49&plot=SEG50&plot=SEW01&plot=SEW02&plot=SEW03&plot=SEW04&plot=SEW05&plot=SEW06&plot=SEW07&plot=SEW08&plot=SEW09&plot=SEW10&plot=SEW11&plot=SEW12&plot=SEW13&plot=SEW14&plot=SEW15&plot=SEW16&plot=SEW17&plot=SEW18&plot=SEW19&plot=SEW20&plot=SEW21&plot=SEW22&plot=SEW23&plot=SEW24&plot=SEW25&plot=SEW26&plot=SEW27&plot=SEW28&plot=SEW29&plot=SEW30&plot=SEW31&plot=SEW32&plot=SEW33&plot=SEW34&plot=SEW35&plot=SEW36&plot=SEW37&plot=SEW38&plot=SEW39&plot=SEW40&plot=SEW41&plot=SEW42&plot=SEW43&plot=SEW44&plot=SEW45&plot=SEW46&plot=SEW47&plot=SEW48&plot=SEW49&plot=SEW50&sensor=P_RT_NRT&sensor=P_RT_NRT_01&sensor=P_RT_NRT_02&sensor=rH_200&sensor=Ta_200&sensor=Ta_200_max&sensor=Ta_200_min&aggregation=month&interpolated=false&quality=physical&width=1692&height=100&by_month=true&col_plot=true"
path_S_a = "http://137.248.191.219:8080/tsdb/query_csv?plot=SEG01&plot=SEG02&plot=SEG03&plot=SEG04&plot=SEG05&plot=SEG06&plot=SEG07&plot=SEG08&plot=SEG09&plot=SEG10&plot=SEG11&plot=SEG12&plot=SEG13&plot=SEG14&plot=SEG15&plot=SEG16&plot=SEG17&plot=SEG18&plot=SEG19&plot=SEG20&plot=SEG21&plot=SEG22&plot=SEG23&plot=SEG24&plot=SEG25&plot=SEG26&plot=SEG27&plot=SEG28&plot=SEG29&plot=SEG30&plot=SEG31&plot=SEG32&plot=SEG33&plot=SEG34&plot=SEG35&plot=SEG36&plot=SEG37&plot=SEG38&plot=SEG39&plot=SEG40&plot=SEG41&plot=SEG42&plot=SEG43&plot=SEG44&plot=SEG45&plot=SEG46&plot=SEG47&plot=SEG48&plot=SEG49&plot=SEG50&plot=SEW01&plot=SEW02&plot=SEW03&plot=SEW04&plot=SEW05&plot=SEW06&plot=SEW07&plot=SEW08&plot=SEW09&plot=SEW10&plot=SEW11&plot=SEW12&plot=SEW13&plot=SEW14&plot=SEW15&plot=SEW16&plot=SEW17&plot=SEW18&plot=SEW19&plot=SEW20&plot=SEW21&plot=SEW22&plot=SEW23&plot=SEW24&plot=SEW25&plot=SEW26&plot=SEW27&plot=SEW28&plot=SEW29&plot=SEW30&plot=SEW31&plot=SEW32&plot=SEW33&plot=SEW34&plot=SEW35&plot=SEW36&plot=SEW37&plot=SEW38&plot=SEW39&plot=SEW40&plot=SEW41&plot=SEW42&plot=SEW43&plot=SEW44&plot=SEW45&plot=SEW46&plot=SEW47&plot=SEW48&plot=SEW49&plot=SEW50&sensor=P_RT_NRT&sensor=P_RT_NRT_01&sensor=P_RT_NRT_02&sensor=rH_200&sensor=Ta_200&sensor=Ta_200_max&sensor=Ta_200_min&aggregation=month&interpolated=false&quality=physical&width=1692&height=100&by_year=true&col_plot=true"

userpwd <- paste("a###g-ui", ":", "###", sep = "")


# HEG and HEW month
txt_H_m <- RCurl::getURL(path_H_m, userpwd = userpwd)
if(length(txt_H_m)>0) {
  if(length(grep("ERROR", txt_H_m)) == 0){
    con <- textConnection(txt_H_m)
    data_H_m <- try(read.table(con, skip = 0, header = TRUE, sep=","),TRUE)
    #r <- write.csv(data_a, file = "/tmp/RtmpZ6llJ4/plots_m.csv", append = TRUE, sep=",")
    close(con)
  }
}
# HEG and HEW year
txt_H_a <- RCurl::getURL(path_H_a, userpwd = userpwd)
if(length(txt_H_a)>0) {
  if(length(grep("ERROR", txt_H_a)) == 0){
    con <- textConnection(txt_H_a)
    data_H_a <- try(read.table(con, skip = 0, header = TRUE, sep=","),TRUE)
    #r <- write.csv(data_a, file = "/tmp/RtmpZ6llJ4/plots_m.csv", append = TRUE, sep=",")
    close(con)
  }
}
# AEG and AEW month
txt_A_m <- RCurl::getURL(path_A_m, userpwd = userpwd)
if(length(txt_A_m)>0) {
  if(length(grep("ERROR", txt_A_m)) == 0){
    con <- textConnection(txt_A_m)
    data_A_m <- try(read.table(con, skip = 0, header = TRUE, sep=","),TRUE)
    # r <- write.csv(data_a, file = "/tmp/RtmpZ6llJ4/plots_m.csv", append = TRUE)
    close(con)
  }
}
# AEG and AEW year
txt_A_a <- RCurl::getURL(path_A_a, userpwd = userpwd)
if(length(txt_A_a)>0) {
  if(length(grep("ERROR", txt_A_a)) == 0){
    con <- textConnection(txt_A_a)
    data_A_a <- try(read.table(con, skip = 0, header = TRUE, sep=","),TRUE)
    # r <- write.csv(data_a, file = "/tmp/RtmpZ6llJ4/plots_m.csv", append = TRUE)
    close(con)
  }
}
# SEG and SEW month
txt_S_m <- RCurl::getURL(path_S_m, userpwd = userpwd)
if(length(txt_S_m)>0) {
  if(length(grep("ERROR", txt_S_m)) == 0){
    con <- textConnection(txt_S_m)
    data_S_m<- try(read.table(con, skip = 0, header = TRUE, sep=","),TRUE)
    # r <- write.csv(data_a, file = "/tmp/RtmpZ6llJ4/plots_m.csv", append = TRUE)
    close(con)
  }
}
# SEG and SEW year
txt_S_a<- RCurl::getURL(path_S_a, userpwd = userpwd)
if(length(txt_S_a)>0) {
  if(length(grep("ERROR", txt_S_a)) == 0){
    con <- textConnection(txt_S_a)
    data_S_a<- try(read.table(con, skip = 0, header = TRUE, sep=","),TRUE)
    # r <- write.csv(data_a, file = "/tmp/RtmpZ6llJ4/plots_m.csv", append = TRUE)
    close(con)
  }
}

# Binden die Daten
df_met_m <- rbind(data_H_m, data_A_m, data_S_m)
df_met_a <- rbind(data_H_a, data_A_a, data_S_a)

#df_met_m <- be_io_met_monthly(df_met_m )
#df_met_a <- be_io_met_annual(df_met_a)

# Data per url
df_met_m <- be_io_met_monthly_urlData(df_met_m )
df_met_a <- be_io_met_annual_urlData(df_met_a)

df_lui <- be_io_lui(paste0(path_data, "lui.csv"))
df_lut <- be_io_lut(paste0(path_data, "lut.csv"))
df_bio <- read.table(paste0(path_data, "biomasse.csv"), header = TRUE, sep = ";", dec = ",")


# Copy the values from column P_RT_NRT_02 into column P_RT_NRT only for HET*
df_met_m$P_RT_NRT[df_met_m$plotID == "HET38"] <- df_met_m$P_RT_NRT_02[df_met_m$plotID == "HET38"]
df_met_m$P_RT_NRT[df_met_m$plotID == "HET38"] <- df_met_m$P_RT_NRT_02[df_met_m$plotID == "HET38"]

# Deseason annual air temperature
df_met_m <- be_deseason_m(df_met_m)
df_met_a <- be_deseason_a(df_met_a)


# COMBINE DATASETS
df_lui_lut <- merge(df_lui, df_lut, by=c("plot","year"), all.x = TRUE)
df_met_m <- merge(df_met_m, df_lui_lut, 
                  by.x=c("plot","g_a"), by.y=c("plot","year"),
                  all.x = TRUE)
df_met_a <- merge(df_met_a, df_lui_lut, 
                  by.x=c("plot","g_pa"), by.y=c("plot","year"),
                  all.x = TRUE)
df_bio <- merge(df_met_a[df_met_a$g_a== "2009",], df_bio,by.x=c("plot"),
                all.x = TRUE)

head(df_met_m)
head(df_met_a)
head(df_bio)


# PLOTS
# Create plots per exploratory and land cover type
belc_ta <- unique(df_met_m$g_belc[df_met_m$g_belc != "AET" & 
                                    df_met_m$g_belc != "SET"])
belc_p <- c("AEG", "HEG", "SEG")

# Temperature
# Mean monthly air temperature over all years per Exploratory (single plots)
 lapply(belc_ta, function(x){
   x="HEG"
   png(paste0(path_output, "be_plot_ta_mm_box_combined", x,".png"),
           width     = 3880,
          height    = 4808,
          units     = "px",
          res       = 200,
    pointsize = 1
     )
   be_plot_ta_mm_box(data = df_met_m[df_met_m$g_belc == x,], title = x)  
   dev.off()
   })

# Mean monthly air temperature over all years per Exploratory (combined plot)
#png(paste0(path_output, "be_plot_ta_mm_box_combined.png"),
 #        width     = 3880,
  #       height    = 4808,
  #       units     = "px",
  #       res       = 200,
         # pointsize = 1
  #  )
#be_plot_ta_mm_box_combined(data = df_met_m, notch = TRUE, title = "Mean monthly air temperature over all years per Exploratory (combined plot)")  
#dev.off()

# Air temperature deviations from long term mean per month, year and Exploratory
lapply(belc_p, function(x){
png(paste0(path_output, "be_plot_ta_ds_mm_box_combined", "HEG",".png"),
    width     = 3880,
    height    = 4808,
    units     = "px",
    res       = 200,
    pointsize = 1
)
  be_plot_ta_mm_ds_box(data = df_met_m[df_met_m$g_belc == x,], title = x) 
  dev.off()
 })

# Monthly air temperature deviations from long term mean per year and Exploratory
png(paste0(path_output, "be_plot_ta_ds_mm_box_combined.png"),
        width     = 3880,
       height    = 4808,
      units     = "px",
       res       = 200,
 pointsize = 1
  )
be_plot_ta_mm_ds_box_combined(data = df_met_m, notch = TRUE, title = "Monthly air temperature deviations from long term mean per year and Exploratory")  
dev.off()

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
#lapply(belc_p, function(x){
 # be_plot_pr_mm_box(data = df_met_m[df_met_m$g_belc == x,], title = x)  
#})

# Mean monthly rainfall over all years per Exploratory (combined plot)
png(paste0(path_output, "be_plot_pr_mm_box_combined.png"),
    width     = 3880,
    height    = 4808,
    units     = "px",
    res       = 200,
    pointsize = 1
)
be_plot_pr_mm_box_combined(data = df_met_m, title = "Mean monthly rainfall over all years per Exploratory (combined plot)") 
dev.off()


# Rainfall deviations from long term mean per month, year and Exploratory
 #(belc_p, function(x){
 #  be_plot_pr_mm_ds_box (data = df_met_m[df_met_m$g_belc == 'x',], title = "Reinfall deviations from long term mean per month, year and Exploratory")  
 #})

# Monthly rainfall total deviations from long term mean per year and Exploratory
 png(paste0(path_output, "be_plot_pr_mm_ds_box_combined.png"),
     width     = 3880,
     height    = 4808,
     units     = "px",
     res       = 200,
     pointsize = 1
 )
be_plot_pr_mm_ds_box_combined(data = df_met_m, title = NULL)  
dev.off()

#Test tnauss
data <- df_met_m[df_met_m$g_belc == "HEG",]
data <- df_met_m
title <- "test"

data$LUI_cut <- cut(data$LUI, c(0, median(data$LUI, na.rm = TRUE), 
                                max(data$LUI, na.rm = TRUE)))



data$LUI_cut <- cut(data$LUI, quantile(data$LUI, probs = seq(0, 1, 0.1), na.rm = TRUE))
data$LUI_cut <- cut(data$LUI, seq(0, 5, 1))
data$M_std_cut <- cut(data$M_std, seq(0, 5, 1))
data$G_std_cut <- cut(data$G_std, seq(0, 10, 1))
data$F_std_cut <- cut(data$F_std, seq(0, 7, 1))

png(paste0(path_output, "be_plot_LUI_Ta_200_mm_ds.png"),
    width     = 3880,
    height    = 4808,
    units     = "px",
    res       = 200,
    pointsize = 1
)

ggplot(data[!is.na(data$LUI_cut),], aes(x = LUI_cut, y = Ta_200_mm_ds, fill = g_belc),) + geom_boxplot(notch=TRUE)
dev.off()

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
dataBM <- df_bio[df_bio$g_belc == "SEG",]
title <- "test BM"
dataBM$BM_cut <- cut(dataBM$BM, quantile(dataBM$BM, probs = seq(0, 1, 0.1), na.rm = TRUE))
#dataBM$BM_cut <- cut(dataBM$BM, seq(0, 5, 1))
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
