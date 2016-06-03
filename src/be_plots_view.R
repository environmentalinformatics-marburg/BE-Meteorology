library(mapview)
library(sp)
library(raster)

path_output <- "/home/dogbert/"
path_data <- "/home/dogbert/data/"
path_data <- "D:/active/exploratorien/data/"
path_output <- "D:/active/exploratorien/output/"


######################### HAINICH ######################
data_hai <- read.table(paste0(path_data, "hai.csv"), header = TRUE, sep = ",", dec = ".")
coordinates(data_hai) <- ~Lon+Lat
proj4string(data_hai) <- CRS("+init=epsg:4326")

## SpatialPoints ##
data_hai_pts <- as(data_hai, "SpatialPoints")
# all layers of meuse
mapview(data_hai, burst = TRUE)
#mapview(data_hai_pts)


######################### SCH ######################
png(paste0(path_output, "sch.png"),
        width     = 3880,
         height    = 4808,
         units     = "px",
         res       = 200,
         # pointsize = 1
     )
data_sch <- read.table(paste0(path_data, "sch.csv"), header = TRUE, sep = ",", dec = ".")
coordinates(data_sch) <- ~Lon+Lat
proj4string(data_sch) <- CRS("+init=epsg:4326")

## SpatialPoints ##
data_sch_pts <- as(data_sch, "SpatialPoints")
# all layers of meuse
p1<-mapview(data_sch, burst = TRUE)
#print(p1)
#mapview(data_sch_pts)
#dev.off()

######################### ALB ######################
data_alb <- read.table(paste0(path_data, "alb.csv"), header = TRUE, sep = ",", dec = ".")
coordinates(data_alb) <- ~Lon+Lat
proj4string(data_alb) <- CRS("+init=epsg:4326")

## SpatialPoints ##
data_alb_pts <- as(data_alb, "SpatialPoints")
# all layers of meuse
mapview(data_alb, burst = TRUE)
#mapview(data_alb_pts)