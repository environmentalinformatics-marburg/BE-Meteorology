# Set path ---------------------------------------------------------------------
if(Sys.info()["sysname"] == "Windows"){
  filepath_base <- "C:/Users/tnauss/permanent/plygrnd/exploratorien/"
} else {
  filepath_base <- "/media/permanent/active/exploratorien/"
}

path_data <- paste0(filepath_base, "/data/")
path_dwd <- paste0(path_data, "/dwd/")
path_forest <- paste0(path_data, "/forest/")
path_lui <- paste0(path_data, "/lui/")
path_smi <- paste0(path_data, "/smi/")
path_plots <- paste0(path_data, "/plots/")
path_releves <- paste0(path_data, "/releves/")
path_rdata <- paste0(path_data, "/rdata/")
path_met_a <- paste0(path_data, "/met_a/")
path_met_m <- paste0(path_data, "/met_m/")
path_met_d <- paste0(path_data, "/met_d/")
path_met_h <- paste0(path_data, "/met_h/")
path_met_h_uf <- paste0(path_data, "/met_h_unfilled/")
path_temp <- paste0(path_data, "/temp/")
path_output <- paste0(path_data, "/output/")
path_vis <- paste0(path_data, "/vis/")
path_plots <- paste0(path_data, "/plots/")


# Set libraries ----------------------------------------------------------------
library(biodivTools) # devtools::install_github("environmentalinformatics-marburg/biodivTools")
library(doParallel)
library(CAST)
library(caret)
library(grid)
library(gridExtra)
library(rgeos)
library(ggplot2)
library(mapview)
library(metTools)  # devtools::install_github("environmentalinformatics-marburg/metTools")
library(raster)
library(reshape2)
library(rgdal)
library(sp)
library(vegan)
library(yaml)

# Other settings ---------------------------------------------------------------
rasterOptions(tmpdir = path_temp)

saga_cmd <- "C:/OSGeo4W64/apps/saga/saga_cmd.exe "
# initOTB("C:/OSGeo4W64/bin/")
initOTB("C:/OSGeo4W64/OTB-5.8.0-win64/OTB-5.8.0-win64/bin/")


