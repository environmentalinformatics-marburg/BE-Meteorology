#!/usr/bin/Rscript
# note: needs to be run from parent directory.
source("src/000_set_environment.R")

datetime_format <- "%Y-%m-%dT%H:%M"
#datetime_format <- "%Y-%m-%dT%H"

src_path <- path_rdata
dst_path <- paste0(path_data, "csv_tubedb/")

dir.create(file.path(dst_path))


convert_sensor_file <- function(sensor, scr_filename) {
  plot <- stringr::str_match(pattern = ".*_(.*).rds", scr_filename)[1,2]
  
  src <- readRDS(paste0(src_path, scr_filename))
  
  datetime <- as.character(src$datetime, format=datetime_format)
  
  dst <- data.table::data.table(datetime=datetime)
  
  dst[[sensor]] <- src[[sensor]]
  
  dst_filename <- paste0(plot, "f", "_", sensor, ".csv")
  
  data.table::fwrite(dst, file = paste0(dst_path, dst_filename))
  
  return(dst_filename)
}

convert_sensor <- function(sensor) {
  
  prefix <- "df_met_be_h_"
  
  pattern <- paste0(prefix, sensor, "_.*\\.rds")
  
  scr_filenames <- list.files(src_path, pattern=pattern)
  
  #scr_filename <- scr_filenames[1]
  
  result = lapply(scr_filenames, function(fn) {convert_sensor_file(sensor, fn)})
  return(result)
}

# sensors <- c("Ta_200", "rH_200")
sensors <- c("Ta_200")

result = lapply(sensors, convert_sensor)

print(result)















