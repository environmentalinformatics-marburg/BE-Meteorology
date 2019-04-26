# --- example content of optional file 'src/config_local.R' ---
#
# path_input = "c:/my_data/project1/input/"
# path_output = "c:/my_data/project1/output/"
#
# if(!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)
#
# --- ---

filepath_base <- "."
path_input = paste0(filepath_base, "/input/")
path_output = paste0(filepath_base, "/output/")

if(!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)