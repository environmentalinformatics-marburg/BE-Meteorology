# --- example content of optional file 'src/config_local.R' ---
#
# path_input = "c:/my_data/project1/input/"
# path_output = "c:/my_data/project1/output/"
#
# if(!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)
#
# --- ---


path_input = "input/"
path_output = "output/"

if(!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)