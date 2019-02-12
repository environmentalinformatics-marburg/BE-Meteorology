filename_config_global = "src/config_global.R"
filename_config_local = "src/config_local.R"

# --- example content of optional file 'src/config_local.R' ---
#
# path_input = "c:/my_data/project1/input/"
# path_output = "c:/my_data/project1/output/"
#
# if(!dir.exists(path_output)) dir.create(path_output, recursive = TRUE)
#
# --- ---

if(file.exists(filename_config_local)) {
  source(filename_config_local)
} else {
  source(filename_config_global)
}