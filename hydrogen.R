##################################
# HYDROGEN PROJECTS DATABASE
# Laura A.
##################################

main_dir <- getwd()
dir.create("Elab")
res_dir_path <- paste0(main_dir, "/", "Elab")

# 0. INSTALL - LOAD LIBRARIES
#----------------------------
libs <- c(
  "readxl", "tidyverse", "dplyr",
  "ggplot2"
)
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)){
  install.packages(libs[!installed_libs])
}
invisible(lapply(libs, library, character.only = T))
