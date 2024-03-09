# update_Rfig
# runs directory containing all calculation files. Each Rcal file should have 2-digits prior to the file name which maintains it's position (order) in which the files are run. files can be run independently (verified by clearing the global environment between each source('file)). For the most part Rcal files use data files from the Rdat directory. Rcal outputs are exported to the Rout directory. Rfig directory contains scripts that generate figures and tables, exported into Rfig 
# Sean Kinard
# Last Edit: 2023-05-23

source('Rcal/00_setup_packages.R')

my_scripts <- list.files("/home/kinard/Documents/Research/Dissertation/03_Autochthony/Rfig") %>% 
  discard(.p=str_detect, pattern = "csv") %>% 
  discard(.p=str_detect, pattern = "png") %>% 
  discard(.p=str_detect, pattern = "pdf")

for(i in 1:length(my_scripts)) {
  source(paste("Rfig/", my_scripts[i], sep=''))
  rm(list=ls())
  my_scripts <- list.files("/home/kinard/Documents/Research/Dissertation/03_Autochthony/Rfig") %>% 
    discard(.p=str_detect, pattern = "csv") %>% 
    discard(.p=str_detect, pattern = "png") %>% 
    discard(.p=str_detect, pattern = "pdf")  }

# End update_R_fig