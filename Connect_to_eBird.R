#Author: Melina Takvorian, melina.takvorian@colostate.edu
#Last edited: 10/20/2025

#DESCRIPTION: 
#This R Script connects the user to the eBird status and trends dataset, allowing them to pull specific species range data. 
#It requires that you have an access key, with permission granted from eBird. 

#SETUP----
##set working directory to pull the files----
setwd("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/AF Viewer OY5 Melina and Gillian/T&E Range Map Data/eBirdDataWrangling")

##install packages----
# Package names
packages <- c("tidyverse","tidyr","dplyr","sf","terra","tmap", "leaflet", "ebirdst")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#importing eBird data through R ----

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("ebird/ebirdst")

----------------------------------------------------------------------  
  #SET ACCESS KEY FOR DATA DOWNLOAD
  #NEED TO REQUEST ACCESS FROM EBIRD? Go here: (https://science.ebird.org/en/status-and-trends/download-data)
  set_ebirdst_access_key("") 
-------------------------------------------------------------------
  
  #use load_ranges() to get species ranges
  species_names <- ebirdst_runs #ebirdst_runs has all the available species in it

#example of downloading & mapping species ----
# ebirdst_download_status("Gray Vireo",
#                           path = ebirdst_data_dir(),
#                           download_ranges = TRUE,
#                           pattern = "_27km_")
# 
# grayVireo <- load_ranges(
#     "Gray Vireo",
#     resolution = "27km",
#     smoothed = TRUE,
#     path = ebirdst_data_dir()
#   )
# 
# tm_shape(World, bbox = st_bbox(grayVireo)) + 
#   tm_polygons(fill = "gray90", col = "white") +  # background map
#   tm_shape(grayVireo) +  # zoom to polygon extent
#   tm_polygons("season")



