#This script will try to narrow down the list of species without range maps from our sources (USFWS, NOAA, and eBird) 
#based off of the lists that we produced in the other scripts.

#SETUP----
##set working directory to pull the files----
setwd("C:/Users/melinata/Documents/RangeMapsWork/")

##install packages----
# Package names
packages <- c("readxl","xlsx", "tidyr","dplyr","stringr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

##load in files ----
m_eBird_df <- read.csv(paste0(getwd(), "/eBird-RangeMaps/eBird-Species-RangeMaps/MATCHED_list.csv"))
m_noaa_df <- read.csv(paste0(getwd(), "/NOAA-USFWS-RangeMaps/Output/extracted_NOAA_matches.csv"))
m_usfws_df <- read.csv(paste0(getwd(), "/NOAA-USFWS-RangeMaps/Output/extracted_USFWS_matches.csv"))
cemml_raw <- read_xlsx("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/_Excel Files For Viewer/Species Assessments - Viewer.xlsx")

#trim CEMML master list dataset----
# remove spaces in column names that we will keep because it is annoying to deal with in R
cemml_raw <- cemml_raw %>% rename(
  "Species.Common.Name" = `Species Common Name`,
  "Species.Latin.Name" = `Species Latin Name`,
  "Species.ID" = `Species ID#`
)

# remove the columns that are tallying each base up
cemml_raw <- cemml_raw %>% select(Species.Common.Name,Species.Latin.Name,Species.ID)

#Combine lists of matches ----
  ##pull species names lists out per source
  m_usfws_species <- m_usfws_df$SCINAME
  m_noaa_species <- m_noaa_df$Scienti
  m_eBird_list <- m_eBird_df$Species.Latin.Name
  
  ##create big list
  m_big_list <- append(m_usfws_species, m_noaa_species, after = length(m_usfws_species))
  m_big_list <- append(m_eBird_list, m_big_list, after = length(m_big_list))  
  m_short_list <- unique(m_big_list)  
  m_short_list <- as.list(m_big_list)
  
#filter from CEMML list ----
  no_sources <- cemml_raw %>% 
    filter(!Species.Latin.Name %in% m_big_list)
  
  
#write CSV with dataframe
  excel_location <- "N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/_RangeMaps/NoRangeMaps"
  xlsx::write.xlsx(no_sources, paste0(excel_location, "/Species-need-other-map-sources.csv"))
  