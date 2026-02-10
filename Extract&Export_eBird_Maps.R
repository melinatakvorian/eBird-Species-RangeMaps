#CREATING NESTED LIST OF MATCHED SPECIES FROM EBIRD DATA

#SETUP----
##set working directory to pull the files----
setwd("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/AF Viewer OY5 Melina and Gillian/T&E Range Map Data/eBirdDataWrangling")

##install packages----
# Package names
packages <- c("readxl","tidyverse","tidyr","dplyr","stringr","sf","terra","tmap", "leaflet", "ebirdst")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


# MATCH TEST OF SPECIES ----

#setup eBird connection
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("ebird/ebirdst")

#set access key for data download
set_ebirdst_access_key("")

#use load_ranges() to get species ranges
species_names <- ebirdst_runs #ebirdst_runs has all the available species in it

#pull smartsheet list
cemml_raw <- read_xlsx("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/_Excel Files For Viewer/Species Assessments - Viewer.xlsx")

#trim CEMML master list dataset
# remove spaces in column names that we will keep because it is annoying to deal with in R
cemml_raw <- cemml_raw %>% rename(
  "Species.Common.Name" = `Species Common Name`,
  "Species.Latin.Name" = `Species Latin Name`,
  "Species.ID" = `Species ID#`
)

# remove the columns that are tallying each base up
cemml_raw <- cemml_raw %>% select(Species.Common.Name,Species.Latin.Name,Species.ID)



#MATCH TEST ----
#Matches the full name to the full name 
#i.e. matches 'Genus species subspecies' to 'Genus species subspecies'

##CEMML & eBird Match ----

#run match
match_cemml_ebird <- match(cemml_raw$Species.Latin.Name, species_names$scientific_name)

#create list of matched names
realmatch <- character(length(match_cemml_ebird))

for(i in 1:length(match_cemml_ebird)){
  val <- match_cemml_ebird[i]
  realmatch[i] <- species_names$scientific_name[val]
}

realmatch <- realmatch[!is.na(realmatch)]

rangefile <- as.list(realmatch) #the list of species that eBird has that we need!
tester <- rangefile #create a copy, in case

#Append geospatial file to species in nested list ----

for(i in 1:length(rangefile)){
  species <- realmatch[i]
  
  ebirdst_download_status(species,
                          path = ebirdst_data_dir(),
                          download_ranges = TRUE,
                          pattern = "_27km_",
                          force = FALSE)
  
  df_to_append <- load_ranges(
    species,
    resolution = "27km",
    smoothed = TRUE,
    path = ebirdst_data_dir()
  )
  
  rangefile[[i]]$data <- df_to_append
  
  ##add speciesID to datasets----
  speciesID <- which(cemml_raw$Species.Latin.Name == species)
  speciesID <- as.numeric(speciesID)
  
  rangefile[[i]][[2]]$speciesID <- speciesID
  
  print(paste(species, "speciesID: ",speciesID, " | has been added"))
  #a line after this will say whether or not the species file already existed
  
  #message showing all species that were added after this was run
  if(i == length(rangefile)){
    print(paste("the following species' geometries were added: ", realmatch))
  }
  
}

##merging breeding migration layers----
testlist <- rangefile
list <- c()
for(i in 1:length(testlist)){
  if(length(testlist[[i]][[2]]$season) > 3){
    df <- testlist[[i]][[2]]
    
    merged_geom1 <- st_union(df[c(3,4),]) 
    
    # Take attributes from row 3 (or customize later)
    merged_row1 <- df[3,]
    merged_row1$geom <- merged_geom1
    
    # Combine merged row with rows 1 and 2
    testlist[[i]][[2]] <- rbind(
      df[c(1,2),],  # keep rows 1 and 2
      merged_row1            # add merged polygon
    )
    
    testlist[[i]][[2]]$season[3] <- "migration"
    
    #testlist[[i]][[2]]$draw_order <- seq_len(nrow(testlist[[i]][[2]]))
    
    print(paste(testlist[[i]][[2]]$scientific_name[1], " merged 2 rows | index number: ", i))
  }
  else if(length(testlist[[i]][[2]]$season) == 3){ #label the third one as just 'migration' 
    testlist[[i]][[2]]$season[3] <- "migration"
    print(paste(testlist[[i]][[2]]$scientific_name[1], " relabled row | index number: ", i))
  }
  else if(length(testlist[[i]][[2]]$season) < 3){ #print a message to check that nothing else was missed
    print(paste("SEE HERE: ", testlist[[i]][[2]]$scientific_name[1], " has less than 3 rows ",
                testlist[[i]][[2]]$season[1], " ", testlist[[i]][[2]]$season[2],
                " | index number: ", i))
    
    list[length(list)+1] <- testlist[[i]][[2]]$scientific_name[1] #create a list of the species that do not have migratory ranges
    list[length(list)+1] <- i
    
  }
  
  #print all of the species that do not have migratory ranges
  if(i == length(testlist)){
    print("THESE SPECIES WERE NOT CHANGED BECAUSE THEY DO NOT HAVE MIGRATORY RANGES: ")
    print(list)
  }
}


#exporting files to folder ----

#store folder path
shapefile_folder <- "N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/AF Viewer OY5 Melina and Gillian/T&E Range Map Data/SHAPEFILES/Temporary"

#identify files that are already completed
speciesdone <- list.files(path = shapefile_folder, pattern = "\\.shp$")

#remove .shp ending to be able to run comparison later
for(i in 1:length(speciesdone)){
  strL <- str_length(speciesdone[i])
  newL <- strL - 4
  speciesdone[i] <- str_sub(speciesdone[i],1,newL)
}

#EXPORT ALL FILES IN LIST THAT HAVE NOT BEEN DONE ALREADY
#list <- c()
for(i in 1:length(testlist)){
  
  #pull list item out so that it can be saved individually
  species_object <- testlist[[i]][[2]]
  
  ##rename columns to be <10 characters long----
  species_object <- rename(species_object,
                           spec_code = species_code,
                           sci_name = scientific_name, 
                           comm_name = common_name,
                           predic_yr = prediction_year,
                           start_dt = start_date,
                           end_dt = end_date)
  
  #Column name key below:
  #species_code -> spec_code
  #scientific_name -> sci_name
  #common_name -> comm_name
  #prediction_year -> predict_yr
  #type (not changed)
  #season (not changed)
  #start_date (not changed)
  #end_date (not changed)
  #geom (not changed)
  #speciesID (not changed)
  
  ##pull species common name ----
  
  #use common name
  name <- species_object$comm_name[1] #pull common name
  name <- gsub(" ", "", tools::toTitleCase(name)) #take out space, use TitleCase
  name <- gsub("'", "", name) #remove apostrophes, ArcGIS Pro does not like them
  
  #CHECK TO SEE IF ALREADY COMPLETED -> IF NOT, create unique name for file
  if(name %in% speciesdone) next #SKIP THIS SPECIES BECAUSE IT IS ALREADY DONE
  
  #create name using name
  shapefile_location <- paste0(shapefile_folder,"/", name, ".shp")
  
  #export shapefile to file path
  st_write(species_object, shapefile_location, append=FALSE)
  
  #PRINT what has been exported - not finished
  # list[length(list+1)] <- name
  # 
  # if(i == length(testlist)){
  #   print("THESE SPECIES WERE EXPORTED TO THE SHAPEFILES FOLDER: ")
  #   print(list)
  # }
  
}
