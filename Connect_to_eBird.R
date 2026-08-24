#Author: Melina Takvorian, melina.takvorian@colostate.edu
#Last edited: 10/20/2025

#DESCRIPTION: 
#This R Script connects the user to the eBird status and trends dataset, allowing them to pull specific species range data. 
#It requires that you have an access key, with permission granted from eBird. 

#SETUP----
##set working directory to pull the files----
setwd("N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/_RangeMaps/eBird-Species-RangeMaps")

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
  set_ebirdst_access_key("k26qd60hsqu7") 
-------------------------------------------------------------------
  
  #use load_ranges() to get species ranges
  species_names <- ebirdst_runs #ebirdst_runs has all the available species in it

#example of downloading & mapping species ----
ebirdst_download_status("Black Rail",
                          path = ebirdst_data_dir(),
                          download_ranges = TRUE,
                          pattern = "_27km_")

blackrail <- load_ranges(
    "blkrai",
    resolution = "27km",
    smoothed = TRUE,
    path = ebirdst_data_dir()
  )

tm_shape(World, bbox = st_bbox(blackrail)) +
  tm_polygons(fill = "gray90", col = "white") +  # background map
  tm_shape(blackrail) +  # zoom to polygon extent
  tm_polygons("season")

#Mountain Plover
ebirdst_download_status("Mountain Plover",
                        path = ebirdst_data_dir(),
                        download_ranges = TRUE,
                        pattern = "_27km_")

mountainplover <- load_ranges(
  "mouplo",
  resolution = "27km",
  smoothed = TRUE,
  path = ebirdst_data_dir()
)

tm_shape(World, bbox = st_bbox(mountainplover)) +
  tm_polygons(fill = "gray90", col = "white") +  # background map
  tm_shape(mountainplover) +  # zoom to polygon extent
  tm_polygons("season")

#Yellow-billed Cuckoo
ebirdst_download_status("Yellow-billed Cuckoo",
                        path = ebirdst_data_dir(),
                        download_ranges = TRUE,
                        pattern = "_27km_")

yellowbilledc <- load_ranges(
  "yebcuc",
  resolution = "27km",
  smoothed = TRUE,
  path = ebirdst_data_dir()
)

tm_shape(World, bbox = st_bbox(yellowbilledc)) +
  tm_polygons(fill = "gray90", col = "white") +  # background map
  tm_shape(yellowbilledc) +  # zoom to polygon extent
  tm_polygons("season")

#transforming data
##merging breeding migration layers----
testlist <- c(blackrail, yellowbilledc, mountainplover)

list <- c()
for(i in 1:length(testlist)){
  if(length(testlist[[i]][[2]]$season) > 3){
    df <- testlist[[i]][[2]]
    
    merged_geom1 <- sf::st_union(df[c(3,4),]) 
    merged_geom <- sf::st_make_valid(merged_geom1) #validate geometry
    
    # Take attributes from row 3 (or customize later)
    merged_row1 <- df[3,]
    sf::st_geometry(merged_row1) <- merged_geom1
    
    # Combine merged row with rows 1 and 2
    testlist[[i]][[2]] <- rbind(
      df[c(1,2),],  # keep rows 1 and 2
      merged_row1            # add merged polygon
    )
    
    testlist[[i]][[2]]$season[3] <- "migration"
    #testlist[[i]][[2]]$drawOrder[3] <- 1
    testlist[[i]][[2]] <- st_make_valid(testlist[[i]][[2]])  # <-- validate the whole object
    
    print(paste(testlist[[i]][[2]]$scientific_name[1], " merged 2 rows | index number: ", i))
  }
  else if(length(testlist[[i]][[2]]$season) == 3){ #label the third one as just 'migration' 
    testlist[[i]][[2]]$season[3] <- "migration"
    print(paste(testlist[[i]][[2]]$scientific_name[1], " relabled row | index number: ", i))
  }
  else if(length(testlist[[i]][[2]]$season) < 3){ #print a message to check that nothing else was missed
    print(paste("SEE HERE: ", testlist[[i]][[2]]$scientific_name[1], " has less than 3 rows: ",
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

