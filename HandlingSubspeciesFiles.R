#Append geospatial file to species in nested list ----

#realmatch <- c("Laterallus jamaicensis", "Coccyzus americanus", "Anarhynchus montanus")
realmatch <- c("Calidris canutus")
rangefile <- as.list(realmatch)

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
  
  # ##add speciesID to datasets----
  # speciesID <- which(cemml_raw$Species.Latin.Name == species)
  # speciesID <- as.numeric(speciesID)
  # 
  # rangefile[[i]][[2]]$speciesID <- cemml_raw$Species.ID[speciesID]
  # 
  # print(paste(species, "speciesID: ",speciesID, " | has been added"))
  # #a line after this will say whether or not the species file already existed
  
  #message showing all species that were added after this was run
  if(i == length(rangefile)){
    print(paste("the following species' geometries were added: ", realmatch))
  }
  
}

#merging breeding migration layers----
testlist <- rangefile
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

#if there is only breeding, postbreeding migration, and prebreeding migration layers ----
for(i in 1:length(testlist)){
  if(length(testlist[[i]][[2]]$season) > 2){
    df <- testlist[[i]][[2]]
    
    merged_geom1 <- sf::st_union(df[c(2,3),]) 
    merged_geom <- sf::st_make_valid(merged_geom1) #validate geometry
    
    # Take attributes from row 3 (or customize later)
    merged_row1 <- df[2,]
    sf::st_geometry(merged_row1) <- merged_geom1
    
    # Combine merged row with rows 1
    testlist[[i]][[2]] <- rbind(
      df[c(1),],  # keep rows 1
      merged_row1            # add merged polygon
    )
    
    testlist[[i]][[2]]$season[2] <- "migration"
    #testlist[[i]][[2]]$drawOrder[3] <- 1
    testlist[[i]][[2]] <- st_make_valid(testlist[[i]][[2]])  # <-- validate the whole object
    
    print(paste(testlist[[i]][[2]]$scientific_name[1], " merged 2 rows | index number: ", i))
  }
}


#Add drawing order column----
  for(i in 1:length(testlist)){
    bird <- testlist[[i]][[2]]
    bird <- bird %>% 
      mutate(drawOrder = case_when(
        season == "migration"            ~ 1,
        season == "nonbreeding"          ~ 2,
        season == "breeding"             ~ 3,
        season == "resident"             ~ 1,
        TRUE                             ~ 1
      )
      )
    
    testlist[[i]][[2]] <- bird
  }


#Add speciesID column---
#I DID THIS IN ARCPRO INSTEAD. 
#I ALSO RENAMED THE SHAPEFILE NAMES TO INCLUDE THE FULL SUBSPECIES NAME, SO THAT IT WOULD APPEAR CORRECT

#exporting SHAPEfiles to folder ----

#store folder path
shapefile_folder <- "N:/RStor/CEMML/ClimateChange/0_Natural Resources Teams/Wildlife/_TEVAs/_RangeMaps/Shapefiles/Temporary"

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
  #if(name %in% speciesdone) next #SKIP THIS SPECIES BECAUSE IT IS ALREADY DONE
  
  # #create name using name
  shapefile_location <- paste0(shapefile_folder,"/", name, ".shp")
  
  #export shapefile to file path
  st_write(species_object, shapefile_location, append=FALSE)
  
  
}


#example of downloading & mapping species ----
#least tern
ebirdst_download_status("Red Knot",
                        path = ebirdst_data_dir(),
                        download_ranges = TRUE,
                        pattern = "_27km_")

leater1 <- load_ranges(
  "redkno",
  resolution = "27km",
  smoothed = TRUE,
  path = ebirdst_data_dir()
)

tm_shape(World, bbox = st_bbox(leater1)) +
  tm_polygons(fill = "gray90", col = "white") +  # background map
  tm_shape(leater1) +  # zoom to polygon extent
  tm_polygons("season")
