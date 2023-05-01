#### i. LIBRARY IMPORTS ####
## Tables
library(data.table)
library(readxl)
library(rgdal)
library(lubridate)
library(tidyr)
library(broom)

## Plots
library(ggplot2)
library(maps)
library(scales)
library(ggthemes)
library(ggpubr)
library(gstat)
library(markdown)
library(ggtext)
library(patchwork)
library(egg)
library(zoo)

## Data download
library(dataRetrieval)

## Analysis
library(glmnet)
library(Hmisc)

#### iii. SET DIRECTORIES ####
# Set root directory
wd_root <- getwd()

# Imports folder (store all import files here)
wd_imports <- paste0(wd_root,"/imports/")
# Exports folder (save all figures, tables here)
wd_exports <- paste0(wd_root,"/exports/")

wd_figures <- paste0(wd_root, "/figures/")

# Subfolders for files
wd_mining_mapping_import_folder <- paste0(wd_imports, 'mining_river_profiles/')
wd_mining_mapping_folder <- paste0(wd_imports, 'mining_data_for_earth_engine/')
wd_landsat_data <- paste0(wd_imports,'landsat_data_from_earth_engine/')
wd_oil_palm_subfolder <- paste0(wd_imports, 'oil_palm_and_mining_rivers_ssc_data/')

# Create folders within root directory to organize outputs if those folders do not exist
export_folder_paths <- c(wd_imports, wd_exports, wd_figures,
                         wd_mining_mapping_import_folder, wd_mining_mapping_folder,
                         wd_landsat_data, wd_oil_palm_subfolder)

for(i in 1:length(export_folder_paths)){
  path_sel <- export_folder_paths[i]
  if(!dir.exists(path_sel)){
    # dir.create(path_sel)
    print(paste0('create: ', path_sel))
  }else{
    print(paste0('already exists: ', path_sel))
  }
}

#### 1. IMPORT/EXPORT RAW DATA FROM GOOGLE EARTH ####
## Export for Google Earth Engine
## **Careful: if files already exist, this will overwrite them**.
## Choose a new output name or move previous file to avoid overwriting.
## Can also add argument: `overwrite_layer = FALSE` to `writeOGR`.

#### 1A. RIVER PROFILES FOR SSC TIMESERIES ANALYSIS ####
## Import full metadata
profile_metadata <- data.table(read_excel(paste0(wd_imports,'agm-profile-metadata.xlsx'), sheet = 'profile_data'))
profile_reference_metadata <- data.table(read_excel(paste0(wd_imports,'agm-profile-metadata.xlsx'), sheet = 'reference_profiles'))

profile_names_ids <- rbind(profile_metadata[,.(`Profile id`, `Profile name`)],
                           profile_reference_metadata[,.(`Profile id`, `Profile name`)],
                           use.names = T)
## Import all river profiles from each sub-file
## Write to shapefile
all_profile_files <- list.files(wd_mining_mapping_import_folder, pattern = 'river_mining_global_profiles')

for(i in 1:length(all_profile_files)){
  if(i == 1){unmatched_count <- 0}
  profile_file_sel <- all_profile_files[i] # path to selected file
  profile_file_write <- gsub('.kml', '', profile_file_sel) # folder to write
  # Import
  river_mining_profiles_import <- readOGR(
                    paste0(wd_mining_mapping_import_folder, profile_file_sel))
  profile_ids <- data.table(`Profile id` = river_mining_profiles_import@data[["Name"]])
  
  profile_ids <- profile_ids[,':='(order = 1:nrow(profile_ids))]
  profile_ids_merge <- merge(profile_ids, 
                             profile_names_ids,
                       by = 'Profile id',
                       all.x = T)[order(order)]
  
  unmatched_profiles_sel <- profile_ids_merge[is.na(`Profile name`), .(`Profile id`)]
  
  if(nrow(unmatched_profiles_sel) > 0 & unmatched_count == 0){
    unmatched_profiles <- unmatched_profiles_sel
    unmatched_count <- nrow(unmatched_profiles)
  }else if(nrow(unmatched_profiles_sel) > 0){
    unmatched_profiles <- rbind(unmatched_profiles, unmatched_profiles_sel, use.names = T)
    unmatched_count <- unmatched_count + nrow(unmatched_profiles)
  }
     
  profile_ids_merge <- profile_ids_merge[,':='(`Profile name` = 
                                                 ifelse(is.na(`Profile name`),
                                                        `Profile id`, `Profile name`))]
  
  # Change name from profile ID to profile name
  # Add property for profile ID
  river_mining_profiles_import@data[["Name"]] <- profile_ids_merge$`Profile name`
  river_mining_profiles_import@data[["Profile id"]] <- profile_ids_merge$`Profile id`
  
  # Export
  writeOGR(river_mining_profiles_import, dsn = paste0(wd_mining_mapping_folder, profile_file_write), 
           layer = profile_file_write, driver = 'ESRI Shapefile', overwrite_layer = T)
  
}

# Save unmatched profile names to filee
fwrite(unmatched_profiles, file = paste0(wd_mining_mapping_import_folder, 'unmatched_profiles.csv'))

#### 1B. MINING-EXTENT POLYGONS FOR SMALL RIVER ANALYSIS ####
# Mining area polygons
river_mining_polygons_import <- readOGR(paste0(wd_mining_mapping_import_folder,'ASGM_global_polygons_20230404.kml'))
writeOGR(river_mining_polygons_import, dsn = paste0(wd_mining_mapping_folder, '/ASGM_global_polygons_20230404'), 
         layer = 'ASGM_global_polygons_20230404', driver = 'ESRI Shapefile', overwrite_layer = T)

