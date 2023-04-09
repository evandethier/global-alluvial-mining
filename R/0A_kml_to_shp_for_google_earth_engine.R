
library(rgdal)
#### IMPORT/EXPORT RAW DATA FROM GOOGLE EARTH ####
wd_root <- getwd()
wd_imports <- paste0(wd_root, '/imports/')
wd_mining_mapping_import_folder <- paste0(wd_imports, 'mining_river_profiles/')
wd_mining_mapping_folder <- paste0(wd_imports, 'mining_data_for_earth_engine/')
## Export for Google Earth Engine

# First Transect Batch
glasgm_transects_import <- readOGR(paste0(wd_mining_mapping_import_folder, 'asgm-global-transects.kml'))
writeOGR(glasgm_transects_import, dsn = './mining_data_for_earth_engine/glasgm_transects_import', layer = 'glasgm_transects_import', driver = 'ESRI Shapefile')

# Second Transect Batch
glasgm_transects_import <- readOGR(paste0(wd_mining_mapping_import_folder,'asgm-global-transects-2.kml'))
writeOGR(glasgm_transects_import, dsn = './mining_data_for_earth_engine/glasgm_transects_import_2', layer = 'glasgm_transects_import_2', driver = 'ESRI Shapefile')
# Third Transect Batch
glasgm_transects_import <- readOGR(paste0(wd_mining_mapping_import_folder,'agm-global-transects-3.kml'))
writeOGR(glasgm_transects_import, dsn = './mining_data_for_earth_engine/glasgm_transects_import_3', layer = 'glasgm_transects_import_3', driver = 'ESRI Shapefile')

# Fourth Transect Batch
glasgm_transects_import <- readOGR(paste0(wd_mining_mapping_import_folder,'agm-global-transects-4.kml'))
writeOGR(glasgm_transects_import, dsn = './mining_data_for_earth_engine/glasgm_transects_import_4', layer = 'glasgm_transects_import_4', driver = 'ESRI Shapefile')

# Fifth Transect Batch
glasgm_transects_import <- readOGR(paste0(wd_mining_mapping_import_folder,'agm-global-transects-5.kml'))
writeOGR(glasgm_transects_import, dsn = './mining_data_for_earth_engine/glasgm_transects_import_5', layer = 'glasgm_transects_import_5', driver = 'ESRI Shapefile')

# SIXTH Transect Batch
glasgm_transects_import <- readOGR(paste0(wd_mining_mapping_import_folder,'agm-global-transects-6.kml'))
writeOGR(glasgm_transects_import, dsn = './mining_data_for_earth_engine/glasgm_transects_import_6', layer = 'glasgm_transects_import_6', driver = 'ESRI Shapefile')

# Seventh Transect Batch
glasgm_transects_import <- readOGR(paste0(wd_mining_mapping_import_folder,'agm-global-transects-7.kml'))
writeOGR(glasgm_transects_import, dsn = './mining_data_for_earth_engine/glasgm_transects_import_7', layer = 'glasgm_transects_import_7', driver = 'ESRI Shapefile')

# Seventh Transect Batch
glasgm_transects_import <- readOGR(paste0(wd_mining_mapping_import_folder,'agm-global-transects-8.kml'))
writeOGR(glasgm_transects_import, dsn = './mining_data_for_earth_engine/glasgm_transects_import_8', layer = 'glasgm_transects_import_8', driver = 'ESRI Shapefile')

# Mining area polygons
glasgm_polygons_import <- readOGR(paste0(wd_mining_mapping_import_folder,'ASGM_global_polygons_20230404.kml'))
writeOGR(glasgm_polygons_import, dsn = paste0(wd_mining_mapping_folder, '/ASGM_global_polygons_20230404'), layer = 'ASGM_global_polygons_20230404', driver = 'ESRI Shapefile')

