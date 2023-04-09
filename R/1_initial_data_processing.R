# Import Landsat data (raw output from Google Earth Engine)
#### i. LIBRARY IMPORTS ####
library(data.table)

library(ggplot2)
library(maps)
library(scales)
library(ggthemes)
library(ggpubr)
library(gstat)
library(markdown)
library(ggtext)

library(lubridate)
library(dataRetrieval)
library(maps)
library(glmnet)
library(rgdal)
library(Hmisc)
library(zoo)

library(readxl)
library(patchwork)
library(egg)

#### ii. THEMES ####
theme_evan <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(linetype = 'dashed',color = 'grey70'),
    panel.grid.major.x = element_blank(),
    # panel.grid = element_blank(),
    legend.position = 'none',
    panel.border = element_rect(size = 0.5),
    text = element_text(size=8),
    axis.text = element_text(size = 8), 
    plot.title = element_text(size = 9)
  )

theme_evan_facet <- theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid = element_blank(),
    # legend.position = 'none',
    panel.border = element_rect(size = 0.5),
    strip.background = element_rect(fill = 'white'),
    text = element_text(size=12),
    axis.text = element_text(size = 12), 
    plot.title = element_text(size = 13)
  )
season_facet <- theme_evan_facet + theme(
  legend.position = 'none', 
  strip.background = element_blank(),
  strip.text = element_text(hjust = 0, margin = margin(0,0,0,0, unit = 'pt'))
)

fancy_scientific_modified <- function(l) { 
  # turn in to character string in scientific notation 
  if(abs(max(log10(l), na.rm = T) - min(log10(l), na.rm = T)) > 2 | 
     # min(l, na.rm = T) < 0.01 | 
     max(l, na.rm = T) > 1e5){ 
    l <- log10(l)
    label <- parse(text = paste("10^",as.character(l),sep = ""))
  }else{
    label <- parse(text = paste(as.character(l), sep = ""))
  }
  # print(label)
  # return(parse(text=paste("'Discharge [m'", "^3* s", "^-1 ", "*']'", sep="")))
  return(label)
}

lat_dd_lab <- function(l){
  label <- c()
  for(i in 1:length(l)){
    label_sel <- ifelse(l[i] < 0, paste0(abs(l[i]), '°S'), 
                        paste0(abs(l[i]), '°N'))
    label <- c(label, label_sel)
  }
  return(label)}

long_dd_lab <- function(l){
  label <- c()
  for(i in 1:length(l)){
    label_sel <- ifelse(l[i] < 0, paste0(abs(l[i]), '°W'), 
                        paste0(abs(l[i]), '°E'))
    label <- c(label, label_sel)
  }
  return(label)}


#### iii. SET DIRECTORIES ####
# Set root directory
wd_root <- getwd()

# Imports folder (store all import files here)
wd_imports <- paste0(wd_root,"/imports/")
# Exports folder (save all figures, tables here)
wd_exports <- paste0(wd_root,"/exports/")

wd_figures <- paste0(wd_root, "/figures/")

# Create folders within root directory to organize outputs if those folders do not exist
export_folder_paths <- c(wd_imports, wd_exports, wd_figures)
for(i in 1:length(export_folder_paths)){
  path_sel <- export_folder_paths[i]
  if(!dir.exists(path_sel)){
    dir.create(path_sel)}
}


#### 1. IMPORT DATA AND DEFINE COLUMNS ####
# Data can be downloaded from <10.5281/zenodo.7699122>
# Once downloaded, the `landsat_data_from_earth_engine` folder 
# needs to be moved into the global-asgm-imports folder.
# The following code should then run.
# Can be done using: https://inbo.github.io/inborutils/reference/download_zenodo.html package

# Import Landsat river profile data for each batch of mining sites
# Get filenames of each batch of mining sites
profile_reflectance_data_files <- list.files(pattern = 'ls57_rawBands_b7lt500.csv',
                                             paste0(wd_imports,'landsat_data_from_earth_engine/'))

# Combine Landsat sample data into one data.table
asgm_river_import <- rbindlist(
  lapply(
    paste0(wd_imports, 'landsat_data_from_earth_engine/', profile_reflectance_data_files), 
    fread),
  use.names = T, fill = T)[,':='(.geo = NULL)]

# Function to calculate distance along transect
# Uses final extension of ID string (after the last underscore)
getDistance <- function(x){return(as.numeric(strsplit(x, '_')[[1]][6]))}

# Function to get country from name of transect. 
# Will replace this with master metadata table
getCountry <- function(x){return(strsplit(x, '_')[[1]][1])}

# Add distance and country columns to main data table
# **Takes about three minutes**
asgm_river_import <- asgm_river_import[,':='(
  distance_km = lapply(`system:index`, getDistance),
  country = lapply(name, getCountry))]


#### 1A. PREPARE DATA COLUMNS, MAKE LANDSAT MATCHUP, CALCULATE SSC ESTIMATE ####
# Import Landsat data
# **Takes about three minutes**

# Landsat data do have station information
# They also have latitude and longitude
asgm_river_import <- na.omit(asgm_river_import[,
                                          ':='(
                                            # site_no = paste0('st_', site_no),
                                            site_no = name,
                                            station_nm = distance_km,
                                            # Rename columns for simplicity
                                            B1 = B1_median,
                                            B2 = B2_median,
                                            B3 = B3_median,
                                            B4 = B4_median,
                                            B5 = B5_median,
                                            B6 = B6_median,
                                            B7 = B7_median,
                                            num_pix = B2_count,
                                            sample_dt = dmy(date),
                                            landsat_dt = dmy(date)
                                          )]
                             , cols = c('B1','B2','B3','B4','B5','B7'))[
                               B1 > 0 & B2 > 0 & B3 > 0 & B4 > 0 & B5 > 0 & B7 > 0 &
                                 B1 < 5000 & B2 < 5000 & B3 < 5000 & B4 < 5000 & B6 < 4000][
                                   ,':='( 
                                     # add new columns with band ratios
                                     B1.2 = B1^2,
                                     B2.2 = B2^2,
                                     B3.2 = B3^2,
                                     B4.2 = B4^2,
                                     B5.2 = B5^2,
                                     B7.2 = B7^2,
                                     B2.B1 = B2/B1,
                                     B3.B1 = B3/B1,
                                     B4.B1 = B4/B1,
                                     B5.B1 = B5/B1,
                                     B7.B1 = B7/B1,
                                     B3.B2 = B3/B2,
                                     B4.B2 = B4/B2,
                                     B5.B2 = B5/B2,
                                     B7.B2 = B7/B2,
                                     B4.B3 = B4/B3,
                                     B5.B3 = B5/B3,
                                     B7.B3 = B7/B3,
                                     B5.B4 = B5/B4,
                                     B7.B4 = B7/B4,
                                     B7.B5 = B7/B5,
                                     Latitude = lat,
                                     Longitude = lon
                                     # station_nm = paste0(0,station_no),
                                     # site_no = paste0(0,site_no)
                                   )][ 
                                     # select only columns of interest
                                     ,.(site_no, station_nm, 
                                        # distance_km,
                                        # width, drainage_area_km2,
                                        Latitude,Longitude,sample_dt, num_pix, 
                                        # snow_ice_qa_count, 
                                        landsat_dt,
                                        B1,B2,B3,B4,B5,B6,B7,B2.B1,B3.B1,B4.B1,B5.B1,B7.B1,B3.B2,B4.B2,B5.B2,
                                        B7.B2,B4.B3,B5.B3,B7.B3,B5.B4,B7.B4,B7.B5, B1.2,B2.2,B3.2,B4.2,B5.2,B7.2
                                     )][site_no != "0"][
                                       !((B6 < 2800 & B1 > 900 & B2 > 900 & B3 > 900 & B5 > 300 & B7 > 200 & B1 > B3 & B1 < B4) | # Elimate snowy & cold images
                                           (B1 > 700 & B1/B2 > 1.2 & B5 > 200)|
                                           ((B1 + B2 + B3 + B4) > 3200 & B3 < B1 & B3/B1 < 1.5 & B6 < 2750 & B5 > 300) |
                                           (B4 > 1500 & B4/B3 > 1.5 & B6 < 2800)| # This eliminates many cloudy/snowy images at high latitudes
                                           ((B1 + B2 + B3 + B4) > 4000 & B6 < 2750 & B5 > 300 & abs(Latitude) > 40)
                                           # (B1 > 700 &
                                              # snow_ice_qa_count > (num_pix * 10) & 
                                              # snow_ice_qa_count > 500 &
                                              # B3/B1 < 1.5)
                                           )
                                     ][
                                       ,':='(month = month(sample_dt),
                                             decade = ifelse(year(sample_dt) < 1990, 1990,
                                                             ifelse(year(sample_dt) > 2019, 2020,
                                                                    year(sample_dt) - year(sample_dt)%%5)))]
# Change station_nm column to a distance column (10 km increments)
asgm_river_import <- asgm_river_import[,':='(station_nm = as.numeric(station_nm)*2 - (as.numeric(station_nm)*2)%%10)]

#### 2. CALCULATE SSC ####
# Apply SSC calibration models to make predictions based on new surface reflectance inputs (cluster needed)
# First, import function file
ssc_cluster_funs <- readRDS(paste0(wd_imports, 'SSC_cluster_function.rds'))

# And import cluster centers
clusters_calculated_list <- readRDS(paste0(wd_imports,'cluster_centers.rds'))
# Set number of cluster centers (6)
cluster_n_best <- 6
clustering_vars <- colnames(clusters_calculated_list[[cluster_n_best]]$centers)

# Scaling for cluster calculation
site_band_scaling <- readRDS(paste0(wd_imports,'site_band_scaling_all.rds'))
# Regressors
regressors_all <- c('B1', 'B2', 'B3', 'B4', 'B5', 'B7', # raw bands
                    'B1.2', 'B2.2', 'B3.2', 'B4.2', 'B5.2', 'B7.2', # squared bands
                    'site_no', # no clear way to add categorical variables
                    'B2.B1', 'B3.B1', 'B4.B1', 'B5.B1', 'B7.B1', # band ratios
                    'B3.B2', 'B4.B2', 'B5.B2', 'B7.B2',
                    'B4.B3', 'B5.B3', 'B7.B3',
                    'B5.B4', 'B7.B4', 'B7.B5')

# For base, cluster, and site predictions
getSSC_pred <- function(lm_data, regressors, cluster_funs){ # Version that includes site specification
  lm_data$pred_st <- NA
  lm_data[,ssc_subset:=cluster_sel] # clusters
  subsets <- unique(lm_data$ssc_subset)
  for(i in subsets){ # for individual cluster models
    # print(i)
    regressors_sel <- regressors[-which(regressors == 'site_no')]
    lm_data_lm <- lm_data[ssc_subset == i] # only chooses sites within cluster
    
    ssc_lm <- cluster_funs[[i]]
    glm_pred <- predict(ssc_lm, newx = as.matrix(lm_data_lm[,..regressors_sel]), s = "lambda.1se")
    lm_data[ssc_subset == i, pred_cl:= glm_pred]
    lm_data_lm <- NA
    # lm_data$res_cl[which(lm_data$ssc_subset == i)] <- resid(ssc_lm)
  }
  return(lm_data)
}

# Calculate cluster based on cluster function including scaling

# Cluster reflectance values by year, find breakpoint, cluster based on pre/post?
# OR do cluster by deforestation?
getCluster_monthly_decadal <- function(df,clustering_vars,n_centers, kmeans_object){
  # Compute band median at each site for clustering variables
  site_band_quantiles_all <- df[
      # n_insitu_samples_bySite][N_insitu_samples > 12
      ,.(N_samples = .N,
         B1 = median(B1),
         B2 = median(B2),
         B3 = median(B3),
         B4 = median(B4),
         # B5 = median(B5),
         # B7 = median(B7),
         B2.B1 = median(B2.B1),
         B3.B1 = median(B3.B1),
         B4.B1 = median(B4.B1),
         B3.B2 = median(B3.B2),
         B4.B2 = median(B4.B2),
         B4.B3 = median(B4.B3),
         B4.B3.B1 = median(B4.B3/B1)), 
      by = .(station_nm,site_no, month, decade)]
  
  site_band_quantile_scaled <- scale(site_band_quantiles_all[,..clustering_vars], 
                                     center = attributes(site_band_scaling)$`scaled:center`[clustering_vars], 
                                     scale = attributes(site_band_scaling)$`scaled:scale`[clustering_vars])
  
  closest.cluster <- function(x) {
    cluster.dist <- apply(kmeans_object$centers, 1, function(y) sqrt(sum((x-y)^2)))
    return(which.min(cluster.dist)[1])
  }
  site_band_quantiles_all$cluster <- apply(site_band_quantile_scaled, 1, closest.cluster)
  
  df_cluster <- merge(df,site_band_quantiles_all[,c('site_no','station_nm','cluster', 'month','decade')], by = c('site_no', 'station_nm','month','decade'))
  df_cluster$cluster_sel <- df_cluster$cluster
  return(df_cluster)
  
}
# Get cluster for each site based on typical spectral profile
# This takes a long time to run
asgm_river_landsat_cl <- getCluster_monthly_decadal(asgm_river_import, 
                                       clustering_vars,cluster_n_best, 
                                       clusters_calculated_list[[cluster_n_best]])


# Run SSC prediction algorithm to get clustered prediction for SSC
asgm_river_landsat_pred <- getSSC_pred(na.omit(asgm_river_landsat_cl, cols = c(regressors_all, 'cluster_sel')), 
                                  regressors_all, ssc_cluster_funs)[,':='(
                                    SSC_mgL = ifelse(pred_cl > 5.5, NA, 10^pred_cl),
                                    month = month(sample_dt),
                                    decade = ifelse(year(sample_dt) < 1990, 1990,
                                                    ifelse(year(sample_dt) > 2019, 2015, 
                                                           year(sample_dt) - year(sample_dt)%%5)))]

# Add distance column based on station_nm (which has been standing in for distance)
asgm_river_landsat_pred <- asgm_river_landsat_pred[,':='(distance_km = station_nm,
                                                         year = year(sample_dt))][
                          ,':='(distance_10km = distance_km - distance_km%%10)][
                            ,.(site_no, station_nm, distance_km, distance_10km, month, year, decade, Latitude, Longitude,sample_dt,
                               num_pix, B1 = round(B1), B2 = round(B2), B3 = round(B3), B4 = round(B4), B6 = round(B6),
                               cluster, SSC_mgL)
                          ]

fwrite(asgm_river_landsat_pred, paste0(wd_imports,'asgm_river_landsat_pred.csv'))


#### FILL GAPS IN TIMESERIES WITH MOVING AVERAGE CALCULATIONS ####
#### REDUCE TRANSECT DATA BY YEAR, DISTANCE ####
# Other sites need to be treated differently
# First, calculate SSC mean and SD for each year
ssc_avg_timeseries <- asgm_river_landsat_pred[SSC_mgL < 15000][
  ,.(SSC_mgL = mean(SSC_mgL, na.rm = T),
     SSC_mgL_sd = sd(SSC_mgL, na.rm = T)),
  by = .(site_no, distance_10km, month, year)
]

# Also calculate 3-year moving average SSC avg. and SD for each site, 10 km reach
# To calculate moving average first must formally structure data into a regular matrix (dimensions: site, time, distance)
# Make a data.table for each site, 10 km reach, month, and year 
# Get min and max months
min_year <- min(year(asgm_river_landsat_pred$sample_dt))
max_year <- max(year(asgm_river_landsat_pred$sample_dt))

n_years <- max_year - min_year + 1

max_dist <- max(asgm_river_landsat_pred$distance_10km, na.rm = T)/10 + 1

n_transects <- length(unique(asgm_river_landsat_pred$site_no))

months_all <- rep(sort(rep(c(1:12), max_dist)), n_transects*n_years)
years_all <- rep(sort(rep(c(min_year:max_year), 12 * max_dist)), n_transects)

site_nos_all <- sort(rep(unique(asgm_river_landsat_pred$site_no), 12 * max_dist * n_years))

distances_all <- rep(c(0:(max_dist-1)), 12 * n_years * n_transects) * 10

# Blank table of every possible transect, distance, year, month combination
ssc_matrix <- data.table(site_no = site_nos_all, distance_10km = distances_all, 
                         year = years_all, month = months_all)[
                           ,':='(id = paste0(substr(site_no, 0, 3), '_', year, '_', month, '_', distance_10km))
                         ]

# Fill blank table with data from Landsat. All blanks remain to keep table regular
# (Merge takes a couple minutes)
ssc_avg_timeseries_reg <- merge(ssc_matrix, ssc_avg_timeseries,
                                by = c('site_no', 'distance_10km','month','year'),
                                all = T)


# Function to get a rolling count of non-NA observations
getN <- function(x){
  x_N = length(which(!is.na(x)))
  return(x_N)
}

# Calculate 3-yr rolling mean, SD, and N observations for each transect + distance combination
# (n = 36 months in 3-year period)
ssc_ma_timeseries <- ssc_avg_timeseries_reg[order(year + month/12)
                                            ,':='(SSC_mgL_3yr = frollmean(SSC_mgL, n = 36, fill = NA, na.rm = T),
                                                  SSC_mgL_sd_3yr = frollapply(FUN = sd, SSC_mgL,n = 36, fill = NA, na.rm = T),
                                                  SSC_mgL_N_3yr = frollapply(FUN = getN, SSC_mgL,n = 36, fill = NA)),
                                            by = .(site_no, distance_10km)]

fwrite(ssc_ma_timeseries, paste0(wd_imports,'river_mining_ssc_ma_timeseries.csv'))
