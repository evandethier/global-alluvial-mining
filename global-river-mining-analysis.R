#### LIBRARY IMPORTS ####
library(data.table)

library(ggplot2)
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

# First, remove all existing data
rm(list = ls())

#### THEMES ####
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

abbrev_year <- function(l){
  label <- c()
  for(i in 1:length(l)){
    label_sel <- paste0("'",substr(as.character(l[i]),3,4))
    label <- c(label, label_sel)
  }
  return(label)}

#### SET DIRECTORIES ####
# Set root directory
wd_root <- "/Users/e.dethier/Library/CloudStorage/OneDrive-BowdoinCollege/research/global-alluvial-mining"

# Imports folder (store all import files here)
wd_imports <- paste0(wd_root,"/global-asgm-imports/")
# Exports folder (save all figures, tables here)
wd_exports <- paste0(wd_root,"/global-asgm-exports/")

wd_figures <- paste0(wd_exports, "global-asgm-figures/")

# Create folders within root directory to organize outputs if those folders do not exist
export_folder_paths <- c(wd_imports, wd_exports, wd_figures)
for(i in 1:length(export_folder_paths)){
  path_sel <- export_folder_paths[i]
  if(!dir.exists(path_sel)){
    dir.create(path_sel)}
}

# Set working directory for import and export
setwd(wd_imports)


#### IMPORT DATA ####
# Then import landsat data
asgm_river_landsat_pred <- fread('asgm_river_landsat_pred.csv')

# Import SSC data (derived from landsat)
ssc_ma_timeseries <- fread('river_mining_ssc_ma_timeseries.csv')
# All mining locations
# All locations
glasgm_locations_import <- readOGR('ASGM_global_sites_10232022.kml')
# writeOGR(glasgm_locations_import, dsn = './mining_data_for_earth_engine/glasgm_locations_import', layer = 'glasgm_locations_import', driver = 'ESRI Shapefile')

global_asgm_datatable_import <- data.table(data.frame(glasgm_locations_import))[
  ,':='(Latitude = coords.x2, Longitude = coords.x1)
][,.(Latitude, Longitude, Name)]

# Import site metadata
site_metadata <- data.table(read_excel('agm-transect-metadata.xlsx'))

# Import topographic data for river transects (distance, elevation, drainage area)
topo_data <- rbindlist(lapply( 
  list.files(pattern = 'topo'), 
  fread),
  use.names = T, fill = T)[
    # ,':='(distance_km = unlist(lapply(`system:index`, strsplit, '_'))[2])
    ,c('id','distance_km') := tstrsplit(`system:index`, '_', fixed=TRUE)
  ][,':='(site_no = name,
          distance_km = as.numeric(distance_km) * 2,
          elevation_m = elev_min,
          drainage_area_km2 = flow_acc_max * 0.2,
          Latitude = lat,
          Longitude = lon)][
            ,':='(distance_10km = distance_km - distance_km%%10)][
              , by = .(site_no, distance_10km),
              lapply(.SD, median),
              .SDcols = c('elevation_m','drainage_area_km2','Latitude','Longitude')
            ]


# Import gold price data from gold.org
gold_prices <- fread('gold-prices-monthly.csv')[
  ,':='(year = month(mdy(Month))/12 + year(mdy(Month)))
]

# Some transects have incomplete data due to narrow river, so need to be discarded
transects_invalid <- c('indonesia_alue_tho_agm_region', # No data after 2012
                       'indonesia_tutut_agm_region' # No data after 2012
)

#### COMPUTE BASIC STATISTICS ON RAW DATA ####

# Number of sites, transect length, images per site, pixels per segment
site_summary_stats <- asgm_river_landsat_pred[,.(distance_km = max(distance_km, na.rm = T),
                                                 n_imgs = uniqueN(sample_dt),
                                                 avg_n_pix = mean(num_pix, na.rm = T)),
                                              by = .(site_no)]

# Avg. n images per river
avg_imgs_per_river <- mean(site_summary_stats$n_imgs, na.rm = T)
max_imgs_per_river <- max(site_summary_stats$n_imgs, na.rm = T)
min_imgs_per_river <- min(site_summary_stats$n_imgs, na.rm = T)


# Add display names for some of the longer countries and continents
# Rename Philippines correctly
site_metadata <- site_metadata[,':='(country_display = 
                                       ifelse(Country == 'Democratic Republic of the Congo', 'Dem. Rep. Congo',
                                              ifelse(Country == 'Central African Republic', 'C. African Rep.',
                                                     Country)),
                                     continent_display = 
                                       ifelse(Continent == 'North America', 'N. Am',
                                              ifelse(Continent == 'South America', 'S. Am',
                                                     ifelse(Continent == 'Europe', 'Eur.',
                                                            Continent))))]
site_metadata <- site_metadata[,':='(country_display = ifelse(grepl('Phil', country_display), 
                                                              'Philippines', country_display))]

# Number of sites for gold
mining_mineral <- site_metadata[,.(mineral = .N), by = `Mining type`]
# Plot number of mining areas per country
country_n_sites_summary <- ggplot(site_metadata) + 
  geom_bar(aes(x = country_display, fill = continent_display), color = 'black', lwd = 0.2, alpha = 0.8, width = 0.85) +
  facet_grid(rows = vars(continent_display), scales = 'free_y', switch = "y", space = "free_y") +
  coord_flip(clip = 'off') +
  season_facet + 
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  scale_x_discrete(limits=rev) +
  scale_fill_manual(values = c('#A66F8D','#395953','grey70', '#F2BC57','#D98841','#BF5349')) +
  theme(panel.background = element_rect(fill = 'grey95'),
        panel.grid.major.y = element_line(color = 'white', size = 0.25),
        panel.grid.minor.y = element_line(color = 'white', size = 0.25)) +
  labs(x = 'Country',
       y = 'N mining areas') +
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    plot.title = element_text(face = "bold", hjust = 0),
    strip.text.y = element_text(margin = margin(c(3,0,3,0)), angle = 270, face = "bold", hjust = 0.5),
    strip.placement = "outside",
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )

country_n_sites_summary <- ggplotGrob(country_n_sites_summary)

# Remove clipping from left side strip label to allow for full display
for(i in which(grepl("strip-l", country_n_sites_summary$layout$name))){
  country_n_sites_summary$grobs[[i]]$layout$clip <- "off"
}

ggsave(country_n_sites_summary, filename = paste0(wd_figures, 'country_n_sites_summary.pdf'),
       width = 5, height = 10, useDingbats = F)
ggsave(country_n_sites_summary, filename = paste0(wd_figures, 'country_n_sites_summary.png'),
       width = 5, height = 10)

#### MAP MINING SITES ####
# Create data table from all AGM locations
# Each site (indiv. mining area with coordinates) is joined to metadata (mining onset, affected/unaffected kms, etc.)
global_asgm_datatable <- merge(global_asgm_datatable_import[,':='(site_no = Name)],
                               site_metadata[,':='(site_no = `AGM district name`)], 
                               by = 'site_no', all.y = TRUE)[
                                 ,':='(`Mining onset` = ifelse(!is.na(`Mining onset`), `Mining onset`, 1980))
                               ]
# How many individual sites:
n_agm_sites <- nrow(global_asgm_datatable)
# Transects with multiple sites
transect_n_sites <- global_asgm_datatable[!(grepl('TSTM', Name) | is.na(`Transect name`) | `Transect name` == 'NA' | `Reference reach (if "Reference", ref. reach; if name of reach, that reach is ref. reach for this row)` == 'Reference')][
  ,.(N_sites_per_transect = .N),
  by = .(`Transect name`)
]
# How many individual countries
n_rm_countries <- unique(global_asgm_datatable$country_display)
# How many sites on rivers too small to map
headwater_sites <- global_asgm_datatable[grepl('TSTM', Name) | 
                                         is.na(`Transect name`) |
                                         `Transect name` == 'NA']
n_agm_sites_TSTM <- nrow(headwater_sites)

# All river names
river_names <- unique(asgm_river_landsat_pred$site_no)
# How many transects
n_agm_transects <- nrow(transect_n_sites)

# Export total number of reaches
fwrite(data.table('site_tstm' = c('Headwater sites', 'Large river sites'), 'N_total_rivers' = c(n_agm_sites_TSTM, n_agm_transects)), file = 'small_and_large_river_n_sites.csv')
# How many sites within 20 degrees of Equator
sites_within_20_degrees <- nrow(global_asgm_datatable[(Latitude) <= 20])

# Number of sites with mining onset after 2000
sites_post_2000 <- site_metadata[`Mining onset` > 2000][
  ,.(n_sites_post_2000 = .N,
     percent_sites_post_2000 = round(.N/nrow(site_metadata)*100,1))]
sites_post_2007 <- site_metadata[`Mining onset` >= 2007][
  ,.(n_sites_post_2007 = .N,
     percent_sites_post_2007 = round(.N/nrow(site_metadata)*100,1))]
sites_post_2010 <- site_metadata[`Mining onset` >= 2010][
  ,.(n_sites_post_2010 = .N,
     percent_sites_post_2010 = round(.N/nrow(site_metadata)*100,1))]


# Plot summary statistics for transects
# Transect images/river histogram
site_summary_n_imgs_plot <- ggplot(site_summary_stats) + 
  geom_histogram(aes(x = n_imgs), bins = 20, fill = '#F2BC57', color = 'black', lwd = 0.2) +
  season_facet + 
  scale_x_log10(expand = expansion(mult = c(0.1,0.1))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  theme(panel.background = element_rect(fill = 'grey95'),
        panel.grid.major.y = element_line(color = 'white', size = 0.25),
        panel.grid.minor.y = element_line(color = 'white', size = 0.25)) +
  labs(x = 'N images per river',
       y = 'N rivers')

# Transect distance histogram
site_summary_distance_plot <- ggplot(site_summary_stats) + 
  geom_histogram(aes(x = distance_km), bins = 20, fill = '#BF5349', color = 'black', lwd = 0.2) +
  season_facet + 
  # scale_x_log10(expand = expansion(mult = c(0.1,0.1))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  theme(panel.background = element_rect(fill = 'grey95'),
        panel.grid.major.y = element_line(color = 'white', size = 0.25),
        panel.grid.minor.y = element_line(color = 'white', size = 0.25)) +
  labs(x = 'River kilometers',
       y = 'N rivers')

# Number of pixels per transect segment histogram
site_summary_n_pixels_plot <- ggplot(site_summary_stats) + 
  geom_histogram(aes(x = avg_n_pix), bins = 20, fill = 'grey70', color = 'black', lwd = 0.2) +
  season_facet + 
  scale_x_log10(expand = expansion(mult = c(0.1,0.1))) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  theme(panel.background = element_rect(fill = 'grey95'),
        panel.grid.major.y = element_line(color = 'white', size = 0.25),
        panel.grid.minor.y = element_line(color = 'white', size = 0.25)) +
  labs(x = 'N pixels per river segment',
       y = 'N rivers')

# Transects longer than 500 km
transects_500km <- site_summary_stats[distance_km > 500]
# Calculate number of new mining areas per year
n_sites_per_yr <- global_asgm_datatable[,.(n_sites = .N),
                                        by = `Mining onset`][order(`Mining onset`)][
                                          ,':='(n_sites_cumul = cumsum(n_sites))
                                        ]
# Mining onset histogram (by site, not transect)
mining_onset_histogram <- ggplot(global_asgm_datatable) + 
  geom_bar(aes(x = `Mining onset`), fill = '#F2BE22', color = 'black', lwd = 0.2) +
  geom_line(data = n_sites_per_yr, aes(x = `Mining onset`, y = n_sites_cumul/3.5)) +
  season_facet + 
  scale_x_continuous(breaks = c(1980,1990,2000,2010,2020), 
                     labels = c('Pre-\nLandsat',1990,2000,2010,2020)) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~.*1, breaks = c(0, 100, 200, 300)/3.5,
                                         labels = function(x){return(x*3.5)},
                                         name = 'N sites (cumul.)'),
                     expand = expansion(mult = c(0,0.1))) +
  geom_vline(xintercept = 1982, color = 'black', lwd = 0.25) +
  # scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  theme(panel.background = element_rect(fill = 'grey95'),
        panel.grid.major.y = element_line(color = 'white', size = 0.25),
        panel.grid.minor.y = element_line(color = 'white', size = 0.25)) +
  labs(x = 'Year of mining onset',
       y = 'N sites (ann.)')

ggsave(mining_onset_histogram, filename = paste0(wd_figures, 'mining_onset_histogram.pdf'),
       width = 8, height = 2.5, useDingbats = F)
ggsave(mining_onset_histogram, filename = paste0(wd_figures, 'mining_onset_histogram.png'),
       width = 8, height = 2.5)


# Combine number of images/river and river distance
site_summary_distance_nimages_plot <- ggarrange(site_summary_n_imgs_plot, site_summary_distance_plot,
                                                nrow = 2, labels = c('a','b'))
ggsave(site_summary_distance_nimages_plot, filename = paste0(wd_figures, 'site_summary_distance_nimages_plot.pdf'),
       width = 4, height = 7, useDingbats = F)
ggsave(site_summary_distance_nimages_plot, filename = paste0(wd_figures, 'site_summary_distance_nimages_plot.png'),
       width = 4, height = 7)

world_map <- data.table(map_data(map = 'world'))
# Map each mining site (individual mining areas)
global_asgm_datatable <- global_asgm_datatable[,':='(
  simple_mining_category = ifelse(!(`Mining type` %in% c('Gold', 'Diamonds', 'Nickel')), 'Other', `Mining type`))]
global_asgm_sites_map <- ggplot() +
  geom_path(data = world_map, aes(x = long, y = lat, group = group),
            fill = 'grey95', color = 'grey10', lwd = 0.2) +
  geom_point(data = global_asgm_datatable[order(`Mining onset`)], stroke = 0.2, size = 2.75,
             aes(x = Longitude, y = Latitude, fill = `Mining onset`, shape = simple_mining_category
             )) +
  season_facet +
  scale_fill_gradientn(colors = c('#185359', '#F2BE22')) +
  scale_shape_manual(values = c('Gold' = 21, 'Diamonds' = 23, 'Nickel' = 22, 'Other' = 24)) +
  scale_x_continuous(limits = c(-183, 195), expand = expansion(mult = c(0, 0)),
                     breaks = seq(-180,180,60),
                     labels = long_dd_lab,
                     sec.axis = dup_axis()) +
  scale_y_continuous(limits = c(-30, 67), expand = expansion(mult = c(0, 0)),
                     breaks = seq(-60,80,20),
                     labels = lat_dd_lab,
                     sec.axis = dup_axis()) + 
  labs(
    x = '',
    y = ''
  ) +
  theme(legend.position = c(0.02, 0.4),
        legend.justification = 'left',
        legend.key.width = unit(0.4,"cm"),
        legend.title = element_markdown())

ggsave(global_asgm_sites_map, filename = paste0(wd_figures, 'global_asgm_sites_map.pdf'),
       width = 11, height = 4, useDingbats = F)
ggsave(global_asgm_sites_map, filename = paste0(wd_figures, 'global_asgm_sites_map.png'),
       width = 11, height = 4)


####  IDENTIFY AND ANALYZE REFERENCE REACHES ####
# Calculate average SSC for each 10 km segment

# Determine reference period for each transect
# For transects with mining, reference period will be the non-mining period 
# (Usually beginning. Sometimes middle or end, like in Brazil)
# For transects without mining, reference period will be entire record
# TO DO: For transects with mining at different points along, reference period may vary depending on watershed position
# After determining reference period for each transect, calculate the avg. & SD of SSC for the reference period
# Do this on a reach-by-reach basis, probably 10 km

# Then for each transect, make a column that determines whether it will be a by-reach reference 
# (for transects with a pre-mining period).
# Match based on this column: `Reference reach (if "Reference", ref. reach; if name of reach, that reach is ref. reach for this row)`

# Transect metadata (different from site metadata, which refers to mining areas: some transects have multiple mining areas)
transect_metadata <- site_metadata[,':='(site_no = `Transect name`)][
  ,.(mining_onset = min(`Mining onset`, na.rm = T)),
  by = .(site_no, Country, Continent)
]

transect_metadata <- transect_metadata[site_no != 'NA' & !duplicated(transect_metadata$site_no)]


# Add and rename columns for metadata
site_metadata <- site_metadata[
  ,':='(transect_name = `Transect name`,
        ref_reach = `Reference reach (if "Reference", ref. reach; if name of reach, that reach is ref. reach for this row)`)][
          ,':='(ref_reach = ifelse(ref_reach == 'Reference', `Transect name`, ref_reach))]

fwrite(site_metadata, 'rm_site_metadata.csv')
# Get timespan and distances for each reference reach
site_reference_period <- site_metadata[
  ,.(onset_year = min(`Mining onset`, na.rm = T),
     reference_period_start = min(`Reference period start`, na.rm = T),
     reference_period_end = min(`Reference period end`, na.rm = T),
     reference_distance_start = min(`Reference km start`, na.rm = T),
     reference_distance_end = max(`Reference km end`, na.rm = T)),
  by = .(ref_reach)][
    , ':='(reference_period_start = 
             ifelse(!is.na(reference_period_start), reference_period_start, 1984),
           reference_period_end = 
             ifelse(!is.na(reference_period_end), reference_period_end, onset_year),
           reference_distance_start = 
             ifelse(!is.infinite(reference_distance_start), reference_distance_start, 0),
           reference_distance_end = 
             ifelse(!is.infinite(reference_distance_end), reference_distance_end, Inf)
    )
  ][,.(ref_reach, reference_period_start, reference_period_end, reference_distance_start, reference_distance_end)]
site_reference_period <- na.omit(site_reference_period, cols = 'ref_reach')

# Join reference reach timespan table with table of every transect
# Every row will have a unique transect. Some transects share a reference reach
site_reference_period <- site_reference_period[site_metadata[,.(transect_name, ref_reach)], on = 'ref_reach']
site_reference_period <- site_reference_period[!duplicated(site_reference_period)]

# Make a table just of reference reaches, no transect duplicates
reference_reaches_and_periods <- site_reference_period[!duplicated(site_reference_period$ref_reach)][
  ,':='(site_no = ref_reach)
][,.(site_no, reference_period_start, reference_period_end, reference_distance_start, reference_distance_end)]
# Get rivers that are reference reaches
reference_rivers <- unique(reference_reaches_and_periods$site_no)

# Make a data table of just the reference river data
# limit table to only within the reference period window
# Then take an average and standard deviation for the period
reference_river_data <- asgm_river_landsat_pred[site_no %chin% reference_rivers][
  reference_reaches_and_periods, on = c('site_no')
][year >= reference_period_start & year < reference_period_end & 
    distance_10km <= reference_distance_end & distance_km >= reference_distance_start &
    SSC_mgL < 4500][
      ,.(SSC_mgL_reference = mean(SSC_mgL, na.rm = T),
         SSC_mgL_reference_sd = sd(SSC_mgL, na.rm = T)),
      by = .(site_no, Latitude, Longitude, distance_10km)
    ]


# Re-join reference data with table of all transects
# This table has a column for the transect, a column for the reference transect (col. renamed as "ref_reach" for join),
# Columns for reference mean and SD (computed every 10 km)
# Need to allow cartesian join because some transects share a reference reach
reference_river_data <- site_reference_period[
  reference_river_data[,':='(ref_reach = site_no, site_no = NULL)],
  on = 'ref_reach', allow.cartesian = T]

# Make label column for whether river is a reference, or if whole record is mining
reference_river_data <- reference_river_data[
  ,':='(site_no = transect_name,
        reference_river = ifelse(ref_reach == transect_name, 'Mining river', 'Reference river'))]

# Plot reference reaches
reference_reach_combined_plot <- ggplot(reference_river_data[reference_river == 'Reference river'][
  ,':='(SSC_mgL_reference_mean = mean(SSC_mgL_reference, na.rm = T)),
  by = .(ref_reach, distance_10km)], 
  # aes(x = distance_10km, y = ref_reach, fill = SSC_mgL_reference)) + 
  aes(x = distance_10km, y = SSC_mgL_reference_mean)) + 
  geom_line(aes(group = ref_reach), color = 'black') +
  geom_line(aes(group = ref_reach, y = SSC_mgL_reference), alpha = 0.3) +
  season_facet + 
  # geom_tile(color = 'black', lwd = 0.1) + 
  # scale_fill_gradientn(colors = c('blue','grey90','red'), trans = 'log10', oob = squish) +
  season_facet + 
  facet_wrap(.~ref_reach, scales = 'free_x') +
  scale_x_continuous(expand = expansion(mult = c(0,0))) +
  labs(
    x = 'Distance',
    y = ''
  )


# Make simple reference table (summary of reference river for entire reference reach over whole reference period)
# (as opposed to a segment-specific reference)
reference_river_data_simple <- reference_river_data[
  ,.(SSC_mgL_reference = mean(SSC_mgL_reference, na.rm = T),
     SSC_mgL_reference_sd = sqrt(
       sum((SSC_mgL_reference_sd - mean(SSC_mgL_reference, na.rm = T))^2, na.rm = T)/sqrt(.N)),
     N_obs_ref = .N,
     reference_period_start = min(reference_period_start, na.rm = T),
     reference_period_end = max(reference_period_end, na.rm = T),
     reference_distance_start = min(reference_distance_start, na.rm = T),
     reference_distance_end = max(reference_distance_end, na.rm = T)
  ),
  by = .(site_no, reference_river, ref_reach)]
# Transects with pre-mining (or post-mining) data have reference data matched by kilometer


#### REFERENCE REACH NOTES ####
# Angola andrada reference reach ends at 78 km
# Colombia: colombia_antioquia_agm_region very high SSC reference. Worth checking.

#### JOIN TRANSECT DATA WITH REFERENCE DATA ####
# GOALS:
# 1. Join moving average data with reference data. Two join types:
# a) Simple (just annual avg. & SD for mining area joined with single avg & sd reference)
# b) Sophisticated (annual avg. for every 10 km reach joined to corresponding reach)

# Simple join: 3-yr moving avg. joined to single avg. reference data
# ssc_ma_annual_transect_simple_avg <- ssc_ma_annual_transect_avg[
#   reference_river_data_simple, on = 'site_no'
# ]


# Summarize data by distance, month, etc.
# Also eliminate NA data
# 1. Get just a single observation to reduce large data.table (from June of each year)
ssc_ma_annual <- ssc_ma_timeseries[month == 6 & !is.na(SSC_mgL_3yr)][
  transect_metadata, on = 'site_no'
]


# 2. Get monthly average for each river segment and year
ssc_ma_annual_avg_monthly <- ssc_ma_timeseries[!is.na(SSC_mgL_3yr)][
  ,.(SSC_mgL_3yr = mean(SSC_mgL_3yr, na.rm = T)),
  by = .(site_no, distance_10km, month, year)
][
  transect_metadata, on = 'site_no'
]

# 3. Get annual average for each river segment and year
ssc_ma_annual_avg_full <- ssc_ma_timeseries[
  ,.(SSC_mgL_3yr = mean(SSC_mgL_3yr, na.rm = T),
     SSC_mgL_sd_3yr = mean(SSC_mgL_sd_3yr, na.rm = T),
     SSC_mgL_N_3yr = mean(SSC_mgL_N_3yr, na.rm = T)),
  by = .(site_no, distance_10km, year)][
    transect_metadata, on = 'site_no'
  ]


# Add standard deviation and number of observations for later Z-score calculation
ssc_ma_annual_avg_full <- ssc_ma_annual_avg_full[
  ,':='(
    SSC_mgL_sd_3yr = sqrt(
      sum((SSC_mgL_sd_3yr - mean(SSC_mgL_3yr, na.rm = T))^2, na.rm = T)/sqrt(.N)),
    N_obs = SSC_mgL_N_3yr
  ),
  by = .(site_no, distance_10km, Country, Continent, year)]

# Join by distance
ssc_ma_annual_full_transect_avg <- ssc_ma_annual_avg_full[
  reference_river_data_simple, on = c('site_no')
][
  ,':='(reference = ifelse((site_no == ref_reach) & ((distance_10km <= reference_distance_end & 
                                                        distance_10km >= reference_distance_start &
                                                        year >= reference_period_start &
                                                        year <= reference_period_end & 
                                                        reference_river != 'Reference') |
                                                       (reference_river == 'Reference' & 
                                                          distance_10km <= reference_distance_end & 
                                                          distance_10km >= reference_distance_start)), 'Reference', 'Active mining'),
        N_obs = ifelse(is.na(N_obs), 0, round(N_obs)))
]

#### CALCULATE Z-SCORES FOR EVERY MONTHLY OBSERVATION FOR EACH 10 KM RIVER REACH ####
# Calculate simple Z-score for each year for each 10 km reach along each river transect
# Each z-score is relative to a reference distribution of SSC values
# Reference mean and SD is uniform for each transect (doesn't vary with distance)
ssc_ma_annual_full_transect_avg <- ssc_ma_annual_full_transect_avg[
  ,':='(SSC_z_score = (SSC_mgL_3yr - SSC_mgL_reference)/(sqrt(SSC_mgL_sd_3yr^2/N_obs + SSC_mgL_reference_sd^2/N_obs_ref)))
][,':='(country_display = 
          ifelse(Country == 'Democratic Republic of the Congo', 'Dem. Rep. Congo',
                 ifelse(Country == 'Central African Republic', 'C. African Rep.',
                        Country)),
        continent_display = 
          ifelse(Continent == 'North America', 'N. Am.',
                 ifelse(Continent == 'South America', 'S. Am.',
                        Continent)))][
                          ,':='(continent_country = paste0(continent_display, '_', country_display, ' '))                          
                        ][
                          # Add topographic data (TO DO: ADD PERU TO TOPOGRAPHIC DATA IMPORT, CURRENTLY MISSING)
                          # topo_data[,.(site_no, distance_10km, drainage_area_km2)],
                          # on = c('site_no','distance_10km')
                        ]

# Determine pre- or post-mining
ssc_ma_annual_full_transect_avg <- ssc_ma_annual_full_transect_avg[
  ,':='(
    # Normalize drainage area by drainage area at top of transect
    # drainage_area_norm = drainage_area_km2/min(drainage_area_km2, na.rm = T),
    pre_post_mining = ifelse(reference == 'Active mining', 'Mining era', 'Pre-mining')),
  by = .(site_no)
]

# Import transect name for display
transect_name_display <- data.table(read_excel('agm-transect-metadata.xlsx'))[
  ,.(`Transect name`, `Transect full display name`, `Transect display name`)][
    ,':='(transect_display_name = `Transect display name`,
          site_no = `Transect name`)
  ][,.(transect_display_name, site_no)]


transect_name_display <- na.omit(transect_name_display[!duplicated(transect_name_display)])

ssc_ma_annual_full_transect_avg <- ssc_ma_annual_full_transect_avg[transect_name_display, on = 'site_no']
#### SELECT AND EXPORT OIL PALM SITES FOR SUB-ANALYSIS ####
## This analysis is done in the oil_palm_sub_analysis.R script ##
# Test moving average for a given transect + distance combination
selected_ma_sites_all <- c('indonesia_batang_hari_trib2','indonesia_batang_hari_bedaro_agm_region','indonesia_batang_hari',
                           'indonesia_kampar_trib','indonesia_maura_soma_agm_region','indonesia_west_kalimantan_monggo_agm_region',
                           'indonesia_kapuas_trib','indonesia_kahayan','indonesia_nabire_barat','ghana_pra_dn',
                           'ghana_pra_up','indonesia_batang_asai_upper_agm_region','indonesia_south_sumatra_soetangegoh_agm_region',
                           'indonesia_lampung_agm_region','liberia_cavalla_river_agm_region')

for(oil_palm_site_sel in 1:length(selected_ma_sites_all)){
  selected_ma_site <- selected_ma_sites_all[oil_palm_site_sel]

# Join with transect name display for plotting
ssc_ma_timeseries_test <- transect_name_display[
  ssc_ma_timeseries[site_no == selected_ma_site], on = 'site_no'
]
# Set country and mining onset
ssc_ma_timeseries_test$Country <- transect_metadata[site_no == ssc_ma_timeseries_test$site_no[1]]$Country
ssc_ma_timeseries_test$mining_onset <- transect_metadata[site_no == ssc_ma_timeseries_test$site_no[1]]$mining_onset

# Export data for analysis in oil_palm_sub_analysis.R script
fwrite(ssc_ma_timeseries_test, file = paste0('ssc_ma_timeseries_', selected_ma_site, '.csv'))
}


#### INDIVIDUAL MINING TRANSECT TIMESERIES ####
mining_onset_table <- site_metadata[`Transect name` %in% transect_name_display$site_no,
                                    .(site_no, continent_display, `Mining onset`, `Reference km end`, `Reference period start`, `Reference period end`)][
  ,.(
     `Reference period end` = min(`Reference period end`, na.rm = T),
     `Reference period start` = min(`Reference period start`, na.rm = T),
     `Reference km end` = min(`Reference km end`, na.rm = T),
     `Mining onset` = min(`Mining onset`, na.rm = T)),
  by = .(site_no, continent_display)
][!is.na(site_no)]

# Representative transect panels
representative_transects <- c('brazil_kayapo_preserve_agm_region', 'venezuela_guyana_chicanan_agm_region','mali_faleme_river_agm_upstream', 
                              'indonesia_nabire_barat', 'myanmar_shaduzup_agm_region', 'drc_kibali_river_upper_agm_region', 'peru_rio_malinowski_agm_region')

# Run through every transect and export timeseries from 20-50 km downstream of upstream-most mining
# Save data for representative transects for subsequent visualization
plot_save <- 'no save'
for(mining_transect_num in 1:nrow(mining_onset_table)){
  if(mining_transect_num == 1){
    k <- 1
  }
  selected_ma_site <- mining_onset_table[mining_transect_num]$site_no
  ssc_ma_timeseries_ex <- transect_name_display[
    ssc_ma_timeseries[site_no == selected_ma_site], on = 'site_no'][
      mining_onset_table[site_no == selected_ma_site], on = 'site_no'][
        ,':='(`Reference km end` = ifelse(`Reference km end` == Inf, 0, `Reference km end`))][
        ifelse(is.na(`Reference km end`), distance_10km > 20 & distance_10km < 30, 
               distance_10km >= (`Reference km end` + 20) & distance_10km < (`Reference km end` + 50))]
  
  if(nrow(ssc_ma_timeseries_ex[!is.na(SSC_mgL)]) > 75){
  # Plot line plot of average standardized SSC relative to mining onset
  mining_onset <- ifelse(ssc_ma_timeseries_ex$`Mining onset`[1] < 1984, 1984, ssc_ma_timeseries_ex$`Mining onset`[1])
  reference_period_start <- ssc_ma_timeseries_ex$`Reference period start`[1]
  reference_period_end <- ssc_ma_timeseries_ex$`Reference period end`[1]
  # If there was a pause in mining that gives us the reference period (as for several rivers in Brazil)
  # Make a reference period in the middle of the mining period (with mining period on both sides)
  if(reference_period_start > 1984){
    reference_period_start <- reference_period_start
    mining_onset_multiple <- c(mining_onset, reference_period_end)
    mining_period_rect <- data.table(xmin = mining_onset_multiple, xmax = c(reference_period_start,Inf), ymin = -Inf, ymax = Inf)
    reference_period_rect <- data.table(xmin = reference_period_start, xmax = mining_onset_multiple[length(mining_onset_multiple)], 
                                        ymin = -Inf, ymax = Inf)
    mining_onset <- c(mining_onset_multiple, reference_period_start)
  }else{
    mining_period_rect <- data.table(xmin = mining_onset, xmax = Inf, ymin = -Inf, ymax = Inf)
    reference_period_rect <- data.table(xmin = reference_period_start, xmax = mining_onset, ymin = -Inf, ymax = Inf)
  }
  
  mining_onset_save <- data.table(mining_onset = mining_onset, 
                                  site_no = selected_ma_site, 
                                  transect_display_name = ssc_ma_timeseries_ex$transect_display_name[1],
                                  continent_display = ssc_ma_timeseries_ex$continent_display[1])
  if(plot_save == 'save'){
  # Plot raw SSC and moving average
  # (Commented out to save time)
  ssc_ma_timeseries_ex_plot <- ggplot() +
    geom_rect(data = reference_period_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              # fill = '#3D7543', alpha = 0.2, inherit.aes = F) +
              fill = '#185359', alpha = 0.2, inherit.aes = F) +  
    geom_rect(data = mining_period_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              # fill = '#3D7543', alpha = 0.2, inherit.aes = F) +
              fill = '#F2BE22', alpha = 0.2, inherit.aes = F) +
    geom_line(data = ssc_ma_timeseries_ex[!is.na(SSC_mgL)],
              aes(x = (year + month/12), y = SSC_mgL, color = 'Monthly average'), lty = 'dashed') +
    geom_point(data = ssc_ma_timeseries_ex[!is.na(SSC_mgL)],
               aes(x = (year + month/12), y = SSC_mgL, color = 'Monthly average')) +
    stat_summary(geom = 'line', data = ssc_ma_timeseries_ex[!is.na(SSC_mgL_3yr)], aes(x = (year + month/12), y = SSC_mgL_3yr, color = '3-yr moving average'), color = 'red') +
    geom_vline(xintercept = mining_onset) +
    scale_color_manual(values = c('Monthly average' = 'black', '3-yr moving average' = 'red')) +
    scale_x_continuous(limits = c(1984, NA)) +
    # facet_wrap(.~paste0(Country, '\n', transect_display_name)) +
    season_facet +
    # facet_wrap(.~(distance_10km)) +
    theme(
      legend.position = c(0.2, 0.85),
      legend.background = element_rect(color = 'black')
    ) +
    labs(
      x = '',
      y = 'SSC (mg/L)',
      color = 'Metric'
    )
  
  # print(ssc_ma_timeseries_ex_plot)
  
  
    # Save plot (commented out to speed up process)
    ggsave(ssc_ma_timeseries_ex_plot, filename = paste0(wd_figures, 'transect_timeseries_plots/ssc_ma_timeseries_', selected_ma_site, '.pdf'),
           width = 6, height = 4, useDingbats = F)
    ggsave(ssc_ma_timeseries_ex_plot, filename = paste0(wd_figures, 'transect_timeseries_plots/ssc_ma_timeseries_', selected_ma_site, '.png'),
           width = 6, height = 4)
  }
  
  if(selected_ma_site %in% representative_transects){
    if(k == 1){
      selected_ma_ts_data <- ssc_ma_timeseries_ex[!is.na(SSC_mgL)]
      selected_mining_onset_data <- mining_onset_save
    }else{
      selected_ma_ts_data <- rbind(selected_ma_ts_data, ssc_ma_timeseries_ex[!is.na(SSC_mgL)], use.names = T, fill = T)
      selected_mining_onset_data <- rbind(selected_mining_onset_data, mining_onset_save, use.names = T, fill = T)
    }
    k <- k + 1
  }
}}

# Include Kayapo
representative_of_continent <- c('brazil_kayapo_preserve_agm_region', 'indonesia_nabire_barat', 'myanmar_shaduzup_agm_region', 'drc_kibali_river_upper_agm_region')
# Include Malinowski
representative_of_continent <- c('peru_rio_malinowski_agm_region', 'indonesia_nabire_barat', 'myanmar_shaduzup_agm_region', 'drc_kibali_river_upper_agm_region')

# Daily data from selected representative rivers
representative_continent_timeseries_data <- selected_ma_ts_data[site_no %chin% representative_of_continent & SSC_mgL < 8000][
  ,':='(`Reference period start` = ifelse(site_no == 'brazil_kayapo_preserve_agm_region', 1996, `Reference period start`),
        `Reference period end` = ifelse(site_no == 'brazil_kayapo_preserve_agm_region', 2009, `Reference period end`))][
  ,':='(period = ifelse(year > `Reference period end` | year < (`Reference period start`), 
               'Active mining', 'Reference'),
        multiple_mining_period = ifelse(year < `Reference period end` & year < `Reference period start`, 'First', 'Second'))
]

representative_continent_summary_data <- representative_continent_timeseries_data[
  ,.(`Reference period start` = min(`Reference period start`, na.rm = T),
     `Reference period end` = min(`Reference period end`, na.rm = T)
     ),
  by = .(transect_display_name, continent_display, site_no)
]
representative_continent_timeseries_plots <- ggplot(representative_continent_timeseries_data, 
                                                    aes(x = (year + month/12), y = SSC_mgL, 
                                                        color = period)) +
  # stat_summary(geom = 'point', fun = mean.se()) +
  # stat_summary(geom = 'path', pch = 21, stroke = 0.2, fun.data = mean_se, aes(group = paste0(period,multiple_mining_period))) +
  stat_summary(geom = 'path', pch = 21, stroke = 0.2, fun.data = mean_se) +
  facet_wrap(.~paste0(continent_display, '\n', transect_display_name), scales = 'free_y', ncol = 1) +
  geom_rect(data = representative_continent_summary_data, 
            aes(xmin = `Reference period start`, xmax = `Reference period end` + 1, ymin = -Inf, ymax = Inf), inherit.aes = F, alpha = 0.2) +
  geom_vline(data = representative_continent_summary_data, aes(xintercept = `Reference period end` + 1)) +
  # geom_vline(data = representative_continent_summary_data[`Reference period start` > 1984], aes(xintercept = `Reference period start`)) +
  scale_color_manual(values = c('Reference' = '#185359', 'Active mining' = '#F2BE22')) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  season_facet +
  labs(
    x = '',
    y = 'SSC (mg/L)',
    color = 'Metric'
  )
  

#### AVERAGE ANNUAL Z-SCORE FOR EACH TRANSECT (NO DISTANCE COMPONENT) ####
# Compute average z-score for each year along full mining area for each transect
ssc_ma_annual_avg_zscore <- ssc_ma_annual_full_transect_avg[
  ,.(SSC_mgL = mean(SSC_mgL_3yr, na.rm = T),
     SSC_mgL_sd = sd(SSC_mgL_3yr, na.rm = T),
     SSC_z_score = mean(SSC_z_score, na.rm = T),
     N_obs = round(mean(N_obs, na.rm = T))),
  by = .(site_no, country_display, year, continent_display, continent_country, 
         reference_period_start, reference_period_end, reference_distance_start, reference_distance_end)
]

# Compute average SSC and z-score along full mining area for each transect, for whole period (mining vs. not)
ssc_ma_annual_avg_SSC <- ssc_ma_annual_full_transect_avg[
  ,.(SSC_mgL = mean(SSC_mgL_3yr, na.rm = T),
     SSC_mgL_sd = sd(SSC_mgL_3yr, na.rm = T),
     SSC_z_score = mean(SSC_z_score, na.rm = T),
     N_obs = round(mean(N_obs, na.rm = T))),
  by = .(site_no, country_display, continent_display, continent_country, 
         reference_period_start, reference_period_end, reference_distance_start, reference_distance_end,
         reference)
]


# Plot Z-score distributions for reference vs. active mining
# at example rivers
reference_vs_active_z_score_histograms_example_plot <- ggplot(ssc_ma_annual_full_transect_avg[
  grepl(pattern = 'peru', x = site_no) & 
    !is.na(SSC_z_score)]) +
  geom_histogram(aes(y = SSC_z_score, fill = reference), bins = 20,
                 color = 'black', lwd = 0.5, position = 'identity', alpha = 0.6) +
  season_facet + 
  facet_wrap(.~site_no, scales = 'free') +
  scale_fill_manual(values = c('Active mining' = '#F2BE22', 'Reference' = '#185359')) +
  theme(
    legend.position = 'top'
  ) +
  labs(
    x = 'N samples',
    y = 'SSC Z-Score'
  )

# Exclude if:
# Column with `Reference km end`, if distance_10km < `Reference km end`, no post-mining z-score
# Column with `Upstream km unaffected` if distance_10km < `Upstream km unaffected`, no post-mining z-score
# Column with `Reference reach ... ` (get full col name). If  `Reference reach` == 'Reference', no post-mining z-score

# ssc_ma_annual_full_transect_avg <- ssc_ma_annual_full_transect_avg[site_metadata[
#   ,.(`Reference reach (if \"Reference\", ref. reach; if name of reach, that reach is ref. reach for this row)`,
#      `Reference km end`,
#      `Upstream km unaffected`,
#      site_no)], on = 'site_no']
# 
# ssc_ma_annual_full_transect_avg <- ssc_ma_annual_full_transect_avg[
#   ,':='(active_mining = ifelse(`Reference reach (if \"Reference\", ref. reach; if name of reach, that reach is ref. reach for this row)` ==
#                                  site_no &
#                                  ((!is.na(`Reference km end`) & `Reference km end` > distance_10km) | 
#                                   (!is.na(`Upstream km unaffected`) & `Upstream km unaffected` > distance_10km) |
#                                  `Reference reach (if \"Reference\", ref. reach; if name of reach, that reach is ref. reach for this row)` == 'Reference'),
#                                'No mining', 'Active mining'))
# ]

ssc_response_streams_subset <- ssc_ma_annual_full_transect_avg[
  !grepl('reference', site_no) &
    continent_display != 'N. Am.' &
    !(Country %chin% c('Brazil')) &
    # reference == 'Active mining' & 
    mining_onset > 1980 & 
    year - mining_onset + 3 < 31 & 
    # (distance_10km < (reference_distance_end + 50) | 
    #    is.infinite(reference_distance_end) & distance_10km < 50) &
    !is.na(SSC_z_score) & 
    (year - mining_onset) > -20
]

# Pre- and post-mining histograms for all sites
ssc_response_streams_subset_means <- ssc_response_streams_subset[
  ,.(SSC_z_score = mean(SSC_z_score, na.rm = T)),
  by = .(reference)
]
# Histogram plot of mining vs. reference at each river
ssc_response_streams_ssc_distribution_vs_reference_plot <- ggplot(ssc_response_streams_subset) +
  geom_histogram(aes(y = SSC_z_score, fill = reference), bins = 20,
                 color = 'black', lwd = 0.2, position = 'identity', alpha = 0.6) +
  geom_hline(data = ssc_response_streams_subset_means, aes(yintercept = SSC_z_score, color = reference), lwd = 0.5, lty = 'dashed') +
  # geom_segment(aes(x = 0, xend = Inf, y = -10, yend = -10)) +
  geom_segment(aes(x = 0, xend = 0, y = -10, yend = 25)) +
  # geom_density(aes(y = SSC_z_score, fill = reference), bins = 20,
  #                color = 'black', lwd = 0.5, alpha = 0.6) +
  # geom_hline(data = dt_annual_avg_SSC, aes(yintercept = SSC_mgL, color = reference), lty = 'dashed') +
  season_facet + 
  # facet_wrap(.~paste0(country_display, '\n', transect_display_name), scales = 'free', ncol = 5) +
  scale_fill_manual(values = c('Active mining' = '#F2BE22', 'Reference' = '#185359')) +
  scale_color_manual(values = c('Active mining' = '#F2BE22', 'Reference' = '#185359')) +
  theme(
    legend.position = 'top',
    panel.border = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_markdown()
    # axis.text.x = element_text(angle = 90, vjust = 0.5))
  ) +
  scale_y_continuous(limits = c(-10, 25), expand = expansion(mult =0)) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  labs(
    x = 'N samples',
    y = 'SSC Z-Score',
    color = 'Period', 
    fill = 'Period'
  )


ssc_response_streams_subset_summary <- ssc_response_streams_subset[
  ,.(N_years = uniqueN(year),
     N_meas = sum(N_obs, na.rm = T)),
  by = .(site_no, Country, Continent, mining_onset, country_display, continent_display, continent_country, transect_display_name)
]

n_sites_ssc_response <- ssc_response_streams_subset_summary[
  ,.(N_rivers = .N)
]

n_sites_ssc_response_continent <- ssc_response_streams_subset_summary[
  ,.(N_rivers = .N),
  by = .(continent_display)
]


# Plot with years since mining onset
ssc_z_ts_from_mining_onset_plot <- ggplot(ssc_response_streams_subset,
aes(x = year - mining_onset + 3, y = SSC_z_score)) +
  stat_summary(fun.data = median_hilow, fun.args = list(conf.int = 0.5), 
               geom = 'ribbon', color = '#185359', fill = '#185359', alpha = 0.2) +
  stat_summary(fun = "median", geom = "line", color = '#185359', size = 0.5) +
  # stat_summary(fun = "median", geom = "line", color = "black", size = 0.5, # Adds individual lines for each river
  #              aes(group = site_no), alpha = 0.2) +
  # facet_wrap(.~country_display, scales = 'free_y') +
  # scale_y_continuous(limits = c(NA, 40)) +
  season_facet + 
  geom_vline(xintercept = 0) +
  scale_x_continuous(expand = expansion(mult = 0)) +
  labs(
    x = 'Years since mining onset',
    y = '**SSC Z-Score**<br>(SDs relative to pre-mining)'
  ) +
  theme(axis.title.y = element_markdown())

# Facet by continent
ssc_z_ts_from_mining_onset_continent_plot <- ssc_z_ts_from_mining_onset_plot + 
  facet_wrap(.~continent_display) +
  geom_text(data = n_sites_ssc_response_continent, aes(x = -Inf, y = Inf, label = paste0('N=',N_rivers)),
            hjust = -0.1, vjust = 2, size = 3.5)

# Add individual lines
ssc_z_ts_from_mining_onset_continent_plot_with_indiv <- ssc_z_ts_from_mining_onset_continent_plot + 
  stat_summary(fun = "median", geom = "line", color = "black", size = 0.5, # Adds individual lines for each river
               aes(group = site_no), alpha = 0.2) +
  scale_y_continuous(limits = c(NA, 20))

ssc_z_ts_from_mining_onset_combined_plot <- ssc_z_ts_from_mining_onset_plot /
  # ssc_z_ts_from_mining_onset_continent_plot +
  ssc_z_ts_from_mining_onset_continent_plot_with_indiv +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))


ggsave(ssc_z_ts_from_mining_onset_combined_plot, filename = paste0(wd_figures, 'ssc_z_ts_from_mining_onset_combined_plot.pdf'),
       useDingbats = F, width = 5, height = 8)
ggsave(ssc_z_ts_from_mining_onset_combined_plot, filename = paste0(wd_figures, 'ssc_z_ts_from_mining_onset_combined_plot.png'),
       width = 5, height = 8)

ssc_z_ts_combined_with_representative_rivers <- (((
  (ssc_z_ts_from_mining_onset_plot + 
     geom_text(data = n_sites_ssc_response, aes(x = -Inf, y = Inf, label = paste0('N=',N_rivers)),
                                               hjust = -0.1, vjust = 2, size = 3.5)) + 
     inset_element(ssc_response_streams_ssc_distribution_vs_reference_plot + 
                     theme(legend.position = 'none',
                           axis.title.y = element_blank(),
                           axis.text.y = element_text(size = 9),
                           plot.background = element_blank()), 
                   0.5, 0, 1, 0.43)) /
  # ssc_z_ts_from_mining_onset_continent_plot +
  ssc_z_ts_from_mining_onset_continent_plot_with_indiv) | 
    representative_continent_timeseries_plots + 
            scale_x_continuous(labels = abbrev_year, expand = expansion(mult = 0)) +
            scale_y_continuous(expand = expansion(mult = 0))
  ) +
  plot_layout(ncol = 2, widths = c(2, 1.2)) +
  # plot_annotation(tag_levels = 'a') &
  plot_annotation(tag_levels = list(c('a', '', 'b', 'c'))) &
  theme(plot.tag = element_text(face = 'bold')) 

ggsave(ssc_z_ts_combined_with_representative_rivers, filename = paste0(wd_figures, 'ssc_z_ts_combined_with_representative_rivers.pdf'),
       useDingbats = F, width = 5.5, height = 7.5)
ggsave(ssc_z_ts_combined_with_representative_rivers, filename = paste0(wd_figures, 'ssc_z_ts_combined_with_representative_rivers.png'),
       width = 5.5, height = 7.5)  
#### PLOT Z-SCORE RASTER ####

# Function to remove the label for the first of two facet columns
# Only one axis is labeled with the facet
remove_first_faceter_labeller <- function(x){
  loc <- unlist(gregexpr('_', x)) + 1
  facet <- substr(x, loc, nchar(x))
  return(facet)
}
# Plot Z-score timeseries raster (wrt 1984-1991 reference)
# plot colored by z-score
ssc_zscore_timeseries_raster_plot <- ggplot(ssc_ma_annual_avg_zscore[!is.na(continent_country)], 
                                            aes(x = year, y = site_no, fill = SSC_z_score)) + 
  geom_tile(color = 'black', lwd = 0.1) + 
  scale_fill_gradientn(colors = c('blue','grey90','red'), limits = c(-5,5), oob = squish,
                       breaks = c(-4, -2, 0, 2, 4)) +
  season_facet + 
  # facet_grid(continent_display + country_display ~., scales = 'free_y', switch = "y", space = "free_y") +
  facet_grid(continent_country ~., scales = 'free_y', switch = "y", space = "free_y",
             labeller = labeller(.rows = remove_first_faceter_labeller)) +
  scale_x_continuous(limits = c(min(ssc_ma_annual_avg_zscore$year, na.rm = T), max(ssc_ma_annual_avg_zscore$year, na.rm = T)), 
                     expand = expansion(add = c(-0.50,-0.5))) +
  guides(fill = guide_colourbar(barwidth = 1, barheight = 30, frame.colour = 'black', frame.linewidth = 1, ticks.colour = 'black')) +
  theme(plot.background = element_blank(),
        # axis.text.y = element_text(size = 3),
        panel.border = element_rect(size = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y.left = element_markdown(size = 12, angle = 0, hjust = 1),
        legend.position = 'right',
        axis.text = element_markdown(size = 14),
        axis.title = element_markdown(size = 16),
        legend.title = element_markdown(size = 18),
        legend.text = element_markdown(size = 16)
  ) +
  labs(
    x = 'Year',
    y = '',
    fill = '**SSC**<br>**Z-Score**'
  )

ssc_zscore_timeseries_raster_plot <- ggplotGrob(ssc_zscore_timeseries_raster_plot)

# Remove clipping from left side strip label to allow for full display
for(i in which(grepl("strip-l", ssc_zscore_timeseries_raster_plot$layout$name))){
  ssc_zscore_timeseries_raster_plot$grobs[[i]]$layout$clip <- "off"
}

ggsave(ssc_zscore_timeseries_raster_plot, filename = paste0(wd_figures, 'ssc_zscore_timeseries_raster_plot.pdf'),
       width = 10, height = 12, useDingbats = F)
ggsave(ssc_zscore_timeseries_raster_plot, filename = paste0(wd_figures, 'ssc_zscore_timeseries_raster_plot.png'),
       width = 10, height = 12)


# Make plot showing river names to annotate Z-Score plot
ssc_zscore_timeseries_label_plot <- ggplot(ssc_ma_annual_avg_zscore[!is.na(continent_country) & year == 2020][
  transect_name_display, on = 'site_no'], 
                                            aes(x = 1, y = paste0(continent_display, country_display,site_no))) + 
  geom_text(aes(label = transect_display_name), hjust = 0) + 
  season_facet + 
  facet_wrap(continent_display + country_display ~., scales = 'free_y', 
             labeller = labeller(.rows = remove_first_faceter_labeller)) +
  # facet_grid(continent_country ~., scales = 'free_y', switch = "y", space = "free_y",
  #            labeller = labeller(.rows = remove_first_faceter_labeller)) +
  theme(plot.background = element_blank(),
        # axis.text.y = element_text(size = 3),
        panel.border = element_rect(size = 1),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y.left = element_markdown(size = 12, angle = 0, hjust = 1),
        legend.position = 'right',
        axis.text = element_markdown(size = 14),
        axis.title = element_markdown(size = 16),
        legend.title = element_markdown(size = 18),
        legend.text = element_markdown(size = 16)
  ) +
  labs(
    x = 'Year',
    y = '',
    fill = '**SSC**<br>**Z-Score**'
  )

ssc_zscore_timeseries_label_plot <- ggplotGrob(ssc_zscore_timeseries_label_plot)

# Remove clipping from left side strip label to allow for full display
for(i in which(grepl("strip-l", ssc_zscore_timeseries_label_plot$layout$name))){
  ssc_zscore_timeseries_label_plot$grobs[[i]]$layout$clip <- "off"
}

ggsave(ssc_zscore_timeseries_label_plot, filename = paste0(wd_figures, 'ssc_zscore_timeseries_label_plot.pdf'),
       width = 10, height = 12, useDingbats = F)
ggsave(ssc_zscore_timeseries_label_plot, filename = paste0(wd_figures, 'ssc_zscore_timeseries_label_plot.png'),
       width = 10, height = 12)

# TO DO: Make more sophisticated join based on river km
# TO DO: compute Z-score only for most-affected X kms
# Could complicate things for rivers with long transects
# Also, some sites have no good reference. Create custom reference for them? (Nearby rivers, "lowest" period)

#### ANALYSIS OF NATIONAL, CONTINENTAL, AND GLOBAL TRENDS ####
# Initialize some plot elements
financial_crisis_timeline <- data.table(x = c(2008, 2010),
                                        y = c(-Inf, -Inf),
                                        ymax = c(Inf, Inf))
covid_timeline <- data.table(x = c(2020, 2022),
                             y = c(-Inf, -Inf),
                             ymax = c(Inf, Inf))

financial_crisis_label <- data.table(x = 2008, y = -Inf, label = c('Global Financial\nCrisis (2008-09)'))
covid_label <- data.table(x = 2020, y = -Inf, label = c('COVID-19\n(2020-22)'))
# Analysis of Z-score aggregate 
# Global
transect_ts_zscore_ma_plot <- ggplot(ssc_ma_annual_full_transect_avg[
  Continent != 'North America' & abs(SSC_z_score) < 50 & N_obs > 2], 
                                     aes(x = year, y = SSC_z_score)) + 
  stat_summary(fun.data = median_hilow, fun.args = list(conf.int = 0.5), 
               geom = 'ribbon', color = '#185359', fill = '#185359', alpha = 0.2) +
  stat_summary(fun = "median", geom = "line", color = "#185359", size = 0.5) +
  geom_hline(yintercept = 0, lty = 'dashed', lwd = 0.5, color = 'black') +
  scale_x_continuous(limits = c(1984, 2022)) +
  season_facet + 
  scale_color_manual(values = c('#A66F8D','#395953','#F2BC57','#D98841','#BF5349')) +
  labs(
    x = '',
    y = '**SSC Z-Score**<br>(SDs relative to pre-mining)'
  ) + 
  theme(axis.title.y = element_markdown())

# By continent
sites_per_continent <- ssc_ma_annual_full_transect_avg[Continent != 'North America' & abs(SSC_z_score) < 50][
  ,.(N_sites = uniqueN(site_no)), by = .(continent_display)
]
transect_ts_zscore_ma_continent_plot <- transect_ts_zscore_ma_plot + 
  geom_text(data = sites_per_continent, aes(x = 1985, y = 7, label = paste0('N=',N_sites)),
            hjust = 0, vjust = 0, size = 2.5) +
  facet_wrap(.~continent_display)

# By country
sites_per_country <- ssc_ma_annual_full_transect_avg[abs(SSC_z_score) < 50][
  ,.(N_sites = uniqueN(site_no)), by = .(country_display)
]
transect_ts_zscore_ma_country_plot <- transect_ts_zscore_ma_plot + 
  geom_text(data = sites_per_country[country_display != 'Honduras'], aes(x = 1985, y = Inf, label = paste0('N=',N_sites)),
            hjust = 0, vjust = 2, size = 3.5) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020), 
                     labels = c("'90", "'00", "'10", "'20")) +
  facet_wrap(.~country_display, scales = 'free_y')

ggsave(transect_ts_zscore_ma_country_plot, 
       filename = paste0(wd_figures, 'transect_ts_zscore_ma_country_plot.png'),
       width = 9, height = 9)
ggsave(transect_ts_zscore_ma_country_plot, 
       filename = paste0(wd_figures, 'transect_ts_zscore_ma_country_plot.pdf'),
       width = 9, height = 9, useDingbats = F)

ggsave(transect_ts_zscore_ma_inset_plot, 
       filename = paste0(wd_figures, 'transect_ts_zscore_ma_inset_plot.png'),
       width = 7, height = 7)
ggsave(transect_ts_zscore_ma_inset_plot, 
       filename = paste0(wd_figures, 'transect_ts_zscore_ma_inset_plot.pdf'),
       width = 7, height = 7, useDingbats = F)

#### COMPARE SSC TO THE PRICE OF GOLD ####
# Plot the price of gold for same period as SSC
gold_price_ts_plot <- ggplot(gold_prices, 
                             aes(x = year, y = USD)) +
  geom_ribbon(data = financial_crisis_timeline, 
              aes(x = x, ymin = y, ymax = ymax), fill = 'black', alpha = 0.2, inherit.aes = F) + 
  geom_ribbon(data = covid_timeline, 
              aes(x = x, ymin = y, ymax = ymax), fill = 'black', alpha = 0.2, inherit.aes = F) + 
  geom_line() +
  geom_text(data = financial_crisis_label, aes(x = x, y = 1500, label = label), 
            inherit.aes = F, hjust = 1, nudge_x = -0.5, size = 3, lineheight = 1) +
  geom_text(data = covid_label, aes(x = x, y = 1700, label = label), 
            inherit.aes = F, hjust = 1, nudge_x = -0.5, size = 3, lineheight = 1) +
  season_facet + 
  scale_x_continuous(limits = c(1984, 2022)) +
  labs(
    x = 'Year',
    y = 'Gold Price (USD)'
  )

# Combine SSC avg. and gold price plots
SSC_gold_price_comb_ts_plot <- 
  # On top: Gold Price Plot
  (gold_price_ts_plot + labs(x = '') + theme(axis.title.x = element_blank())) /
  # Underneath: Global change in river SSC Z-Score 
  transect_ts_zscore_ma_plot + 
  theme(axis.title.x = element_blank())+ 
  geom_ribbon(data = financial_crisis_timeline, 
              aes(x = x, ymin = y, ymax = ymax), fill = 'black', alpha = 0.2, inherit.aes = F) + 
  geom_ribbon(data = covid_timeline, 
              aes(x = x, ymin = y, ymax = ymax), fill = 'black', alpha = 0.2, inherit.aes = F) + 
  # Then inset into that plot: Continental change in river SSC Z-Score
  inset_element(transect_ts_zscore_ma_continent_plot + 
                  facet_wrap(.~continent_display) +
                  scale_y_continuous(breaks = c(-2.5, 0, 2.5, 5, 7.5), labels = c('', 0,'',5, '')) +
                  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020), 
                                     labels = c("'90", "'00", "'10", "'20")) +
                  theme(axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        axis.text = element_text(size = 9),
                        strip.text = element_text(size = 9),
                        plot.margin = unit(c(0,0,0,0), 'mm'),
                        plot.background = element_blank()),
                left = 0.01, right = 0.5, top = 0.99, bottom = 0.6, ignore_tag = TRUE) +
  # Label plot A, B
  plot_annotation(tag_levels = 'a') +
  # Make gold prices plot 0.35 height of SSC plot
  plot_layout(heights = c(0.35, 1)) &
  theme(plot.tag = element_text(face = 'bold'))

ggsave(SSC_gold_price_comb_ts_plot, 
       filename = paste0(wd_figures, 'SSC_gold_price_comb_ts_plot.png'),
       width = 6, height = 6.5)
ggsave(SSC_gold_price_comb_ts_plot, 
       filename = paste0(wd_figures, 'SSC_gold_price_comb_ts_plot.pdf'),
       width = 6, height = 6.5, useDingbats = F)

#### SSC TIMESERIES CORRELATIONS ####
ssc_annual_avg <- ssc_ma_annual_full_transect_avg[Continent != 'North America' & abs(SSC_z_score) < 50][
  ,.(SSC_z_score = median(SSC_z_score, na.rm = T)),
  by = .(year)
]
gold_prices_annual <- gold_prices[year%%1 == 0]

# Spearman and Pearson Correlation tests for gold prices and mining pollution
pearson_test <- cor.test(gold_prices_annual[year %in% 1986:2021]$USD,ssc_annual_avg[year %in% 1986:2021]$SSC_z_score,
                         method = c('pearson'))

pearson_test_lag4 <- cor.test(gold_prices_annual[year %in% 1989:2021]$USD,ssc_annual_avg[year %in% 1986:2018]$SSC_z_score,
                              method = c('pearson'))

spearman_test <- cor.test(gold_prices_annual[year %in% 1986:2021]$USD,ssc_annual_avg[year %in% 1986:2021]$SSC_z_score,
                          method = c('spearman'))


#### RIVER KILOMETERS AFFECTED BY MINING ####
# Add decade to SSC data.table
ssc_ma_annual_full_transect_avg <- ssc_ma_annual_full_transect_avg[
  ,':='(decade = year - year%%10,
        half_decade = year - year%%5)]

# Calculate elevated and > 3 SD elevated river kms
ssc_ma_annual_full_transect_avg <- ssc_ma_annual_full_transect_avg[
  ,':='(km_elevated = ifelse(SSC_z_score > 0, 1, 0),
        km_3SD = ifelse(SSC_z_score > 3, 1, 0),
        km_2x = ifelse(SSC_mgL_3yr > 2 * SSC_mgL_reference, 1, 0),
        km_5x = ifelse(SSC_mgL_3yr > 5 * SSC_mgL_reference, 1, 0),
        km_10x = ifelse(SSC_mgL_3yr > 10 * SSC_mgL_reference, 1, 0)
  )
]

## BASELINE OF RIVER KMS WITH ACTIVE MINING ALONG OR UPSTREAM
# Total number of rivers with active mining
n_rivers_with_active_mining <- unique(ssc_ma_annual_full_transect_avg[reference == 'Active mining']$site_no)

# River length of active mining, by decade
river_length_by_decade <- ssc_ma_annual_full_transect_avg[
  reference == 'Active mining' & 
    !is.na(SSC_mgL_3yr) & 
    !is.infinite(SSC_mgL_3yr)][
      ,.(active_river = max(SSC_mgL_3yr, na.rm = T)),
      by = .(site_no, half_decade, distance_10km, country_display, continent_display)][
        ,.(active_river_km = .N * 10),
        by = .(site_no, half_decade, country_display, continent_display)]

# Continental total river length affected by mining
river_length_by_decade_summary <- river_length_by_decade[half_decade == 2015][
  ,.(active_river_km = sum(active_river_km, na.rm = T)),
  by = .(continent_display)
]


# River reaches elevated by different amounts (> reference, >2x, >5x, >10x)
river_reaches_Nx_decadal_mean <- ssc_ma_annual_full_transect_avg[reference == 'Active mining'][
  ,.(km_elevated = mean(km_elevated, na.rm = T),
     km_2x = mean(km_2x, na.rm = T),
     km_5x = mean(km_5x, na.rm = T),
     km_10x = mean(km_10x, na.rm = T)),
  by = .(site_no, half_decade, distance_10km, country_display, continent_display)
]

river_reaches_elevated_decadal <- river_reaches_Nx_decadal_mean[km_elevated > 0.5][
  ,.(km_Nx = .N * 10,
     Nx = 'elevated'),
  by = .(site_no, half_decade, country_display, continent_display)
]

river_reaches_2x_decadal <- river_reaches_Nx_decadal_mean[km_2x > 0.5][
  ,.(km_Nx = .N * 10,
     Nx = '2x'),
  by = .(site_no, half_decade, country_display, continent_display)
]
river_reaches_5x_decadal <- river_reaches_Nx_decadal_mean[km_5x > 0.5][
  ,.(km_Nx = .N * 10,
     Nx = '5x'),
  by = .(site_no, half_decade, country_display, continent_display)
]
river_reaches_10x_decadal <- river_reaches_Nx_decadal_mean[km_10x > 0.5][
  ,.(km_Nx = .N * 10,
     Nx = '10x'),
  by = .(site_no, half_decade, country_display, continent_display)
]

# Number of river kilometers elevated over reference period/transect (in 2020)
river_reaches_elevated_2020 <- river_reaches_elevated_decadal[half_decade == 2020]
print(paste0('total river km elevated SSC: ', sum(river_reaches_elevated_2020$km_Nx), ' km'))

# Combine 2020 elevated river reaches with table of all transects + metadata
river_reaches_elevated_2020 <- river_reaches_elevated_2020[transect_name_display, on = 'site_no']

# Write 2020 elevated river km per transect to file
fwrite(river_reaches_elevated_2020, file = 'river_reaches_elevated_2020.csv')

# Combine tables of km elevated by different amounts into single table
river_reaches_Nx_decadal <- rbind(river_reaches_2x_decadal, river_reaches_5x_decadal, river_reaches_10x_decadal,
                                  use.names = T)

river_reaches_Nx_decadal_transect_summary <- river_reaches_Nx_decadal[
  ,.(km_Nx = sum(km_Nx, na.rm = T)),
  by = .(country_display, continent_display, site_no, Nx, half_decade)
]

river_reaches_Nx_2020_transect_summary <- river_reaches_Nx_decadal_transect_summary[half_decade == 2020]
river_reaches_2x_2020_transect_summary <- river_reaches_Nx_2020_transect_summary[Nx == '2x']

river_reaches_Nx_decadal_country_summary <- river_reaches_Nx_decadal[
  ,.(km_Nx = sum(km_Nx, na.rm = T)),
  by = .(country_display, continent_display, Nx, half_decade)
]

# By-country summary of affected river kilometers
river_reaches_Nx_decadal_country_summary <- river_reaches_Nx_decadal[half_decade == 2015][
  ,.(km_Nx = sum(km_Nx, na.rm = T),
     N_rivers = uniqueN(site_no)),
  by = .(country_display, continent_display, Nx, half_decade)
]

# Continent_summary of affected river kilometers (for each category: 2x, 3x, 10x)
river_reaches_Nx_decadal_continent_summary <- dcast.data.table(
  continent_display ~ Nx, value.var = c('km_Nx', 'N_rivers'),
  data = river_reaches_Nx_decadal[half_decade == 2015][
    ,.(km_Nx = sum(km_Nx, na.rm = T),
       N_rivers = uniqueN(site_no)),
    by = .(continent_display, Nx, half_decade)
  ])[
    # ,.(continent_display, `2x`, `5x`, `10x`)][ # if not computing number of rivers per continent
    ,.(continent_display, km_Nx_2x, N_rivers_2x, km_Nx_5x, N_rivers_5x, km_Nx_10x, N_rivers_10x)][
      river_length_by_decade_summary, on = 'continent_display'
    ][order(continent_display)]

fwrite(river_reaches_Nx_decadal_continent_summary, file = paste0(wd_exports, 'river_reaches_Nx_decadal_continent_summary.csv'))

# Plot river kilometers elevated timeseries overall
river_km_elevated_Nx_plot <- ggplot(river_reaches_Nx_decadal[!is.na(km_Nx) & continent_display != 'N. Am.'], aes(x = half_decade, y = km_Nx, color = Nx)) +
  stat_summary(geom = 'line', fun = 'sum', lwd = 0.75) + 
  season_facet +
  scale_color_manual(values = c('2x' = 'black', '5x' = '#735A10', '10x' = '#F2BE22')) +
  scale_y_continuous(label=comma, limits = c(0, NA)) +
  theme(legend.position = 'top',
        axis.title.x = element_blank()) +
  # facet_wrap(.~country_display) +
  labs(
    x = '',
    y = '**Mining-affected river kilometers**',
    color = 'Nx'
  ) + 
  theme(axis.title.y = element_markdown())
# Plot river kilometers elevated timeseries by continent
river_km_elevated_Nx_continent_plot <- river_km_elevated_Nx_plot +
  facet_wrap(.~continent_display) +
  scale_x_continuous(limits = c(1990, NA), breaks = c(1990, 2000, 2010, 2020), 
                     labels = c("'90", "'00", "'10", "'20")) # shorten decade label

# Combine annual continent-specific and general plots of elevated river kilometers
river_km_elevated_Nx_combined_plot <- 
  river_km_elevated_Nx_plot + 
  river_km_elevated_Nx_continent_plot +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold', size = 16),
        legend.text = element_text(size = 14))

ggsave(river_km_elevated_Nx_combined_plot, filename = paste0(wd_figures, 'river_km_elevated_Nx_plot.pdf'),
       width = 7, height = 5, useDingbats = F)
ggsave(river_km_elevated_Nx_combined_plot, filename = paste0(wd_figures, 'river_km_elevated_Nx_plot.png'),
       width = 7, height = 5)


# Rivers with at least one 10 km reach that is always + 3SD 
river_reaches_3SD_decadal_mean <- ssc_ma_annual_full_transect_avg[reference == 'Active mining'][
  ,.(km_3SD = mean(km_3SD, na.rm = T)), 
  by = .(site_no, half_decade, distance_10km)]


river_reaches_3SD_elevated <- river_reaches_3SD_decadal_mean[
  ,.(km_3SD_max = max(km_3SD, na.rm = T)),
  by = .(site_no, distance_10km)
]
# Calculate number of rivers with + 3SD for one reach in one decade
one_10km_reach_ever_elevated <- unique(river_reaches_3SD_elevated[km_3SD_max == 1]$site_no)
n_one_10km_reach_ever_elevated <- length(one_10km_reach_ever_elevated)

# Average reach length elevated
n_reaches_elevated_by_river <- river_reaches_3SD_decadal_mean[km_3SD == 1][
  ,.(N_reaches = .N),
  by = .(site_no, half_decade)
][,':='(river_km_3SD_elevated = N_reaches * 10)][
  river_length_by_decade, on = c('site_no', 'half_decade')
][,':='(river_km_3SD_elevated_percent = round(river_km_3SD_elevated/active_river_km * 100))]

reach_length_elevated_summary <- n_reaches_elevated_by_river[
  ,.(mean_river_km_3SD_elevated = mean(river_km_3SD_elevated, na.rm = T),
     sd_river_km_3SD_elevated = sd(river_km_3SD_elevated, na.rm = T),
     mean_river_km_3SD_elevated_percent = mean(river_km_3SD_elevated_percent, na.rm = T),
     sd_river_km_3SD_elevated_percent = sd(river_km_3SD_elevated_percent, na.rm = T)
  ),
  by = .(half_decade)]

percent_reaches_elevated_3SD_plot <- ggplot(n_reaches_elevated_by_river, 
                                            aes(x = half_decade, y = river_km_3SD_elevated_percent)) + 
  stat_summary(geom = 'line', fun = 'mean', lty = 'dashed') +
  stat_summary() +
  season_facet +
  labs(
    x = '',
    y = '**% Mining-affected river reaches**<br>*(> 3 SD over pre-mining)*'
  ) +
  theme(axis.title.y = element_markdown())
avg_km_elevated_3SD_plot <- ggplot(n_reaches_elevated_by_river, 
                                   aes(x = half_decade, y = river_km_3SD_elevated)) + 
  stat_summary(geom = 'line', fun = 'mean', lty = 'dashed') +
  stat_summary() +
  season_facet +
  labs(
    x = '',
    y = '**Avg. Mining-affected river kms**<br>*(> 3 SD over pre-mining)*'
  ) +
  theme(axis.title.y = element_markdown())


elevated_reaches_summary_plot_combined <- avg_km_elevated_3SD_plot / percent_reaches_elevated_3SD_plot +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))

ggsave(elevated_reaches_summary_plot_combined, filename = paste0(wd_figures, 'elevated_reaches_summary_plot_combined.pdf'),
       useDingbats = F, width = 4, height = 6)
ggsave(elevated_reaches_summary_plot_combined, filename = paste0(wd_figures, 'elevated_reaches_summary_plot_combined.png'),
       width = 4, height = 6)


# Summarize annual elevated river kilometers by country and continent
ssc_river_km_elevated_annual <- ssc_ma_annual_full_transect_avg[
  ,.(km_elevated = sum(km_elevated, na.rm = T) * 10,
     km_3SD = sum(km_3SD, na.rm = T) * 10), 
  by = .(year, country_display, continent_display)]

# Plot annual elevated river kilometers by continent
river_km_elevated_continent_plot <- ggplot(ssc_river_km_elevated_annual[continent_display != 'N. Am.'], 
                                           aes(x = year, y = km_elevated)) +
  stat_summary(aes(color = '> Reference'), geom = 'line', fun = 'sum', lwd = 1) +
  stat_summary(aes(y = km_3SD, color = '+ 3 SD'), geom = 'line', fun = 'sum', lwd = 1) +
  scale_color_manual(values = c('> Reference' = '#735A10', '+ 3 SD' = '#F2BE22')) +
  facet_wrap(.~continent_display) + 
  season_facet +
  scale_x_continuous(limits = c(1990, NA), breaks = c(1990, 2000, 2010, 2020), 
                     labels = c("'90", "'00", "'10", "'20")) +
  scale_y_continuous(label=comma, limits = c(0, NA)) +
  theme(legend.position = 'top',
        axis.title.x = element_blank()) +
  labs(
    x = '',
    y = 'Mining-affected river kilometers',
    color = 'SSC change'
  )

# Plot annual elevated river kilometers
river_km_elevated_plot <- ggplot(ssc_river_km_elevated_annual, aes(x = year, y = km_elevated)) +
  stat_summary(aes(color = '> Reference'), geom = 'line', fun = 'sum', lwd = 1) +
  stat_summary(aes(y = km_3SD, color = '+ 3 SD'), geom = 'line', fun = 'sum', lwd = 1) +
  scale_color_manual(values = c('> Reference' = '#735A10', '+ 3 SD' = '#F2BE22')) +
  season_facet +
  scale_x_continuous(limits = c(1990, NA)) +
  scale_y_continuous(label=comma, limits = c(0, NA)) +
  theme(legend.position = 'top',
        axis.title.x = element_blank()) +
  labs(
    x = '',
    y = 'Mining-affected river kilometers',
    color = 'SSC change'
  )

# Combine annual continent-specific and general plots of elevated river kilometers
river_km_elevated_combined_plot <- 
  river_km_elevated_plot + 
  river_km_elevated_continent_plot +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'),
        legend.position = 'top',
        legend.title = element_text(face = 'bold', size = 16),
        legend.text = element_text(size = 14))

ggsave(river_km_elevated_combined_plot, filename = paste0(wd_figures, 'river_km_elevated_plot.pdf'),
       width = 7, height = 5, useDingbats = F)
ggsave(river_km_elevated_combined_plot, filename = paste0(wd_figures, 'river_km_elevated_plot.png'),
       width = 7, height = 5)

# Focus on longest rivers in dataset
ssc_ma_annual_transects_500km <- ssc_ma_annual_full_transect_avg[site_no %chin% transects_500km$site_no &
                                                                   ref_reach == site_no]
ssc_ma_annual_transects_500km_summary <- ssc_ma_annual_transects_500km[,.(SSC_z_score = median(SSC_z_score, na.rm = T)),
                                                                       by = .(continent_display, country_display, site_no, 
                                                                              distance_10km-distance_10km%%50, decade, reference)]
ssc_ma_annual_transects_500km_plot <- 
  # ggplot(ssc_ma_annual_transects_500km_summary[reference != 'Reference' & !is.na(SSC_z_score)],
  ggplot(ssc_ma_annual_transects_500km_summary[!is.na(SSC_z_score)],
         aes(x = distance_10km, y = SSC_z_score, color = factor(decade))) +
  stat_summary(aes(group = reference, color = reference), geom = 'line', lty = 'dashed', fun = 'mean') + 
  stat_summary(aes(group = reference, color = reference)) + 
  # geom_line(data = ssc_ma_annual_transects_500km_summary[reference == 'Reference' & !is.na(SSC_z_score)], 
  #           aes(group = factor(decade)), color = 'black') +
  season_facet + 
  scale_color_manual(values = c('Reference' = '#185359', 'Active mining' = '#F2BE22')) +
  facet_wrap(.~paste0(country_display, '\n', site_no), scales = 'free') +
  labs(
    x = 'Distance (km)',
    y = '**SSC Z-Score**<br>(SDs relative to pre-mining)'
  ) + 
  theme(axis.title.y = element_markdown())

ggsave(ssc_ma_annual_transects_500km_plot, filename = paste0(wd_figures, 'ssc_ma_annual_transects_500km_plot.pdf'),
       useDingbats = F, width = 9, height = 9)
ggsave(ssc_ma_annual_transects_500km_plot, filename = paste0(wd_figures, 'ssc_ma_annual_transects_500km_plot.png'),
       width = 9, height = 9)

select_500km_rivers <- c('cameroon_kadei_river_agm_region', 'drc_lindi_river_agm_region', 'drc_mongbwalu_mining_region',
                         'mongolia_zamaar_goldfield', 'myanmar_chindwin_gold_lower', 'brazil_luar_da_praia_agm_region')

select_500km_rivers_display_names <- c('Kadei R.', 'Lindi R.', 'Aruwimi R.', 'Orhon R.', 'Chindwin R.', 'Iriri R.')
ssc_ma_annual_transects_select_500km_plot <- 
  # ggplot(ssc_ma_annual_transects_500km_summary[reference != 'Reference' & !is.na(SSC_z_score)],
  ggplot(ssc_ma_annual_transects_500km_summary[!is.na(SSC_z_score) & site_no %chin% select_500km_rivers],
         aes(x = distance_10km, y = SSC_z_score, color = factor(decade))) +
  stat_summary(aes(group = reference, color = reference), geom = 'line', lty = 'dashed', fun = 'mean') + 
  stat_summary(aes(group = reference, color = reference)) + 
  # geom_line(data = ssc_ma_annual_transects_500km_summary[reference == 'Reference' & !is.na(SSC_z_score)], 
  #           aes(group = factor(decade)), color = 'black') +
  season_facet + 
  scale_color_manual(values = c('Reference' = '#185359', 'Active mining' = '#F2BE22')) +
  facet_wrap(.~paste0(country_display, '\n', 
                      factor(site_no, levels = select_500km_rivers, 
                             labels = select_500km_rivers_display_names)), 
             scales = 'free', nrow = 2) +
  labs(
    x = 'Distance (km)',
    y = '**SSC Z-Score**<br>(SDs relative to pre-mining)',
    color = 'Period'
  ) + 
  theme(axis.title.y = element_markdown(),
        legend.position = 'top')

ggsave(ssc_ma_annual_transects_select_500km_plot, filename = paste0(wd_figures, 'ssc_ma_annual_transects_select_500km_plot.pdf'),
       useDingbats = F, width = 7, height = 4)
ggsave(ssc_ma_annual_transects_select_500km_plot, filename = paste0(wd_figures, 'ssc_ma_annual_transects_select_500km_plot.png'),
       width = 7, height = 4)

# Make figure with total river kilometers affected and selected long rivers
combined_river_elevated_long_rivers_plot <- 
  river_km_elevated_combined_plot / (ssc_ma_annual_transects_select_500km_plot &
                                       theme(legend.title = element_text(face = 'bold', size = 16),
                                             legend.text = element_text(size = 14))) +
  plot_layout(heights = c(2, 1.5)) + 
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold', size = 16),
        legend.text = element_text(size = 14))

ggsave(combined_river_elevated_long_rivers_plot, filename = paste0(wd_figures, 'combined_river_elevated_long_rivers_plot.pdf'),
       useDingbats = F, width = 7, height = 8)
ggsave(combined_river_elevated_long_rivers_plot, filename = paste0(wd_figures, 'combined_river_elevated_long_rivers_plot.png'),
       width = 7, height = 8)

#### PLOT MONTHLY AVERAGES FOR SELECT RIVERS ####
# Transect data for only selected 500 km rivers
ssc_transects_500km_color <- ssc_ma_annual_transects_500km[,.(site_no, distance_10km, year, half_decade, decade, reference)][
  asgm_river_landsat_pred[site_no %chin% select_500km_rivers & num_pix > 100][
    ,.(site_no, SSC_mgL, B1, B2, B3, B6, distance_10km, month, year)],
  on = c('site_no', 'distance_10km', 'year')
]

ssc_transects_all_color <- ssc_ma_annual_transects_500km[,.(site_no, distance_10km, year, half_decade, decade, reference)][
  asgm_river_landsat_pred[site_no %chin% select_500km_rivers & num_pix > 100][
    ,.(site_no, SSC_mgL, B1, B2, B3, B6, distance_10km, month, year)],
  on = c('site_no', 'distance_10km', 'year')
]


# All transect data
ssc_transects_color <- ssc_ma_annual_full_transect_avg[
  # ref_reach == site_no][ 
  ,.(site_no, distance_10km, year, half_decade, decade, reference, country_display, continent_display)][
    asgm_river_landsat_pred[
      # num_pix > 35 & # if actually computing color
        !is.na(SSC_mgL) & B6 > 2780 & SSC_mgL < 15000][
      ,.(site_no, SSC_mgL, B1, B2, B3, B6, distance_10km, month, year)],
    on = c('site_no', 'distance_10km', 'year')
  ][transect_name_display, on = 'site_no']


# Summarize color data by decade and month
ssc_transects_500km_color_summary <- ssc_transects_500km_color[
  distance_10km > 50 & distance_10km < 300 & !is.na(SSC_mgL) & B6 > 2780 & SSC_mgL < 15000
][
  ,.(SSC_mgL = mean(SSC_mgL, na.rm = T),
     B3 = mean(B3, na.rm = T),
     B2 = mean(B2, na.rm = T),
     B1 = mean(B1, na.rm = T)
  ),
  by = .(site_no, month-month%%3, reference)
][
  ,':='(red = ifelse(B3 > 2000, 1, B3/2000),
        green = ifelse(B2 > 2000, 1, B2/2000),
        blue = ifelse(B1 > 2000, 1, B1/2000))
]

# Plot monthly mining vs. reference, selected 500 km sites
ssc_monthly_vs_reference_example_plot <- ggplot(ssc_transects_500km_color, aes(x = month, y = SSC_mgL, fill = reference, color = reference, group = reference)) + 
  stat_summary(geom = 'line', lty = 'dashed', fun = 'mean') +
  stat_summary(pch = 21, stroke = 0.2, color = 'black', fun.data = mean_se) +
  # stat_summary(pch = 21, stroke = 0.2, fill = NA, color = 'black') +
  scale_fill_manual(values = c('Reference' = '#185359', 'Active mining' = '#F2BE22')) +
  scale_color_manual(values = c('Reference' = '#185359', 'Active mining' = '#F2BE22')) +
  season_facet + 
  facet_wrap(.~site_no)

# Plot monthly mining vs. reference, all sites
continents_all <- unique(ssc_transects_color$continent_display)
# Each continent, with S.America and Africa each twice
continents_all <- c(continents_all[which(!is.na(continents_all))], 'S. Am.', 'Africa')

early_alphabet_SA_countries <- c('Bolivia', 'Brazil','Colombia')
early_alphabet_Africa_countries <- c('Angola', 'C. African Republic', 'Cameroon', "Cote d'Ivoire", 'Dem. Rep. Congo')

#### PLOT AVERAGE MONTHLY SSC, BEFORE AND DURING MINING AND HISTOGRAMS OF REFERENCE VS. ACTIVE MINING ####
for(i in 1:7){
  if(i == 1){
    j <- 1
    k <- 1}
  # Choose continent to print
  continent_sel <- continents_all[i]
  continent_write_name <- gsub('\\.| ', '', continent_sel)
  # Filter data.table to just selected continent
  dt_sel <- ssc_transects_color[continent_display == continent_sel]
  # Selected transects for this run
  dt_transects <- unique(dt_sel$transect_display_name)
  dt_ma_sel <- ssc_ma_annual_full_transect_avg[continent_display == continent_sel]
  # Split South America into two
  if(continent_write_name == 'SAm'){
    if(j == 1){
      dt_sel <- dt_sel[country_display %chin% early_alphabet_SA_countries]
      dt_ma_sel <- dt_ma_sel[country_display %chin% early_alphabet_SA_countries]
      j <- j + 1}
    else{
      dt_sel <- dt_sel[!(country_display %chin% early_alphabet_SA_countries)]
      dt_ma_sel <- dt_ma_sel[!(country_display %chin% early_alphabet_SA_countries)]
      continent_write_name <- paste0(continent_write_name, '_1')} 
  }
  if(continent_write_name == 'Africa'){
    if(k == 1){
      dt_sel <- dt_sel[country_display %chin% early_alphabet_Africa_countries]
      dt_ma_sel <- dt_ma_sel[country_display %chin% early_alphabet_Africa_countries]
      # dt_annual_avg_SSC <- dt_annual_avg_SSC[country_display %chin% early_alphabet_Africa_countries]
      k <- k + 1}
    else{
      dt_sel <- dt_sel[!(country_display %chin% early_alphabet_Africa_countries)]
      dt_ma_sel <- dt_ma_sel[!(country_display %chin% early_alphabet_Africa_countries)]
      # dt_annual_avg_SSC <- dt_annual_avg_SSC[!(country_display %chin% early_alphabet_Africa_countries)]
      continent_write_name <- paste0(continent_write_name, '_1')} 
  }
  
  # Plot average monthly SSC for each transect: 2 categories -- reference and active mining
  monthly_ssc_mining_vs_reference_plot <- 
    ggplot(dt_sel, aes(x = factor(month), y = SSC_mgL, fill = reference, 
                       group = reference,
                       color = reference)) + 
    stat_summary(geom = 'line', lty = 'dashed', fun = 'mean') +
    stat_summary(pch = 21, stroke = 0.2, color = 'black', fun.data = mean_se) +
    scale_fill_manual(values = c('Reference' = '#185359', 'Active mining' = '#F2BE22')) +
    scale_color_manual(values = c('Reference' = '#185359', 'Active mining' = '#F2BE22')) +
    scale_x_discrete(breaks = c(1, 4, 7, 10), labels = c('Jan', 'Apr', 'Jul', 'Oct')) +
    season_facet + 
    facet_wrap(.~paste0(country_display, '\n', transect_display_name), scales = 'free_y', ncol = 5) +
    labs(
      x = 'Month',
      y = 'SSC (mg/L)',
      color = 'Period', 
      fill = 'Period'
    ) +
    theme(axis.title.y = element_markdown(),
          legend.position = 'top',
          axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  # Set panel size to keep panels the same size across different pages (each page is a major landmass)
  monthly_ssc_mining_vs_reference_plot <- set_panel_size(monthly_ssc_mining_vs_reference_plot, width  = unit(1.15, "in"),
                                                         height = unit(1.15, "in"))
  
  ggsave(monthly_ssc_mining_vs_reference_plot, filename = paste0(wd_figures, continent_write_name, '_monthly_ssc_mining_vs_reference_plot.pdf'),
         width = 9, height = 13, useDingbats = F)
  ggsave(monthly_ssc_mining_vs_reference_plot, filename = paste0(wd_figures, continent_write_name, '_monthly_ssc_mining_vs_reference_plot.png'),
         width = 9, height = 13)
  
  # compute annual average SSC for each transect
  dt_annual_avg_SSC <- dt_ma_sel[
    ,.(SSC_mgL = mean(SSC_mgL_3yr, na.rm = T),
       SSC_mgL_sd = sd(SSC_mgL_3yr, na.rm = T),
       N_obs = .N),
    by = .(site_no, country_display, transect_display_name, continent_display, reference)
  ]
  
  # Test for distribution normality
  for(m in 1:nrow(dt_annual_avg_SSC)){
    transect_display_name_sel <- dt_ma_sel[m]$transect_display_name
    dt_ma_normal_sel <- dt_ma_sel[transect_display_name == transect_display_name_sel & !is.na(SSC_mgL_3yr)]
    distances_sel <- unique(dt_ma_normal_sel$distance_10km)
    # Test at every 10 km segment of every river
    for(distance_sel in distances_sel){
      dt_ma_dist_sel <- dt_ma_normal_sel[distance_10km == distance_sel]
      shapiro_test_ref <- NA
      shapiro_test_active <- NA
      if(nrow(dt_ma_dist_sel[reference == 'Reference']) > 3){
        shapiro_test_ref <- shapiro.test(dt_ma_dist_sel[reference == 'Reference']$SSC_mgL_3yr)$p.value
      }
      
      if(nrow(dt_ma_dist_sel[reference == 'Active mining']) > 3){
        shapiro_test_active <- shapiro.test(dt_ma_dist_sel[reference == 'Active mining']$SSC_mgL_3yr)$p.value
      }
      
      
      # print(shapiro_test_ref)
      # print(shapiro_test_active)
      shapiro_tests_sel <- data.table('Reference' = shapiro_test_ref, 'Active mining' = shapiro_test_active)
      if(distance_sel == min(distances_sel)){
        shapiro_tests_comb <- shapiro_tests_sel
      }else{
        shapiro_tests_comb <- rbind(shapiro_tests_comb, shapiro_tests_sel, use.names = T)
      }}
      if(i == 1 & m == 1){
        shapiro_tests_all <- shapiro_tests_comb
      }else{
        shapiro_tests_all <- rbind(shapiro_tests_all, shapiro_tests_comb, use.names = T)
      }
  }
  
  # Histogram plot of mining vs. reference at each river
  monthly_ssc_distribution_vs_reference_plot <- ggplot(dt_ma_sel) +
    geom_histogram(aes(y = SSC_mgL_3yr, fill = reference), bins = 20,
                   color = 'black', lwd = 0.2, position = 'identity', alpha = 0.6) +
    # geom_density(aes(y = SSC_z_score, fill = reference), bins = 20,
    #                color = 'black', lwd = 0.5, alpha = 0.6) +
    geom_hline(data = dt_annual_avg_SSC, aes(yintercept = SSC_mgL, color = reference), lty = 'dashed') +
    season_facet + 
    facet_wrap(.~paste0(country_display, '\n', transect_display_name), scales = 'free', ncol = 5) +
    scale_fill_manual(values = c('Active mining' = '#F2BE22', 'Reference' = '#185359')) +
    scale_color_manual(values = c('Active mining' = '#F2BE22', 'Reference' = '#185359')) +
    theme(
      legend.position = 'top'
    ) +
    labs(
      x = 'N samples',
      y = 'SSC (mg/L)',
      color = 'Period', 
      fill = 'Period'
    ) +
    theme(axis.title.y = element_markdown(),
          legend.position = 'top',
          axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  # Set panel size to keep panels the same size across different pages (each page is a major landmass)
  monthly_ssc_distribution_vs_reference_plot <- set_panel_size(monthly_ssc_distribution_vs_reference_plot, width  = unit(0.85, "in"),
                                                         height = unit(0.85, "in"))
  
  ggsave(monthly_ssc_distribution_vs_reference_plot, filename = paste0(wd_figures, continent_write_name, '_monthly_ssc_distribution_vs_reference_plot.pdf'),
         width = 9, height = 13, useDingbats = F)
  ggsave(monthly_ssc_distribution_vs_reference_plot, filename = paste0(wd_figures, continent_write_name, '_monthly_ssc_distribution_vs_reference_plot.png'),
         width = 9, height = 13)
}

# Summary of normality 
shapiro_tests_summary <- melt(shapiro_tests_all, measure.vars = c('Reference', 'Active mining'))[
  ,':='(normal = ifelse(value < 0.01, 'non-normal', 'normal'))][
  ,.(N_normal = .N),
  by = .(normal, variable)
]
print(shapiro_tests_summary)

percent_reference_site_normally_distributed <- shapiro_tests_summary[variable == 'Reference' & normal == 'normal']$N_normal/
  sum(shapiro_tests_summary[variable == 'Reference' & !is.na(normal)]$N_normal, na.rm = T) * 100
percent_mining_site_normally_distributed <- shapiro_tests_summary[variable == 'Active mining' & normal == 'normal']$N_normal/
  sum(shapiro_tests_summary[variable == 'Active mining' & !is.na(normal)]$N_normal, na.rm = T) * 100

print(paste0('Percent of reference reaches with normal distributions: ', round(percent_reference_site_normally_distributed,1), '%'))
print(paste0('Percent of active mining reaches with normal distributions: ', round(percent_mining_site_normally_distributed,1), '%'))

monthly_ssc_distribution_vs_reference_plot_example <- ggplot(ssc_ma_annual_full_transect_avg[
  grepl(pattern = 'peru', x = site_no)]) +
  geom_histogram(aes(y = SSC_mgL_3yr, fill = reference), bins = 20,
                 color = 'black', lwd = 0.5, position = 'identity', alpha = 0.6) +
  # geom_density(aes(y = SSC_z_score, fill = reference), bins = 20,
  #                color = 'black', lwd = 0.5, alpha = 0.6) +
  geom_hline(data = ssc_ma_annual_avg_SSC[
    grepl(pattern = 'peru', x = site_no)], 
    aes(yintercept = SSC_mgL, color = reference), lty = 'dashed') +
  season_facet + 
  facet_wrap(.~site_no, scales = 'free') +
  scale_fill_manual(values = c('Active mining' = '#F2BE22', 'Reference' = '#185359')) +
  scale_color_manual(values = c('Active mining' = '#F2BE22', 'Reference' = '#185359')) +
  theme(
    legend.position = 'top'
  ) +
  labs(
    x = 'N samples',
    y = 'SSC (mg/L)',
    color = '',
    fill = ''
  )

#### FISH SUBLETHAL AND LETHAL EFFECTS MODEL ####
# From Singleton, 1986 10% over background levels
fish_lethality <- ssc_transects_color[
  ssc_ma_annual_full_transect_avg[,.(SSC_mgL_reference = mean(SSC_mgL_reference, na.rm = T), 
                                     SSC_mgL_reference_sd = mean(SSC_mgL_reference_sd, na.rm = T)), 
                                     by = .(site_no)],
  on = 'site_no'
][reference == 'Active mining']

fish_lethality <- fish_lethality[,':='(ssc_10p_exceed = SSC_mgL/SSC_mgL_reference,
                                       ssc_10p_exceed_yn = ifelse(SSC_mgL/SSC_mgL_reference > 1.1, 1, 0))]
fish_lethality_summary <- fish_lethality[,.(ssc_10p_exceed = mean(ssc_10p_exceed, na.rm = T),
                                            ssc_10p_exceed_time_fraction = sum(ssc_10p_exceed_yn, na.rm = T)/.N),
                                         by = .(site_no, country_display, continent_display, transect_display_name)]
fish_sublethal_90percent <- nrow(fish_lethality_summary[ssc_10p_exceed_time_fraction > 0.9])
fish_sublethal_50percent <- nrow(fish_lethality_summary[ssc_10p_exceed_time_fraction > 0.5])

#### CALCULATE AND PLOT MONTHLY NUMBER OF IMAGES FOR MINING AND REFERENCE PERIODS ####
ssc_transects_color_test <- ssc_transects_color[site_no == 'ghana_pra_dn']
ssc_transects_monthly_consistency <- ssc_transects_color[
  ,':='(n_images = .N,
        length_km = max(distance_10km, na.rm = T)-min(distance_10km, na.rm = T)), by = .(site_no, transect_display_name, reference)][
          ,':='(samples_per_10km_segment = n_images/length_km*10)]

print(ssc_transects_monthly_consistency)
ssc_transects_monthly_consistency_avg <- ssc_transects_monthly_consistency[
  ,.(monthly_n = mean(.N/length_km*10, na.rm = T)), 
  by = .(site_no, transect_display_name, country_display, continent_display, month, reference, samples_per_10km_segment)][
    ,':='(monthly_fraction_of_images = monthly_n/samples_per_10km_segment,
          reference_label = factor(substr(reference, 1, 1), levels = c('A','R'), labels = c('A','R')))
  ]

print(ssc_transects_monthly_consistency_avg)
continent_monthly_consistency <- unique(ssc_transects_monthly_consistency_avg$continent_display)
continent_monthly_consistency <- continent_monthly_consistency[which(!is.na(continent_monthly_consistency))]
if(length(continent_monthly_consistency) == 5){
  continent_monthly_consistency <- c(continent_monthly_consistency, 'S. Am.', 'Africa')
}else if(length(continent_monthly_consistency) == 6){
  continent_monthly_consistency <- c(continent_monthly_consistency, 'Africa')
}

for(i in 1:7){
  if(i == 1){
    j <- 1
    k <- 1}
  # Choose continent to print
  continent_sel <- continents_all[i]
  continent_write_name <- gsub('\\.| ', '', continent_sel)
  # Filter data.table to just selected continent
  dt_sel <- ssc_transects_monthly_consistency_avg[continent_display == continent_sel]

  # Split South America into two
  if(continent_write_name == 'SAm'){
    if(j == 1){
      dt_sel <- dt_sel[country_display %chin% early_alphabet_SA_countries]
      j <- j + 1}
    else{
      dt_sel <- dt_sel[!(country_display %chin% early_alphabet_SA_countries)]
      continent_write_name <- paste0(continent_write_name, '_1')} 
  }
  if(continent_write_name == 'Africa'){
    if(k == 1){
      dt_sel <- dt_sel[country_display %chin% early_alphabet_Africa_countries]
      # dt_annual_avg_SSC <- dt_annual_avg_SSC[country_display %chin% early_alphabet_Africa_countries]
      k <- k + 1}
    else{
      dt_sel <- dt_sel[!(country_display %chin% early_alphabet_Africa_countries)]
      continent_write_name <- paste0(continent_write_name, '_1')} 
  }
  
  continent_monthly_consistency_sel <- continent_monthly_consistency[i]
  # Plot fraction of images for each month in reference and Active mining datasets
  monthly_consistency_plot <- ggplot(dt_sel, 
                                     aes(y = reference_label, x = factor(month), fill = monthly_fraction_of_images)) +
    geom_tile(color = 'black') +
    facet_wrap(.~paste0(country_display, ', ', transect_display_name),
               ncol = 2) +
    season_facet + 
    scale_fill_gradient(low = 'grey30', high = '#0889BF', limits = c(0, 0.2), oob = squish) +
    scale_x_discrete(expand = expansion(mult = 0)) +
    scale_y_discrete(drop = F, expand = expansion(mult = 0), breaks = c('A','R')) +
    guides(fill = guide_colourbar(title.position = 'top', barheight = 0.5, barwidth = 10, frame.colour = 'black', ticks.colour = 'black')) +
    theme(
      legend.position = 'top'
    ) +
    labs(
      x = 'Month',
      y = 'Period (R = Reference, A = Active mining)',
      fill = 'Fraction of images',
      title = continent_sel
    )
  
  monthly_consistency_plot <- set_panel_size(monthly_consistency_plot, width  = unit(2, "in"),
                                             height = unit(0.24, "in"))
  
  ggsave(monthly_consistency_plot, 
         filename = paste0(wd_figures, 'monthly_consistency_', continent_write_name, '.pdf'),
         useDingbats = F, width = 7, height = 13)
  ggsave(monthly_consistency_plot, 
         filename = paste0(wd_figures, 'monthly_consistency_', continent_write_name, '.png'),
         width = 7, height = 13)
  
}

#### KILOMETERS AFFECTED VS. TOTAL KMS IN BASIN, COUNTRY, LAND MASS, REGION (TROPICS) ####

river_km_by_mining_country <- fread(
  paste0('/river_data_from_earth_engine/', 
         'river_lengths_by_mining_country_1000km2_60m_wide.csv'))[
           ,':='(.geo = NULL)]
river_km_by_tropics <- rbind(fread(
  paste0('/river_data_from_earth_engine/',
         'river_lengths_by_country_1000km2_60m_wide.csv'))[
  ,':='(.geo = NULL)][
    country != 'Myanmar'],
  river_km_by_mining_country[country == 'Myanmar'])

total_river_km_tropics <- sum(river_km_by_tropics$river_km)
print(paste0('total river kilometers in tropics: ', round(total_river_km_tropics)))

mining_rivers_country_basin <- fread(
  paste0('/river_data_from_earth_engine/',
         'mining_rivers_country_basin.csv'))[
  ,':='(.geo = NULL,
        BAS_NAME = NULL,
        COUNTRY = NULL,
        `system:index` = NULL,
        major_basin = BAS_NAME,
        country = COUNTRY)]

major_basins <- mining_rivers_country_basin[,.(n_rivers = .N), by = .(major_basin)]

river_length_by_country_decade_summary <- na.omit(mining_rivers_country_basin[
  river_length_by_decade[half_decade == 2015], on = 'site_no'][
    ,.(active_river_km = sum(active_river_km, na.rm = T)),
    by = .(continent_display, country)
  ][river_km_by_tropics, on = 'country'], cols = 'active_river_km')[
    ,':='(percent_active_river = round(active_river_km/river_km*100,2))
  ]

river_length_by_country_decade_summary[,':='(country = 
                                               ifelse(country == 'Democratic Republic of the Congo', 'Dem. Rep. Congo',
                                                      ifelse(country == 'Central African Republic', 'C. African Rep.',
                                                             country)))]

river_length_by_country_decade_stats <- river_length_by_country_decade_summary[!(country %chin% c('Mauritania', 'Republic of Congo')),
                                                                               .(percent_affected_avg = mean(percent_active_river, na.rm = T),
                                                                                 percent_affected_sd = sd(percent_active_river, na.rm = T),
                                                                                 percent_affected_median = median(percent_active_river, na.rm = T))
]
percent_country_rivers_affected_plot <- ggplot(river_length_by_country_decade_summary, 
                                               aes(x = country, y = river_km)) + 
  geom_col(aes(y = active_river_km), color = NA, fill = '#F59E00') +
  geom_col(fill = NA, color = 'black') +
  season_facet +
  rotate()

# Plot number of mining areas per country
percent_country_rivers_affected_plot <- ggplot(
  river_length_by_country_decade_summary[!(country %chin% c('Mauritania', 'Brazil'))]) + 
  geom_bar(aes(x = country, y = river_km/1000), stat = 'identity', fill = 'white', color = NA, lwd = 0.2, width = 0.85) +
  geom_bar(aes(x = country, y = active_river_km/1000), stat = 'identity', color = NA, fill = '#F59E00', lwd = 0.2, width = 0.85) +
  geom_bar(aes(x = country, y = river_km/1000), stat = 'identity', fill = NA, color = 'black', lwd = 0.2, width = 0.85) +
  # geom_bar(aes(x = country_display, fill = continent_display), color = 'black', lwd = 0.2, alpha = 0.8, width = 0.85) +
  facet_grid(rows = vars(continent_display), scales = 'free_y', switch = "y", space = "free_y") +
  coord_flip(clip = 'off') +
  season_facet + 
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  scale_x_discrete(limits=rev) +
  theme(panel.background = element_rect(fill = 'grey95'),
        panel.grid.major.y = element_line(color = 'white', size = 0.25),
        panel.grid.minor.y = element_line(color = 'white', size = 0.25)) +
  labs(x = 'Country',
       y = 'Moderate & major river length\n(1,000s of km)') +
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    plot.title = element_text(face = "bold", hjust = 0),
    strip.text.y = element_text(margin = margin(c(3,0,3,0)), angle = 270, face = "bold", hjust = 0.5),
    strip.placement = "outside",
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )

percent_country_rivers_affected_brazil_plot <- ggplot(
  river_length_by_country_decade_summary[!(country %chin% c('Mauritania', 'Peru'))]) + 
  geom_bar(aes(x = country, y = river_km/1000), stat = 'identity', fill = 'white', color = NA, lwd = 0.2, width = 0.85) +
  geom_bar(aes(x = country, y = active_river_km/1000), stat = 'identity', color = NA, fill = '#F59E00', lwd = 0.2, width = 0.85) +
  geom_bar(aes(x = country, y = river_km/1000), stat = 'identity', fill = NA, color = 'black', lwd = 0.2, width = 0.85) +
  # geom_bar(aes(x = country_display, fill = continent_display), color = 'black', lwd = 0.2, alpha = 0.8, width = 0.85) +
  facet_grid(rows = vars(continent_display), scales = 'free_y', switch = "y", space = "free_y") +
  coord_flip(clip = 'off') +
  season_facet + 
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  scale_x_discrete(limits=rev) +
  theme(panel.background = element_rect(fill = 'grey95'),
        panel.grid.major.y = element_line(color = 'white', size = 0.25),
        panel.grid.minor.y = element_line(color = 'white', size = 0.25)) +
  labs(x = 'Country',
       y = 'Moderate & major river length\n(1,000s of km)') +
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    plot.title = element_text(face = "bold", hjust = 0),
    strip.text.y = element_text(margin = margin(c(3,0,3,0)), angle = 270, face = "bold", hjust = 0.5),
    strip.placement = "outside",
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )


percent_country_rivers_affected_plot <- ggplotGrob(percent_country_rivers_affected_plot)
percent_country_rivers_affected_brazil_plot <- ggplotGrob(percent_country_rivers_affected_brazil_plot)
# Remove clipping from left side strip label to allow for full display
for(i in which(grepl("strip-l", percent_country_rivers_affected_plot$layout$name))){
  percent_country_rivers_affected_plot$grobs[[i]]$layout$clip <- "off"
}
for(i in which(grepl("strip-l", percent_country_rivers_affected_brazil_plot$layout$name))){
  percent_country_rivers_affected_brazil_plot$grobs[[i]]$layout$clip <- "off"
}



ggsave(percent_country_rivers_affected_brazil_plot, filename = paste0(wd_figures, 'percent_country_rivers_affected_brazil_plot.pdf'),
       width = 5, height = 10, useDingbats = F)
ggsave(percent_country_rivers_affected_plot, filename = paste0(wd_figures, 'percent_country_rivers_affected_plot.pdf'),
       width = 5, height = 10, useDingbats = F)
ggsave(percent_country_rivers_affected_plot, filename = paste0(wd_figures, 'percent_country_rivers_affected_plot.png'),
       width = 5, height = 10)

