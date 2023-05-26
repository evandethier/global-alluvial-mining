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

abbrev_year <- function(l){
  label <- c() 
  for(i in 1:length(l)){
    label_sel <- paste0("'",substr(as.character(l[i]),3,4))
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

# Subfolders for files
wd_mining_mapping_import_folder <- paste0(wd_imports, 'mining_river_profiles/')
wd_mining_mapping_folder <- paste0(wd_imports, 'mining_data_for_earth_engine/')
wd_landsat_data <- paste0(wd_imports,'landsat_data_from_earth_engine/')
wd_river_lengths <- paste0(wd_imports, 'river_data_from_earth_engine/')
wd_oil_palm_subfolder <- paste0(wd_imports, 'oil_palm_and_mining_rivers_ssc_data/')

# Create folders within root directory to organize outputs if those folders do not exist
export_folder_paths <- c(wd_imports, wd_exports, wd_figures,
                         wd_mining_mapping_import_folder, wd_mining_mapping_folder,
                         wd_landsat_data, wd_oil_palm_subfolder, wd_river_lengths)

for(i in 1:length(export_folder_paths)){
  path_sel <- export_folder_paths[i]
  if(!dir.exists(path_sel)){
    # dir.create(path_sel)
    print(paste0('create: ', path_sel))
  }else{
    print(paste0('already exists: ', path_sel))
  }
}



#### 1. IMPORT DATA ####
# Elevated river reaches
river_reaches_elevated_2020 <- fread(paste0(wd_imports, 'river_reaches_elevated_2020.csv'))

# All sites metadata
site_metadata_all <- fread(paste0(wd_imports, 'rm_site_metadata.csv'))[
  # ,':='(site_no = ifelse(is.na(`Profile name`), `AGM district name`, `Profile name`))
  ][
  ,.(`AGM district name`, ID, `Profile name`, River, `Major river`, country_display, continent_display, site_no)
]

# Small river length
# tstm_river_length <- fread(paste0(wd_river_lengths, 'mining_on_small_rivers_length_20230404.csv'))[
tstm_river_length <- fread(paste0(wd_river_lengths, 'mining_on_small_rivers_length_20230513.csv'))[
  ,':='(
    site_no_spec = site_no,
    site_no = gsub(pattern = '_[0-9]+$', replace = '', x = site_no),
    .geo = NULL,
    sum = NULL)][
      ,':='(site_no = ifelse(site_no == 'peru_rio_malinowski_middle_agm_region7', 'peru_rio_malinowski_middle_agm_region', site_no))
    ]
carbon_by_site <- fread(paste0(wd_river_lengths, 'river_mineral_mining_carbon_2023.csv'))[,-c('.geo', 'mean')]
# Correct typos in dataset
tstm_river_length <- tstm_river_length[,':='(site_no = gsub('region7', 'region', site_no))]
tstm_river_length <- tstm_river_length[,':='(site_no = gsub('kyrgystan', 'kyrgyzstan', site_no))]
tstm_river_length <- tstm_river_length[,':='(site_no = gsub('phillipines', 'philippines', site_no))]
tstm_river_length <- tstm_river_length[,':='(site_no = gsub('bagre_upper_agm_region', 'bagre_upper_agm_region_TSTM', site_no))]
tstm_river_length <- tstm_river_length[,':='(site_no = gsub('somotillo_agm_region', 'somotillo_agm_region_TSTM', site_no))]

fwrite(tstm_river_length,
       file = paste0(wd_river_lengths,'mining_on_small_rivers_length_clean.csv'))
#### 2. ANALYZE SMALL RIVER LENGTH AND AREA BY SITE ####
# Sum length from different polygons for each river area
tstm_river_length_summary <- tstm_river_length[
  ,.(small_river_length_km = sum(length_km, na.rm = T),
     area_km2 = sum(area_km2, na.rm = T)),
  by = .(site_no)
][
  ,':='(site_tstm = ifelse(grepl('TSTM', site_no), 'Headwater sites', 'Large river sites'))
]

fwrite(tstm_river_length_summary,
       file = paste0(wd_river_lengths,'mining_on_small_rivers_length_summary.csv'))

## Fig. S10b
# Mining area vs. affected small river length scatter plot
tstm_scatter <- ggplot(tstm_river_length_summary, aes(x = area_km2, y = small_river_length_km, fill = site_tstm)) +
  geom_point(size = 4, color = 'black', pch = 21, lwd = 0.25) +
  scale_fill_manual(values = c('#2EB8F2', '#4007A6')) +
  season_facet + 
  scale_x_log10() +
  scale_y_log10() +
  theme(legend.position = c(0.3, 0.85),
        axis.title.x = element_markdown()) +
  labs(x = 'Mining area (km<sup>2</sup>)',
       y = 'Mining-affected small rivers (width < 50 m)\n(km affected)',
       fill = 'Watershed position')

## Fig. S10c
# Small river mining length boxplot
tstm_length_boxplot <- ggplot(tstm_river_length_summary, aes(x = site_tstm, y = small_river_length_km, fill = site_tstm)) +
  geom_boxplot() +
  scale_fill_manual(values = c('#2EB8F2', '#4007A6')) +
  season_facet + 
  scale_y_log10() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = '',
       y = 'Mining-affected small rivers (width < 50 m)\n(km affected)',
       fill = 'Watershed position')

## Fig. S10a
# Small site river area boxplot
tstm_area_boxplot <- ggplot(tstm_river_length_summary, aes(y = site_tstm, x = area_km2, fill = site_tstm)) +
  geom_boxplot() +
  scale_fill_manual(values = c('#2EB8F2', '#4007A6')) +
  season_facet + 
  scale_x_log10() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank()) +
  labs(x = 'Mining area (km<sup>2</sup>)',
       y = '',
       fill = 'Watershed position') 
  # theme(axis.title.x = element_markdown() # add to show axis


#### 3. CALCULATE RIVER LENGTH AND AREA IMPACTS ####
# Combine small and large river length tables
# Remove all rows with sites that don't have small rivers mapped
# If large river is too small to map (i.e., km_Nx is NA), make that 0
tstm_river_length_summary <- merge(
  tstm_river_length_summary, 
  carbon_by_site[,.(site_no, tons_C_per_ha)],
  by = 'site_no')

small_river_length_meta <- merge(site_metadata_all, 
                                 tstm_river_length_summary, 
                                 all.y = T,
                                 by.x = 'AGM district name', 
                                 by.y = 'site_no')


small_and_large_river_length <- merge(small_river_length_meta, river_reaches_elevated_2020[
  ,.(site_no, half_decade, km_Nx, Nx, profile_display_name)
], 
                                      by = c('site_no'), all.x = T)

# Add site_no column for rivers without it
# Get total large river length and small river length for each site
small_and_large_river_length <- small_and_large_river_length[
  ,':='(site_no = ifelse(is.na(site_no), `AGM district name`, site_no))
][
  ,.(km_Nx = mean(km_Nx, na.rm = T),
     small_river_length_km = sum(small_river_length_km, na.rm = T),
     area_km2 = sum(area_km2, na.rm = T),
     tons_C_per_ha =  mean(tons_C_per_ha, na.rm = T)
     ),
      by = .(site_no, profile_display_name, country_display, continent_display)][
    ,':='(km_Nx = ifelse(is.na(km_Nx), 0, km_Nx))][
    ,':='(total_river_km = small_river_length_km + km_Nx, # Add small and large river length
          profile_display_name = toupper(ifelse(!is.na(profile_display_name), profile_display_name,
                                         gsub('agm region TSTM| TSTM', '', gsub('_', ' ', site_no))))) 
  ][,':='(additional_small_river_fraction = total_river_km/km_Nx,
          site_tstm = ifelse(km_Nx == 0, 'Headwater sites', 'Large river sites'))]

# Add tons of carbon
small_and_large_river_length <- small_and_large_river_length[,':='(
  tons_C = tons_C_per_ha * area_km2 * 100
)]

# Plot biomass
tons_of_biomass_vs_area <- ggplot(small_and_large_river_length[!is.na(continent_display)],  
       aes(x = area_km2, y = tons_C, fill = continent_display)) +
  geom_point(size = 3, pch = 21, stroke = 0.5) +
  season_facet + 
  scale_x_log10(labels = fancy_scientific_modified) +
  scale_y_log10(labels = fancy_scientific_modified) +
  scale_fill_manual(values = c('#A66F8D','#395953','grey70', '#F2BC57','#D98841','#BF5349')) +
  theme(legend.position = c(0.2, 0.85),
        axis.title.x = element_markdown()) +
  labs(x = 'Mining area (km<sup>2</sup>)',
       y = 'Above- and below-ground biomass (tons)',
       fill = 'Landmass')

ggsave(tons_of_biomass_vs_area, filename = paste0(wd_figures, 'tons_of_biomass_vs_area.pdf'),
       width = 6, height = 6, useDingbats = F)
ggsave(tons_of_biomass_vs_area, filename = paste0(wd_figures, 'tons_of_biomass_vs_area.png'),
       width = 6, height = 6)

biomass_per_hectare_plot <- ggplot(small_and_large_river_length[!is.na(continent_display)],  
       aes(x = continent_display, y = tons_C_per_ha, fill = continent_display)) +
  geom_boxplot() +
  season_facet + 
  scale_fill_manual(values = c('#A66F8D','#395953','grey70', '#F2BC57','#D98841','#BF5349')) +
  theme(legend.position = 'none',
        axis.title.x = element_markdown()) +
  labs(x = '',
       y = 'Above- and below-ground biomass (tons/ha)',
       fill = 'Landmass')

ggsave(biomass_per_hectare_plot, filename = paste0(wd_figures, 'biomass_per_hectare_plot.pdf'),
       width = 4, height = 6, useDingbats = F)
ggsave(biomass_per_hectare_plot, filename = paste0(wd_figures, 'biomass_per_hectare_plot.png'),
       width = 4, height = 6)
# 
# biomass_per_hectare_histogram <- ggplot(small_and_large_river_length[!is.na(continent_display)],  
#        aes(y = tons_C_per_ha, fill = continent_display)) +
#   geom_density() +
#   season_facet + 
#   facet_wrap(.~continent_display, nrow = 1) +
#   scale_fill_manual(values = c('#A66F8D','#395953','grey70', '#F2BC57','#D98841','#BF5349')) +
#   theme(legend.position = 'none',
#         axis.title.x = element_markdown()) +
#   labs(x = '',
#        y = 'Above- and below-ground biomass (tons/ha)',
#        fill = 'Landmass')
# 
# ggsave(biomass_per_hectare_plot, filename = paste0(wd_figures, 'biomass_per_hectare_plot.pdf'),
#        width = 4, height = 6, useDingbats = F)
# ggsave(biomass_per_hectare_plot, filename = paste0(wd_figures, 'biomass_per_hectare_plot.png'),
#        width = 4, height = 6)

#### 4. CREATE ANNOTATIONS FOR PLOTS OF SMALL VS LARGE RIVER LENGTH & AREA ####
# Summarize length and area for small and large river groups
small_and_large_river_length_summary <- small_and_large_river_length[
  ,.(small_river_length_km = mean(small_river_length_km, na.rm = T),
     small_river_length_se = sd(small_river_length_km, na.rm = T)/sqrt(.N),
     mining_area_km2 = mean(area_km2, na.rm = T),
     mining_area_km2_se = sd(area_km2, na.rm = T)/sqrt(.N),
     tons_C_per_ha = mean(tons_C_per_ha, na.rm = T),
     tons_C_per_ha_se = sd(tons_C_per_ha, na.rm = T)/sqrt(.N),
     additional_small_river_fraction = mean(additional_small_river_fraction, na.rm = T),
     additional_small_river_fraction_se = sd(additional_small_river_fraction, na.rm = T)/sqrt(.N),
     N_rivers = .N
     ),
  by = .(site_tstm)
]

# Extrapolate river length estimates to full dataset
small_and_large_river_n_sites <- fread(paste0(wd_imports,'small_and_large_river_n_sites.csv'))

small_and_large_river_length_summary <- small_and_large_river_length_summary[small_and_large_river_n_sites, on = 'site_tstm'][
  ,':='(river_km_total = N_total_rivers * small_river_length_km,
        river_km_se = N_total_rivers * small_river_length_km * 
                      small_river_length_se/small_river_length_km,
        river_area_total_km2 = N_total_rivers * mining_area_km2,
        river_area_km2_se = N_total_rivers * mining_area_km2 * 
          mining_area_km2_se/mining_area_km2,
        mining_area_biomass_Mt = 100 * tons_C_per_ha * N_total_rivers * mining_area_km2/1e6,
        mining_area_biomass_Mt_se = 100 * tons_C_per_ha * N_total_rivers * mining_area_km2 * 
          sqrt((mining_area_km2_se/mining_area_km2)^2 + (tons_C_per_ha_se/tons_C_per_ha)^2)/1e6
        )
]

fwrite(small_and_large_river_length_summary, file = paste0(wd_river_lengths, 'small_and_large_river_length_summary.csv'))
total_additional_small_river_length <- small_and_large_river_length_summary[,.(
  additional_river_length = sum(river_km_total, na.rm = T),
  additional_river_length_se = sqrt(sum(river_km_se^2, na.rm = T)),
  mining_area = sum(river_area_total_km2, na.rm = T),
  mining_area_se = sqrt(sum(river_area_km2_se^2, na.rm = T)))]

#### 5. COMBINE PLOTS WITH ANNOTATIONS ####
## Fig. S10
# Combined a,b,c
# Summary plot of mining aerial extent and water length requirements
combined_tstm_summary_annotated_plots <- 
  (tstm_area_boxplot + scale_x_log10(limits = c(0.05, 4000), labels = fancy_scientific_modified) +
     geom_richtext(data = small_and_large_river_length_summary, fill = 'white', label.color = NA,
               aes(y = site_tstm, x = 800, 
                   label = paste0('Avg. = ', round(mining_area_km2, 0), ' km<sup>2</sup><br>(+/- ', round(mining_area_km2_se, 0), ' km SE)<br>',
                                  'N = ', N_rivers, ' areas')))
     ) +
  plot_spacer() + 
  (tstm_scatter + scale_y_log10(limits = c(1, 3000)) + scale_x_log10(limits = c(0.05, 4000), labels = fancy_scientific_modified)) + 
  (tstm_length_boxplot + 
     scale_y_log10(limits = c(1, 3000)) +
     geom_richtext(data = small_and_large_river_length_summary, fill = 'white', label.color = NA,
               aes(x = site_tstm, y = 2000, 
                   label = paste0('Avg. = ', round(small_river_length_km, 0), ' km<br>(+/- ', round(small_river_length_se, 0), ' km SE)<br>',
                                  'N = ', N_rivers, ' areas')))) +
  plot_layout(heights = c(0.3, 0.5), widths = c(0.5, 0.33)) +
    plot_annotation(tag_levels = 'a')

ggsave(combined_tstm_summary_annotated_plots, filename = paste0(wd_figures, 'figS10_combined_tstm_summary_annotated_plots.pdf'),
       width = 8, height = 8, useDingbats = F)
ggsave(combined_tstm_summary_annotated_plots, filename = paste0(wd_figures, 'figS10_combined_tstm_summary_annotated_plots.png'),
       width = 8, height = 8)

#### 6. PLOT MINING LENGTH AND AREA BY COUNTRY ####
# Summarize length and area by country
small_and_large_river_length_by_country <- small_and_large_river_length[
  ,.(small_river_length_km = mean(small_river_length_km, na.rm = T),
     small_river_length_se = sd(small_river_length_km, na.rm = T)/sqrt(.N),
     mining_area_km2 = mean(area_km2, na.rm = T),
     mining_area_km2_se = sd(area_km2, na.rm = T)/sqrt(.N),
     additional_small_river_fraction = mean(additional_small_river_fraction, na.rm = T),
     additional_small_river_fraction_se = sd(additional_small_river_fraction, na.rm = T)/sqrt(.N),
     N_rivers = .N,
     small_river_length_total_km = sum(small_river_length_km, na.rm = T),
     mining_area_total_km2 = sum(area_km2, na.rm = T),
     large_river_length_total_km = sum(km_Nx, na.rm = T)
  ),
  by = .(country_display, continent_display)
]

# Total mapped length and area
small_and_large_river_total_mapped = small_and_large_river_length_by_country[,.(
  small_river_length_km = sum(small_river_length_total_km, na.rm = T),
  mining_area_km2 = sum(mining_area_total_km2, na.rm = T)
)]
# Rivers by country
river_length_by_country <- melt(small_and_large_river_length_by_country, 
                                id.vars = c('country_display','continent_display'),
                                measure.vars = c('small_river_length_total_km','large_river_length_total_km'),
                                value.name = c('river_length_total_km'))[
                                  ,':='(site_type = ifelse(variable == 'large_river_length_total_km',
                                                           'Headwater sites', 'Large river sites'))
                                ]

# Plot length by country
county_river_length_summary_plot <- ggplot(river_length_by_country[!is.na(country_display)],
                                           aes(x = country_display, y = river_length_total_km, fill = site_type)) + 
  geom_col(color = 'black', linewidth = 0.25) +
  # rotate() + 
  scale_fill_manual(values = c('#2EB8F2', '#4007A6')) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  season_facet + 
  # facet_wrap(.~continent_display, scales = 'free_y', ncol = 1) +
  theme(legend.position = 'top') +
  labs(y = 'River length (km)',
       x = '',
       fill = '') +
  facet_grid(rows = vars(continent_display), scales = 'free_y', switch = "y", space = "free_y")  +
  scale_x_discrete(limits=rev) +
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    plot.title = element_text(face = "bold", hjust = 0),
    strip.text.y = element_text(margin = margin(c(3,0,3,0)), angle = 270, face = "bold", hjust = 0.5),
    strip.placement = "outside",
    axis.title.y = element_blank(),
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )  +
  coord_flip(clip = 'off')

county_river_length_summary_plot <- ggplotGrob(county_river_length_summary_plot)

# Remove clipping from left side strip label to allow for full display
for(i in which(grepl("strip-l", county_river_length_summary_plot$layout$name))){
  county_river_length_summary_plot$grobs[[i]]$layout$clip <- "off"
}

ggsave(county_river_length_summary_plot, filename = paste0(wd_figures, 'county_river_length_summary_plot.pdf'),
       width = 6, height = 8, useDingbats = F)
ggsave(county_river_length_summary_plot, filename = paste0(wd_figures, 'county_river_length_summary_plot.png'),
       width = 6, height = 8)

# Plot area by country
county_mining_area_summary_plot <- ggplot(small_and_large_river_length_by_country[
  !is.na(country_display) & mining_area_total_km2 > 0.1],
                                           aes(x = country_display, y = mining_area_total_km2)) + 
  geom_col(color = 'black', linewidth = 0.25, fill = 'grey40') +
  # rotate() + 
  scale_fill_manual(values = c('#2EB8F2', '#4007A6')) +
  season_facet + 
  # facet_wrap(.~continent_display, scales = 'free_y', ncol = 1) +
  theme(legend.position = 'top') +
  labs(y = 'Mapped mining area (km<sup>2</sup>)',
       x = '',
       fill = '') +
  facet_grid(rows = vars(continent_display), scales = 'free_y', switch = "y", space = "free_y")  +
  scale_x_discrete(limits=rev) +
  scale_y_log10(labels = fancy_scientific_modified, expand = expansion(mult = c(0.0001,0.1))) +
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    plot.title = element_text(face = "bold", hjust = 0),
    strip.text.y = element_text(margin = margin(c(3,0,3,0)), angle = 270, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = 'grey95'),
    strip.placement = "outside",
    axis.title.y = element_blank(),
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    axis.title.x = element_markdown()
  )  +
  coord_flip(clip = 'off')

county_mining_area_summary_plot <- ggplotGrob(county_mining_area_summary_plot)

# Remove clipping from left side strip label to allow for full display
for(i in which(grepl("strip-l", county_mining_area_summary_plot$layout$name))){
  county_mining_area_summary_plot$grobs[[i]]$layout$clip <- "off"
}

ggsave(county_mining_area_summary_plot, filename = paste0(wd_figures, 'county_mining_area_summary_plot.pdf'),
       width = 6, height = 8, useDingbats = F)
ggsave(county_mining_area_summary_plot, filename = paste0(wd_figures, 'county_mining_area_summary_plot.png'),
       width = 6, height = 8)



