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

library(tidyr)
library(broom)


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

wd_oil_palm_subfolder <- paste0(wd_imports, 'oil_palm_and_mining_rivers_ssc_data/')

# Create folders within root directory to organize outputs if those folders do not exist
export_folder_paths <- c(wd_imports, wd_exports, wd_figures, wd_oil_palm_subfolder)
for(i in 1:length(export_folder_paths)){
  path_sel <- export_folder_paths[i]
  if(!dir.exists(path_sel)){
    dir.create(path_sel)}
}




#### 1. IMPORT SSC DATA FROM OIL PALM REGIONS ####
# Make a list of file paths that store data from oil palm sites
individual_river_ma_data_files <- list.files(pattern = 'ssc_ma_timeseries_', wd_oil_palm_subfolder)

# Import metadata about oil palm occurrence along river profiles
selected_ma_sites_all <- fread(paste0(wd_oil_palm_subfolder, 'oil_palm_sites.csv'))

# Import data from selected files and combine into a single data.table
palm_oil_ssc_ma_timeseries <- rbindlist(lapply( 
  paste0(paste0(wd_oil_palm_subfolder, individual_river_ma_data_files)),
  fread),
  use.names = T, fill = T)

# Restrict to oil palm and mining timeframes and river reaches
palm_oil_ssc_ma_timeseries <- merge(palm_oil_ssc_ma_timeseries, selected_ma_sites_all, by = 'site_no')
palm_oil_ssc_ma_timeseries <- palm_oil_ssc_ma_timeseries[year < year_end & 
                                                         distance_10km > distance_km_start  &
                                                         distance_10km < distance_km_end]

# Make columns for SSC anomaly and determining mining activity
palm_oil_ssc_ma_timeseries <- palm_oil_ssc_ma_timeseries[
        ,':='(ssc_anomaly = (SSC_mgL_3yr - mean(SSC_mgL_3yr, na.rm = T))/sd(SSC_mgL_3yr, na.rm = T),
              period = ifelse(year < mining_onset, 'Oil palm', 'Active mining')),
                           by = .(site_no, mining_onset)]

# Summarize anomaly for each year
palm_oil_ssc_ma_timeseries_anomaly <- palm_oil_ssc_ma_timeseries[,.(ssc_anomaly = mean(ssc_anomaly, na.rm = T)), 
                           by = .(site_no, year, mining_onset, period)]

#### 2. CALCULATE PRE-MINING AND POST-MINING SSC INCREASE SLOPES ####

getPrePost_mining_slopes <- function(site_no_sel){
  dt <- palm_oil_ssc_ma_timeseries[site_no == site_no_sel]
  # Split by mining onset
  dt_pre <- dt[year < mining_onset & !is.na(SSC_mgL_3yr)]
  dt_post <- dt[year >= (mining_onset-1) & !is.na(SSC_mgL_3yr)]
  
  # Get pre and post slopes
  lm_pre <- lm(SSC_mgL_3yr ~ year, data = dt_pre)
  lm_post <- lm(SSC_mgL_3yr ~ year, data = dt_post)
  
  lm_pre_summary <- data.table(tidy(lm_pre))[term == 'year'][
    ,.(estimate, std.error, statistic, p.value)][
      ,':='(period = 'Oil palm',
            site_no = dt_pre$site_no[1])
    ]
  lm_post_summary <- data.table(tidy(lm_post))[term == 'year'][
    ,.(estimate, std.error, statistic, p.value)][
      ,':='(period = 'Active mining',
            site_no = dt_pre$site_no[1])
    ]
  
  slope_summary <- rbind(lm_pre_summary, lm_post_summary, use.names = T)
  
  return(slope_summary)
}

# Apply function to each 
palm_oil_comparison_comb <- rbindlist(lapply(selected_ma_sites_all$site_no, getPrePost_mining_slopes),
                                      use.names = T, fill = T)

# Calculate slope change from oil palm period to mining period
palm_oil_slope_change <- dcast.data.table(site_no ~ period, value.var = 'estimate', 
                                              data = palm_oil_comparison_comb)
palm_oil_slope_change <- palm_oil_slope_change[
  ,':='(fraction_change = (`Active mining` - `Oil palm`)/abs(`Oil palm`))]

#### 3. STATISTICS FOR MANUSCRIPT ####
# Average slope change for all rivers
palm_oil_avg_slope_change <- palm_oil_slope_change[
              ,.(slope_change_mean = mean(fraction_change, na.rm = T),
                 slope_change_se = sd(fraction_change, na.rm = T)/sqrt(.N),
                 N_rivers = .N)]

# Average change in mg/L/yr
palm_oil_comparison_summary <- palm_oil_comparison_comb[
  ,.(N_rivers = .N,
     change_SSC_mgL_yr_mean = mean(estimate, na.rm = T),
     change_SSC_mgL_yr_se = sd(estimate, na.rm = T)/sqrt(.N)),
  by = .(period)
]

#### 4. PLOTS FOR MANUSCRIPT ####
## Extended data Fig. 2c
# Plot boxplot of slope pre- and post-mining onset
palm_oil_sites_pre_post_mining_slope_boxplot <- ggplot(palm_oil_comparison_comb, 
       aes(x = factor(period, levels = c('Oil palm', 'Active mining')), y = estimate,
           fill = period)) +
  geom_boxplot() +
  geom_text(data = palm_oil_comparison_summary, aes(y = 220, 
            label = paste0('Avg. = ', round(change_SSC_mgL_yr_mean, 0), ' mg/L/yr\n', 'N rivers = ', N_rivers))) +
  season_facet +
  scale_fill_manual(values = c('#F2BE22','#3D7543')) +
  labs(
    x = '',
    y = 'Change in SSC (mg/L/yr)',
    fill = ''
  )

## Extended data Fig. 2b
# Plot line plot of average standardized SSC relative to mining onset
palm_oil_period_rect <- data.table(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf)
mining_period_rect <- data.table(xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf)
palm_oil_sites_pre_post_mining <- 
  ggplot(palm_oil_ssc_ma_timeseries_anomaly, aes(x = year - mining_onset, y = ssc_anomaly)) +
  geom_vline(xintercept = 0, lty = 'dashed') +
  geom_rect(data = palm_oil_period_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            # fill = '#3D7543', alpha = 0.2, inherit.aes = F) +
            fill = 'grey70', alpha = 0.2, inherit.aes = F) +
  # geom_rect(data = mining_period_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
  #           fill = '#F2BE22', alpha = 0.2, inherit.aes = F) +
  stat_summary(geom = 'line') + 
  stat_summary() + 
  stat_smooth(aes(group = period, color = period), method = 'lm') + 
  geom_text(data = palm_oil_comparison_summary, aes(x = -16, y = 1.65, hjust = 0, vjust = 1,
                label = paste0('Oil palm cultivation\n(Pre-mining)'))) +
  geom_text(data = palm_oil_comparison_summary, aes(x = 0.7, y = 1.65, hjust = 0, vjust = 1,
                label = paste0('Active mining\n(Oil palm cultivation\nongoing)'))) +
  scale_x_continuous(limits = c(-16, 20)) +
  scale_color_manual(values = c('#F2BE22', '#3D7543')) +
  season_facet + 
  labs(
    x = 'Year since mining onset',
    y = 'SSC standardized anomaly'
  )

# print(palm_oil_sites_pre_post_mining)

## Extended data Fig. 2a
# Plot case study example
batang_hari_trib2_dt <- palm_oil_ssc_ma_timeseries[site_no == 'indonesia_batang_hari_trib2']
batang_hari_trib2_palm_oil_period_rect <- data.table(xmin = -Inf, 
                                                     xmax = batang_hari_trib2_dt$mining_onset[1], 
                                                     ymin = -Inf, ymax = Inf)
batang_hari_trib2_ssc_ma_timeseries_plot <- ggplot() + 
  geom_rect(data = batang_hari_trib2_palm_oil_period_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = 'grey70', alpha = 0.2, inherit.aes = F) +
  geom_line(data = batang_hari_trib2_dt[!is.na(SSC_mgL)],
            aes(x = (year + month/12), y = SSC_mgL, color = 'Monthly average'), lty = 'dashed') +
  geom_point(data = batang_hari_trib2_dt[!is.na(SSC_mgL)],
             aes(x = (year + month/12), y = SSC_mgL, color = 'Monthly average')) +
  geom_line(data = batang_hari_trib2_dt[!is.na(SSC_mgL_3yr)], aes(x = (year + month/12), y = SSC_mgL_3yr, color = '3-yr moving average')) +
  geom_vline(xintercept = batang_hari_trib2_dt$mining_onset[1], lty = 'dashed') +
  geom_text(data = palm_oil_comparison_summary, aes(x = 1987, y = 1500, hjust = 0, vjust = 1,
                                                    label = paste0('Oil palm cultivation\n(Pre-mining)'))) +
  scale_color_manual(values = c('Monthly average' = 'black', '3-yr moving average' = 'red')) +
  facet_wrap(.~paste0(Country, '\n', profile_display_name, '\n', gsub('profile_','p',`Profile id`))) +
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

# print(batang_hari_trib2_ssc_ma_timeseries_plot)

## Extended data Fig. 2
# Combine a,b,c
palm_oil_sites_pre_post_mining_comb_plot <- batang_hari_trib2_ssc_ma_timeseries_plot /
                                            (palm_oil_sites_pre_post_mining + 
                                            palm_oil_sites_pre_post_mining_slope_boxplot) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))

ggsave(palm_oil_sites_pre_post_mining_comb_plot, 
       filename = paste0(wd_figures, 'figE2_palm_oil_sites_pre_post_mining_comb_plot.pdf'),
       width = 8.5, height = 10, useDingbats = F)
ggsave(palm_oil_sites_pre_post_mining_comb_plot, 
       filename = paste0(wd_figures, 'figE2_palm_oil_sites_pre_post_mining_comb_plot.png'),
       width = 8.5, height = 10)
   