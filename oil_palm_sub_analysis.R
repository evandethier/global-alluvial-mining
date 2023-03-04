#### IMPORT SSC DATA FROM OIL PALM REGIONS ####
library(tidyr)
library(broom)
kampar_trib = fread("ssc_ma_timeseries_indonesia_kampar_trib.csv")[distance_10km < 130 & year < 2015]
batang_hari_trib2 = fread("ssc_ma_timeseries_indonesia_batang_hari_trib2.csv")[distance_10km == 60]
batang_hari_bedaro = fread("ssc_ma_timeseries_indonesia_batang_hari_bedaro_agm_region.csv")
batang_hari = fread("ssc_ma_timeseries_indonesia_batang_hari.csv")[distance_10km - distance_10km%%100 == 600]
indonesia_west_kalimantan_monggo = fread("ssc_ma_timeseries_indonesia_west_kalimantan_monggo_agm_region.csv")
indonesia_kapuas_trib = fread("ssc_ma_timeseries_indonesia_kapuas_trib.csv")
indonesia_kahayan = fread("ssc_ma_timeseries_indonesia_kahayan.csv")
indonesia_nabire_barat = fread("ssc_ma_timeseries_indonesia_nabire_barat.csv")
ghana_pra_dn = fread("ssc_ma_timeseries_ghana_pra_dn.csv")
ghana_pra_up = fread("ssc_ma_timeseries_ghana_pra_up.csv")
indonesia_batang_asai_upper_agm_region = fread("ssc_ma_timeseries_indonesia_batang_asai_upper_agm_region.csv")
indonesia_south_sumatra_soetangegoh_agm_region = fread("ssc_ma_timeseries_indonesia_south_sumatra_soetangegoh_agm_region.csv")[distance_10km < 150]
indonesia_lampung_agm_region = fread("ssc_ma_timeseries_indonesia_lampung_agm_region.csv")
liberia_cavalla_river_agm_region = fread("ssc_ma_timeseries_liberia_cavalla_river_agm_region.csv")[distance_10km < 240]
indonesia_maura_soma_agm_region = fread("ssc_ma_timeseries_indonesia_maura_soma_agm_region.csv")

palm_oil_ssc_ma_timeseries <- rbind(kampar_trib, batang_hari_trib2, batang_hari_bedaro, batang_hari, 
      indonesia_west_kalimantan_monggo, indonesia_kapuas_trib, indonesia_kahayan, indonesia_south_sumatra_soetangegoh_agm_region,
      indonesia_nabire_barat, ghana_pra_dn, ghana_pra_up, indonesia_batang_asai_upper_agm_region,
      indonesia_lampung_agm_region, liberia_cavalla_river_agm_region, indonesia_maura_soma_agm_region)

palm_oil_ssc_ma_timeseries <- palm_oil_ssc_ma_timeseries[
        ,':='(ssc_anomaly = (SSC_mgL_3yr - mean(SSC_mgL_3yr, na.rm = T))/sd(SSC_mgL_3yr, na.rm = T),
              period = ifelse(year < mining_onset, 'Oil palm', 'Active mining')),
                           by = .(site_no, mining_onset)]

palm_oil_ssc_ma_timeseries_anomaly <- palm_oil_ssc_ma_timeseries[,.(ssc_anomaly = mean(ssc_anomaly, na.rm = T)), 
                           by = .(site_no, year, mining_onset, period)]

#### CALCULATE PRE-MINING AND POST-MINING SSC INCREASE SLOPES ####

getPrePost_mining_slopes <- function(dt){
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

kampar_pre_mining_slope <- getPrePost_mining_slopes(kampar_trib)
print(kampar_pre_mining_slope)

batang_hari_trib2_slope <- getPrePost_mining_slopes(batang_hari_trib2[year < 2007])
print(batang_hari_trib2_slope)

batang_hari_bedaro_slope <- getPrePost_mining_slopes(batang_hari_bedaro)
print(batang_hari_bedaro_slope)

batang_hari_slope <- getPrePost_mining_slopes(batang_hari)
print(batang_hari_slope)

indonesia_batang_asai_upper_agm_region_slope <- getPrePost_mining_slopes(indonesia_batang_asai_upper_agm_region)
print(indonesia_batang_asai_upper_agm_region_slope)

indonesia_west_kalimantan_monggo_slope <- getPrePost_mining_slopes(indonesia_west_kalimantan_monggo)
print(indonesia_west_kalimantan_monggo_slope)

indonesia_kapuas_trib_slope <- getPrePost_mining_slopes(indonesia_kapuas_trib)
print(indonesia_kapuas_trib_slope)

indonesia_kahayan_slope <- getPrePost_mining_slopes(indonesia_kahayan)
print(indonesia_kahayan_slope)

indonesia_nabire_barat_slope <- getPrePost_mining_slopes(indonesia_nabire_barat)
print(indonesia_nabire_barat_slope)

indonesia_south_sumatra_soetangegoh_agm_region_slope <- getPrePost_mining_slopes(indonesia_south_sumatra_soetangegoh_agm_region)
print(indonesia_south_sumatra_soetangegoh_agm_region_slope)

indonesia_lampung_agm_region_slope <- getPrePost_mining_slopes(indonesia_lampung_agm_region)
print(indonesia_lampung_agm_region_slope)

indonesia_maura_soma_agm_region_slope <- getPrePost_mining_slopes(indonesia_maura_soma_agm_region)
print(indonesia_maura_soma_agm_region_slope)

ghana_pra_dn_slope <- getPrePost_mining_slopes(ghana_pra_dn)
print(ghana_pra_dn_slope)

ghana_pra_up_slope <- getPrePost_mining_slopes(ghana_pra_up)
print(ghana_pra_up_slope)

liberia_cavalla_river_agm_region_slope <- getPrePost_mining_slopes(liberia_cavalla_river_agm_region)
print(liberia_cavalla_river_agm_region_slope)

palm_oil_comparison_comb <- rbind(kampar_pre_mining_slope, batang_hari_trib2_slope, batang_hari_bedaro_slope,
                                  batang_hari_slope, indonesia_batang_asai_upper_agm_region_slope, 
                                  indonesia_west_kalimantan_monggo_slope, indonesia_kapuas_trib_slope,
                                  indonesia_kahayan_slope, indonesia_nabire_barat_slope, ghana_pra_dn_slope,
                                  ghana_pra_up_slope, indonesia_south_sumatra_soetangegoh_agm_region_slope,
                                  indonesia_lampung_agm_region_slope, liberia_cavalla_river_agm_region_slope,
                                  indonesia_maura_soma_agm_region_slope)

# Calculate slope change from oil palm period to mining period
palm_oil_slope_change <- dcast.data.table(site_no ~ period, value.var = 'estimate', 
                                              data = palm_oil_comparison_comb)
palm_oil_slope_change <- palm_oil_slope_change[
  ,':='(fraction_change = (`Active mining` - `Oil palm`)/abs(`Oil palm`))]

#### STATISTICS FOR MANUSCRIPT ####
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

#### PLOTS FOR MANUSCRIPT ####
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

print(palm_oil_sites_pre_post_mining)

# Plot case study example
batang_hari_trib2_palm_oil_period_rect <- data.table(xmin = -Inf, xmax = batang_hari_trib2$mining_onset[1], ymin = -Inf, ymax = Inf)
batang_hari_trib2_ssc_ma_timeseries_plot <- ggplot() + 
  geom_rect(data = batang_hari_trib2_palm_oil_period_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = 'grey70', alpha = 0.2, inherit.aes = F) +
  geom_line(data = batang_hari_trib2[!is.na(SSC_mgL)],
            aes(x = (year + month/12), y = SSC_mgL, color = 'Monthly average'), lty = 'dashed') +
  geom_point(data = batang_hari_trib2[!is.na(SSC_mgL)],
             aes(x = (year + month/12), y = SSC_mgL, color = 'Monthly average')) +
  geom_line(data = batang_hari_trib2[!is.na(SSC_mgL_3yr)], aes(x = (year + month/12), y = SSC_mgL_3yr, color = '3-yr moving average'), color = 'red') +
  geom_vline(xintercept = batang_hari_trib2$mining_onset[1], lty = 'dashed') +
  geom_text(data = palm_oil_comparison_summary, aes(x = 1987, y = 1500, hjust = 0, vjust = 1,
                                                    label = paste0('Oil palm cultivation\n(Pre-mining)'))) +
  scale_color_manual(values = c('Monthly average' = 'black', '3-yr moving average' = 'red')) +
  facet_wrap(.~paste0(Country, '\n', transect_display_name)) +
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

print(batang_hari_trib2_ssc_ma_timeseries_plot)

palm_oil_sites_pre_post_mining_comb_plot <- batang_hari_trib2_ssc_ma_timeseries_plot /
                                            (palm_oil_sites_pre_post_mining + 
                                            palm_oil_sites_pre_post_mining_slope_boxplot) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))

ggsave(palm_oil_sites_pre_post_mining_comb_plot, 
       filename = paste0(wd_figures, 'palm_oil_sites_pre_post_mining_comb_plot.pdf'),
       width = 8.5, height = 10, useDingbats = F)
ggsave(palm_oil_sites_pre_post_mining_comb_plot, 
       filename = paste0(wd_figures, 'palm_oil_sites_pre_post_mining_comb_plot.png'),
       width = 8.5, height = 10)
   