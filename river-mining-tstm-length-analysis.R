#### IMPORT DATA ####
# Elevated river reaches
river_reaches_elevated_2020 <- fread('river_reaches_elevated_2020.csv')
river_reaches_elevated_2020[grepl('inambari', site_no)]
river_reaches_elevated_2020[grepl('quimiri', site_no)]

# All sites metadata
site_metadata_all <- fread('rm_site_metadata.csv')[
  # ,':='(site_no = ifelse(is.na(`Transect name`), `AGM district name`, `Transect name`))
  ][
  ,.(`AGM district name`, `Transect name`, River, `Major river`, country_display, continent_display, site_no)
]

# Small river length
tstm_river_length <- fread('mining_on_small_rivers_length.csv')[
  ,':='(
    site_no_spec = site_no,
    site_no = gsub(pattern = '_[0-9]+$', replace = '', x = site_no),
    .geo = NULL,
    sum = NULL)][
      ,':='(site_no = ifelse(site_no == 'peru_rio_malinowski_middle_agm_region7', 'peru_rio_malinowski_middle_agm_region', site_no))
    ]

#### ANALYZE SMALL RIVER LENGTH BY SITE ####
tstm_river_length_summary <- tstm_river_length[
  ,.(small_river_length_km = sum(length_km, na.rm = T),
     area_km2 = sum(area_km2, na.rm = T)),
  by = .(site_no)
][
  ,':='(site_tstm = ifelse(grepl('TSTM', site_no), 'Headwater sites', 'Large river sites'))
]
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

# Summary plot of mining aerial extent and water length requirements
combined_tstm_summary_plots <- tstm_area_boxplot + plot_spacer() + tstm_scatter + tstm_length_boxplot +
  plot_layout(heights = c(0.2, 1), widths = c(1, 0.2))

ggsave(combined_tstm_summary_plots, filename = paste0(wd_figures, 'combined_tstm_summary_plots.pdf'),
       width = 5, height = 6, useDingbats = F)
ggsave(combined_tstm_summary_plots, filename = paste0(wd_figures, 'combined_tstm_summary_plots.png'),
       width = 5, height = 6)
  


#### JOIN ALL SITE DATA ####
# Combine small and large river length tables
# Remove all rows with sites that don't have small rivers mapped
# If large river is too small to map (i.e., km_Nx is NA), make that 0
small_river_length_meta <- merge(site_metadata_all, tstm_river_length_summary, 
                                      all.y = T,
                                        by.x = 'AGM district name', by.y = 'site_no')


small_and_large_river_length <- merge(small_river_length_meta, river_reaches_elevated_2020[
  ,.(site_no, half_decade, km_Nx, Nx, transect_display_name)
], 
                                      by = c('site_no'), all.x = T)

# Add site_no column for rivers without it
# Get total large river length and small river length for each site
small_and_large_river_length <- small_and_large_river_length[
  ,':='(site_no = ifelse(is.na(site_no), `AGM district name`, site_no))
][
  ,.(km_Nx = mean(km_Nx, na.rm = T),
     small_river_length_km = sum(small_river_length_km, na.rm = T),
     area_km2 = sum(area_km2, na.rm = T)
     ),
      by = .(site_no, transect_display_name, country_display, continent_display)][
    ,':='(km_Nx = ifelse(is.na(km_Nx), 0, km_Nx))][
    ,':='(total_river_km = small_river_length_km + km_Nx, # Add small and large river length
          transect_display_name = toupper(ifelse(!is.na(transect_display_name), transect_display_name,
                                         gsub('agm region TSTM| TSTM', '', gsub('_', ' ', site_no))))) 
  ][,':='(additional_small_river_fraction = total_river_km/km_Nx,
          site_tstm = ifelse(km_Nx == 0, 'Headwater sites', 'Large river sites'))]

small_and_large_river_length <- small_and_large_river_length[]
#### PLOT SMALL VS LARGE RIVER LENGTH ###
river_small_and_large_length_plot <- ggplot(small_and_large_river_length, aes(fill = site_tstm)) +
  geom_segment(aes(x = paste0(country_display, ', ', transect_display_name), xend = paste0(country_display, ', ', transect_display_name), y = km_Nx, yend = total_river_km)) +
  geom_point(aes(x = paste0(country_display, ', ', transect_display_name), y = km_Nx), size = 4, color = 'black', pch = 21, lwd = 0.25) +
  geom_point(aes(x = paste0(country_display, ', ', transect_display_name), y = total_river_km), size = 4, color = 'black', pch = 21, lwd = 0.25) +
  scale_fill_manual(values = c('#2EB8F2', '#4007A6')) +
  facet_wrap(.~site_tstm, scales = 'free') +
  season_facet + 
  theme(
    # legend.position = c(0.2, 0.9),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = 'Mining-affected small rivers (width < 50 m)\n(km affected)',
       fill = 'Watershed position')

small_and_large_river_length_summary <- small_and_large_river_length[
  ,.(small_river_length_km = mean(small_river_length_km, na.rm = T),
     small_river_length_se = sd(small_river_length_km, na.rm = T)/sqrt(.N),
     mining_area_km2 = mean(area_km2, na.rm = T),
     mining_area_km2_se = sd(area_km2, na.rm = T)/sqrt(.N),
     additional_small_river_fraction = mean(additional_small_river_fraction, na.rm = T),
     additional_small_river_fraction_se = sd(additional_small_river_fraction, na.rm = T)/sqrt(.N),
     N_rivers = .N
     ),
  by = .(site_tstm)
]

# Extrapolate river length estimates to full dataset
small_and_large_river_n_sites <- fread('small_and_large_river_n_sites.csv')

small_and_large_river_length_summary <- small_and_large_river_length_summary[small_and_large_river_n_sites, on = 'site_tstm'][
  ,':='(river_km_total = N_total_rivers * small_river_length_km,
        river_km_se = N_total_rivers * small_river_length_km * small_river_length_se/small_river_length_km)
]

total_additional_small_river_length <- small_and_large_river_length_summary[,.(additional_river_length = sum(river_km_total, na.rm = T))]

river_affected_length_for_small_and_large <- ggplot(small_and_large_river_length, aes(x = site_tstm, y = small_river_length_km, fill = site_tstm)) +
  geom_boxplot() +
  geom_richtext(data = small_and_large_river_length_summary, fill = 'white', label.color = NA,
            aes(x = site_tstm, y = 2000, 
                label = paste0(round(small_river_length_km, 0), ' km<br>(+/- ', round(small_river_length_se, 1), ' km SE)<br>',
                               'N = ', N_rivers, ' areas'))) +
  scale_fill_manual(values = c('#2EB8F2', '#4007A6')) +
  season_facet + 
  scale_y_log10() +
  labs(x = 'Watershed position',
       y = 'Small rivers affected by mining (width < 50 m)\n(km affected)')

# Summary plot of mining aerial extent and water length requirements
combined_tstm_summary_annotated_plots <- 
  (tstm_area_boxplot + scale_x_log10(limits = c(0.05, 4000), labels = fancy_scientific_modified) +
     geom_richtext(data = small_and_large_river_length_summary, fill = 'white', label.color = NA,
               aes(y = site_tstm, x = 800, 
                   label = paste0('Avg. = ', round(mining_area_km2, 1), ' km<sup>2</sup><br>(+/- ', round(mining_area_km2_se, 1), ' km SE)<br>',
                                  'N = ', N_rivers, ' areas')))
     ) +
  plot_spacer() + 
  (tstm_scatter + scale_y_log10(limits = c(1, 3000)) + scale_x_log10(limits = c(0.05, 4000), labels = fancy_scientific_modified)) + 
  (tstm_length_boxplot + 
     scale_y_log10(limits = c(1, 3000)) +
     geom_richtext(data = small_and_large_river_length_summary, fill = 'white', label.color = NA,
               aes(x = site_tstm, y = 2000, 
                   label = paste0('Avg. = ', round(small_river_length_km, 0), ' km<br>(+/- ', round(small_river_length_se, 1), ' km SE)<br>',
                                  'N = ', N_rivers, ' areas')))) +
  plot_layout(heights = c(0.3, 0.5), widths = c(0.5, 0.33))

ggsave(combined_tstm_summary_annotated_plots, filename = paste0(wd_figures, 'combined_tstm_summary_annotated_plots.pdf'),
       width = 8, height = 8, useDingbats = F)
ggsave(combined_tstm_summary_annotated_plots, filename = paste0(wd_figures, 'combined_tstm_summary_annotated_plots.png'),
       width = 8, height = 8)


