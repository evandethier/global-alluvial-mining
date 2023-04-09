# Import land conversion data
# Calculate cumulative annual deforestation by land use
land_conversion <- rbindlist(lapply(
  paste0(wd_imports, 'agm-land-conversion-stats-annual/', 
         list.files(paste0(wd_imports, '/agm-land-conversion-stats-annual'))), 
  fread),
  use.names = T, fill = T)[
    ,':='(bare_ground_km2 = `1`,
          water_km2 = `2`,
          mixed_use_km2 = `3`)][
            ,.(name, year, bare_ground_km2, water_km2, mixed_use_km2, mining_length_km)
          ][,':='(mining_length_cumul_km = cumsum(mining_length_km),
                  bare_ground_cumul_km2 = cumsum(bare_ground_km2),
                  water_cumul_km2 = cumsum(water_km2),
                  mixed_use_cumul_km2 = cumsum(mixed_use_km2)),
            by = .(name)]
