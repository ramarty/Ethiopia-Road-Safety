# Distance Toll Plazas

# Load Data --------------------------------------------------------------------
aae_points <- readRDS(file.path(aae_dir, "Data", "expressway", "aae_points.Rds"))

# Distance Plazas --------------------------------------------------------------
aae_points$dist_plaza_k2_km  <- abs(aae_points$distance_from_addis - 2*1000) / 1000
aae_points$dist_plaza_k16_km <- abs(aae_points$distance_from_addis - 16*1000) / 1000
aae_points$dist_plaza_k33_km <- abs(aae_points$distance_from_addis - 33*1000) / 1000
aae_points$dist_plaza_k52_km <- abs(aae_points$distance_from_addis - 52*1000) / 1000
aae_points$dist_plaza_k60_km <- abs(aae_points$distance_from_addis - 60*1000) / 1000
aae_points$dist_plaza_k64_km <- abs(aae_points$distance_from_addis - 64*1000) / 1000

aae_points$dist_plaza_nearest_km <- with(aae_points, 
                                        pmin(dist_plaza_k2_km, 
                                             dist_plaza_k16_km, 
                                             dist_plaza_k33_km,
                                             dist_plaza_k52_km, 
                                             dist_plaza_k60_km, 
                                             dist_plaza_k64_km))

# Export -----------------------------------------------------------------------
aae_points %>%
  dplyr::select(-c(latitude, longitude)) %>%
  saveRDS(file.path(aae_dir, "Data", "segments_data", "individual_data", "aae_points_dist_toll_plazas.Rds"))

