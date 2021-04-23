# Turn Angle Along AAE

# Load Data --------------------------------------------------------------------
aae_points <- readRDS(file.path(aae_dir, "Data", "expressway", "aae_points.Rds"))

# Turning Angle ----------------------------------------------------------------
# (A) Turn angle calculated from 3 points: location, and neighboring points. We 
# calculate the turn angle using neighboring points at different distances; grabbing
# 'neighboring' points 10m away up to 500m away.
# (B) Each point is separated by 10 meters
aae_points$turnangle_10m  <- map_dbl(1:nrow(aae_points), determine_turn_angle, aae_points, 1) 
aae_points$turnangle_50m  <- map_dbl(1:nrow(aae_points), determine_turn_angle, aae_points, 5) 
aae_points$turnangle_100m <- map_dbl(1:nrow(aae_points), determine_turn_angle, aae_points, 10) 
aae_points$turnangle_250m <- map_dbl(1:nrow(aae_points), determine_turn_angle, aae_points, 250) 
aae_points$turnangle_500m <- map_dbl(1:nrow(aae_points), determine_turn_angle, aae_points, 500) 

# Export -----------------------------------------------------------------------
aae_points %>%
  dplyr::select(-c(latitude, longitude)) %>%
  saveRDS(file.path(aae_dir, "Data", "segments_data", "individual_data", "aae_points_turn_angle.Rds"))

