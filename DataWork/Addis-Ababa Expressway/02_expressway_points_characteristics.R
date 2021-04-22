# Divide Expressway into Points 
# and determine characteristics
# of the expressway (eg, curvieness)

# TODO: Divide up points and have it line up (100+10, etc)
# TODO: Curviness

# Load Data --------------------------------------------------------------------
aae <- readRDS(file.path(aae_dir, "Data", "addis_adama_express.Rds"))

# Divide Into Points -----------------------------------------------------------
equal_distant_projection <- paste("+proj=aeqd +lat_0=",-1.283333," +lon_0=",36.816667, sep="")
aae <- spTransform(aae, CRS(equal_distant_projection)) 

num_points <- gLength(aae) / 10

aae_points <- aae %>%
  spsample(n = num_points, type = "regular") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) %>%
  as.data.frame() %>%
  dplyr::rename(longitude = coords.x1,
                latitude  = coords.x2) %>%
  mutate(distance_from_addis = 1:n()*10)

# Turning Angle ----------------------------------------------------------------
# (A) Turn angle calculated from 3 points: location, and neighboring points. We 
# calculate the turn angle using neighboring points at different distances; grabbing
# 'neighboring' points 10m away up to 500m away.
# (B) Each point is separated by 10 meters
aae_points$turnangle_10m  <- map_dbl(1:nrow(aae_points), determine_turn_angle, 1) 
aae_points$turnangle_50m  <- map_dbl(1:nrow(aae_points), determine_turn_angle, 5) 
aae_points$turnangle_100m <- map_dbl(1:nrow(aae_points), determine_turn_angle, 10) 
aae_points$turnangle_250m <- map_dbl(1:nrow(aae_points), determine_turn_angle, 250) 
aae_points$turnangle_500m <- map_dbl(1:nrow(aae_points), determine_turn_angle, 500) 

# Export -----------------------------------------------------------------------
saveRDS(aae_points, file.path(aae_dir, "Data", "addis_adama_express_points.Rds"))
write.csv(aae_points, file.path(aae_dir, "Data", "addis_adama_express_points.csv"),
          row.names = F)


