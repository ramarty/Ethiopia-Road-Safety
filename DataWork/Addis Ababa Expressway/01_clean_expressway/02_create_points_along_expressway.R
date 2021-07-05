# Divide Expressway into Points 

# Load Data --------------------------------------------------------------------
aae <- readRDS(file.path(aae_dir, "Data", "expressway", "addis_adama_express.Rds"))

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
  dplyr::mutate(distance_from_addis = 1:n()*10)

# Export -----------------------------------------------------------------------
saveRDS(aae_points, file.path(aae_dir, "Data", "expressway", "aae_points.Rds"))
write.csv(aae_points, file.path(aae_dir, "Data", "expressway", "aae_points.csv"), row.names = F)

