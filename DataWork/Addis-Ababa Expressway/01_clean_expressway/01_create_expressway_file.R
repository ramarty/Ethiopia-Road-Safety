# Create Addis-Adaba Expressway Polyline

# Load Data --------------------------------------------------------------------
rsdp <- readOGR(file.path(rsdp_dir, "RawData", "All_Network_2016.shp"))

# Subset to Addis Adama --------------------------------------------------------
addis_adama_express <- rsdp[rsdp$LINKNAME %in% "Addis - Adama (Toll Road)",]

# Export -----------------------------------------------------------------------
saveRDS(addis_adama_express, file.path(aae_dir, "Data", "expressway", "addis_adama_express.Rds"))

addis_adama_express %>% 
  st_as_sf() %>%
  st_write(file.path(aae_dir, "Data", "expressway", "addis_adama_express.geojson"),
           delete_dsn = T)


