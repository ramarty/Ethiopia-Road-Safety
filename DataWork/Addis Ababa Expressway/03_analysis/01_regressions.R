# Merge and Clean AAE Data

# Load Data --------------------------------------------------------------------
aae_points <- readRDS(file.path(aae_dir, "Data", "segments_data", "merged_data", "aae_segment_data.Rds"))

head(aae_points)

aae_points$N_crashes

felm(N_crashes ~ turnangle_100m, data = aae_points)

aae_points %>% 
  ggplot() +
  geom_line(aes(x = distance_from_addis,
                 y = N_crashes_to_adama_MA_sum_1000m,
                 color = "To Adama")) +
  geom_line(aes(x = distance_from_addis,
                y = N_crashes_to_addis_MA_sum_1000m,
                color = "To Addis")) +
  labs(color = "Direction")


