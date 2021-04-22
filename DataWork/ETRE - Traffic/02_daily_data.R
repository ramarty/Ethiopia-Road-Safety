# Make Daily Dataset

# 1. Load data -----------------------------------------------------------------
traffic_df <- readRDS(file.path(etre_traffic_dir, "FinalData", "traffic.Rds"))

# 2. Aggregate Data ------------------------------------------------------------
traffic_df %>%
  mutate(date = trans_occur_time %>% as.Date)


