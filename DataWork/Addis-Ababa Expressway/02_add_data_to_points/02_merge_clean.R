# Merge and Clean AAE Data

# Load and Merge Data ----------------------------------------------------------
aae_points <- readRDS(file.path(aae_dir, "Data", "expressway", "aae_points.Rds"))

aae_data <- file.path(aae_dir, "Data", "segments_data", "individual_data") %>%
  list.files(full.names = T, pattern = "*.Rds") %>%
  lapply(readRDS) %>% 
  reduce(merge, by = "distance_from_addis")

aae_points <- aae_points %>%
  left_join(aae_data, by = "distance_from_addis")

# Moving Average of Variables --------------------------------------------------
# Calcualte moving average of select variables (ie, taking average of variable
# within X meters of location)

calc_moving_avg <- function(i, aae_points, vars, window, fun = "mean"){
  # Calcualtes moving average for ith point
  # ARGs:
  # --i: ith point
  # --aae_points: Dataframe of AAE points (must have "distance_from_addis", which is distance in meters)
  # --vars: character vector of variables to summarize
  # --window: window (in meters) to calculate moving average
  # --fun: function ("mean" or "sum")
  
  aae_points_i <- aae_points[i,]
  
  aae_points_window <- aae_points[(aae_points$distance_from_addis >= aae_points_i$distance_from_addis-window) &
                                    (aae_points$distance_from_addis <= aae_points_i$distance_from_addis+window),]
  
  if(fun == "mean") agg_df <- aae_points_window[,vars] %>% apply(2, mean) 
  if(fun == "sum") agg_df <- aae_points_window[,vars]  %>% apply(2, sum) 
  
  out_df <- agg_df %>%
    t %>%
    as.data.frame() %>%
    rename_all(function(x) paste0(x, "_MA_", fun, "_", window, "m")) 
  out_df$distance_from_addis <- aae_points_i$distance_from_addis
  
  return(out_df)
}

## Crashes
ma_crash_100_df <- map_df(1:nrow(aae_points), calc_moving_avg, aae_points, 
                          names(aae_points) %>% str_subset("N_crashes"), 
                          500, "sum") 

ma_crash_500_df <- map_df(1:nrow(aae_points), calc_moving_avg, aae_points, 
                          names(aae_points) %>% str_subset("N_crashes"), 
                          1000, "sum")

## Turning Angles
ma_turnangle_100_df <- map_df(1:nrow(aae_points), calc_moving_avg, aae_points, 
                              names(aae_points) %>% str_subset("turnangle"), 
                              500, "mean")

ma_turnangle_500_df <- map_df(1:nrow(aae_points), calc_moving_avg, aae_points, 
                              names(aae_points) %>% str_subset("turnangle"), 
                              1000, "mean")

## Merge
aae_points <- aae_points %>%
  left_join(ma_crash_100_df, by = "distance_from_addis") %>%
  left_join(ma_crash_500_df, by = "distance_from_addis") %>%
  left_join(ma_turnangle_100_df, by = "distance_from_addis") %>%
  left_join(ma_turnangle_500_df, by = "distance_from_addis")

# Export -----------------------------------------------------------------------
saveRDS(aae_points, file.path(aae_dir, "Data", "segments_data", "merged_data", "aae_segment_data.Rds"))

