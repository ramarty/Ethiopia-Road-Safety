# Turn Angle Along AAE

# Load Data --------------------------------------------------------------------
aae_points <- readRDS(file.path(aae_dir, "Data", "expressway", "aae_points.Rds"))
crashes_df <- readRDS(file.path(etre_crashes_dir, "FinalData", "crashes.Rds"))

# Cleanup Crashes --------------------------------------------------------------
crashes_df <- crashes_df %>%
  dplyr::filter(!is.na(distance_from_addis)) %>%
  dplyr::mutate(direction = direction %>% str_replace_all("to ", "to_"))

# Aggregate Crashes by Segment -------------------------------------------------
crashes_all <- crashes_df %>%
  dplyr::group_by(distance_from_addis) %>%
  dplyr::summarise(N_crashes = n())

crashes_dir <- crashes_df %>%
  filter(!is.na(direction)) %>%
  dplyr::group_by(distance_from_addis, direction) %>%
  dplyr::summarise(N = n()) %>%
  pivot_wider(id_cols = distance_from_addis,
              values_from = N,
              names_from = direction) %>%
  rename_at(vars(-distance_from_addis),function(x) paste0("N_crashes_", x)) 

crashes_annual <- crashes_df %>%
  filter(!is.na(year)) %>%
  dplyr::group_by(distance_from_addis, year) %>%
  dplyr::summarise(N = n()) %>%
  pivot_wider(id_cols = distance_from_addis,
              values_from = N,
              names_from = year) %>%
  rename_at(vars(-distance_from_addis),function(x) paste0("N_crashes_", x)) 

crashes_dir_annual <- crashes_df %>%
  filter(!is.na(direction),
         !is.na(year)) %>%
  dplyr::group_by(distance_from_addis, direction, year) %>%
  dplyr::summarise(N = n()) %>%
  pivot_wider(id_cols = distance_from_addis,
              values_from = N,
              names_from = c(direction, year)) %>%
  rename_at(vars(-distance_from_addis),function(x) paste0("N_crashes_", x)) 

crashes_type <- crashes_df %>%
  mutate(type_of_accident_simple = type_of_accident_simple %>% 
           str_replace_all("[:punct:]", "") %>%
           str_replace_all(" ", "_")) %>%
  dplyr::group_by(distance_from_addis, type_of_accident_simple) %>%
  dplyr::summarise(N = n()) %>%
  pivot_wider(id_cols = distance_from_addis,
              values_from = N,
              names_from = c(type_of_accident_simple)) %>%
  rename_at(vars(-distance_from_addis), function(x) paste0("N_crashes_bytype_", x)) 

# Merge ------------------------------------------------------------------------
aae_points <- aae_points %>%
  left_join(crashes_all, by = "distance_from_addis") %>%
  left_join(crashes_dir, by = "distance_from_addis") %>%
  left_join(crashes_annual, by = "distance_from_addis") %>%
  left_join(crashes_dir_annual, by = "distance_from_addis") %>%
  left_join(crashes_type, by = "distance_from_addis") %>%
  mutate_all(replace_na, 0)

# Export -----------------------------------------------------------------------
aae_points %>%
  dplyr::select(-c(latitude, longitude)) %>%
  saveRDS(file.path(aae_dir, "Data", "segments_data", "individual_data", "aae_points_crashes.Rds"))

