# Daily Trends

# Load Data --------------------------------------------------------------------
hourly_df <- readRDS(file.path(dailyhourly_dir, "FinalData", "hourly.Rds"))

# Aggregate Data ---------------------------------------------------------------
hourly_df <- hourly_df %>%
  mutate_at(vars(N_vehicles, contains("holiday")),  replace_na, 0) %>%
  mutate(N_vehicles = N_vehicles / 10000,
         speed_p90 = speed_p90 / 10,
         precip_mm = precip_mm / 10)

felm(crash ~ N_vehicles + speed_p90 + precip_mm + holiday_plusminus_1day | direction | 0 | 0, data = hourly_df) %>%
  summary()

