# Daily Trends

# Load Data --------------------------------------------------------------------
hourly_df <- readRDS(file.path(dailyhourly_dir, "FinalData", "hourly.Rds"))

# Aggregate Data ---------------------------------------------------------------
hourly_df <- hourly_df %>%
  mutate(N_vehicles = N_vehicles / 10000,
         speed_p90 = speed_p90 / 10,
         precip_mm = precip_mm / 10) %>%
  filter(N_vehicles > 0)

felm(crash ~ N_vehicles + speed_p90 + precip_mm + holiday_plusminus_1day | 0 | 0 | 0, data = hourly_df) %>%
  summary()

felm(crash ~ N_vehicles | 0 | 0 | 0, data = hourly_df) %>%
  summary()

felm(crash ~ N_vehicles | 0 | 0 | 0, data = daily_df) %>%
  summary()
