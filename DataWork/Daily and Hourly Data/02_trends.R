# Daily Trends

library(lubridate)

# Load Data --------------------------------------------------------------------
daily_df <- readRDS(file.path(dailyhourly_dir, "FinalData", "daily.Rds"))

# Aggregate Data ---------------------------------------------------------------
daily_agg_df <- daily_df %>%
  group_by(date) %>%
  dplyr::summarise(N_vehicles = sum(N_vehicles),
                   speed_p50 = mean(speed_p50))

daily_annual_df <- daily_df %>%
  mutate(year = date %>% year) %>%
  group_by(year) %>%
  dplyr::summarise(N_vehicles = sum(N_vehicles),
                   speed_p50 = mean(speed_p50)) %>%
  mutate(year = paste0(year, "-06-01") %>% ymd())

# Figure -----------------------------------------------------------------------
daily_agg_df %>%
  ggplot() +
  geom_line(aes(x = date,
                y = N_vehicles)) +
  labs(x = NULL,
       y = "N Vehicle",
       title = "Number of Vehicles on Addis Ababa Expressway") +
  theme_ipsum() +
  ggsave(file.path(dailyhourly_dir, "Outputs", "n_vehicles.png"),
         height = 4, width = 6)


ggplot() +
  geom_line(data = daily_agg_df,
            aes(x = date,
                y = speed_p50),
            color = "dodgerblue1",
            alpha = 0.5,
            size = 0.5) +
  geom_line(data = daily_annual_df,
            aes(x = year,
                y = speed_p50),
            size = 1) +
  labs(x = NULL,
       y = "Speed (km/hr)",
       title = "Median Speed on Addis Adama Expressway",
       subtitle = "Daily Values and Annual Average") +
  theme_ipsum() +
  ggsave(file.path(dailyhourly_dir, "Outputs", "speed_p50_trends.png"),
         height = 5, width = 7)



