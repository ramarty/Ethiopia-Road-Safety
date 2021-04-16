# Trends

crashes_df <- readRDS(file.path(etre_crashes_dir, "FinalData", "crashes.Rds"))

crashes_df <- crashes_df %>%
  mutate(month = accident_date %>% round_date(unit = "month")) %>%
  filter(!is.na(month)) %>%
  filter(month >= as.Date("2010-01-01")) %>%
  group_by(month) %>%
  dplyr::summarise(N = n())

crashes_df %>%
  ggplot() +
  geom_line(aes(x = month, y = N),
            size = 1) +
  labs(x = NULL,
       y = "Number of Crashes",
       title = "Number of Monthly Crashes from 2015 - 2017\non Addis Adama Expressway") +
  theme_ipsum() + 
  ggsave(file.path(etre_crashes_dir, "Outputs",
                   "crashes_monthly.png"),
         height = 4.5, width = 8)




