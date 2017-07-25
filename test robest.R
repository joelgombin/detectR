library(robets)
library(sweep)
library(timekit)
library(tidyverse)
logs <- read_csv2("./data/complete_log_clean.csv")

cluster <- create_cluster(3)

cleaned_logs <- logs %>%
  select(-X1, - year_day) %>%
  complete(url, date, fill = list(log_count = 0)) %>%
  group_by(url) %>%
  filter(mean(log_count) > 2) %>%
  arrange(date) %>%
  nest()

cleaned_logs <- cleaned_logs %>%
  mutate(data.ts = map(.x = data,
                       .f = tk_ts,
                       start = 2017))

models <- cleaned_logs %>%
  mutate(fit = map(data.ts, robets))


augmented_models <- models %>%
  mutate(tidy = map(fit, sw_augment, timekit_idx = TRUE, rename_index = "date")) %>%
  unnest(tidy, .drop = TRUE)
