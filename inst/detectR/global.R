library(urltools)
library(tidyverse)
livres <- read_csv("http://www.openedition.org/index.html?page=coverage&pubtype=livre&export=csv",
                   col_types = cols(
                     Title = col_character(),
                     `ISBN (print format)` = col_character(),
                     `ISBN (electronic format)` = col_character(),
                     `Electronic Publication Date` = col_date(format = ""),
                     Url = col_character(),
                     Authors = col_character(),
                     Publisher = col_character(),
                     `Print Publication Date (Year)` = col_integer(),
                     Abstract = col_character(),
                     `Access policy` = col_character(),
                     `Main language (ISO 639-1)` = col_character()
                   ))
revues <- read_csv("http://www.openedition.org/index.html?page=coverage&pubtype=revue&export=csv")
carnets <- read_csv("http://www.openedition.org/index.html?page=coverage&pubtype=carnet&export=csv")
livres <- livres %>%
  mutate(Title = stringr::str_replace_all(Title, "'|’", "'"))
revues <- revues %>%
  mutate(Title = stringr::str_replace_all(Title, "'|’", "'"))
carnets <- carnets %>%
  mutate(Title = stringr::str_replace_all(Title, "'|’", "'"))

publications <- bind_rows(hypotheses = carnets %>% select(Title, Url),
                          books = livres %>%  select(Title, Url),
                          revues = revues %>% select(Title, Url), .id = "plateforme") %>%
  mutate(Url = map(Url, url_parse)) %>%
  unnest(Url) %>%
  mutate(url = case_when(plateforme %in% c("carnets", "revues") ~ domain,
                         TRUE ~ paste0(domain, "/", path))) %>%
  select(plateforme, Title, url)

load("./data/anomaly_detection.Rdata")
urls <- anomaly_detection$url_prediction_anomalies %>%
  group_by(url) %>%
  arrange(-fit_sigma_ratio) %>%
  slice(1) %>%
  ungroup %>%
  arrange(-fit_sigma_ratio)

