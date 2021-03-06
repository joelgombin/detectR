#library(RMySQL)
library(RMariaDB)
library(dbplyr)
library(pool)
library(MonetDBLite)
library(lubridate)
library(detectR)
library(tidyverse)


# conn <- DBI::dbConnect(MySQL(),
#                        host = "127.0.0.1",
#                        user = "rstudio",
#                        pass = "mae5ooCh",
#                        dbname = "piwik",
#                        port = 3307)
# conn <- DBI::dbConnect(MariaDB(),
#                        host = "127.0.0.1",
#                        user = "rstudio",
#                        pass = "mae5ooCh",
#                        dbname = "piwik",
#                        port = 3307)
conn <- dbPool(MariaDB(),
               host = "127.0.0.1",
               user = "rstudio",
               pass = "mae5ooCh",
               dbname = "piwik",
               port = 3307)
DBI::dbListTables(conn)

monetdb_con <- dbPool(monetdblite(), embedded = "~/data/monetdb_test/")


log_visit <- tbl(conn, "log_visit")
log_action <- tbl(conn, "log_action")
log_link_visit_action <- tbl(conn, "log_link_visit_action")


visites <- log_visit %>% 
  select(idvisit, visit_last_action_time, visit_first_action_time, visit_total_actions, referer_type, referer_name, referer_url, referer_keyword, location_browser_lang, config_browser_name, config_device_type, config_os, config_os_version, visitor_localtime, config_resolution, visit_total_time, location_city, location_country) %>% 
  collect()

DBI::dbWriteTable(monetdb_con, "visites", visites)

tbl_visites <- tbl(monetdb_con, "visites")

visites_actions <- log_link_visit_action %>% 
  select(idlink_va, idvisit, idaction_url, server_time) %>%
  collect()

DBI::dbWriteTable(monetdb_con, "visites_actions", visites_actions)

tbl_visites_actions <- tbl(monetdb_con, "visites_actions")

actions <- log_action %>% 
  filter(type %in% 1) %>% 
  collect()

DBI::dbWriteTable(monetdb_con, "actions", actions)

tbl_actions <- tbl(monetdb_con, "actions")


all_actions <- tbl_visites %>% 
  left_join(
    tbl_visites_actions,
    by = c("idvisit")
  ) %>% 
  left_join(
    tbl_actions, 
    by = c("idaction_url" = "idaction")
  )

all_actions <- all_actions %>% 
  collect


DBI::dbWriteTable(monetdb_con, "all_actions", all_actions)

tbl_all_actions <- tbl(monetdb_con, "all_actions")

tbl_all_actions <- tbl_all_actions %>% 
  collect()

library(rex)

rex_mode()

re_domaines <- rex(start,
                   group(
                     maybe(zero_or_more(number),
                           "-")
                     ),
                 capture(
                   group(zero_or_more(alnum),
                     group(maybe(dot),
                      "revues.org")
                   ) %or% 
                   group("books.openedition.org") %or%
                   group("calenda.org") %or%
                   group(
                     group(zero_or_more(alnum),
                         maybe(dot)),
                     "hypotheses.org")
                 ),
                 anything,
                 end)



library(urltools)
# `mydomain<-` <- Vectorize(`domain<-`)  
urls <- tbl_all_actions %>% 
  pull(name) %>% 
  url_parse %>% 
  mutate(scheme = "http") %>% 
  mutate(parameter = NA) %>% 
  mutate(fragment = NA) %>% 
  mutate(port = NA) %>% 
  mutate(domain = re_substitutes(domain, re_domaines, "\\1"))

tbl_all_actions$domain <- urls$domain

# tbl_all_actions$url <- urls %>% 
#   url_compose() %>% 
#   stringr::str_replace_all(" ", "")


  
# tbl_all_actions$url <- urls$value

tbl_all_actions <- tbl_all_actions %>%
  mutate(date = floor_date(ymd_hms(visit_last_action_time), "day"))

DBI::dbWriteTable(monetdb_con, "all_actions", tbl_all_actions, overwrite = TRUE)

# test agrégation

unique_valid_urls <- tbl_all_actions %>% 
  distinct(url) %>% 
  pull(url) %>% 
  validate_url()

toutes_visites <- get_visits(tbl_all_actions, unique_valid_urls, from = lubridate::ymd("2017-01-01"))

request <- paste0('SELECT idvisit, url, date FROM all_actions WHERE url IN (\'', paste0(unique_valid_urls[1:10], collapse = "\', \'"), '\') AND "date" >= \'', ymd("2017-01-01"), '\' AND "date" <= \'', Sys.Date(), '\'')

test <- dbGetQuery(monetdb_con, request)


toutes_visites <- get_visits(monetdb_con, from = lubridate::ymd("2017-01-01"), to = lubridate::ymd("2017-08-04"))

dbWriteTable(monetdb_con, toutes_visites, "visites_par_jour", overwrite = TRUE)

# tests préparation données pour modélisation

toutes_visites <- tbl(monetdb_con, "visites_par_jour") %>% 
  collect()

assez_visites <- toutes_visites  %>% 
  group_by(url) %>% 
  filter(n() > 10)

assez_visites_expanded <- assez_visites %>% 
  complete(date, url, fill = list(n = 0))

aggregated_visites <- get_aggregated_visites(assez_visites)

visites_ts <- get_calendar_time_series(aggregated_visites)

aggregated_visites$aggregated_frequent <- correct_time_series(aggregated_visites$aggregated_frequent, visites_ts)

anomaly_detection <- detection_anomalies_rcs(aggregated_visites$aggregated_frequent) # compter une quizaine de minutes d'exécution. Va aller croissant à mesure que le volume de données va augmenter, mais probablement moins que liénairement (car probablement plus impacté par nombre d'urls que nb de jours pour chaque url)

copy_to(monetdb_con, anomaly_detection$url_prediction_anomalies, name = "url_prediction_anomalies", overwrite = TRUE, temporary = FALSE)
