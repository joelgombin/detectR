library(RMySQL)
library(dbplyr)
library(MonetDBLite)
library(lubridate)
library(tidyverse)


conn <- DBI::dbConnect(MySQL(),
                       host = "127.0.0.1",
                       user = "rstudio",
                       pass = "mae5ooCh",
                       dbname = "piwik",
                       port = 3307)
DBI::dbListTables(conn)

monetdb_con <- DBI::dbConnect(monetdblite(), "~/data/monetdb")


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
                 capture(
                   group(zero_or_more(alnum),
                   group(maybe(dot),
                  "revues.org")) %or% 
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
`mydomain<-` <- Vectorize(`domain<-`)  
urls <- tbl_all_actions %>% 
  pull(name) %>% 
  url_parse %>% 
  mutate(scheme = "http") %>% 
  mutate(parameter = NA) %>% 
  mutate(fragment = NA) %>% 
  mutate(port = NA) %>% 
  mutate(domain = re_substitutes(domain, re_domaines)) %>%
  url_compose() %>% 
  stringr::str_replace_all(value, " ", "") %>% 
  as_tibble
  
tbl_all_actions$url <- urls$value

tbl_all_actions <- tbl_all_actions %>%
  mutate(date = floor_date(ymd_hms(visit_last_action_time), "day"))

DBI::dbWriteTable(monetdb_con, "all_actions", tbl_all_actions, overwrite = TRUE)
