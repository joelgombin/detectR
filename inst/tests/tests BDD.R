library(pool)
library(MonetDBLite)
library(dplyr)

conn1 <- dbPool(RMariaDB::MariaDB(),
               host = "127.0.0.1",
               user = "rstudio",
               pass = "mae5ooCh",
               dbname = "piwik",
               port = 3307)
conn1bis <- dbPool(RMySQL::MySQL(),
                host = "127.0.0.1",
                user = "rstudio",
                pass = "mae5ooCh",
                dbname = "piwik",
                port = 3307)
conn2 <- dbPool(MonetDBLite::MonetDBLite(),
                embedded = "/data/monetdb/")

# get last records of the existing BD

all_actions <- tbl(conn2, "all_actions")

all_actions %>%
  select(idvisit) %>% 
  arrange(desc(idvisit))

all_actions %>%
  select(idaction_url) %>% 
  arrange(desc(idaction_url))

test <- get_tbl_visits(conn1, from = 32117705)
# fails :  Error in result_fetch(res@ptr, n) : 
# Incorrect key file for table '/tmp/#sql_81a_0.MYI'; try to repair it [126]

test <- get_tbl_visits(conn1bis, from = 32117705)
# outputs a 0-row tibble

conn <- poolCheckout(conn1bis)
test <- dbGetQuery(conn, "SELECT  `idvisit` AS `idvisit`, `visit_last_action_time` AS `visit_last_action_time`, `visit_first_action_time` AS `visit_first_action_time`, `visit_total_actions` AS `visit_total_actions`, `referer_type` AS `referer_type`, `referer_name` AS `referer_name`, `referer_url` AS `referer_url`, `referer_keyword` AS `referer_keyword`, `location_browser_lang` AS `location_browser_lang`, `config_browser_name` AS `config_browser_name`, `config_device_type` AS `config_device_type`, `config_os` AS `config_os`, `config_os_version` AS `config_os_version`, `visitor_localtime` AS `visitor_localtime`, `config_resolution` AS `config_resolution`, `visit_total_time` AS `visit_total_time`, `location_city` AS `location_city`, `location_country` AS `location_country` FROM `log_visit` WHERE (`idvisit` > 32117705)")
# works \o/
poolReturn(conn)

# post rewriting
test <- get_tbl_visits(conn1, from = 32117705)
# \o/ works!

actions <- get_tbl_actions(conn1, from = 4013924)
# \o/ works!

get_tbl_visit_actions(conn1, 53351474)
# \o/ works!

extract_and_load(conn1, conn2, from = 34127327)
# \o/ works!
