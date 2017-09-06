# for debugging

message(Sys.time(), "\n")

suppressPackageStartupMessages(library(pool))
suppressPackageStartupMessages(library(MonetDBLite))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(detectR))

mysql <- config::get("mysql", file = "~/detectR/inst/cron/config.yml")
monetdb <- config::get("monetdb", file = "~/detectR/inst/cron/config.yml")

conn1 <- dbPool(drv = eval(parse(text = mysql$drv)),
                host = mysql$host,
                user = mysql$user,
                pass = mysql$pass,
                dbname = mysql$dbname,
                port = mysql$port)
conn2 <- dbPool(drv = eval(parse(text = monetdb$drv)),
                embedded = monetdb$embedded)

all_actions <- tbl(conn2, "all_actions")

from <- all_actions %>%
  select(idvisit) %>% 
  arrange(desc(idvisit)) %>% 
  collect(n = 1) %>% 
  pull(idvisit) + 1

extract_and_load(conn1, conn2, from = from, progress = FALSE)

from <- tbl(conn2, "visites_par_jour") %>% 
  summarise(from = max(date)) %>% 
  pull(from)

toutes_visites <- get_visits(conn2, from = from + lubridate::ddays(1), to = lubridate::ymd(Sys.Date()) - lubridate::ddays(1))

dbWriteTable(conn2, "visites_par_jour", toutes_visites, append = TRUE)

poolClose(conn1)
poolClose(conn2)
message(Sys.time(), "\n")
