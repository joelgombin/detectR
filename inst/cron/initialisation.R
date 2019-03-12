library(pool)
library(tidyverse)
library(detectR)


mysql <- config::get("mysql", file = "~/detectR/inst/cron/config.yml")
monetdb <- config::get("monetdb", file = "~/detectR/inst/cron/config.yml")

conn1 <- dbPool(drv = eval(parse(text = mysql$drv)),
                host = mysql$host,
                user = mysql$user,
                pass = mysql$pass,
                dbname = mysql$dbname,
                port = mysql$port)
conn2 <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "/data/monetdb/")

from <- lubridate::ymd("2017-01-01")
to <- lubridate::ymd("2017-02-28")

extract_and_load(conn1, conn2, from = from, to = to, progress = TRUE, append = FALSE)

from <- lubridate::ymd("2017-02-28")
to <- lubridate::ymd("2017-03-01")

extract_and_load(conn1, conn2, from = from, to = to, progress = TRUE)


from <- lubridate::ymd("2017-03-01")
to <- lubridate::ymd("2017-05-30")

extract_and_load(conn1, conn2, from = from, to = to, progress = TRUE)

from <- lubridate::ymd("2017-05-30")
to <- lubridate::ymd("2017-05-31")

extract_and_load(conn1, conn2, from = from, to = to, progress = TRUE)


from <- lubridate::ymd("2017-05-31")
to <- lubridate::ymd("2017-07-31")

extract_and_load(conn1, conn2, from = from, to = to, progress = TRUE, forbid_duplicate = FALSE)

from <- lubridate::ymd("2017-07-31")
to <- lubridate::ymd("2017-09-30")

extract_and_load(conn1, conn2, from = from, to = to, progress = TRUE)

from <- lubridate::ymd("2017-09-30")
to <- lubridate::ymd("2017-12-01")

extract_and_load(conn1, conn2, from = from, to = to, progress = TRUE)

from <- lubridate::ymd("2017-12-01")
to <- lubridate::ymd("2018-01-03")

extract_and_load(conn1, conn2, from = from, to = to, progress = TRUE)


from <- lubridate::ymd("2017-01-01")
to <- lubridate::ymd("2017-02-28")

toutes_visites <- get_visits(conn2, table = "all_actions", from = from, to = to)
dbWriteTable(conn2, "visites_par_jour", toutes_visites, append = TRUE)

from <- lubridate::ymd("2017-03-01")
to <- lubridate::ymd("2018-01-03")

toutes_visites <- get_visits(conn2, table = "all_actions", from = from, to = to)
dbWriteTable(conn2, "visites_par_jour", toutes_visites, append = TRUE)


assez_visites <- tbl(conn2, "visites_par_jour") %>% 
  group_by(url) %>% 
  collect %>% 
  filter(n() > 10) %>% 
  ungroup()

# assez_visites <- toutes_visites  %>% 
#   group_by(url) %>% 
#   filter(n() > 10)

if (nrow(assez_visites) > 0) {
  assez_visites_expanded <- assez_visites %>% 
    mutate(date = as.numeric(date)) %>% 
    tidyr::complete(date, url, fill = list(n = 0)) %>% 
    mutate(date = as.Date(as.POSIXct(date, origin = "1970-01-01 00:00.00 UT")))
  
  aggregated_visites <- get_aggregated_visites(assez_visites)
  
  visites_ts <- get_calendar_time_series(aggregated_visites)
  
  aggregated_visites$aggregated_frequent <- correct_time_series(aggregated_visites$aggregated_frequent, visites_ts)
  
  anomaly_detection <- detection_anomalies_rcs(aggregated_visites$aggregated_frequent) # compter une quizaine de minutes d'exécution. Va aller croissant à mesure que le volume de données va augmenter, mais probablement moins que liénairement (car probablement plus impacté par nombre d'urls que nb de jours pour chaque url)
  
  # on stocke les données. En l'état des choses, on réévalue tout tous les jours, donc on écrase la table
  dbWriteTable(conn2, "url_prediction_anomalies", anomaly_detection$url_prediction_anomalies, overwrite = TRUE)
  dbWriteTable(conn2, "url_prediction", anomaly_detection$url_prediction, overwrite = TRUE)
  dbWriteTable(conn2, "url_prediction_count", anomaly_detection$url_prediction_count, overwrite = TRUE)
}


poolClose(conn1)
poolClose(conn2)
monetdblite_shutdown()

## sauvergarde en CSV

for (i in dplyr::db_list_tables(conn2)) {
  export_csv(conn2, i, output_path = glue::glue("/data/detectR/dumps/{i}.csv"))
}
