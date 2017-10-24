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

# run the model to extract outliers

toutes_visites <- tbl(conn2, "visites_par_jour") %>% 
  collect %>% 
  group_by(url) %>% 
  filter(n() > 10)

assez_visites <- toutes_visites  %>% 
  group_by(url) %>% 
  filter(n() > 10)

if (nrow(assez_visites) > 0) {
  assez_visites_expanded <- assez_visites %>% 
    tidyr::complete(date, url, fill = list(n = 0))
  
  aggregated_visites <- get_aggregated_visites(assez_visites)
  
  visites_ts <- get_calendar_time_series(aggregated_visites)
  
  aggregated_visites$aggregated_frequent <- correct_time_series(aggregated_visites$aggregated_frequent, visites_ts)
  
  anomaly_detection <- detection_anomalies_rcs(aggregated_visites$aggregated_frequent) # compter une quizaine de minutes d'exécution. Va aller croissant à mesure que le volume de données va augmenter, mais probablement moins que liénairement (car probablement plus impacté par nombre d'urls que nb de jours pour chaque url)
  
  # on stocke les données. En l'état des choses, on réévalue tout tous les jours, donc on écrase la table
  copy_to(conn2, anomaly_detection$url_prediction_anomalies, name = "url_prediction_anomalies", overwrite = TRUE, temporary = FALSE)
  copy_to(conn2, anomaly_detection$url_prediction, name = "url_prediction", overwrite = TRUE, temporary = FALSE)
  copy_to(conn2, anomaly_detection$url_prediction_count, name = "url_prediction_count", overwrite = TRUE, temporary = FALSE)
}

# faire une copie de sauvegarde de la bdd

file.copy(monetdb$embedded, "/data/monetdb_backup/", recursive = TRUE, overwrite = TRUE)

poolClose(conn1)
poolClose(conn2)
message(Sys.time(), "\n")
