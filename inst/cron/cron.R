# for debugging

message("Starting daily update at ", Sys.time(), "\n")

suppressPackageStartupMessages(library(pool))
suppressPackageStartupMessages(library(MonetDBLite))
suppressPackageStartupMessages(library(detectR))
suppressPackageStartupMessages(library(attempt))
suppressPackageStartupMessages(library(tidyverse))


mysql <- config::get("mysql", file = "~/detectR/inst/cron/config.yml")
monetdb <- config::get("monetdb", file = "~/detectR/inst/cron/config.yml")

conn1 <- dbPool(drv = eval(parse(text = mysql$drv)),
                host = mysql$host,
                user = mysql$user,
                pass = mysql$pass,
                dbname = mysql$dbname,
                port = mysql$port)

# tester que la connexion est saine. Sinon - réinitialiser.
conn2 <- attempt({DBI::dbConnect(MonetDBLite::MonetDBLite(), "/data/monetdb/")}, 
                 msg = "Erreur de connexion à la base de données MonetDBLite")

if ("try-error" %in% class(conn2)) {
  warning("Procédure de récupération des sauvegardes CSV\n")
  message("Suppression de la base existante")
  unlink("/data/monetdb/", recursive = TRUE)
  message("Création du dossier")
  dir.create("/data/monetdb")
    conn2 <- DBI::dbConnect(MonetDBLite::MonetDBLite(), "/data/monetdb/")
  walk(list.files("/data/detectR/dumps/"), 
       function(file) {
         monetdb.read.csv(conn2, paste0("/data/detectR/dumps/", file), tablename = gsub(".csv", "", file), locked = TRUE, nrow.check = 5000)
         tmp <- data.table::fread(paste0("/data/detectR/dumps/", file))
        dbWriteTable(conn2, gsub(".csv", "", file), tmp)
      })
}



if ("all_actions" %in% db_list_tables(conn2)) {
  all_actions <- tbl(conn2, "all_actions")
  
  from <- all_actions %>%
    select(visit_first_action_time) %>% 
    arrange(desc(visit_first_action_time)) %>% 
    collect(n = 5) %>% 
    mutate(date = as.Date(visit_first_action_time)) %>% 
    arrange(date) %>% 
    slice(1) %>% 
    pull(date) + 1
} else {
  from <- "2017-01-01"
}

extract_and_load(conn1, conn2, from = from, forbid_duplicate = FALSE, progress = FALSE)

from <- tbl(conn2, "visites_par_jour") %>% 
  summarise(from = max(date)) %>% 
  pull(from)

toutes_visites <- get_visits(conn2, table = "all_actions", from = from + lubridate::ddays(1), to = lubridate::ymd(Sys.Date()) - lubridate::ddays(1))

dbWriteTable(conn2, "visites_par_jour", toutes_visites, append = TRUE)

# run the model to extract outliers

assez_visites <- tbl(conn2, "visites_par_jour") %>% 
  collect %>% 
  group_by(url) %>% 
  filter(n() > 30)

if (nrow(assez_visites) > 0) {
  assez_visites_expanded <- assez_visites %>% 
    mutate(date = as.numeric(date)) %>% 
    tidyr::complete(date, url, fill = list(n = 0)) %>% 
    mutate(date = as.Date(as.POSIXct(date, origin = "1970-01-01 00:00.00 UT")))
  
  aggregated_visites <- get_aggregated_visites(assez_visites, absolute_days = 30, relative_days = 0.5)
  
  visites_ts <- get_calendar_time_series(aggregated_visites)
  
  aggregated_visites$aggregated_frequent <- correct_time_series(aggregated_visites$aggregated_frequent, visites_ts)
  
  anomaly_detection <- detection_anomalies_rcs(aggregated_visites$aggregated_frequent) # compter une quizaine de minutes d'exécution. Va aller croissant à mesure que le volume de données va augmenter, mais probablement moins que liénairement (car probablement plus impacté par nombre d'urls que nb de jours pour chaque url)
  
  # on stocke les données. En l'état des choses, on réévalue tout tous les jours, donc on écrase la table
  dbWriteTable(conn2, "url_prediction_anomalies", anomaly_detection$url_prediction_anomalies, overwrite = TRUE)
  dbWriteTable(conn2, "url_prediction", anomaly_detection$url_prediction, overwrite = TRUE)
  dbWriteTable(conn2, "url_prediction_count", anomaly_detection$url_prediction_count, overwrite = TRUE)
}

# faire une copie de sauvegarde de la bdd

for (i in dplyr::db_list_tables(conn2)) {
  export_csv(conn2, i, output_path = glue::glue("/data/detectR/dumps/{i}.csv"), chunk_length = 500000)
  gc()
}

poolClose(conn1)
DBI::dbDisconnect(conn2)
monetdblite_shutdown()
message(Sys.time(), "\n")
