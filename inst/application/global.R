library(shiny)
library(shinyjs)
library(shinyWidgets)
library(urltools)
library(solrium)
library(memoise)
library(detectR)
library(lubridate)
library(future)
plan(multiprocess)
library(tidyverse)
solr_connect('http://147.94.102.65:8983/solr/documents/select')

enableBookmarking(store = "server")

fc <- cache_filesystem("~/.Rcache")
# memoisation of the solr call. Reset it every day.
m_solr_facet <- memoise(solr_facet, cache = fc, ~timeout(60*60*24))
m_get_metadata <- memoise(get_metadata, cache = fc)

publications <- m_solr_facet(q = "*:*", facet.pivot = c("platform", "site_url", "site_titre"), facet.limit = -1)$facet_pivot$`platform,site_url,site_titre` %>% unnest()

# lauching APIs - asynchronously!
library(plumber)
r <- plumber::plumb(system.file("API/APIs.R", package = "detectR"))
zzz %<-% {r$run(port = 6666)}


# load("./data/anomaly_detection.Rdata")
# anomaly_detection$url_prediction_anomalies <- anomaly_detection$url_prediction_anomalies %>% 
#   mutate(url = paste0("http://", url))

# anomalies <- read_csv("/data/main_anomalies.csv")
# scheme(anomalies$url) <- "http"

anomalies <- httr::GET(paste0("http://localhost:6666/get_outliers?from=", "2017-01-01", "&to=", Sys.Date())) %>% content(as = "parsed")



load_urls <- function() {
  urls <- anomalies %>%
    group_by(url) %>%
    arrange(-fit_sigma_ratio) %>%
    slice(1) %>%
    ungroup %>%
    arrange(-fit_sigma_ratio) %>% 
    mutate(url = if_else(stringr::str_detect(url, "http://"), url, paste0("http://", url)))
  m_get_metadata(urls = urls$url, 
                 fields = c("id", "platform", "site_url", "naked_titre"),
                 sleep_time = 0.01)
}

m_load_urls <- memoise(load_urls, cache = fc, ~timeout(60*60*24))

urls <- m_load_urls()

publis <- list(
  "Hypothèses" = publications %>% 
    filter(platform %in% "Hypotheses.org") %>% 
    pull(site_url),
  "Revues" = publications %>% 
    filter(platform %in% "Revues.org") %>% 
    pull(site_url),
  "Books" = publications %>% 
    filter(platform %in% "OpenEdition Books") %>% 
    pull(site_url)
)

names(publis$Hypothèses) <- publications %>% 
  filter(platform %in% "Hypotheses.org") %>% 
  pull(site_titre)
names(publis$Revues) <- publications %>% 
  filter(platform %in% "Revues.org") %>% 
  pull(site_titre)
names(publis$Books) <- publications %>% 
  filter(platform %in% "OpenEdition Books") %>% 
  pull(site_titre)


## logs  
# library(DBI)
# library(MonetDBLite)
# monetdb_con <- dbConnect(MonetDBLite::monetdblite(), "/data/monetdb")
# tbl_all_actions <- tbl(monetdb_con, "all_actions")

# rendering
m_get_visits <- memoise(get_visits, cache = fc)
m_render <- memoise(rmarkdown::render, cache = fc)


