library(shinyWidgets)
library(urltools)
library(solrium)
library(memoise)
library(tidyverse)
solr_connect('http://147.94.102.65:8983/solr/documents/select')

fc <- cache_filesystem("~/.Rcache")
# memoisation of the solr call. Reset it every day.
m_solr_facet <- memoise(solr_facet, cache = fc, ~timeout(60*60*24))
m_solr_search <- memoise(solr_search, cache = fc)

publications <- m_solr_facet(q = "*:*", facet.pivot = c("platform", "site_url", "site_titre"), facet.limit = -1)$facet_pivot$`platform,site_url,site_titre` %>% unnest()

load("./data/anomaly_detection.Rdata")

load_urls <- function() {
  urls <- anomaly_detection$url_prediction_anomalies %>%
    group_by(url) %>%
    arrange(-fit_sigma_ratio) %>%
    slice(1) %>%
    ungroup %>%
    arrange(-fit_sigma_ratio) %>% 
    mutate(url = if_else(stringr::str_detect(url, "http://"), url, paste0("http://", url)))
  map2_df(seq(1, length(urls$url), by = 100), seq(100, length(urls$url)+100, by = 100), ~ m_solr_search(q = paste0('id:(', paste0('"', urls$url[.x:min(.y, length(urls$url))], '"', collapse = ","), ")"), fl=c("id", "platform", "site_url", "naked_titre"), wt = "json", rows = 101))
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
