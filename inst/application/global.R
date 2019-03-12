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
library(stringr)
library(tidyverse)
solr_conn <- SolrClient$new(host = '147.94.102.65', path = 'solr/documents/select', port = '8983')

# enableBookmarking(store = "server") # pour l'url bookmarking

fc <- cache_filesystem("~/.Rcache")
# memoisation of the solr call. Reset it every day.
m_solr_facet <- memoise(solr_facet, cache = fc, ~timeout(60*60*24))  # attention, mettre à jour avec nouvelle version solr
m_get_metadata <- memoise(get_metadata, cache = fc)

publications <- m_solr_facet(conn = solr_conn, params = list(q = "*:*"), facet.pivot = "platform,site_url,site_titre", facet.limit = -1)$facet_pivot$`platform,site_url,site_titre` %>% unnest()

publications <- publications %>% 
  mutate(site_url = fix_revues_org(site_url)) # migration revues.org => journals.openedition.org

anomalies <- httr::GET(paste0("http://localhost:6666/get_outliers?from=", "2017-01-01", "&to=", Sys.Date())) %>% 
  httr::content(as = "text") %>% 
  jsonlite::fromJSON(simplifyDataFrame=TRUE) %>% 
  as_tibble() %>% 
  mutate(url = fix_revues_org(url))




load_urls <- function() {
  urls <- anomalies %>%
    group_by(url) %>%
    arrange(desc(value)) %>%
    slice(1) %>%
    ungroup %>%
    arrange(desc(value)) %>% 
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
  "OpenEdition Journals" = publications %>% 
    filter(platform %in% "OpenEdition Journals") %>% 
    pull(site_url),
  "Books" = publications %>% 
    filter(platform %in% "OpenEdition Books") %>% 
    pull(site_url)
)

names(publis$Hypothèses) <- publications %>% 
  filter(platform %in% "Hypotheses.org") %>% 
  pull(site_titre)
names(publis$`OpenEdition Journals`) <- publications %>% 
  filter(platform %in% "OpenEdition Journals") %>% 
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


