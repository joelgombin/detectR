library(solrium)
library(tidyverse)
solr_connect('http://147.94.102.65:8983/solr/documents/select')

load("../data/frequentation/urls_journals_clean.Rdata")

solr_search(q = 'id:("http://books.openedition.org/efr/3374","http://books.openedition.org/efr/3381")', fl=c("id", "url", "naked_titre"), wt = "csv")

solr_search(q = paste0('id:(', paste0('"', urls_journals_clean$ID[1:100], '"', collapse = ","), ")"), fl=c("id", "url", "naked_titre"), wt = "csv", rows = 100)

test <- map2_df(seq(1, length(urls_journals_clean$ID), by = 100), seq(100, length(urls_journals_clean$ID)+100, by = 100), ~ solr_search(q = paste0('id:(', paste0('"', urls_journals_clean$ID[.x:.y], '"', collapse = ","), ")"), fl=c("id", "url", "naked_titre"), wt = "csv", rows = 200))

library(httr)
test2 <- GET(url = "http://147.94.102.65:8983/solr/documents/select",
    query = paste0("id:(", paste0('"', urls_journals_clean$ID, '"', collapse = ","), ")"),
    limit = 5000, 
    fields = c("id", "url", "naked_titre"))
## error 414

test3 <- POST(url = "http://147.94.102.65:8983/solr/documents/select", 
              body = list(query = paste0("id:(", paste0('"', urls_journals_clean$ID, '"', collapse = ","), ")"),
                          limit = 5000, 
                          fields = c("id", "url", "naked_titre")),
              encode = "json"
)
# error 400

POST(url = "http://147.94.102.65:8983/solr/documents/select", 
     body = list(query = paste0("id:(", paste0('"', c("http://iqbal.hypotheses.org/111","http://iqbal.hypotheses.org/3578"), '"', collapse = ","), ")"),
                 limit = 5000, 
                 params = list(wt = "csv"),
                 fields = c("id", "url", "naked_titre")),
     encode = "json"
) %>% 
  content("text") %>% 
  read_csv()

# works! but apparently there can't be more than 500 urls at a time

get_metadata_chunk <- function(urls, fields = c("id", "url", "naked_titre"), backend, .pb = NULL) {
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  result <- httr::POST(url = backend,
             body = list(query = paste0("id:(", paste0('"', urls, '"', collapse = ","), ")"),
             params = list(wt = "csv"),
             fields = fields),
     encode = "json"
)
  if (httr::http_error(result)) {
    stop("Erreur ! La première url demandée est ", urls[1], "et la dernière est ", urls[length(urls)])
  }
  result <- result %>% 
    httr::content("text") %>% 
    readr::read_csv()
  result[match(urls, result$id),]
}

get_metadata <- function(urls, fields = c("id", "url", "naked_titre"), sleep_time = 0.05, backend = "http://147.94.102.65:8983/solr/documents/select") {
  urls <- gsub('\"', "", urls)
  pb <- dplyr::progress_estimated(length(urls) %/% 500)
  purrr::map2_df(
    seq(1, length(urls), by = 500), 
    seq(500, length(urls)+500, by = 500),
    function(start, end) {
      Sys.sleep(sleep_time)
      get_metadata_chunk(urls[start:min(length(urls), end)], fields, backend = backend, .pb = pb)
    }
  )
}

library(crul)
conn <- HttpClient$new(
  url = "http://147.94.102.65:8983/solr/documents/select"
)
conn$get("", 
         query = list(query = paste0("id:(", paste0('"', urls_journals_clean$ID, '"', collapse = ","), ")"),
                      fields = c("id", "url", "naked_titre"))
         )
