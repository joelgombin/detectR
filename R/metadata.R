#' Get the metadata from the solr backends from a selection of urls (by chunk)
#' 
#' @param urls a vector of urls
#' @param fields the fields to get (vector)
#' @param backend the solr backend
#' @param .pb an object to be used as a progress bar
#' 
#' value a tibble, with as many rows as there are elements in `urls`. 
#' 
#' @export
#' @importFrom magrittr %>%
get_metadata_chunk <- function(urls, fields = c("id", "url", "naked_titre"), backend, .pb = NULL) {
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  result <- httr::POST(url = backend,
                       body = list(query = paste0("id:(", paste0('"', urls, '"', collapse = ","), ")"),
                                   params = list(wt = "csv"),
                                   limit = length(urls) +1,
                                   fields = fields),
                       encode = "json"
  )
  if (httr::http_error(result)) {
    stop("Erreur ! La première url demandée est ", urls[1], "et la dernière est ", urls[length(urls)])
  }
  result <- result %>%
    httr::content("text") %>%
    readr::read_csv(col_types = readr::cols(datepubli = readr::col_character(),
                                     datepublipapier = readr::col_character(),
                                     datemisenligne = readr::col_character(),
                                     dateacceslibre = readr::col_character()))
  result[match(urls, result$id),]
}

#' Get the metadata from the solr backends from a selection of urls
#' 
#' @param urls a vector of urls
#' @param fields the fields to get (vector)
#' @param sleep_time how long (in seconds) the function should wait before two API calls. Be gentle to the backend!
#' @param step how many values should be sent to the API at once (an integer). Advised value is 500; choosing a higher value may lead to getting errors 400.
#' @param backend the solr backend
#' 
#' value a tibble, with as many rows as there are elements in `urls`. 
#' 
#' @export
#' @importFrom magrittr %>%
get_metadata <- function(urls, fields = c("id", "url", "naked_titre"), sleep_time = 0.05, step = 500, backend = "http://147.94.102.65:8983/solr/documents/select") {
  urls <- gsub('\\\\', "", urls)
  urls <- gsub('"', "", urls)
  pb <- dplyr::progress_estimated(length(urls) %/% step)
  purrr::map2_df(
    seq(1, length(urls), by = step), 
    seq(step, length(urls)+step, by = step),
    function(start, end) {
      Sys.sleep(sleep_time)
      get_metadata_chunk(urls[start:min(length(urls), end)], fields, backend = backend, .pb = pb)
    }
  ) %>% 
    tibble::as_tibble()
}
