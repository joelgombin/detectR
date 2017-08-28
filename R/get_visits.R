#' Get number of visits for a given URL
#' @importFrom magrittr %>% 
#' @export
#' @param backend table (usually a SQL table) friom which the data is agregated 
#' @param visited_urls a vector of urls 
get_visits <- function(backend, visited_urls = NULL, from = Sys.Date(), to = Sys.Date()) {
  tmp_df <- backend %>% 
    select(idvisit, url, date)
  if (!is.null(visited_urls)) {
    tmp_df <- tmp_df %>% 
      filter(url %in% visited_urls)
  } 
  tmp_df %>% 
    filter(url != "http://") %>% 
    filter(!is.na(url)) %>% 
    filter(date >= from,
           date <= to) %>% 
    collect %>% 
    group_by(url) %>% 
    distinct(idvisit, .keep_all = TRUE) %>% 
    group_by(date, url) %>% 
    summarise(n = n()) %>% 
    ungroup
}
