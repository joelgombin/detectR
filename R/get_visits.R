#' Get number of visits for a given URL
#' @importFrom magrittr %>% 
#' @export
#' @param conn a connection to the database from which the data is retrieved
#' @param table the table name from which the data is agregated 
#' @param visited_urls a vector of urls 
#' @details This function only retains one hit per visit (in the meaning of piwik). Only non-zero values are kept; in order to show zero values, please use `tidyr::complete`.
get_visits <- function(conn, table = "all_actions", visited_urls = NULL, from = Sys.Date(), to = Sys.Date()) {
  
  if (is.null(visited_urls)) {
    request <- paste0('SELECT idvisit, url, date FROM ', table,' WHERE "date" >= \'', lubridate::ymd("2017-01-01"), '\' AND "date" <= \'', Sys.Date(), '\'')
  } else {
    request <- paste0('SELECT idvisit, url, date FROM ', table, ' WHERE url IN (\'', paste0(visited_urls, collapse = "\', \'"), '\') AND "date" >= \'', lubridate::ymd("2017-01-01"), '\' AND "date" <= \'', Sys.Date(), '\'')
  }
  
#  cat(request) for debug
  
  tmp_df <- DBI::dbGetQuery(conn, request)
  
  tmp_df %>% 
    dplyr::filter(validate_url(url, logical = TRUE)) %>% 
    dplyr::filter(date >= from, date <= to) %>% 
    dplyr::group_by(url) %>% 
    dplyr::distinct(idvisit, .keep_all = TRUE) %>% 
    dplyr::group_by(date, url) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::ungroup()
    
}
