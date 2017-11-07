#' Get number of visits for a given URL
#' @importFrom magrittr %>% 
#' @export
#' @param conn a connection to the database from which the data is retrieved
#' @param table the table name from which the data is agregated 
#' @param visited_urls a vector of urls 
#' @details This function only retains one hit per visit (in the meaning of piwik). Only non-zero values are kept; in order to show zero values, please use `tidyr::complete`.
get_visits <- function(conn, table = "all_actions", visited_urls = NULL, from = "2017-01-01", to = Sys.Date()) {
  
  date <- "date"
  from <- as.character(from)
  to <- as.character(to)
  
  if (is.null(visited_urls)) {
    request <- glue::glue_sql("SELECT idvisit, url, date FROM {`table`} WHERE {`date`} >= {from} AND {`date`} <= {to}", .con = conn)
  } else {
    request <- glue::glue_sql("SELECT idvisit, url, date FROM {`table`} WHERE url IN ({visited_urls*}) AND {`date`} >= {from} AND {`date`} <= {to}", .con = conn)
      
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
