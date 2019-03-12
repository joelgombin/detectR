#' Get number of visits for a given URL
#' @importFrom magrittr %>% 
#' @export
#' @param conn a connection to the database from which the data is retrieved
#' @param table the table name from which the data is agregated 
#' @param visited_urls a vector of urls 
#' @details This function only retains one hit per visit (in the meaning of piwik). Only non-zero values are kept; in order to show zero values, please use `tidyr::complete`.
get_visits <- function(conn, table = "visites_par_jour", visited_urls = NULL, from = "2017-01-01", to = Sys.Date()) {
  
  idvisit <- "idvisit"
  date <- "date"
  url <- "url"
  from <- as.character(from)
  to <- as.character(to)
  
  if (is.null(visited_urls)) {
    request <- glue::glue_sql("SELECT {`date`},{`url`}, COUNT(*) AS n 
                               FROM (SELECT {`idvisit`}, {`date`}, {`url`}
                               FROM {`table`} WHERE {`date`} >= {from} AND {`date`} <= {to} 
                               GROUP BY {`idvisit`},{`date`},{`url`}) tmptable
                               GROUP BY {`date`},{`url`}", .con = conn)
  } else {
    request <- glue::glue_sql("SELECT {`date`},{`url`}, COUNT(*) AS n 
                               FROM (SELECT {`idvisit`}, {`date`}, {`url`}
                               FROM {`table`} WHERE url IN ({visited_urls*}) AND {`date`} >= {from} AND {`date`} <= {to} 
                               GROUP BY {`idvisit`},{`date`},{`url`}) tmptable
                               GROUP BY {`date`},{`url`}", .con = conn)
      
  }
  
#  cat(request) for debug
  
  tmp_df <- DBI::dbGetQuery(conn, request) %>% tibble::as_tibble()
  
  tmp_df <- tmp_df %>% 
    dplyr::mutate(url = correct_urls(url)) %>% 
    filter(!is.na(url)) %>% 
    dplyr::group_by(date, url) %>% 
    summarise(n = sum(n)) 

    
  return(tmp_df)
}
