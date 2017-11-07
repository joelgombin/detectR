library(tidyverse)

#* @get /get_visits
get_visits <- function(visited_urls = NULL, from = "2017-01-01", to = "2017-08-01") {
  monetdb <- config::get("monetdb", file = "~/detectR/inst/cron/config.yml")
  on.exit({pool::poolClose(conn)})
  conn <- pool::dbPool(drv = eval(parse(text = monetdb$drv)),
                  embedded = monetdb$embedded)
  tbl_all_actions <- dplyr::tbl(conn, "all_actions")
  
  from <- lubridate::ymd(from)
  to <- lubridate::ymd(to)
  
  tmp <- detectR::get_visits(conn, "all_actions", visited_urls, from, to)
  return(tmp)
}

#* @get /get_outliers
get_outliers <- function(from = "2017-01-01", to = "2017-08-01") {
  monetdb <- config::get("monetdb", file = "~/detectR/inst/cron/config.yml")
  on.exit({pool::poolClose(conn)})
  conn <- pool::dbPool(drv = eval(parse(text = monetdb$drv)),
                 embedded = monetdb$embedded)
  
  tmp <- dplyr::tbl(conn, "url_prediction_anomalies") %>% collect()
  return(tmp)
}

#* @assets /data/dashboards /dashboards
get_dashboard <- function(res) {
  list()
}
