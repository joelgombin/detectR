#* @get /get_visits
get_visits <- function(visited_urls, from = "2017-01-01", to = "2017-08-01") {
  library(dplyr)
  monetdb <- config::get("monetdb", file = "~/detectR/inst/cron/config.yml")
  conn <- dbPool(drv = eval(parse(text = monetdb$drv)),
                  embedded = monetdb$embedded)
  tbl_all_actions <- tbl(conn2, "all_actions")
  
  from <- lubridate::ymd(from)
  to <- lubridate::ymd(to)
  
  detectR::get_visits(conn, "all_actions", visited_urls, from, to)
}

#* @assets /data/dashboards /dashboards
get_dashboard <- function(res) {
  list()
}
