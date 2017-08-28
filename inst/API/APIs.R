#* @get /get_visits
get_visits <- function(visited_urls, from = "2017-01-01", to = "2017-08-01") {
  library(dplyr)
  monetdb_con <- DBI::dbConnect(MonetDBLite::monetdblite(), "/data/monetdb")
  tbl_all_actions <- tbl(monetdb_con, "all_actions")
  
  from <- lubridate::ymd(from)
  to <- lubridate::ymd(to)
  
  detectR::get_visits(tbl_all_actions, visited_urls, from, to)
}

#* @assets /data/dashboards /dashboards
get_dashboard <- function(res) {
  list()
}
