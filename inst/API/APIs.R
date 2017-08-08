#* @get /get_visits
get_visits <- function(visited_url, from = "2017-01-01", to = "2017-08-01") {
  library(dplyr)
  monetdb_con <- DBI::dbConnect(MonetDBLite::monetdblite(), "/data/monetdb")
  tbl_all_actions <- tbl(monetdb_con, "all_actions")
  
  from <- lubridate::ymd(from)
  to <- lubridate::ymd(to)
  
  detectR::get_visits(visited_url, from, to, tbl_all_actions)
}

#* @assets /data/dashboards /dashboards
get_dashboard <- function(res) {
  list()
}
