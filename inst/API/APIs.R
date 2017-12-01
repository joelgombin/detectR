library(tidyverse)

#* @get /get_visits
get_visits <- function(visited_urls = NULL, from = "2017-01-01", to = "2017-08-01") {
  monetdb <- config::get("monetdb", file = "~/detectR/inst/API/config.yml")
  on.exit({
    pool::poolClose(conn)
    MonetDBLite::monetdblite_shutdown()
    })
  conn <- pool::dbPool(drv = eval(parse(text = monetdb$drv)),
                  embedded = monetdb$embedded)

  from <- lubridate::ymd(from)
  to <- lubridate::ymd(to)
  
  tmp <- detectR::get_visits(conn, "visites_par_jour", visited_urls, from, to)
  return(tmp)
}

#* @get /get_outliers
get_outliers <- function(from = "2017-01-01", to = "2017-08-01") {
  monetdb <- config::get("monetdb", file = "~/detectR/inst/API/config.yml")
  on.exit({
    pool::poolClose(conn)
    MonetDBLite::monetdblite_shutdown()
  })
  conn <- pool::dbPool(drv = eval(parse(text = monetdb$drv)),
                 embedded = monetdb$embedded)
  date <- "date"
  request <- glue::glue_sql("SELECT * FROM url_prediction_anomalies WHERE {`date`} >= {from} AND {`date`} <= {to}", .con = conn)
  tmp <- tibble::as_tibble(DBI::dbGetQuery(conn, request))
  return(tmp)
}

#* @assets /data/dashboards /dashboards
get_dashboard <- function(res) {
  list()
}
