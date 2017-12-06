library(sergeant)
library(tidyverse)

db <- src_drill("localhost")

write_to_parquet <- function(x, path, db) {
  dir.create(paste0("/data/tmp/", path))
  tmp <- paste0("/data/tmp/", path,"/data.csvh")
  write_csv(x, tmp)
  query <- paste0("CREATE TABLE dfs.tmp.`/parquet/", path, "/data.parquet`  AS SELECT * FROM dfs.tmp.`", paste0("/tmp/", path, "/data.csvh"), "`")
  dbGetQuery(db$con, 
             query
  )
  unlink(tmp)
}

tbl_all_actions <- tbl_all_actions %>% 
  mutate(date = lubridate::date(server_time))

tbl_all_actions %>% 
  nest(-date) %>% 
  rowwise() %>% 
  do({
    write_to_parquet(.$data, paste0("tbl_all_actions/", .$date), db)
  })
