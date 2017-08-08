library(MonetDBLite)
library(tidyverse)

monetdb_con <- DBI::dbConnect(monetdblite(), "~/data/monetdb")

tbl_all_actions <- tbl(monetdb_con, "all_actions")

stats <- tbl_all_actions %>% 
  filter(!url %in% "http://") %>% 
  distinct(idvisit, url) %>% 
  group_by(url) %>% 
  summarise(n = n()) %>% 
  collect

