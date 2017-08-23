library(MonetDBLite)
library(lubridate)
library(hrbrthemes)
library(tidyverse)
library(svglite)

monetdb_con <- DBI::dbConnect(monetdblite(), "~/data/monetdb")

tbl_all_actions <- tbl(monetdb_con, "all_actions")

times <- tbl_all_actions %>% 
  select(visit_last_action_time) %>% 
  collect

visits_per_day <- times %>% 
  group_by(day = floor_date(ymd_hms(visit_last_action_time), "day")) %>% 
  summarise(n = n())

gg <- visits_per_day %>% 
  ggplot(aes(x = day, y = n)) + 
  geom_line()

gg <- gg +
  theme_ipsum() +
  labs(x = "", y = "Nombre de visites",
       title = "si on regarde les visites brutes, il y a bien un pic en février")

cairo_pdf("./img/visites.pdf", width = 10, height = 7)
gg
dev.off()

### une seule visite par page par visite

visites_uniques <- tbl_all_actions %>% 
  select(idvisit, visit_last_action_time, name) %>% 
  collect %>% 
  distinct(idvisit, name, .keep_all = TRUE) 
  
gg_visites_uniques <- visites_uniques %>% 
  group_by(day = floor_date(ymd_hms(visit_last_action_time), "day")) %>% 
  summarise(n = n())
  
cairo_pdf("./img/visites_uniques.pdf", width = 10, height = 7)
gg_visites_uniques %>% 
  ggplot(aes(x = day, y = n)) +
  geom_line() +
  theme_ipsum() +
  labs(x = "", y = "Nombre de visites uniques",
       title = "Si on regarde les visites uniques, pas de pic en février.") 
dev.off()
  

# nombre de visites par url par visite

left_join(visits_per_day, gg_visites_uniques, by = "day") %>% 
  mutate(r = n.x / n.y) %>% 
  arrange(desc(r))

cairo_pdf("./img/ratio.pdf", width = 10, height = 7)
left_join(visits_per_day, gg_visites_uniques, by = "day") %>% 
  mutate(r = n.x / n.y) %>% 
  ggplot(aes(x = day, y = r)) +
  geom_line()+
  theme_ipsum() +
  labs(x = "", y = "Nombre moyen de consultations par visite",
       title = "On voit bien le problème quand on regarde\nle nombre moyen de consultations par visite.") 
dev.off()
