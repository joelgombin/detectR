#' Get number of visits for a given URL
#' @importFrom magrittr %>% 
#' @export
get_visits <- function(visited_url, from, to, backend) {
  backend %>% 
    select(idvisit, url, date) %>% 
    filter(url == visited_url) %>% 
    filter(date >= from,
           date <= to) %>% 
    select(-url) %>% 
    collect %>% 
    distinct(idvisit, .keep_all = TRUE) %>% 
    group_by(date) %>% 
    summarise(n = n()) %>% 
    ungroup
}
