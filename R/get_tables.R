#' get the table about visits
#' @param conn a connection to the relevant DB
#' @param from the visit ID to start from

get_tbl_visits <- function(conn, from) {
  # log_visit <- dplyr::tbl(conn, "log_visit")
  # visites <- log_visit %>% 
  #   dplyr::select(idvisit, visit_last_action_time, visit_first_action_time, visit_total_actions, referer_type, referer_name, referer_url, referer_keyword, location_browser_lang, config_browser_name, config_device_type, config_os, config_os_version, visitor_localtime, config_resolution, visit_total_time, location_city, location_country) %>% 
  #   dplyr::filter(idvisit > from) %>% 
  #   dplyr::collect()
  # if (inherits(conn, "Pool")) {
  #   conn <- pool::poolCheckout(conn)
  #   comes_from_pool <- TRUE
  # } else {
  #   comes_from_pool <-FALSE
  # }
  
  visites <- DBI::dbGetQuery(conn, pool::sqlInterpolate(conn, "SELECT  `idvisit` AS `idvisit`, `visit_last_action_time` AS `visit_last_action_time`, `visit_first_action_time` AS `visit_first_action_time`, `visit_total_actions` AS `visit_total_actions`, `referer_type` AS `referer_type`, `referer_name` AS `referer_name`, `referer_url` AS `referer_url`, `referer_keyword` AS `referer_keyword`, `location_browser_lang` AS `location_browser_lang`, `config_browser_name` AS `config_browser_name`, `config_device_type` AS `config_device_type`, `config_os` AS `config_os`, `config_os_version` AS `config_os_version`, `visitor_localtime` AS `visitor_localtime`, `config_resolution` AS `config_resolution`, `visit_total_time` AS `visit_total_time`, `location_city` AS `location_city`, `location_country` AS `location_country` FROM `log_visit` WHERE (`idvisit` > ?from)", from = from))
  # DBI::dbBind(visites, list(from))
  # DBI::dbFetch(visites)
  # DBI::dbClearResult(visites)
  
  # if (comes_from_pool) {
  #   pool::poolReturn(conn)
  # }
  
  visites <- tibble::as_tibble(visites)
  
  visites <- visites %>% 
    mutate(idvisit = as.double(idvisit),
           visit_last_action_time = as.character(visit_last_action_time),
           visit_first_action_time = as.character(visit_first_action_time),
           visit_total_actions = as.double(visit_total_actions),
           referer_type = as.double(referer_type),
           visitor_localtime = as.character(visitor_localtime),
           visit_total_time = as.double(visit_total_time)) 
  
  return(visites)
}

#' get the table about actions
#' @inheritParams get_visits
#' @param from the action ID to start from

get_tbl_actions <- function(conn, from) {
  # log_action <- dplyr::tbl(conn, "log_action")
  # actions <- log_action %>% 
  #   dplyr::filter(visit_last_action_time > from,
  #                 visit_last_action_time <= to) %>% 
  #   dplyr::filter(type %in% 1) %>% 
  #   dplyr::collect()
  
  actions <- DBI::dbGetQuery(conn, pool::sqlInterpolate(conn, "SELECT * FROM `log_action` WHERE (`idaction` > ?from) AND (`type` IN (1.0))", from = from))
  
  actions <- tibble::as_tibble(actions)
  
  actions <- actions %>% 
    mutate(hash = as.double(hash),
           type = as.double(type))
  
  return(actions)
}

#' get the table linking visits and actions
#' @inheritParams get_visits

get_tbl_visit_actions <- function(conn, from) {
  # log_link_visit_action <- dplyr::tbl(conn, "log_link_visit_action")
  # visites_actions <- log_link_visit_action %>% 
  #   dplyr::select(idlink_va, idvisit, idaction_url, server_time) %>%
  #   dplyr::filter(visit_last_action_time > from,
  #                 visit_last_action_time <= to) %>% 
  #   dplyr::collect()
  
  visites_actions <- DBI::dbGetQuery(conn, pool::sqlInterpolate(conn, "SELECT `idlink_va` AS `idlink_va`, `idvisit` AS `idvisit`, `idaction_url` AS `idaction_url`, `server_time` AS `server_time` FROM `log_link_visit_action` WHERE (`idvisit` > ?from)", from = from))
  
  visites_actions <- tibble::as_tibble(visites_actions)
  
  visites_actions <- visites_actions %>% 
    mutate(idlink_va = as.double(idlink_va),
           idvisit = as.double(idvisit),
           idaction_url = as.double(idaction_url),
           server_time = as.character(server_time))
  
  return(visites_actions)
}

#' collect all actions
#' @import urltools
#' @import rex
#' @param tbl_visites a table containing the visits
#' @param tbl_actions a table containing the actions
#' @param tbl_visites_actions a table linking visits and actions

get_tbl_all_actions <- function(tbl_visites, tbl_actions, tbl_visites_actions) {
  # a regex for cleaning domain names
  # re_domaines <- rex(start,
  #                    capture(
  #                      group(zero_or_more(alnum),
  #                            group(maybe(dot),
  #                                  "revues.org")) %or% 
  #                        group("books.openedition.org") %or%
  #                        group("calenda.org") %or%
  #                        group(
  #                          group(zero_or_more(alnum),
  #                                maybe(dot)),
  #                          "hypotheses.org")
  #                    ),
  #                    anything,
  #                    end)
  re_domaines <- "^((?:(?:(?:(?:(?:[[:alnum:]])*(?:(?:\\.)?revues\\.org))|(?:books\\.openedition\\.org))|(?:calenda\\.org))|(?:(?:(?:[[:alnum:]])*(?:\\.)?)hypotheses\\.org))).*$"
  
  # let's join all tables
  tbl_all_actions <- tbl_visites %>% 
    dplyr::left_join(
      tbl_visites_actions,
      by = c("idvisit")
    ) %>% 
    dplyr::left_join(
      tbl_actions, 
      by = c("idaction_url" = "idaction")
    ) %>% 
    dplyr::collect()
  
  # let's vectorise the `domain<-` function
  `mydomain<-` <- Vectorize(`domain<-`)  
  
  # let's clean up the URLs
  urls <- tbl_all_actions %>% 
    dplyr::pull(name) %>% 
    urltools::url_parse() %>% 
    dplyr::mutate(scheme = "http") %>% 
    dplyr::mutate(parameter = NA) %>% 
    dplyr::mutate(fragment = NA) %>% 
    dplyr::mutate(port = NA) %>% 
    dplyr::mutate(domain = sub(re_domaines, "\\1", domain))
  
  tbl_all_actions$url <- urls %>%
    urltools::url_compose() %>% 
    stringr::str_replace_all(" ", "") %>% 
    dplyr::as_tibble() %>% 
    dplyr::pull(value)
  
  # let's get the day of each visit/action
  tbl_all_actions <- tbl_all_actions %>%
    dplyr::mutate(date = lubridate::floor_date(lubridate::ymd_hms(visit_last_action_time), "day"))

  tbl_all_actions$domain <- urls$domain
  
  return(tbl_all_actions)
}

#' Extract from a given DB and load into another DB
#' 
#' @param conn1 a connection to the first database.
#' @param conn2 a connection to the second database.
#' @inheritParams get_visits
#' @param progress whether a progress bar should be shown.
#' @export
#' @import utils
#' 
extract_and_load <- function(conn1, conn2, from, progress = TRUE) {
  pb <- txtProgressBar(style = 3)
  
  tbl_visits <- get_tbl_visits(conn1, from = from)
  setTxtProgressBar(pb, 0.1)
  DBI::dbWriteTable(conn2, "visites", tbl_visits, append = TRUE)
  setTxtProgressBar(pb, 0.2)
  
  tbl_actions <- get_tbl_actions(conn1, from = 0) # we need all actions, since they're not chronological
  setTxtProgressBar(pb, 0.3)
  DBI::dbWriteTable(conn2, "actions", tbl_actions, overwrite = TRUE)
  setTxtProgressBar(pb, 0.4)
  
  tbl_visit_actions <- get_tbl_visit_actions(conn1, from = from)
  setTxtProgressBar(pb, 0.5)
  DBI::dbWriteTable(conn2, "visites_actions", tbl_visit_actions, append = TRUE)
  setTxtProgressBar(pb, 0.6)
  
  tbl_all_actions <- get_tbl_all_actions(tbl_visits, tbl_actions, tbl_visit_actions)
  setTxtProgressBar(pb, 0.8)
  DBI::dbWriteTable(conn2, "all_actions", tbl_all_actions, append = TRUE)
  setTxtProgressBar(pb, 1)
  close(pb)
  return(dplyr::tbl(conn2, "all_actions"))
}
