#' Function to set up a logs_dataset object.
#'
#'  Contains:
#'  - The original data
#'  - The dataset of "frequent" visits (with number of days higher than threshold) to be used on standard modeling with rms
#'  - The dataset of urls with "seldom" visits (with number of days lower than threshold) to be used on Markov analysis, as the data entries are to low to allow for standard modeling.
#' @export
#' @param tbl_visites a raw table with the visits per day per url
#' @param absolute_days the number of days a url should have at least one visit to be taken into account
#' @param relative_days the proportion of days a url should have at least one visit to be taken into account
get_aggregated_visites <- function(tbl_visites, absolute_days=10, relative_days=0.9) {
  
  tbl_visites <- tbl_visites %>%
    dplyr::filter(n > 0) %>%  # make sure we don't have records with 0 visit
    dplyr::rename(nb_visits = n)
  
  #We get the max date recorded in the full dataset (for every urls).
  max_absolute_date <- lubridate::ymd(max(tbl_visites$date))
  
  #We generate an aggregated dataset with filter  so that every url has at least the absolute number of observations (absolute_days) and the relative number (relative_days)
  aggregated_selected = tbl_visites %>%
    dplyr::group_by(url) %>%
    dplyr::summarise(days_covered = n(), min_date = as.Date(min(date))) %>%
    dplyr::filter(days_covered > absolute_days) %>%
    dplyr::mutate(period_covered = as.numeric(max_absolute_date - min_date)) %>%
    dplyr::mutate(date_prop = days_covered / period_covered) %>%
    dplyr::filter(date_prop > relative_days)
  
  aggregated_visits <- tbl_visites %>%
    dplyr::mutate(selected = ifelse(url %in% aggregated_selected$url, TRUE, FALSE))   #We only do one run to check if the urls are in the selected dataset in order to save up some computation time.
  
  aggregated_visits_frequent <- aggregated_visits %>%
    dplyr::filter(selected) %>%
    dplyr::select(-selected)
  
  aggregated_visits_seldom <- aggregated_visits %>%
    dplyr::filter(!selected) %>%
    dplyr::select(-selected)
  
  aggregated_visits_frequent <- complete_time(aggregated_visits_frequent)
  
  aggregated_visits_frequent <- aggregated_selected %>%
    dplyr::left_join(aggregated_visits_frequent)
  
  aggregated_visits_frequent <- aggregated_visits_frequent %>%
    dplyr::mutate(diff_days = as.numeric(as.Date(date) - min_date) + 1) %>%
    dplyr::filter(diff_days >= 1)
  
  logs_board <- list(aggregated_complete = tbl_visites, aggregated_frequent = aggregated_visits_frequent, aggregated_seldom = aggregated_visits_seldom)
  class(logs_board) <- "logs_dataset"
  
  return(logs_board)
}
