

#Functions to get and prepare the log dataset before delving into modeling.



#' Functions to read and clean the aggregated piwick files.
#' @export
#' @importFrom magrittr %>%
read_aggregated_piwick_file <- function(aggregated_piwick_file) {
  urls <- readr::read_csv(aggregated_piwick_file)

  not_url = c("Autres", "http://www.revues.org/")

  urls <- urls %>%
    dplyr::filter(!url %in% not_url) %>%
    dplyr::filter(grepl("https", url))

  urls <- urls %>%
    dplyr::mutate(url = gsub("\\?.*", "", url)) %>% #first we remove all the doc with ? arguments.
    dplyr::filter(grepl("\\d+$", url)) %>% #We only keep articles (that ends with a number id)
    dplyr::group_by(date, url) %>%
    dplyr::summarise(nb_visits = sum(nb_visits),
              nb_uniq_visitors = sum(nb_uniq_visitors),
              entry_nb_visits = sum(entry_nb_visits),
              entry_nb_uniq_visitors = sum(entry_nb_uniq_visitors),
              entry_bounce_count = sum(entry_bounce_count),
              exit_nb_visits = sum(exit_nb_visits),
              exit_nb_uniq_visitors = sum(exit_nb_uniq_visitors),
              avg_time_on_page = mean(avg_time_on_page)) %>% #We merge any duplicate and sum the relevant values
    dplyr::ungroup()

  return(urls)
}

#' A function to retrieve all the "missing" dates and sets the number of visits to 0.
#' @export
#' @importFrom magrittr %>%
complete_time <- function(aggregated_frequent) {
  time_sequence <- seq(aggregated_frequent$date[which.min(aggregated_frequent$date)], aggregated_frequent$date[which.max(aggregated_frequent$date)], by="day") #First we get the complete sequence of "hours" during the period (by selection the min and max time recorded in timestamp)
  aggregated_frequent <- aggregated_frequent %>%
    dplyr::group_by(url) %>%
    tidyr::complete(date = time_sequence, fill = list(nb_visits = 0)) %>%
    ungroup()
  return(aggregated_frequent)
}

#' Function to set up a logs_dataset object.
#'
#'  Contains:
#'  - The original data
#'  - The dataset of "frequent" visits (with number of days higher than threshold) to be used on standard modeling with rms
#'  - The dataset of urls with "seldom" visits (with number of days lower than threshold) to be used on Markov analysis, as the data entries are to low to allow for standard modeling.
#' @export
#' @importFrom magrittr %>%
get_aggregated_piwick <- function(aggregated_piwick_file, absolute_days=10, relative_days=0.9) {

  #We open the cleaned file containing all the logs
  aggregated_piwick <- readr::read_csv(aggregated_piwick_file) %>%
    dplyr::rename(nb_visits = log_count) %>%
    dplyr::select(-`X1`)

  #We get the max date recorded in the full dataset (for every urls).
  max_absolute_date <- lubridate::ymd(max(aggregated_piwick$date))

  #We generate an aggregated dataset with filter  so that every url has at least the absolute number of observations (absolute_days) and the relative number (relative_days)
  aggregated_piwick_selected = aggregated_piwick %>%
    dplyr::group_by(url) %>%
    dplyr::summarise(days_covered = n(), min_date = min(date)) %>%
    dplyr::filter(days_covered > absolute_days) %>%
    dplyr::mutate(period_covered = as.numeric(max_absolute_date-min_date)) %>%
    dplyr::mutate(date_prop = days_covered/period_covered) %>%
    dplyr::filter(date_prop > relative_days)

  aggregated_piwick_visits <- aggregated_piwick %>%
    dplyr::mutate(selected = ifelse(url %in% aggregated_piwick_selected$url, "yes", "no"))   #We only do one run to check if the urls are in the selected dataset in order to save up some computation time.

  aggregated_piwick_visits_frequent <- aggregated_piwick_visits %>%
    dplyr::filter(selected %in% "yes") %>%
    dplyr::select(-selected)

  aggregated_piwick_visits_seldom <- aggregated_piwick_visits %>%
    dplyr::filter(selected %in% "no") %>%
    dplyr::select(-selected)

  aggregated_piwick_visits_frequent <- complete_time(aggregated_piwick_visits_frequent)

  aggregated_piwick_visits_frequent <- aggregated_piwick_selected %>%
    dplyr::left_join(aggregated_piwick_visits_frequent)

  aggregated_piwick_visits_frequent <- aggregated_piwick_visits_frequent %>%
    dplyr::mutate(diff_days = as.numeric(date - min_date) + 1) %>%
    dplyr::filter(diff_days >= 1)

  logs_board <- list(aggregated_complete = aggregated_piwick, aggregated_frequent = aggregated_piwick_visits_frequent, aggregated_seldom = aggregated_piwick_visits_seldom)
  class(logs_board) <- "logs_dataset"

  return(logs_board)
}
