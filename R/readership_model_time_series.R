
week_day_list = c("1" = "Dimanche", "2" = "Lundi", "3" = "Mardi", "4" = "Mercredi", "5" = "Jeudi", "6" = "Vendredi", "7" = "Samedi")


#' A general function to get all the relevant element to decompose a calendar time serie (based on the absolute date of publication)

#' The main outputs are stored into a "time_serie_board" object. It includes:
#' - The original data frame grouped by days ("all_per_day") with additional "corrected" values (for instance, substracting seasonality, trend, outliers…)
#' - The seasonal week day values determined by time decomposition.
#' - A cubic polynomial model for the global trend (also determined by time decomposition)
#' - A set of "extreme" outliers defined by cook distance. From time to time, the website seems especially inactive (probably a maintenance issue)
#' @export
#' @importFrom magrittr %>%
get_calendar_time_series <- function(logs_board){
  all_per_day <- logs_board$aggregated_frequent %>%
    dplyr::ungroup() %>%
    dplyr::mutate(day_publication = lubridate::yday(date)) %>%
    dplyr::group_by(day_publication, date) %>%
    dplyr::summarise(mean_days = mean(nb_visits)) %>%
    dplyr::ungroup()
  all_mean <- mean(all_per_day$mean_days)
  time_decomposition <- stats::decompose(stats::ts(all_per_day$mean_days, freq=7))
  fit_trend <- get_trend_model(time_decomposition, all_per_day)
  predict_trend <- stats::predict(fit_trend, all_per_day)
  predict_trend <- 1+(1-predict_trend/all_mean)
  all_per_day$trend <- predict_trend
  wday_values <- get_wday_values(time_decomposition, all_per_day, all_mean)
  all_per_day <- all_per_day %>%
    dplyr::mutate(week_day_number = lubridate::wday(date)) %>%
    dplyr::left_join(wday_values) %>%
    dplyr::mutate(seasonal_corrected_days = mean_days * i_prop_seasonal_value) %>%
    dplyr::mutate(fully_corrected_mean_days = seasonal_corrected_days*trend) %>%
    dplyr::ungroup()
  models_time = tibble::tibble(c("trend"), list(fit_trend))
  colnames(models_time) <- c("name", "model")
  outliers_time <- get_outliers(all_per_day)
  complete_time <- list(all_per_day=all_per_day, weekday_values = wday_values, models_time = models_time, outliers_time=outliers_time)
  class(complete_time) <- "time_serie_board"
  return(complete_time)
}

#' a function to get a polynomial cubic model on the trend (determined by time decomposition)
#' @export
#' @importFrom magrittr %>%
get_trend_model <- function(time_decomposition, all_per_day){
  trend <- tibble(time_decomposition$trend, all_per_day$date, all_per_day$day_publication)
  colnames(trend) <- c("trend", "date", "day_publication")
  trend <- trend %>%
    dplyr::filter(!is.na(trend))
  fit_trend <- lm(trend ~ poly(day_publication, 3), data = trend)
  return(fit_trend)
}

#' a function to retrieve the effect of each day of the week (determined by time decomposition)
#' @export
#' @importFrom magrittr %>%
get_wday_values <- function(time_decomposition, all_per_day, all_mean){
  wday_values <- tibble::tibble(time_decomposition$seasonal[1:7], wday(all_per_day$date)[1:7])
  colnames(wday_values) <- c("seasonal_value", "week_day_number")
  wday_values <-  wday_values %>%
    dplyr::mutate(week_day_name = factor(week_day_number, labels = week_day_list)) %>%
    dplyr::mutate(prop_seasonal_value = ((all_mean + seasonal_value)/all_mean)) %>%
    dplyr::mutate(i_prop_seasonal_value = 1+(1-prop_seasonal_value))
  return(wday_values)
}

#' @export
#' @importFrom magrittr %>%
get_outliers <- function(all_per_day) {
  lm_cleaned_time <- lm(fully_corrected_mean_days ~ day_publication, data=all_per_day)
  cooksd <- cooks.distance(lm_cleaned_time)
  cooksd <- t(t(cooksd[cooksd>0.1]))
  cooksd <- tibble::tibble(row.names(cooksd), cooksd[,1])
  colnames(cooksd) <- c("day_publication", "outlier_value")
  cooksd <- cooksd %>%
    dplyr::mutate(day_publication = as.numeric(day_publication)) %>%
    dplyr::mutate(replacement_day=ifelse(day_publication %in% nrow(cooksd), day_publication-1, day_publication+1))
  cooksd$original_value <- all_per_day$fully_corrected_mean_days[cooksd$day_publication]
  cooksd$replacement_value <- all_per_day$fully_corrected_mean_days[cooksd$replacement_day]
  cooksd <- cooksd %>%
    dplyr::mutate(corrective_value = 1 + (1 - original_value / replacement_value))
  return(cooksd)
}

#' @export
#' @importFrom magrittr %>%
correct_time_series <- function(aggregated_frequent, complete_time) {

  correct_week_days <- aggregated_frequent %>%
    dplyr::left_join(complete_time$all_per_day %>% select(date, trend, i_prop_seasonal_value)) %>%
    dplyr::mutate(nb_visits_ts = nb_visits*i_prop_seasonal_value) %>%
    dplyr::mutate(nb_visits_ts = nb_visits_ts*trend) %>%
    dplyr::mutate(nb_visits_ts = round(nb_visits_ts, digits = 1))

  return(correct_week_days)

}
