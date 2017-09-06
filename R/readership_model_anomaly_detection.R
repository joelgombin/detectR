#' Proceed with the rcs regression for each group
#' @export
get_rcs_group <- function(diff_days, log_value_estimated, knots){
  appd = tibble::tibble(diff_days, log_value_estimated)
  fit <- lm(log_value_estimated ~ rms::rcs(diff_days, knots), data=appd)
  prediction <- stats::predict(fit, newdata = appd)
  return(fit)
}

#' Extract the anomalies for each group (i. e. each point beyond confidence interval)
#' @export
get_rcs_anomalies <- function(fit, log_value_estimated, diff_days, knots) {
  appd <- tibble::tibble(diff_days, log_value_estimated)
  knot_defined <- Hmisc::rcspline.eval(appd$diff_days, nk=knots)
  fit_sigma_ratio <- broom::augment(fit)$.sigma
  sigma_mean <- mean(fit_sigma_ratio)
  fit_sigma_ratio <- (abs(sigma_mean-fit_sigma_ratio)/sigma_mean)*100
  fit_confidence <- tibble::tibble(appd, stats::predict(fit, interval="prediction"), fit_sigma_ratio)
  return(fit_confidence)
}

#' A Function to detect the anomalies using restricted cubic square regression (from RMS)
#' @param details While you can use a different number of knots, 3 is generally a good fit.
#' @importFrom magrittr %>%
#' @export
detection_anomalies_rcs <- function(sample_complete, knots=3){

  message("calculating the rcs models")

  sample_complete <- sample_complete %>% 
    dplyr::mutate(log_value_estimated = nb_visits_ts)

  url_prediction_models <- sample_complete %>%
    dplyr::group_by(url) %>%
    dplyr::do(model_rcs = get_rcs_group(.$diff_days, .$log_value_estimated, knots), log_value_estimated = .$log_value_estimated, diff_days = .$diff_days, date = .$date)

  model_register <- url_prediction_models %>% 
    dplyr::select(url, model_rcs) %>% 
    dplyr::ungroup()

  message("getting the list of anomalies")

  url_prediction <- url_prediction_models %>%
    dplyr::do(url = .$url, date = .$date, model_rcs = .$model_rcs, predictions = get_rcs_anomalies(.$model_rcs, .$log_value_estimated, .$diff_days, 3), log_value_estimated = .$log_value_estimated, diff_days = .$diff_days) %>%
    tidyr::unnest(url) %>%
    tidyr::unnest(predictions) %>%
    dplyr::ungroup()

  url_prediction_anomalies <- url_prediction %>%
    dplyr::filter(log_value_estimated>upr)

  url_prediction_count <- url_prediction_anomalies %>%
    dplyr::group_by(url) %>%
    dplyr::summarise(anomalies = n()) %>%
    dplyr::arrange(desc(anomalies)) %>%
    dplyr::ungroup()

  anomalies_dataset <- list(url_prediction = url_prediction,
                            model_register = model_register,
                            url_prediction_anomalies = url_prediction_anomalies,
                            url_prediction_count = url_prediction_count)
  class(anomalies_dataset) <- "anomalies_board"

  return(anomalies_dataset)
}

#' Get a graph of the anomalies.
#' @import ggplot2
#' @importFrom magrittr %>% 
#' @export
graph_anomalies_rcs <- function(anomalies_dataset, article, show_sigma=FALSE){
  article_prediction <- anomalies_dataset$url_prediction %>%
    dplyr::filter(url == article)
  article_prediction <- article_prediction %>%
    dplyr::mutate(anomaly_detection = ifelse(log_value_estimated > upr, "excess", "normal")) %>%
    dplyr::mutate(anomaly_detection = ifelse(log_value_estimated < lwr, "undervalue", anomaly_detection))
  if (show_sigma == TRUE) {
    article_prediction <- article_prediction %>%
      dplyr::mutate(anomaly_sigma = ifelse(anomaly_detection %in% "excess", fit_sigma_ratio, 2))
    compare_graph <- ggplot(article_prediction, aes(diff_days, log_value_estimated)) +
      geom_point(aes(col=anomaly_detection, size=log(anomaly_sigma))) +
      geom_line(aes(diff_days, fit), col="blue") +
      geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
      labs(title = article, subtitle="La taille des points d'anomalies indique l'ampleur du sigma", x="Jours", y="Visites") +
      guides(color=FALSE, size=FALSE)
  }
  else {
    compare_graph <- ggplot(article_prediction, aes(diff_days, log_value_estimated)) +
      geom_point(aes(col=anomaly_detection)) +
      geom_line(aes(diff_days, fit), col="blue") +
      geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
      labs(title = article, x="Jours", y="Visites") +
      guides(color=FALSE)
  }
  compare_graph
}

