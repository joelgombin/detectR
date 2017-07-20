#' Proceed with the rcs regression for each group
#' @import tidyverse rms broom
#' @export
get_rcs_group <- function(diff_days, log_value_estimated, knots){
  appd = data_frame(diff_days, log_value_estimated)
  fit <- lm(log_value_estimated ~ rcs(diff_days, knots), data=appd)
  prediction <- predict(fit, newdata = appd)
  return(fit)
}

#' Extract the anomalies for each group (i. e. each point beyond confidence interval)
#' @import tidyverse rms broom
#' @export
get_rcs_anomalies <- function(fit, log_value_estimated, diff_days, knots) {
  appd <- data_frame(diff_days, log_value_estimated)
  knot_defined <- rcspline.eval(appd$diff_days, nk=knots)
  fit_sigma_ratio <- augment(fit)$.sigma
  sigma_mean <- mean(fit_sigma_ratio)
  fit_sigma_ratio <- (abs(sigma_mean-fit_sigma_ratio)/sigma_mean)*100
  fit_confidence <- data.frame(appd, predict(fit, interval="prediction"), fit_sigma_ratio)
  return(fit_confidence)
}

#' A Function to detect the anomalies using restricted cubic square regression (from RMS)
#' @param details While you can use a different number of knots, 3 is generally a good fit.
#' @import tidyverse rms broom
#' @export
detection_anomalies_rcs <- function(sample_complete, knots=3){

  setOldClass(c("tbl_df", "tbl", "data.frame"))
  anomalies_board <- setClass("anomalies_board", slots = c(url_prediction="tbl_df", model_register="tbl_df", url_prediction_anomalies="tbl_df", url_prediction_count="tbl_df"))

  message("calculating the rcs models")

  sample_complete <- sample_complete %>% mutate(log_value_estimated = nb_visits_ts)

  url_prediction_models <- sample_complete %>%
    group_by(url) %>%
    do(model_rcs = get_rcs_group(.$diff_days, .$log_value_estimated, knots), log_value_estimated = .$log_value_estimated, diff_days = .$diff_days, date = .$date)

  model_register <- url_prediction_models %>% select(url, model_rcs) %>% ungroup()

  message("getting the list of anomalies")

  url_prediction <- url_prediction_models %>%
    do(url = .$url, date = .$date, model_rcs = .$model_rcs, predictions = get_rcs_anomalies(.$model_rcs, .$log_value_estimated, .$diff_days, 3), log_value_estimated = .$log_value_estimated, diff_days = .$diff_days) %>%
    unnest(url) %>%
    unnest(predictions) %>%
    ungroup()

  url_prediction_anomalies <- url_prediction %>% filter(log_value_estimated>upr)

  url_prediction_count <- url_prediction_anomalies %>%
    group_by(url) %>%
    summarise(anomalies = n()) %>%
    arrange(desc(anomalies)) %>%
    ungroup()

  anomalies_dataset <- anomalies_board(url_prediction=url_prediction, model_register=model_register, url_prediction_anomalies=url_prediction_anomalies, url_prediction_count=url_prediction_count)

  return(anomalies_dataset)
}

#' Get a graph of the anomalies.
#' @import tidyverse rms broom
#' @export
graph_anomalies_rcs <- function(anomalies_dataset, article, show_sigma=FALSE){
  article_prediction <- anomalies_dataset@url_prediction %>%
    filter(url == article)
  article_prediction <- article_prediction %>%
    mutate(anomaly_detection = ifelse(log_value_estimated > upr, "excess", "normal")) %>%
    mutate(anomaly_detection = ifelse(log_value_estimated < lwr, "undervalue", anomaly_detection))
  if (show_sigma == TRUE) {
    article_prediction <- article_prediction %>%
      mutate(anomaly_sigma = ifelse(anomaly_detection == "excess", fit_sigma_ratio, 2))
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
  show(compare_graph)
}

