
shinyServer(function(input, output, session) {

  observeEvent({input$hypotheses
                input$revues
                input$books}, 
               {
                updateSelectizeInput(session,
                           "publications",
                           choices = publis[c(input$hypotheses, input$revues, input$books)],
                           selected = input$publications)
  })

  output$urls <- DT::renderDataTable({
    if (is_null(input$publications)) {
      anomaly_detection$url_prediction_anomalies %>% 
        right_join(urls, by = c("url" = "id")) %>% 
        left_join(publications, by = c("site_url", "platform")) %>% 
        select(platform, site_titre, naked_titre, url, fit_sigma_ratio) %>% 
        arrange(desc(fit_sigma_ratio)) %>% 
        mutate(fit_sigma_ratio = round(fit_sigma_ratio)) %>% 
        rename(Plateforme = platform, Publication = site_titre, Document = naked_titre, URL = url, intensité = fit_sigma_ratio)
    } else {
      anomaly_detection$url_prediction_anomalies %>% 
        right_join(
          urls %>% 
            filter(site_url %in% input$publications),
          by = c("url" = "id")
        ) %>% 
        left_join(publications, by = c("site_url", "platform")) %>% 
        select(platform, site_titre, naked_titre, url, fit_sigma_ratio) %>% 
        arrange(desc(fit_sigma_ratio)) %>% 
        mutate(fit_sigma_ratio = round(fit_sigma_ratio)) %>% 
        rename(Plateforme = platform, Publication = site_titre, Document = naked_titre, URL = url, intensité = fit_sigma_ratio)
    }
  },
  server = TRUE,
  selection = "single")

})
