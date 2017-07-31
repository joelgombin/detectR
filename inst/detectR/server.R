
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
    anomaly_detection$url_prediction_anomalies %>% 
      right_join(
        urls %>% 
          filter(site_url %in% input$publications),
        by = c("url" = "id")
      ) %>% 
      select(platform, site_url, naked_titre, url, fit_sigma_ratio) %>% 
      arrange(desc(fit_sigma_ratio)) %>% 
      mutate(fit_sigma_ratio = round(fit_sigma_ratio))
  },
  server = TRUE,
  selection = "single")
  
})
