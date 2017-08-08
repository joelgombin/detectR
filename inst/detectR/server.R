
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

  output$episode <- DT::renderDataTable({
    if (is_null(input$publications)) {
      anomalies %>% 
        select(-platform) %>% 
        right_join(urls %>% 
                     filter(platform %in% c("Hypotheses.org", "Revues.org", "OpenEdition Books", "Calenda")[c(input$hypotheses, input$revues, input$books, input$calenda)]), 
                   by = c("url" = "id")) %>% 
        left_join(publications, by = c("site_url", "platform")) %>% 
        select(platform, site_titre, naked_titre, url, fit_sigma_ratio) %>% 
        arrange(desc(fit_sigma_ratio)) %>% 
        mutate(fit_sigma_ratio = round(fit_sigma_ratio)) %>% 
        rename(Plateforme = platform, Publication = site_titre, Document = naked_titre, URL = url, intensité = fit_sigma_ratio)
    } else {
      anomalies %>% 
        select(-platform) %>% 
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

  observe({
    shinyjs::toggleState("go", !is.null(input$episode_rows_selected) && input$episode_rows_selecteds != "")
  })
  
})
