
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
  
  df <- reactive({
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
  })
  
  output$episode <- DT::renderDataTable({
    df()
  },
  server = TRUE,
  selection = "single")
  
  observe({
    if (!is.null(input$episode_rows_selected)) {
      if (input$episode_rows_selected %in% "") {
        shinyjs::disable("go")
      } else {
        shinyjs::enable("go")
      }
    } else {
      shinyjs::disable("go")
    }
  })
  
  path <- eventReactive(input$go, {
    withProgress(message = "Préparation de l'explorateur", value = 0.5, expr = {
      
      
      tmp <- m_render(system.file("application/dashboard.Rmd", package = "detectR"), 
                      output_file = paste0("/data/dashboards/dashboard_", digest::sha1(df()[input$episode_rows_selected, "URL"]), "_", as.character(input$periode[1]), "_", as.character(input$periode[2]), ".html"),
                      params = list(periode_start = as.character(input$date[1]),
                                    periode_end = as.character(input$date[2]),
                                    url = df()[input$episode_rows_selected, "URL"]
                      )
      )
      setProgress(1)
    })
    return(tmp)
  })
  
  observeEvent(input$go, {
    hide("panel")
    hide("table")
    showElement("dash")
  })
  
  output$dashboard <- renderUI({
    withProgress(message = "affichage du dashboard en cours...", value = 0.1, {
      tmp <- tempfile()
      setProgress(value = 0.2, message = paste0("écriture sur", tmp))
      writeChar(httr::GET(paste0("http://localhost:", port, "/", sub("/data/", "", path()))) %>% content(as = "text"), con = tmp)
      includeHTML(tmp)
      setProgress(value = 1)
    })
  })
  
  
})
