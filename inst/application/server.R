
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
#        select(-platform) %>% 
        right_join(urls %>% 
                     filter(platform %in% c("Hypotheses.org", "OpenEdition Journals", "OpenEdition Books", "Calenda")[c(input$hypotheses, input$revues, input$books, input$calenda)]), 
                   by = c("url" = "id")) %>% 
        left_join(publications, by = c("site_url", "platform")) %>% 
        select(platform, site_titre, naked_titre, url, value, date) %>% 
        arrange(desc(value)) %>% 
        filter(value >= input$seuil * max(anomalies$value, na.rm = TRUE)) %>% 
        mutate(value = round(value)) %>% 
        rename(Plateforme = platform, Publication = site_titre, Document = naked_titre, URL = url, Date = date, intensité = value) 
    } else {
      anomalies %>% 
#        select(-platform) %>% 
        right_join(
          urls %>% 
            filter(site_url %in% input$publications),
          by = c("url" = "id")
        ) %>% 
        left_join(publications, by = c("site_url", "platform")) %>% 
        select(platform, site_titre, naked_titre, url, value, date) %>% 
        arrange(desc(value)) %>% 
        filter(value >= input$seuil * max(anomalies$value, na.rm = TRUE)) %>% 
        mutate(value = round(value)) %>% 
        rename(Plateforme = platform, Publication = site_titre, Document = naked_titre, URL = url, Date = date, intensité = value)
    }
  })
  
  output$episode <- DT::renderDataTable({
    DT::datatable(df(), 
                  selection = "single",
                  options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'))
                  ) %>% 
      DT::formatDate("Date", method = 'toLocaleDateString', params = list('fr-FR'))
  },
  server = TRUE)
  
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
      
#      cat(str(df()), "\\n")
      cat(df() %>% slice(input$episode_rows_selected) %>% pull("Date"), "\n")
      cat(df() %>% slice(input$episode_rows_selected) %>% pull("URL"), "\n")
      cat(as.character(input$periode[1]), "\n")
      cat(as.character(input$periode[2]), "\n")
      # cat(periode_start = as.character(input$date[1]), "\\n")
      # cat(periode_end = as.character(input$date[2]), "\\n")
      # cat(date_episode = df()[input$episode_rows_selected, "Date"], "\\n")
      # cat(url = df()[input$episode_rows_selected, "URL"], "\\n")
      
      tmp <- m_render(system.file("application/dashboard.Rmd", package = "detectR"), 
                      output_file = paste0("/srv/shiny-server/umberto/rapports/dashboards/dashboard_", digest::sha1(df()[input$episode_rows_selected, "URL"]), "_", as.character(input$periode[1]), "_", as.character(input$periode[2]), ".html"),
                      params = list(periode_start = as.character(input$periode[1]),
                                    periode_end = as.character(input$periode[2]),
                                    date_episode = as.Date(df() %>% slice(input$episode_rows_selected) %>% pull("Date")),
                                    url = df() %>% slice(input$episode_rows_selected) %>% pull("URL")
                      )
      )
      setProgress(1)
    })
    return(paste0("https://shiny.labocleo.org/umberto/rapports/dashboards/dashboard_", digest::sha1(df()[input$episode_rows_selected, "URL"]), "_", as.character(input$periode[1]), "_", as.character(input$periode[2]), ".html")) # à vérifier
  })
  
  observeEvent(input$go, {
    hide("panel")
    hide("table")
    showElement("dash")
  })
  
  output$dashboard <- renderUI({
      setProgress(value = 1)
      tags$iframe(src=path(), height=600, width=535)
    })

  
})
