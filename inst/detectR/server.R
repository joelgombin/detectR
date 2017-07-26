
shinyServer(function(input, output, session) {

  plateformes <- reactive({
    out <- c()

    req(input$hypotheses)
    req(input$revues)
    req(input$books)

    if (input$hypotheses) {
      out <- c(out, "hypotheses")
    } else {
      out <- setdiff(out, "hypotheses")
    }
    if (input$revues) {
      out <- c(out, "revues")
    } else {
      out <- setdiff(out, "revues")
    }
    if (input$books) {
      out <- c(out, "books")
    } else {
      out <- setdiff(out, "books")
    }
    return(out)
  })

  publications_selected <- reactive({
    input$publications
  })


  observeEvent(plateformes(), {

    updateSelectizeInput(session,
                         "publications",
                         choices = publications %>% filter(plateforme %in% plateformes()) %>% pull(Title),
                         selected = publications_selected(),
                         server = TRUE)
  })

  output$urls <- DT::renderDataTable({
    unique(anomaly_detection$model_register$url)
  })
})
