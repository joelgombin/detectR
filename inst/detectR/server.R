
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
  
  
})
