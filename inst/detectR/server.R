
shinyServer(function(input, output, session) {


  observe({
    updateSelectizeInput(session,
                           "publications",
                           choices = publis[c(input$hypotheses, input$revues, input$books)]
                           )
#                           selected = publications_selected())
  })
  
  
})
