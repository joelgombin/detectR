

function(request) {
  fluidPage(
    useShinyjs(),  # Include shinyjs
    
#            theme = "bootstrap.min.css",
    titlePanel("Bienvenue dans le détecteur de lecteurs inattendus !"),
    sidebarLayout(
      div(id = "panel", 
          sidebarPanel(
            tags$h3("Plateformes :"),
            br(),
            switchInput("hypotheses",
                        "Hypothèses",
                        value = TRUE),
            switchInput("revues",
                        "Revues.org",
                        value = TRUE),
            switchInput("books",
                        "OpenEdition Books",
                        value = TRUE),
            switchInput("calenda",
                        "Calenda",
                        value = TRUE),
            tags$h3("Publications :"),
            br(),
            selectizeInput("publications",
                           NULL,
                           choices = publis,
                           multiple = TRUE,
                           options = list(
                             placeholder = "Choisir un ou plusieurs espaces de publications :"
                           )
                          ),
            tags$h3("Période :"),
            br(),
            dateRangeInput("periode",
                           "Choisir la période concernée",
                           start = "2017-01-01",
                           end = Sys.Date(),
                           min = "2017-01-01",
                           max = Sys.Date(),
                           format = "dd/mm/yyyy",
                           weekstart = 1,
                           language = "fr",
                           separator = "à")
            )
          ),
      mainPanel(
        div(id = "table",
            tags$h2("Choisissez l'épisode de lecteurs inattendus que vous voulez explorer"),
            DT::dataTableOutput('episode'),
            disabled(
              actionButton("go", label = "Explorez !")
            )
           ),
        hidden(
          div(id = "dash",
            htmlOutput("dashboard")
          )
        )
      )
    )
  )
}
