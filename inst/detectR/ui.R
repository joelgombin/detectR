shinyUI(
  fluidPage(title = "Bienvenue dans le détecteur de lecteurs inattendus !",
            theme = "bootstrap.min.css",
#    titlePanel("Bienvenue dans le détecteur de lecteurs inattendus !"),
    fluidRow(
      h1("Bienvenue dans le détecteur de lecteurs inattendus !", style = "text-align:center")
    ),
    fluidRow(
      br(),
      br()
    ),
    fluidRow(
      column(1),
      column(3,
             tags$h2("Plateformes :"),
             br(),
             shinyBS::bsButton("hypotheses",
                               "hypotheses.org",
                               icon("rss-square",
                                    class = "fa-fw",
                                    lib = "font-awesome"),
                               size = "large",
                               type = "toggle",
                               block = TRUE,
                               value = FALSE),
             shinyBS::bsButton("revues",
                               "revues.org",
                               icon("newspaper-o",
                                    class = "fa-fw",
                                    lib = "font-awesome"),
                               size = "large",
                               type = "toggle",
                               block = TRUE,
                               value = FALSE),
             shinyBS::bsButton("books",
                               "books.openedition.org",
                               icon("book",
                                    class = "fa-fw",
                                    lib = "font-awesome"),
                               size = "large",
                               type = "toggle",
                               block = TRUE,
                               value = FALSE)),
      column(1),
      column(3,
             tags$h2("Publications :"),
             br(),
                 selectizeInput(
                   "publications",
                   NULL,
                   NULL,
                   multiple = TRUE,
                   options = list(
                     placeholder = "Choisir un ou plusieurs espaces de publications :"
    #                  optgroups = list(
    #                    list(value = "hypotheses", label = "hypotheses.org"),
    #                    list(value = "revues", label = "revues.org"),
    #                    list(value = "books", label = "books.openedition.org")
    #                  ),
    #                  optgroupField = 'plateforme',
    #                  optgroupOrder = c("hypotheses", "revues", "books"),
    #                  searchField = c("Title", "url"),
    #                  render = I("{
    #   option: function(item, escape) {
    #     return '<div>' + escape(item.Title + '</div>');
    #   }
    # }")
                   )
                 )
      ),
    column(1),
    column(3,
           tags$h2("Période :"),
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
           ),
    column(1)
    ),
    fluidRow(
      column(3),
      column(6,
             tags$h2("Choisissez la publication que vous voulez explorer"),
             DT::dataTableOutput('urls')
             ),
      column(3)
    )
  )
)
