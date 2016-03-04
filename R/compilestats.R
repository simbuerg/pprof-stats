compilestatsUI <- function(id, label = "Compilestats") {
  ns <- NS(id)
  experiment_1 <- ns("experiment_1")
  experiment_2 <- ns("experiment_2")
  projects <- ns("projects")
  groups <- ns("groups")
  single_ui <- ns("single_ui")
  pairwise_ui <- ns("pairwise_ui")

  tagList(
    fluidRow(
      box(title = "Experiments", solidHeader = TRUE, width = 6,
          selectInput(experiment_1, label = "experiment_1", multiple = FALSE, choices = NULL, width = '100%'),
          selectInput(experiment_2, label = "experiment_2", multiple = FALSE, choices = NULL, width = '100%')),
      box(title = "Filters", solidHeader = TRUE, width = 6,
          selectInput(projects, label = "Projects", multiple = TRUE, choices = NULL, width = '100%'),
          selectInput(groups, label = "Groups", multiple = TRUE, choices = NULL, width = '100%'))
    ),
    fluidRow(
      column(width = 12,
             wellPanel(
               dataTableOutput(ns("compairTable"))
             ))
    )
  )
}

compilestatsMenu <- function() {
  menuItem("Compilestats", tabName = "compilestats")
}

# Compilestats module server function
compilestats <- function(input, output, session, db, exps) {

  compHeight <- reactive({
    if (is.null(input$experiment_2))
      h <- 0
    else
      h <- length(input$experiment_2)
    return(h)
  })

  output$compairTable = DT::renderDataTable({
    validate(
      need(input$experiment_1, "Select experiment_1 to compare."),
      need(input$experiment_2, "Select experiment_2 to compare.")
    )
    if (length(input$experiment_1) == 0 ||
        length(input$experiment_2) == 0 )
      return(NULL)

    d <- experiment_cstats_comp(db(), input$experiment_1, input$experiment_2, input$projects, input$groups)
    print(head(d))
    d
  })

  observe({
    db <- db()
    exps <- exps()
    projects = projects(db)
    groups = groups(db)

    updateSelectInput(session, "experiment_1", choices = c(getSelections(NULL, exps)), selected = 0)
    updateSelectInput(session, "experiment_2", choices = c(getSelections(NULL, exps)), selected = 0)
    updateSelectInput(session, "projects", choices = projects, selected = 0)
    updateSelectInput(session, "groups", choices = groups, selected = 0)
  })

  return(NULL)
}
