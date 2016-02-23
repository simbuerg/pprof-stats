summaryUI <- function(id, label = "Summary") {
  ns <- NS(id)

  tagList(
    fluidRow(
      h3("Experiments"),
      p("The following experiments are available in this database instance of the pprof-study.")
    ),
    fluidRow(
      column(width = 12,
             dataTableOutput(ns("experiments-table"))
      )
    ),
    fluidRow(
      h3("Summary per experiment"),
      p("This shows a quick summary for all projects that were executed for a given experiment")
    ),
    fluidRow(
      column(width = 4,
             selectInput(ns("all"), label = "Experiment", multiple = FALSE, choices = NULL, width = '100%')),
      column(width = 12,
             wellPanel(
               dataTableOutput(ns("summary-table"))
             ))
    )
  )
}

summary <- function(input, output, session, db, exps) {
  output$`summary-table` = DT::renderDataTable({
    validate(
      need( input$all, "Select an experiment first.")
    )
    get_projects_per_experiment(db(), input$all)
  })

  output$`experiments-table` = DT::renderDataTable({
    validate(
      need( input$db, "I need a database connection first.")
    )
    get_experiments(db())
  })

  observe({
    updateSelectInput(session, "all", choices = c(getSelections(NULL, exps)), selected = 0)
  })
}

summaryMenu <- function() {
  menuItem("Summary", tabName = "summary")
}
