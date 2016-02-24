summaryUI <- function(id, label = "Summary") {
  ns <- NS(id)

  tagList(
    fluidRow(box(title="Experiments", width=12,
      p("The following experiments are available in this database instance of the pprof-study."),
      dataTableOutput(ns("allTable"))
    )),
    fluidRow(box(title="Summary per experiment", width=12,
      p("This shows a quick summary for all projects that were executed for a given experiment")
    )),
    fluidRow(
      column(width = 4,
             selectInput(ns("all"), label = "Experiment", multiple = FALSE, choices = NULL, width = '100%')),
      column(width = 12,
             wellPanel(
               dataTableOutput(ns("summaryTable"))
             ))
    )
  )
}

summary <- function(input, output, session, db, exps) {
  output$allTable = DT::renderDataTable({ exps() })
  output$summaryTable = DT::renderDataTable({
    validate(need(input$all, "Select an experiment first."))
    get_projects_per_experiment(db(), input$all)
  })

  observe({
    db <- db()
    exps <- exps()
    updateSelectInput(session, "all", choices = c(getSelections(NULL, exps)), selected = 0)
  })
}

summaryMenu <- function() {
  menuItem("Summary", tabName = "summary")
}
