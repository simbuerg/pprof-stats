perfUI <- function(id, label = "Perf") {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(width = 4,
             selectInput(ns("experiments"), label = "Profile Experiment", multiple = FALSE, choices = NULL, width = '100%')),
      column(width = 4,
             selectInput(ns("projects"), label = "Projects", multiple = FALSE, choices = NULL, width = '100%'))
    ),
    fluidRow(
      p("Run-time profile per project, presented as flamegraph.")
    ),
    fluidRow(column(width = 12,
                    htmlOutput(ns("flamegraph")))),
    hr()
  )
}

perf <- function(input, output, session, db, exps) {
  output$flamegraph = renderText({
    validate(
      need(input$experiments, "Select a PERF-compatible experiment first."),
      need(input$projects, "Select a Project first.")
    )

    return(flamegraph(db(), input$experiments, input$projects))
  })

  observe({
    db <- db()
    exps <- exps()

    updateSelectInput(session, "experiments", choices = c(getSelections("pj-perf", exps)), selected = 0)
    if (!is.null(input$perfExperiments)) {
      updateSelectInput(session, "projects", choices = perfProjects(db, input$perfExperiments), selected = 0)
    }
  })
}

perfMenu <- function() {
  menuItem("Profiles", tabName = "profiles")
}
