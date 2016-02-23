if (!require("devtools")) {
  install.packages("devtools")
  library(devtools)
}

devtools::load_all()
pprof::init()

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "PolyJIT"),
    dashboardSidebar(
      sidebarMenu(
        pprof::summaryMenu(),
        pprof::comparisonMenu(),
        pprof::speedupMenu(),
        pprof::speedupPerProjectMenu(),
        pprof::perfMenu(),
        pprof::tasksMenu(),
        selectInput(
          "db",
          label = "Database",
          choices = c("home", "buildbot", "develop"),
          multiple = FALSE,
          selected = 0
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "summary", pprof::summaryUI("summary_ui")),
        tabItem(tabName = "comparison", pprof::comparisonUI("comparison_ui")),
        tabItem(tabName = "speedupExperiment", pprof::speedupUI("speedup_ui")),
        tabItem(tabName = "speedupProject", pprof::speedupPerProjectUI("speedup_per_project_ui")),
        tabItem(tabName = "profiles", pprof::perfUI("perf_ui")),
        tabItem(tabName = "tasks", pprof::tasksUI("tasks_ui"))
      )
    )
  ),
  server = function(input, output, session) {
    con <- NULL

    db <- reactive({
      if (!is.null(con)) {
        dbDisconnect(conn = con)
      }
      validate(need(input$db, "Select a Database."))
      if (input$db == 'home')
        con <- dbConnect(RPostgres::Postgres(), dbname = 'pprof', user = 'pprof', password = 'pprof', port = 5432, host = '172.18.0.2' )
      else if (input$db == 'buildbot')
        con <- dbConnect(RPostgres::Postgres(), dbname = 'pprof-bb', user = 'bb', password = 'bb', port = 5432, host = 'debussy.fim.uni-passau.de')
      else if (input$db == 'develop')
        con <- dbConnect(RPostgres::Postgres(), dbname = 'pprof', user = 'pprof', password = 'pprof', port = 5432, host = 'debussy.fim.uni-passau.de')
      cat("DB:", input$db, "\n")
      return(con)
    })
    exps <- reactive({
      get_experiments(db())
    })

    callModule(pprof::comparison, "comparison_ui", db, exps)
    callModule(pprof::speedup, "speedup_ui", db, exps)
    callModule(pprof::tasks, "tasks_ui", db, exps)
    callModule(pprof::perf, "perf_ui", db, exps)
    callModule(pprof::speedupPerProject, "speedup_per_project_ui", db, exps)

    session$onSessionEnded(function() {
      if (!is.null(con))
        dbDisconnect(con)
    })
  }
)
