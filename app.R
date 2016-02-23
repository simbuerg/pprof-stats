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
        uiOutput("dbui")
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
    d <- login.data("./.pglogin")
    db <- NULL

    shiny::observeEvent(input$db, {
      name <- input$db
      if (name %in% d$name) {
        if (!is.null(db)) {
          dbDisconnect(conn = db)
        }

        login <- d[d$name==name,]
        print(login)
        db <- dbConnect(RPostgres::Postgres(),
                         dbname = as.character(login$dbname),
                         user = as.character(login$user),
                         password = as.character(login$password),
                         port = as.character(login$port),
                         host = as.character(login$host))
        exps <- get_experiments(db)

        callModule(pprof::comparison, "comparison_ui", reactive(db), reactive(exps))
        callModule(pprof::speedup, "speedup_ui", reactive(db), reactive(exps))
        callModule(pprof::tasks, "tasks_ui", reactive(db), reactive(exps))
        callModule(pprof::perf, "perf_ui", reactive(db), reactive(exps))
        callModule(pprof::speedupPerProject, "speedup_per_project_ui", reactive(db), reactive(exps))
      }
    })

    output$dbui <- renderUI({
      selectInput(
        "db",
        label = "Database",
        choices = c("", as.vector(d$name)),
        multiple = FALSE,
        selected = 1
      )
    })

    session$onSessionEnded(function() {
      if (!is.null(db))
        dbDisconnect(db)
    })
  }
)
