speedupUI <- function(id, label = "Speedup") {
  ns <- NS(id)
  baseline <- ns("baseline")
  jitExperiments <- ns("jitExperiments")
  papiExperiments <- ns("papiExperiments")
  plotAmdahl <- ns("plotAmdahl")
  projects <- ns("projects")
  groups <- ns("groups")
  table <- ns("table")
  plotSize <- ns("plotSize")
  numCols <- ns("numCols")
  minY <- ns("minY")
  maxY <- ns("maxY")
  plotTime <- ns("plotTime")
  ui <- ns("ui")

  tagList(
    fluidRow(
      box(title = "Experiment", status = "primary", solidHeader = TRUE, width = 4,
          selectInput(baseline, label = "Baseline", multiple = FALSE, choices = NULL, width = '100%'),
          selectInput(jitExperiments, label = "time based experiment", multiple = FALSE, choices = NULL, width = '100%'),
          paste("Compare the selected experiment to the selected baseline experiment. This takes the
                 single-core configuration of the given baseline and compares it to all configurations
                 of the selected experiment per project.")
      ),
      box(title = "PAPI Experiment", status = "primary", solidHeader = TRUE, width = 4,
          selectInput(papiExperiments, label = "PAPI", multiple = FALSE, choices = NULL, width = '100%'),
          checkboxInput(plotAmdahl, label = "Draw Amdahl speedup", value = TRUE),
          paste("You can pick an optional PAPI-based experiment to derive a theoretical upper
                 limit for the speedup factor using Amdahl's law.")
      ),
      box(title = "Filters", status = "primary", solidHeader = TRUE, width = 4,
          selectInput(projects, label = "Projects", multiple = TRUE, choices = NULL, width = '100%'),
          selectInput(groups, label = "Groups", multiple = TRUE, choices = NULL, width = '100%')
      )
    ),
    fluidRow(
      tabBox(title = "Visualisation", width = 12,
             tabPanel("Table",
                      dataTableOutput(table)
             ),
             tabPanel("Plot",
                      fluidRow(
                        box(title = "Sizes",
                            numericInput(plotSize, label = "Plot Height (px):", 2400, min = 480, max = 64000),
                            numericInput(numCols, label = "Number of facet columns:", 4, min = 1, max = 42)
                        ),
                        box(title = "Layout",
                            numericInput(minY, label = "Y-Axis min:", -10, min = -1, max = 1000),
                            numericInput(maxY, label = "Y-Axis max:", 10, min = 1, max = 1000),
                            checkboxInput(plotTime, label = "Plot absolute timings", value = FALSE)
                        )
                      ),
                      fluidRow(
                        uiOutput(ui)
                      )
             )
      )
    )
  )
}

speedup <- function(input, output, session, db, exps) {
  output$ui = renderUI({
    ns <- session$ns
    plotOutput(ns("speedup_plot"), width = "100%", height = input$plotSize)
  })

  output$speedup_plot = renderPlot({
    validate(
      need(input$baseline, "Select a RAW-compatible experiment as baseline first."),
      need(input$jitExperiments, "Select a JIT-compatible experiment first."),
      need(input$minY, "Select a minimum for y-axis."),
      need(input$minY, "Select a maximum for y-axis.")
    )

    papi <- trim(input$papiExperiments)
    if (nchar(papi) <= 1 || !input$plotAmdahl) {
      papi <- NULL
    }

    d <- speedup_data(db(),
                      input$baseline,
                      input$jitExperiments,
                      papi,
                      input$projects,
                      input$groups)

    if (nrow(d) > 0) {
      p <- ggplot(data=d, aes(x = cores, y = speedup_corrected, fill = cores, color = cores))

      if (input$plotTime) {
        p <- p + geom_line(aes(y = time), color = "red") +
          geom_line(aes(y = ptime), color = "green") +
          ylab("Runtime in [s]")
      } else {
        p <- p + geom_bar(aes(color = cores), stat = "identity") +
          geom_point(aes(color = cores)) +
          geom_smooth(color = "red") +
          geom_hline(yintercept=1, colour="blue") +
          geom_abline(slope=1, intercept=0, colour="green")
        if (input$plotAmdahl && !is.null(papi)) {
          p <- p + geom_line(aes(x = cores, y = speedup_amdahl), color = "orange")
        }
        p <- p + coord_cartesian(ylim=c(input$minY, input$maxY)) +
          scale_x_discrete() +
          ylab("Speedup Factor")
      }

      p <- p + facet_wrap(~ project_name, ncol = input$numCols) + xlab("Number of cores")
      p
    }
  })

  output$table = renderDataTable({
    validate(
      need(input$baseline, "Select a RAW-compatible experiment as baseline first."),
      need(input$jitExperiments, "Select a JIT-compatible experiment first.")
    )
    papi <- trim(as.character(input$papiExperiments))

    if (nchar(papi) <= 1 || !input$plotAmdahl) {
      papi <- NULL
    }

    return(speedup_data(db(),
                   input$baseline,
                   input$jitExperiments,
                   papi,
                   input$projects,
                   input$groups))
    },
    options = list(
      style = 'bootstrap',
      class = 'table-condensed'
    )
  )

  observe({
      db <- db()
      exps <- exps()
      projects <- projects(db)

      updateSelectInput(session, "groups", choices = groups(db), selected = 0)
      updateSelectInput(session, "jitExperiments", choices = c(getSelections("polyjit", exps),
                                                               getSelections("pj-raw", exps),
                                                               getSelections("polly-openmp", exps),
                                                               getSelections("polly-openmpvect", exps),
                                                               getSelections("polly-vectorize", exps),
                                                               getSelections("polly", exps)), selected = 0)
      updateSelectInput(session, "projects", choices = projects, selected = 0)
      updateSelectInput(session, "papiExperiments", choices = c(getSelections("pj-papi", exps),
                                                                getSelections("papi", exps)), selected = 0)
      updateSelectInput(session, "baseline", choices = c(getSelections(NULL, exps)), selected = 0)
  })
}

speedupMenu <- function() {
  menuItem("Speedup (Experiment)", tabName = "speedupExperiment")
}

speedup_data <- function(c, base, jit, papi, projects = NULL, groups = NULL) {
  extra_filter <- ""
  range_filter <- ""
  if (!is.null(projects)) {
    extra_filter <- sprintf("AND project.name IN (%s)",
                            paste(lapply(as.vector(projects),
                                         function(x) sprintf("\'%s\'", x)),
                                  collapse=", "))
  }
  if (!is.null(groups)) {
    extra_filter <- paste(extra_filter,
                          sprintf(" AND project.group_name IN (%s)",
                            paste(lapply(as.vector(groups),
                                         function(x) sprintf("\'%s\'", x)),
                                  collapse=", ")))
  }

  q <- ""
  if (!is.null(papi)) {
    cat("query-speedup (papi)\n val: (", paste(papi), ")\n")
    q <- query_speedup_papi(extra_filter, base, jit, papi)
  } else {
    cat("query-speedup (no papi)\n")
    q <- query_speedup_no_papi(extra_filter, base, jit)
  }
  return(sql.get(c, q))
}
