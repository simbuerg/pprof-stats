speedupPerProjectUI <- function(id, label = "Speedup per project") {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(width = 4,
             selectInput(ns("projects_per"), label = "Project", multiple = FALSE, choices = NULL, width = '100%')),
      column(width = 4,
             selectInput(ns("baseline"), label = "Baseline", multiple = FALSE, choices = NULL, width = '100%'))),
    fluidRow(
      column(width = 12,
             h4("Speedup measurements"),
             plotOutput(ns("t1Plot")))),
    fluidRow(
      column(width = 12,
             h4("Available experiments"))),
    fluidRow(
      column(width = 12, dataTableOutput(ns("t1")))
    )
  )
}

speedupPerProject <- function(input, output, session, db, exps) {
  expTable <- reactive({ get_experiments_per_project(db(), input$projects_per) })

  output$t1 = renderDataTable({
    validate(
      need(input$projects_per, "Select a project first.")
    )

    t <- expTable()
    rownames(t) <- t[,1]

    return(t[,2:3])
  },
  options = list(
    style = 'bootstrap',
    class = 'table-condensed',
    paging = FALSE,
    server = TRUE
  ))

  output$t1Plot = renderPlot({
    validate(
      need(input$baseline, "Select a baseline for plotting."),
      need(input$projects_per, "Select a project first.")
    )

    t <- expTable()
    if (length(input$t1_rows_selected) > 0) {
      t <- t[input$t1_rows_selected, ]
    }

    d <- speedup_per_project(db(), input$projects_per, input$baseline, t[, 1])

    if (nrow(d) > 0) {
      p <- ggplot(data=d, aes(x = cores, y = speedup_corrected, group=experiment_group))
      p <- p +
        geom_bar(aes(color = experiment_group,
                     fill = experiment_group),
                 stat = "identity", position = "dodge") +
        #geom_line(aes(color = experiment_group)) +
        #geom_smooth(color = "red") +
        geom_hline(yintercept=1, colour="blue") +
        geom_abline(slope=1, intercept=0, colour="green") +
        coord_cartesian(ylim=c(input$minY, input$maxY)) +
        scale_x_discrete() +
        ylab("Speedup Factor")

      p <- p + facet_wrap(~ project_name, ncol = input$numCols) + xlab("Number of cores")
      p
    }
  })

  observe({
    db <- db()
    exps <- exps()
    projects <- projects(db)
    updateSelectInput(session, "projects_per", choices = projects, selected = 0)
    updateSelectInput(session, "baseline", choices = c(getSelections(NULL, exps)), selected = 0)
  })
}

speedupPerProjectMenu <- function() {
  menuItem("Speedup (Project)", tabName = "speedupProject")
}
