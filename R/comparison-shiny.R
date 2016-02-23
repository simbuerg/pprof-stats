comparisonUI <- function(id, label = "Comparison") {
  ns <- NS(id)
  baselines <- ns("baselines")
  experiments <- ns("experiments")
  projects <- ns("projects")
  groups <- ns("groups")
  single_ui <- ns("single_ui")
  pairwise_ui <- ns("pairwise_ui")

  tagList(
    fluidRow(
      box(title = "Experiments", solidHeader = TRUE, width = 6,
          selectInput(baselines, label = "Baselines", multiple = TRUE, choices = NULL, width = '100%'),
          selectInput(experiments, label = "Experiments", multiple = TRUE, choices = NULL, width = '100%')),
      box(title = "Filters", solidHeader = TRUE, width = 6,
          selectInput(projects, label = "Projects", multiple = TRUE, choices = NULL, width = '100%'),
          selectInput(groups, label = "Groups", multiple = TRUE, choices = NULL, width = '100%'))
    ),
    fluidRow(
      tabBox(title = 'Compare experiments', width = 12,
             tabPanel("Single-Core", id = "tabBox",
                      paste("Comparison between the single-core configuration of each possible
                                         baseline and all configurations of the run-time experiments."),
                      htmlOutput(single_ui)
             ),
             tabPanel("Pairwise",
                      paste("For each configuration of each experiment compare it to a matching
                                         configuration for each experiment. A configuration matches, if it
                                         uses the same number of cores."),
                      htmlOutput(pairwise_ui)
             )
      )
    )
  )
}

comparisonMenu <- function() {
  menuItem("Comparison", tabName = "comparison")
}

# Comparison module server function
comparison <- function(input, output, session, db, exps) {
  base_vs_pivot <- reactive({
    validate(
      need(input$baselines, "Select baselines to compare."),
      need(input$experiments, "Select experiments to compare.")
    )
    if (length(input$baselines) == 0 ||
        length(input$experiments) == 0 )
      return(NULL)

    baseline_vs_pivot(db(), input$baselines, input$experiments, input$projects, input$groups)
  })

  compHeight <- reactive({
    if (is.null(input$experiments))
      h <- 0
    else
      h <- length(input$experiments)
    return(h)
  })

  output$single_ui <- renderUI({
    ns <- session$ns
    plotOutput(ns("single"), height = 400 * compHeight())
  })

  output$pairwise_ui <- renderUI({
    ns <- session$ns
    plotOutput(ns("pairwise"), height = 400 * compHeight())
  })

  output$single = renderPlot({
    d <- base_vs_pivot()
    p <- ggplot(data = d, aes(x = gname, y = speedup, colour = pid)) +
      geom_boxplot(aes(group = gname), outlier.size = 0) +
      facet_grid(bid ~ num_cores) +
      scale_x_discrete() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    p
  })

  output$pairwise = renderPlot({
    d <- base_vs_pivot()
    p <- ggplot(data = d, aes(x = num_cores, y = speedup, colour = pid)) +
      geom_boxplot(aes(group = num_cores), outlier.size = 0) +
      facet_grid(gname ~ bid) +
      scale_x_discrete() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    p
  })

  observe({
    db <- db()
    exps <- exps()
    projects = projects(db)
    groups = groups(db)

    updateSelectInput(session, "baselines", choices = c(getSelections(NULL, exps)), selected = 0)
    updateSelectInput(session, "experiments", choices = c(getSelections(NULL, exps)), selected = 0)
    updateSelectInput(session, "projects", choices = projects, selected = 0)
    updateSelectInput(session, "groups", choices = groups, selected = 0)
  })

  return(NULL)
}
