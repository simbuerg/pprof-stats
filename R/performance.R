performanceUI <- function(id, label = "Performance") {
  ns <- NS(id)
  baselines <- ns("baselines")
  experiments <- ns("experiments")
  projects <- ns("projects")
  groups <- ns("groups")
  regions <- ns("regions")
  region_sup_ui <- ns("region_sup_ui")
  region_sup_pcore_ui <- ns("region_sup_pcore_ui")
  region_proportion_pexp_ui <- ns("region_proportion_pexp_ui")
  region_proportion_ppro_ui <- ns("region_proportion_ppro_ui")

  tagList(
    fluidRow(
      box(title = "Experiments", solidHeader = TRUE, width = 6,
          selectInput(baselines, label = "Baselines", multiple = TRUE, choices = NULL, width = '100%'),
          selectInput(experiments, label = "Experiments", multiple = TRUE, choices = NULL, width = '100%')),
      box(title = "Filters", solidHeader = TRUE, width = 6,
          selectInput(projects, label = "Projects", multiple = TRUE, choices = NULL, width = '100%'),
          selectInput(groups, label = "Groups", multiple = TRUE, choices = NULL, width = '100%'),
          selectInput(regions, label = "Regions", multiple = TRUE, choices = NULL, width = '100%'))
    ),
    fluidRow(
      tabBox(title = 'Compare experiments', width = 12,
             tabPanel("Regions speedup",
                      paste("For each configuration of each experiment compare it to a matching
                                         configuration for each experiment. A configuration matches, if it
                                         uses the same number of cores. #TODO rewrite"),
                      htmlOutput(region_sup_ui)
             ),
             tabPanel("Regions speedup per core",
                      paste("For each configuration of each experiment compare it to a matching
                                         configuration for each experiment. A configuration matches, if it
                                         uses the same number of cores. #TODO rewrite"),
                      htmlOutput(region_sup_pcore_ui)
             ),
             tabPanel("Region proportions per experiment",
                      paste("For each configuration of each experiment compare it to a matching
                            configuration for each experiment. A configuration matches, if it
                            uses the same number of cores. #TODO rewrite"),
                      htmlOutput(region_proportion_pexp_ui)
                      ),
             tabPanel("Region proportions per project",
                      paste("For each configuration of each experiment compare it to a matching
                            configuration for each experiment. A configuration matches, if it
                            uses the same number of cores. #TODO rewrite"),
                      htmlOutput(region_proportion_ppro_ui)
             )

      )
    )
  )
}

performanceMenu <- function() {
  menuItem("Performance", tabName = "performance")
}

# Performance module server function
performance <- function(input, output, session, db, exps) {
  base_vs_pivot <- reactive({
    validate(
      need(input$baselines, "Select baselines to compare."),
      need(input$experiments, "Select experiments to compare.")
    )
    if (length(input$baselines) == 0 ||
        length(input$experiments) == 0 )
      return(NULL)

    baseline_vs_pivot_perf(db(), input$baselines, input$experiments, input$projects, input$groups, input$regions)
  })

  regions_portion <- reactive({
    validate(
      need(input$baselines, "Select baselines to compare.")
    )
    if (length(input$baselines) == 0)
      return(NULL)

    regions_data(db(), input$baselines, input$experiments, input$projects, input$groups, input$regions)
  })

  compHeight <- reactive({
    if (is.null(input$experiments))
      h <- 0
    else
      h <- length(input$experiments)
    return(h)
  })

  output$region_sup_ui <- renderUI({
    ns <- session$ns
    plotOutput(ns("region_sup"), height = 400 * compHeight())
  })

  output$region_sup_pcore_ui <- renderUI({
    ns <- session$ns
    plotOutput(ns("region_sup_pcore"), height = 400 * compHeight())
  })

  output$region_proportion_pexp_ui <- renderUI({
    ns <- session$ns
    plotOutput(ns("region_proportion_pexp"), height = 800 * compHeight())
  })

  output$region_proportion_ppro_ui <- renderUI({
    ns <- session$ns
    plotOutput(ns("region_proportion_ppro"), height = 3200 * compHeight())
  })

  output$region_sup = renderPlot({
    d <- base_vs_pivot()
    print(head(d))
    p <- ggplot(data = d, aes(x = num_cores, y = speedup, colour = pid)) +
      geom_boxplot(aes(group = num_cores), outlier.size = 0) +
      facet_grid(gname ~ bid) +
      scale_x_discrete() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    p
  })

  output$region_sup_pcore = renderPlot({
    d <- base_vs_pivot()
    p <- ggplot(data = d, aes(x = num_cores, y = speedup, colour = pid)) +
      geom_boxplot(aes(group = num_cores), outlier.size = 0) +
      facet_grid(gname ~ bid) +
      scale_x_discrete() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    p
  })

  output$region_proportion_pexp = renderPlot({
    #d <- base_vs_pivot()
    d <- regions_portion()
    print(head(d))
    #print(d)
    #p <- ggplot(data = d, aes(x = num_cores, y = speedup, colour = pid)) +
    #  geom_boxplot(aes(group = num_cores), outlier.size = 0) +
    #  facet_grid(gname ~ bid) +
    #  scale_x_discrete() +
    #  theme(axis.text.x = element_text(angle = 90, hjust = 1))
    p <- ggplot(data = d, aes(x = pname, y = proportion)) +
      geom_bar(aes(fill = region_name), stat="identity") +
      facet_grid(experiment_group ~ .)
    p
  })

  output$region_proportion_ppro = renderPlot({
    #d <- base_vs_pivot()
    d <- regions_portion()
    print(head(d))
    #print(d)
    #p <- ggplot(data = d, aes(x = num_cores, y = speedup, colour = pid)) +
    #  geom_boxplot(aes(group = num_cores), outlier.size = 0) +
    #  facet_grid(gname ~ bid) +
    #  scale_x_discrete() +
    #  theme(axis.text.x = element_text(angle = 90, hjust = 1))
    p <- ggplot(data = d, aes(x = experiment_group, y = proportion)) +
      geom_bar(aes(fill = region_name), stat="identity") +
      facet_wrap(~ pname, ncol = 4)
    p
  })

  observe({
    db <- db()
    exps <- exps()
    projects = projects(db)
    groups = groups(db)
    regions = regions(db)

    updateSelectInput(session, "baselines", choices = c(getSelections(NULL, exps)), selected = 0)
    updateSelectInput(session, "experiments", choices = c(getSelections(NULL, exps)), selected = 0)
    updateSelectInput(session, "projects", choices = projects, selected = 0)
    updateSelectInput(session, "groups", choices = groups, selected = 0)
    updateSelectInput(session, "regions", choices = regions, selected = 0)
  })

  return(NULL)
}
