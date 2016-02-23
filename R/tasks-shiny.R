taskTableRenderOpts <- DT::JS(
        "function(data, type, row, meta) {",
        "  if (type === 'display') {",
        "    style = 'label-default'; icon = 'glyphicon-ok';",
        "    if (data === 'completed') { style = 'label-success'; icon = 'glyphicon-ok'; }",
        "    if (data === 'running') { style = 'label-primary'; icon = 'glyphicon-refresh'; }",
        "    if (data === 'failed') { style = 'label-danger'; icon = 'glyphicon-remove'; }",
        "    return '<span class=\"label '+ style +'\" title=\"' + data + '\"><span class=\"glyphicon '+ icon +'\"></span></span>';",
        "  } else {",
        "    return data;",
        "  }",
        "}")

taskTableOpts <- list(
    pageLength = 50,
    rownames = TRUE,
    columnDefs = list(
      list(targets = 5, render = taskTableRenderOpts
      )))

tasksUI <- function(id, label = "Tasks") {
  ns <- NS(id)

  tagList(
    fluidRow(
      selectInput(ns("baseline"), label = "Baseline", multiple = FALSE, choices = NULL, width = '100%'),
      box(title = "Summary", width = 12,
          infoBoxOutput(ns("groupsCompleted")),
          infoBoxOutput(ns("groupsFailed")),
          infoBoxOutput(ns("groupsCount")),
          infoBoxOutput(ns("tasksCompleted")),
          infoBoxOutput(ns("tasksFailed")),
          infoBoxOutput(ns("tasksCount"))
      )
    ),
    fluidRow(
      tabBox(title = "Tool status output", width = 12, height = 1200,
             tabPanel("Filter",
                      box(title = "Task Groups", width = 6,
                          dataTableOutput(ns("taskGroupTable"))
                      ),
                      box(title = "Tasks", width = 6,
                          dataTableOutput(ns("taskTable"))
                      )
             ),
             tabPanel("stdout",
                      verbatimTextOutput(ns("stdout"))),
             tabPanel("stderr",
                      verbatimTextOutput(ns("stderr")))
      )
    )
  )
}

tasks <- function(input, output, session, db, exps) {
  output$taskTable = renderDataTable({
    validate(
      need(input$baseline, "Select an experiment first.")
    )

    tg <- taskGroups(db(), input$baseline)
    if (length(input$taskGroupTable_rows_selected) > 0) {
      tg <- tg[input$taskGroupTable_rows_selected, ]
    }
    t <- tasks(db(), input$baseline, tg[, 1])
    return(t[,2:ncol(t)])
  }, options = list(
    pageLength = -1,
    rownames = FALSE,
    columnDefs = list(
      list(targets = 2, render = taskTableRenderOpts
      ))), style = 'bootstrap', class = 'table-c0ndensed', selection = 'single')

  output$taskGroupTable = renderDataTable({
    validate(
      need(input$baseline, "Select an experiment first.")
    )
    t <- taskGroups(db(), input$baseline)
    return(t[,2:ncol(t)])
  },
  options = list(
    pageLength = 50,
    rownames = FALSE,
    columnDefs = list(
      list(targets = 5, render = taskTableRenderOpts)
    )
  ), style = 'bootstrap', class = 'table-condensed')

  get_selected_run <- reactive({
    validate(
      need(input$baseline, "Select an experiment first.")
    )

    tg <- taskGroups(db(), input$baseline)
    if (length(input$taskGroupTable_rows_selected) > 0) {
      tg <- tg[input$taskGroupTable_rows_selected, ]
    }
    t <- tasks(db(), input$baseline, tg[, 1])
    r <- as.numeric(t[input$taskTable_rows_selected, 1])
    return(r)
  })
  output$groupsCompleted <- renderInfoBox({
    infoBox(
      "Completed (G)", icon = icon("thumbs-up", lib = "glyphicon"), color = "green"
    )
  })
  output$groupsFailed <- renderInfoBox({
    infoBox(
      "Failed (G)", icon = icon("thumbs-up", lib = "glyphicon"), color = "red"
    )
  })
  output$groupsCount <- renderInfoBox({
    infoBox(
      "Total (G)", icon = icon("thumbs-up", lib = "glyphicon"), color = "yellow"
    )
  })
  output$tasksCompleted <- renderInfoBox({
    infoBox(
      "Completed (T)", icon = icon("thumbs-up", lib = "glyphicon"), color = "green"
    )
  })
  output$tasksFailed <- renderInfoBox({
    infoBox(
      "Failed (T)", icon = icon("thumbs-up", lib = "glyphicon"), color = "red"
    )
  })
  output$tasksCount <- renderInfoBox({
    infoBox(
      "Count (T)", icon = icon("thumbs-up", lib = "glyphicon"), color = "yellow"
    )
  })
  output$stdout = renderText({
    validate(
      need(input$taskTable_rows_selected, "No selection yet.")
    )
    r <- get_selected_run()

    if (!is.null(r)) {
      return(paste("\n", stdout(db(), r)))
    }

    return("No stdout found.")
  })
  output$stderr = renderText({
    validate(
      need(input$taskTable_rows_selected, "No selection yet.")
    )
    r <- get_selected_run()

    if (!is.null(r)) {
      return(paste("\n", stderr(db(), r)))
    }

    return("No stderr found.")
  })

  observe({
    exps <- exps()
    updateSelectInput(session, "baseline", choices = c(getSelections(NULL, exps)), selected = 0)
  })
}

tasksMenu <- function() {
  menuItem("Tasks", tabName = "tasks")
}
