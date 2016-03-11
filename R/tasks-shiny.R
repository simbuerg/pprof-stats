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

tasksUI <- function(id, label = "Tasks") {
  ns <- NS(id)

  tagList(
    fluidRow(box(title = "Filter", width = 12,
        selectInput(ns("experiment"), label = "Experiment", multiple = FALSE, choices = NULL)
      )
    ),
    fluidRow(box(title = "Information", width = 12,
        box(title="Group Stats", width = 6,
            plotOutput(ns("groupStats"))
        ),
        box(title="Single Stats", width = 6,
            plotOutput(ns("singleStats"))
        )
      )
    ),
    fluidRow(box(title = "Task Groups", width = 12,
                 dataTableOutput(ns("taskGroupTable")))),
    fluidRow(box(title = "Tasks", width = 12,
                 dataTableOutput(ns("taskTable")))),
    fluidRow(box(title = "stdout", width = 12,
                 verbatimTextOutput(ns("stdout")))),
    fluidRow(box(title = "stderr", width = 12,
                 verbatimTextOutput(ns("stderr")))
    )
  )
}

tasks <- function(input, output, session, db, exps) {
  task.groups <- reactive({
    validate(need(input$experiment, "Select an experiment first."))
    return(db.taskGroups(db(), input$experiment))
  })

  task.tasks <- function(db, experiment, task_groups) {
    return(db.tasks(db, experiment, task_groups[, 1]))
  }

  task.run <- function(db, experiment, selected_task_groups, selected_task) {
    t.groups <- db.taskGroups(db(), input$experiment)
    if (length(selected_task_groups) > 0)
      t.groups <- t.groups[selected_task_groups,]

    t.tasks <- db.tasks(db, input$experiment, t.groups[, 1])
    r <- as.numeric(t.tasks[selected_task, 1])
    return(r)
  }

  output$taskTable = renderDataTable({
    validate(need(input$experiment, "Select an experiment first."))
    t.groups <- db.taskGroups(db(), input$experiment)

    if (length(input$taskGroupTable_rows_selected) > 0)
      t.groups <- t.groups[input$taskGroupTable_rows_selected,]

    t.tasks <- task.tasks(db(), input$experiment, t.groups)
    return(t.tasks[, 2:ncol(t.tasks)])
  }, options = list(
    pageLength = 10,
    rownames = FALSE,
    columnDefs = list(list(targets = 2, render = taskTableRenderOpts))
  ), style = 'bootstrap', class = 'table-c0ndensed', selection = 'single')

  output$taskGroupTable = renderDataTable({
    validate(need(input$experiment, "Select an experiment first."))
    t.groups <- db.taskGroups(db(), input$experiment)
    return(t.groups[, 2:ncol(t.groups)])
  }, options = list(
    pageLength = 10,
    rownames = FALSE,
    columnDefs = list(list(targets = 5, render = taskTableRenderOpts))
  ), style = 'bootstrap', class = 'table-condensed')

  output$stdout = renderText({
    validate(need(input$taskTable_rows_selected, "No selection yet."))
    r <- task.run(db(), input$experiment, input$taskGroupTable_rows_selected, input$taskTable_rows_selected)

    if (!is.null(r)) {
      return(paste("\n", stdout(db(), r)))
    }

    return("No stdout found.")
  })
  output$stderr = renderText({
    validate(need(input$taskTable_rows_selected, "No selection yet."))
    r <- task.run(db(), input$experiment, input$taskGroupTable_rows_selected, input$taskTable_rows_selected)

    if (!is.null(r)) {
      return(paste("\n", stderr(db(), r)))
    }

    return("No stderr found.")
  })

  output$groupStats = renderPlot({
    validate(need(input$experiment, "Select an experiment first."))
    d <- db.groupStats(db(), input$experiment)

    p <- ggplot(d, aes(x = factor(1), y = value, fill = status)) +
      geom_bar(width = 1, stat="identity") +
      coord_polar("y")
    return(p)
  })

  output$singleStats = renderPlot({
    validate(need(input$experiment, "Select an experiment first."))
    d <- db.singleStats(db(), input$experiment)

    p <- ggplot(d, aes(x = factor(1), y = value, fill = status)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y")
    return(p)
  })

  observe({
    db <- db()
    exps <- exps()
    updateSelectInput(
      session,
      "experiment",
      choices = c(getSelections(NULL, exps)),
      selected = 0
    )
  })
}

db.groupStats <- function(c, exp) {
  q <- sprintf("select cast(coalesce(status, 'failed') as varchar) as status, count(status) as value from rungroup WHERE experiment = '%s' GROUP BY rungroup.status;", exp)
  return(sql.get(c, q))
}

db.singleStats <- function(c, exp) {
  q <- sprintf("select cast(coalesce(status, 'failed') as varchar) as status, count(status) as value from run WHERE experiment_group = '%s' GROUP BY run.status;", exp)
  return(sql.get(c, q))
}

tasksMenu <- function() {
  menuItem("Tasks", tabName = "tasks")
}
