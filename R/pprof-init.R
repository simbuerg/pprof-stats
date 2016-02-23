init <- function() {
  if (!require("devtools")) {
    install.packages("devtools")
    library(devtools)
  }
  if (!require("ggplot2")) {
    install.packages("ggplot2")
    library(ggplot2)
  }
  if (!require("reshape2")) {
    install.packages("reshape2")
    library("reshape2")
  }
  if (!require("scales")) {
    install.packages("scales")
    library(scales)
  }
  if (!require("DBI")) {
    devtools::install_github("rstats-db/DBI")
    library(DBI)
  }
  if (!require("RPostgres")) {
    devtools::install_github("rstats-db/RPostgres")
    library(RPostgres)
  }
  if (!require("shiny")) {
    install.packages("shiny")
    library(shiny)
  }
  if (!require("DT")) {
    devtools::install_github("rstudio/DT")
    library(DT)
  }
  if (!require("sm")) {
    install.packages('sm')
    library(sm)
  }
  if (!require("vioplot")) {
    install.packages('vioplot')
    library(vioplot)
  }

  if (!require("shinydashboard")) {
    install.packages("shinydashboard")
    library(shinydashboard)
  }
}
