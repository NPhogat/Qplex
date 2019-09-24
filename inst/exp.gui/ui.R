library(shiny)

shinyUI(fluidPage(

  titlePanel("Analysis of the data of lateral flow duplex assay"),

  sidebarLayout(

    sidebarPanel(

      fileInput("file1","Choose .csv or .txt tab separated file 1 to upload"),

      tags$hr(),

      fileInput("file2","Choose .csv or .txt tab separated file 2 to upload"),

      tags$hr(),

      checkboxInput("header","Header", TRUE),

      selectInput("filetype", "Select the type of both files", choices = c(".csv",".txt")),

      br(),

      selectInput("decp1", "Select the type of separation for file 1", choices = c(",",".")),

      br(),

      selectInput("decp2", "Select the type of separation for file 2", choices = c(",",".")),

      br(),

      numericInput("yin", "Intensity value to compute concentration","1"),

      br(),

      numericInput("mslp", "Slope value to compute concentration","1"),

      br(),

      numericInput("cint", "Y- Intercept value to compute concentration","1"),

      br(),

      actionButton("compute","COMPUTE the analysis results!"),

      p("Click on COMPUTE! button to compute the results"),

      br(),

      downloadButton("download", "Download Results")

    ),

    mainPanel(

      uiOutput("tabset")

    )

  )

))
