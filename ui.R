### Super join MCQ 

library(shiny)
library(tidyverse)
library(ggridges)
library(lubridate)
library(glue)


# UI ---------------------------------------------------------------


ui <- fluidPage(
    
    shinyFeedback::useShinyFeedback(),
    
    titlePanel("SELECx - Combine MCQ Score"),
    hr(),
    
    fluidRow(
        column(8,
               
               helpText("1",tags$sup("st"),": Download .csv or .xlsx from SELECx, Rename that files to English (Short names)"),
               helpText("2",tags$sup("nd"),": Prepare student's ID file that has student's id, student's name
           as column names:\'ID',\'Name' respectively"),
           
           helpText("3",tags$sup("rd"),": Upload multiple files from SELECx"),
           
           fileInput("file", NULL, accept = c(".csv", ".xls",".xlsx"),buttonLabel = "Upload files",
                     placeholder = "choose file .csv or .xlsx",multiple = TRUE)
           
        ),
        
        column(4,
               downloadButton("download", "Download Data .xlsx")
               
        )
        
    ),
    
    fluidRow(
        column(6,
               helpText("4",tags$sup("th"),": Upload student's ID file and choose more column if you want"),
               fileInput("file_id", NULL, accept = c(".csv", ".xls",".xlsx"),buttonLabel = "Upload ID",
                         placeholder = "choose file .csv or .xlsx"),
               
               br(),
               
               checkboxInput("check_readjust","Readjust maximum score ?",value = F),
               tabsetPanel(
                   id = "tab_readjust",
                   type = "hidden",
                   tabPanel("not_show"),
                   tabPanel("show", 
                            helpText("Input new maximum score"),
                            br(),
                            uiOutput("readjust")
                   )
               )
               
               
               
        ),
        
        column(4,offset = 2,
               checkboxInput("add_cols","Add more column from ID file ?",value = FALSE),
               uiOutput("select")
               
               
        )
        
    ),
    hr(),
    
    
    fluidRow(
        column(4,selectInput("plot_opt", choices = c("Density plot" = "density",
                                                     "Histogram plot" = "hist"), 
                             selected = "density",label = NULL)
        ),
        column(4,
               tabsetPanel(
                   id = "tab_binwidth",
                   type = "hidden",
                   tabPanel("not_show"),
                   tabPanel("show",
                            sliderInput("binwidth","Binwidth:",value = 30,min = 0,
                                        max = 100)
                   )
               )),
        column(4,  htmlOutput("min_max"))
    ),
    
    plotOutput("plot", brush = brushOpts("plot_brush",direction = "x")),
    
    
    
    hr(),
    
    h3("Data"),
    br(),
    dataTableOutput("table"),
    
    hr(),
    h3("Missing names"),
    tableOutput("missing")
    
    
    
)

