#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# CNV_Viewer
#my_packages <- c("tidyverse","ggpubr","kableExtra","lookup","readxl","shiny","DT","reticulate","data.table","shinydashboard")
#not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]    # Extract not installed packages
#if(length(not_installed)) install.packages(not_installed)
library(shiny)
library(DT)
library(reticulate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
#library(ggpubr)
#library(kableExtra)
library(lookup)
library(readxl)
library(data.table)
#library(shinydashboard)
#install_miniconda(path = "/home/app")
#conda_create(envname = "r-reticulate")
#py_install("pyairtable",pip = TRUE,envname = "r-reticulate")
#py_install("numpy", pip = TRUE,envname = "r-reticulate")
#py_install("pandas", pip = TRUE,envname = "r-reticulate")          
use_condaenv("r-reticulate")
#use_condaenv(conda = "/root/miniconda3/bin/conda", condaenv = "r-reticulate")
py_run_file("plantsTable_RM.py")

source("shiny_test_concat.R")
import_test <- function(x) {read_xlsx(x, skip =18) %>% mutate("Plate"=x)}


ui = fluidPage(
  titlePanel("CNV Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', label='Choose bFLO file',
                accept = c(".xlsx"),multiple=TRUE),
      fileInput('file2', 'Choose Selection file',
                accept = c(".xlsx"),multiple = TRUE),
      textInput('text1','Experiment',placeholder = "CNV.#.##"),
      actionButton("run_button","Run Analysis",icon=icon("play"))),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Analysis",
          downloadButton('final_airtabledl',label = "Download for Airtable"),
          downloadButton('final_distinctdl',label = "Download Deduplicated Data"),
          dataTableOutput('final')
        ),
        tabPanel(
          title = "bFLO",
          dataTableOutput('bflo_concat')
        ),
        tabPanel(
          title = "Selection",
          dataTableOutput('sel_concat')
        ),
        tabPanel(title="Plots",
                 selectInput('parentplant',"Parent Plant Line","Pick Line"),
                 checkboxGroupInput('targets',"Select Target", choices = c("Copy_Number_bflo","Copy_Number_sel" ), selected = c("Copy_Number_bflo","Copy_Number_sel" )),
                 radioButtons('targets2',"Select sorting",choices = list("Copy_Number_Mean_bflo","Copy_Number_Mean_sel")),
                 plotOutput("plots", dblclick="plot_dblclick",brush = brushOpts(id = "plot_dblclick", resetOnNew = TRUE)),
                 downloadButton(outputId = 'plotdwnld',label="Download Plot")
        )
      )
    )  
  )
)
server = function(input, output,session){
  
  # temp_bflo <-reactive({
  #   req(input$file1)
  #   read_xlsx(input$file1$datapath, skip = 18)
  # })
  # temp_sel <-reactive({
  #   req(input$file2)
  #   read_xlsx(input$file2$datapath, skip = 18)
  # })
  
  #Kinda Working, read in xlsx, append file name column, and then concatenate, cannot run shiny_test on themm
  temp_bflo <- reactive({
    temp = rbindlist(lapply(input$file1$datapath,import_test))
    as.data.frame(temp)
  })
  output$bflo_concat <- renderDataTable(temp_bflo())
  
  temp_sel <- reactive({
    temp =rbindlist(lapply(input$file2$datapath,import_test))
    as.data.frame(temp)
  })
  output$sel_concat <- renderDataTable(temp_sel())
  
  # output$bflo<-renderTable(temp_bflo())
  # output$sel <-renderTable(temp_sel())
  temp_expname <- reactive({print(input$text1)})
  # 
  
  data <- eventReactive(input$run_button, {
    if (!is.null(input$file1$datapath) & !is.null(input$file2$datapath)) {
      shiny_test(temp_bflo = temp_bflo(), temp_sel = temp_sel(), experiment_plate = temp_expname())
    } else if (!is.null(input$file1$datapath) & is.null(input$file2$datapath)) {
      bFlo_only(temp_bflo = temp_bflo(), experiment_plate = temp_expname())
    } else {
      Sel_only(temp_sel = temp_sel(), experiment_plate = temp_expname())
    }
  })
  
  
  output$final <-  renderDataTable(data(),)
  distinctdata <- reactive({distinct(data(),Sample,.keep_all = TRUE)})
  output$final_distinctdl <- downloadHandler(
    filename = function() {
      paste0(input$text1,".csv")
    },
    content = function(file){
      write.csv(distinctdata(),file)
    }
  )
  output$final_airtabledl <- downloadHandler(
    filename = function() {
      paste0(input$text1,".csv")
    },
    content = function(file){
      write.csv(data(),file)
    }
  )
  observe({updateSelectInput(session = session,inputId = 'parentplant', label = "Parent Plant Line", choices = distinct(data(),`Parent Plant Line`), selected = distinct(data(),`Parent Plant Line`))})
  colors <- c("black","red")
  ranges <- reactiveValues(x = NULL, y = NULL)
  #Zoomable
  #plot_fun<- function(data,title){data %>% filter(`Parent Plant Line`== title) %>% mutate(Sample = fct_reorder(Sample,Copy_Number_Mean_bflo)) %>% ggplot() + stat_summary(fun = "mean",aes(Sample,Copy_Number_bflo), geom = "point", color = "black") + stat_summary(fun.data = "mean_se", aes(Sample,Copy_Number_bflo), geom = "errorbar", color = "black") + geom_point(aes(Sample,Copy_Number_bflo), color = "grey", alpha = 0.4) +stat_summary(fun = "mean",aes(Sample,Copy_Number_sel), geom = "point", color = "red") + stat_summary(fun.data = "mean_se", aes(Sample,Copy_Number_sel), geom = "errorbar", color = "red") + geom_point(aes(Sample,Copy_Number_sel), color = "red", alpha = 0.4)+ labs(title=title, y = "Copy Number", x = NULL) + theme_pubclean(base_size = 12) + theme(axis.text.x=element_text(angle = 90, hjust = 0), legend.position="right") + scale_color_manual(colors)+ coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)}
  plot_fun<- function(data,title){data %>% filter(`Parent Plant Line`== title) %>% gather(key = "Target", value = "Copy_Number",Copy_Number_bflo,Copy_Number_sel ) %>% filter(Target %in% as.vector(input$targets)) %>% mutate(Sample = fct_reorder(Sample,.[[input$targets2]])) %>% ggplot() + stat_summary(fun = "mean",aes(Sample,Copy_Number, color = Target), geom = "point") + stat_summary(fun.data = "mean_se", aes(Sample,Copy_Number, color = Target), geom = "errorbar")+ geom_point(aes(Sample,Copy_Number, color = Target), alpha = 0.4) + scale_color_manual(labels=c("bFLO","Selection"), breaks = c("Copy_Number_bflo","Copy_Number_sel"),values = colors) + labs(title=title, y = "Copy Number", x = NULL) + theme_classic(base_size = 12) + theme(axis.text.x=element_text(angle = 90, hjust = 0), legend.position="right") + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)}
  
  plot_bFlo <- function(data,title){data %>% filter(`Parent Plant Line`== title) %>% gather(key = "Target", value = "Copy_Number",Copy_Number_bflo ) %>% filter(Target %in% as.vector(input$targets)) %>% mutate(Sample = fct_reorder(Sample,.[[input$targets2]])) %>% ggplot() + stat_summary(fun = "mean",aes(Sample,Copy_Number, color = Target), geom = "point") + stat_summary(fun.data = "mean_se", aes(Sample,Copy_Number, color = Target), geom = "errorbar")+ geom_point(aes(Sample,Copy_Number, color = Target), alpha = 0.4) + scale_color_manual(labels=c("bFLO"), breaks = c("Copy_Number_bflo"),values = colors) + labs(title=title, y = "Copy Number", x = NULL) + theme_classic(base_size = 12) + theme(axis.text.x=element_text(angle = 90, hjust = 0), legend.position="right") + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)}
  
  plot_Sel <- function(data,title){data %>% filter(`Parent Plant Line`== title) %>% gather(key = "Target", value = "Copy_Number",Copy_Number_sel ) %>% filter(Target %in% as.vector(input$targets)) %>% mutate(Sample = fct_reorder(Sample,.[[input$targets2]])) %>% ggplot() + stat_summary(fun = "mean",aes(Sample,Copy_Number, color = Target), geom = "point") + stat_summary(fun.data = "mean_se", aes(Sample,Copy_Number, color = Target), geom = "errorbar")+ geom_point(aes(Sample,Copy_Number, color = Target), alpha = 0.4) + scale_color_manual(labels=c("Selection"), breaks = c("Copy_Number_sel"),values = "red") + labs(title=title, y = "Copy Number", x = NULL) + theme_classic(base_size = 12) + theme(axis.text.x=element_text(angle = 90, hjust = 0), legend.position="right") + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)}
  
  #plot <- if(!isTruthy(reactive(input$file2))){reactive({plot_fun(data(),input$parentplant)})}else{reactive({plot_bFlo(data(),input$parentplant)})}
  
  output$plots <- renderPlot({
    if (!is.null(input$file1$datapath) & !is.null(input$file2$datapath)) {
      plot_fun(data(), input$parentplant)
    } else if (!is.null(input$file1$datapath) & is.null(input$file2$datapath)) {
      plot_bFlo(data(), input$parentplant)
    } else {
      plot_Sel(data(), input$parentplant)
    }
  }, res = 96)
  
  
  if(!isTruthy(reactive(input$file2))){renderPlot({plot_fun(data(),input$parentplant)}, res = 96)} 
  else{renderPlot({plot_bFlo(data(),input$parentplant)}, res = 96)}
  
  observeEvent(input$plot_dblclick,{
    brush <- input$plot_dblclick
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  output$plotdwnld <- downloadHandler(filename = function() {paste(input$parentplant,".pdf",sep="")},
                                      content = function(file) {
                                        ggsave(filename = file, plot = plot(),height = 8.15, width = 11, dpi = 300)})
  
}

# Run the application 
shinyApp(ui = ui, server = server)