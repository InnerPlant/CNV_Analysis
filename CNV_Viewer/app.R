#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
my_packages <- c("tidyverse","ggpubr","kableExtra","lookup","readxl","shiny","DT","reticulate","data.table","shinydashboard")
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]    # Extract not installed packages
if(length(not_installed)) install.packages(not_installed)
library(shiny)
library(DT)
library(reticulate)
library(tidyverse)
library(ggpubr)
library(kableExtra)
library(lookup)
library(readxl)
library(data.table)
library(shinydashboard)
use_condaenv("r-reticulate")
#py_run_file("plantsTable_RM.py")

source_python("/Users/rmitchell/Documents/CNV_Genotyping_Vis/CNV_Visual/cnv_df.py")

data <- df_cnv %>% left_join(df_plant, by = "Barcode")


# Define UI for application that draws a histogram
ui = fluidPage(
  titlePanel("CNV Viewer"),
  sidebarLayout(
    sidebarPanel(checkboxGroupInput('targets',"Select Target", choices = c("Copy_Number_bFLO","Copy_Number_Selection" ), selected = c("Copy_Number_bFLO","Copy_Number_Selection" )),
                 radioButtons('targets2',"Select sorting",choices = list("Copy_Number_Mean_bflo","Copy_Number_Mean_Selection")),
                 width = 2
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Plots",
          selectInput('parentplant',"Parent Plant Line","Pick Line"),
          plotOutput("plots", dblclick="plot_dblclick",brush = brushOpts(id = "plot_dblclick", resetOnNew = TRUE)),
          downloadButton(outputId = 'plotdwnld',label="Download Plot"),
          dataTableOutput('filteredtbl')
        ),
        tabPanel(
          title = "Plants",
          dataTableOutput('df_plants')
        ),
        tabPanel(
          title = "CNV",
          dataTableOutput('datatbl')
        )
                 
      
      )
    ,width = 10)  
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

   output$df_plants <- renderDataTable(df_plant)
   output$cnv <- renderDataTable(df_cnv)
   
   data <- reactive(df_cnv %>% select(-id) %>% left_join(select(df_plant, -id), by = "Barcode"))
   output$datatbl <- renderDataTable(data())
   
   
   observe({updateSelectInput(session = session,inputId = 'parentplant', label = "Parent Plant Line", choices = distinct(data(),`Parent Plant Line`), selected = distinct(data(),`Parent Plant Line`))})
   colors <- c("black","red")
   ranges <- reactiveValues(x = NULL, y = NULL)
   #Zoomable
   #plot_fun<- function(data,title){data %>% filter(`Parent Plant Line`== title) %>% mutate(Sample = fct_reorder(Barcode,Copy_Number_Mean_bflo)) %>% ggplot() + stat_summary(fun = "mean",aes(Sample,Copy_Number_bFLO), geom = "point", color = "black") + stat_summary(fun.data = "mean_se", aes(Sample,Copy_Number_bFLO), geom = "errorbar", color = "black") + geom_point(aes(Sample,Copy_Number_bFLO), color = "grey", alpha = 0.4) +stat_summary(fun = "mean",aes(Sample,Copy_Number_Selection), geom = "point", color = "red") + stat_summary(fun.data = "mean_se", aes(Sample,Copy_Number_Selection), geom = "errorbar", color = "red") + geom_point(aes(Sample,Copy_Number_Selection), color = "red", alpha = 0.4)+ labs(title=title, y = "Copy Number", x = NULL) + theme_pubclean(base_size = 12) + theme(axis.text.x=element_text(angle = 90, hjust = 0), legend.position="right") + scale_color_manual(colors)+ coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)}
   
   plot_fun<- function(data,title){data %>% filter(`Parent Plant Line`== title) %>% gather(key = "Target", value = "Copy_Number",Copy_Number_bFLO,Copy_Number_Selection ) %>% filter(Target %in% as.vector(input$targets)) %>% mutate(Sample = fct_reorder(Barcode,.[[input$targets2]])) %>% ggplot() + stat_summary(fun = "mean",aes(Sample,Copy_Number, color = Target), geom = "point") + stat_summary(fun.data = "mean_se", aes(Sample,Copy_Number, color = Target), geom = "errorbar")+ geom_point(aes(Sample,Copy_Number, color = Target), alpha = 0.4) + scale_color_manual(labels=c("bFLO","Selection"), breaks = c("Copy_Number_bFLO","Copy_Number_Selection"),values = colors) + labs(title=title, y = "Copy Number", x = NULL) + theme_pubclean(base_size = 12) + theme(axis.text.x=element_text(angle = 90, hjust = 0), legend.position="right") + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)}
   
   plot <- reactive({plot_fun(data(),input$parentplant)})
   
   output$plots <- renderPlot({plot_fun(data(),input$parentplant)}, res = 96)
   
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
   filtereddata<-reactive(data() %>% filter(`Parent Plant Line` == input$parentplant))
   output$filteredtbl <- renderDataTable(filtereddata())
}

# Run the application 
shinyApp(ui = ui, server = server)
