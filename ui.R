library(shiny)
library(shinyjs)
library(shinydashboard)
#library(shinycssloaders)
library(ggtext)
library(shinyWidgets)
library(DT)
#library(data.table)
library(tidyverse)
library(ggthemes)
library(waiter)
#library(feather)
library(arrow)


## Read in datasets
liver_app <- read_parquet("data/feather_files/liver_app.parquet")
skm_app <- read_parquet("data/feather_files/skm_app.parquet")
heart_app <- read_parquet("data/feather_files/heart_app.parquet")
gwat_app <- read_parquet("data/feather_files/gwat_app.parquet")
bat_app <- read_parquet("data/feather_files/bat_app.parquet")
skmMG_app <- read_parquet("data/feather_files/skmMG_app.parquet")
atMG_app <- read_parquet("data/feather_files/atMG_app.parquet")
all_tissues_men <- read_parquet("data/feather_files/all_tissues_men.parquet")
all_tissues_mice<-read_parquet("data/feather_files/all_tissues_mice.parquet")
integrated<-read_parquet("data/feather_files/integrated.parquet")
integrated_wat<-read_parquet("data/feather_files/integrated_wat.parquet")
df <- read_parquet("data/feather_files/df.parquet")
df2 <- read_parquet("data/feather_files/df2.parquet")
df4 <- read_parquet("data/feather_files/df4.parquet")
df6 <- read_parquet("data/feather_files/df6.parquet")
df7 <- read_parquet("data/feather_files/df7.parquet")
df13 <- read_parquet("data/feather_files/df13.parquet")
df11 <- read_parquet("data/feather_files/df11.parquet")
df12 <- read_parquet("data/feather_files/df12.parquet")
df10 <- read_parquet("data/feather_files/df10.parquet")
df14 <- read_parquet("data/feather_files/df14.parquet")
rm<-src("data/README.html")

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

tweaks <- 
    list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 500px;
                                   width: 600px;
                                   -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 3;    /* Firefox */ 
                                   column-count: 3; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
    ))

addResourcePath("tmpuser", getwd())

spinner <- tagList(
  spin_chasing_dots(),
  span("Loading data, this may take more than a minute", style="color:white;")
)

controls <-
    list(tags$div(align = 'left', 
                  class = 'multicol', 
                  checkboxGroupInput(inputId  = 'strain', 
                                     label    = NULL, 
                                     unique(skm_app$Strain),
                                     inline   = FALSE))) 

controlsMG <-
    list(tags$div(align = 'left', 
                  checkboxGroupInput(inputId  = 'group', 
                                     label    = "Plot by group", 
                                     choiceValues = unique(skmMG_app$group),
                                     choiceNames=c("Overweight", "Normal weight"),
                                     inline   = FALSE))) 

function(request) {
    
    sidebar <- dashboardSidebar(
        hr(),
        sidebarMenu(id="tabs",
                    menuItem("ReadMe", tabName = "readme", icon=icon("readme")),
                    menuItem("Plot normalized data", tabName="plot", icon=icon("chart-bar")),
                    menuItem("Plot differential expression", tabName="deseq_plot", icon=icon("dna")),
                    menuItem("Plot integrated data", tabName="int_plot", icon=icon("mortar-pestle")),
                    hr()
                    
        )
    )
    
    body <- dashboardBody(use_waiter(), 
                          waiter_show_on_load(spinner),
                          tweaks,
                          tags$head(
                            tags$style(".selectize-dropdown {position: static}")),
                          tabItems(
                            tabItem(tabName = "readme",
                                    fluidPage(
                                      tags$style(HTML(
                                        includeHTML("data/README.html"))),
                                    )
                            ),
                              tabItem(tabName = "plot",
                                      fluidRow(
                                          column(width = 6, 
                                                 tabBox(width = 12, title="", id="choose_tab", selected="Mice",
                                                        tabPanel(
                                                            title="Mice", id="Mice",
                                                            fluidRow(
                                                                column(
                                                                    width = 12,
                                                                    box(
                                                                        width = NULL, collapsible = TRUE,
                                                                        title = "Enter variables", solidHeader = TRUE,
                                                                        splitLayout(
                                                                            selectInput("dataset", 'Select tissue', choices = c("Skeletal muscle", 
                                                                                                                                "Liver",
                                                                                                                                "Heart",
                                                                                                                                "GWAT",
                                                                                                                                "BAT")),
                                                                            selectizeInput("gene", 'Search for your favorite gene', choices = NULL)
                                                                            
                                                                        )
                                                                    )
                                                                ),
                                                                
                                                                column(
                                                                    width = 12, 
                                                                    box(width = NULL,
                                                                        controls, 
                                                                        actionButton("selectall", label="Deselect"),
                                                                        collapsible = TRUE, 
                                                                        collapsed = FALSE, 
                                                                        title = "Label by mouse strains", solidHeader=TRUE,
                                                                        footer=p(HTML(paste0("<b>Tip:</b> Label by strain to see raw data for each point")))
                                                                        
                                                                    )
                                                                    
                                                                ),
                                                                tags$style(type='text/css', "#submit { margin-top: 25px;}")
                                                                
                                                            )
                                                        ),
                                                        tabPanel(
                                                            title="Humans", id="Humans",
                                                            fluidRow(
                                                                column(
                                                                    width = 12,
                                                                    box(
                                                                        width = NULL, collapsible = TRUE,
                                                                        title = "Enter variables", solidHeader = TRUE,
                                                                        splitLayout(
                                                                            selectInput("datasetMG", 'Select tissue', choices = c("Skeletal muscle", 
                                                                                                                                  "Subcutaneous white adipose tissue")),
                                                                            selectizeInput("geneMG", 'Search for your favorite gene', choices = NULL)
                                                                            
                                                                        )
                                                                    )
                                                                ),
                                                                column(
                                                                    width = 12, 
                                                                    box(width = NULL,
                                                                        controlsMG,
                                                                        collapsible = TRUE, 
                                                                        collapsed = FALSE, 
                                                                        title = "More options", solidHeader=TRUE,
                                                                        #footer=p(HTML(paste0("<b>Tip:</b>")))
                                                                    )
                                                                ),
                                                                tags$style(type='text/css', "#submit { margin-top: 25px;}")
                                                            )
                                                            
                                                        )
                                                        
                                                 )
                                                 
                                                 
                                          ), 
                                          column(width = 6, 
                                                 box(
                                                     width = NULL, collapsible=T, 
                                                     title = "Plot", solidHeader = TRUE, status = "primary",
                                                     uiOutput("test")
                                                 )
                                                 
                                          )
                                          
                                          
                                      )
                                      
                              ), #Closes tabitem "plot"
                              
                              tabItem(tabName = "deseq_plot",
                                      fluidRow(
                                          column(width = 6, 
                                                 tabBox(width = 12, title="", id="choose_tabDE", selected="Mice",
                                                        tabPanel(
                                                            title="Mice", id="Mice",
                                                            fluidRow(
                                                                column(
                                                                    width = 12,
                                                                    box(
                                                                        width = NULL, collapsible = TRUE,
                                                                        title = "Enter variables", solidHeader = TRUE,
                                                                        radioButtons("deseqHMDP", 'Select tissue', choices = c("Skeletal muscle", 
                                                                                                                               "Liver",
                                                                                                                               "Heart",
                                                                                                                               "GWAT",
                                                                                                                               "BAT")
                                                                                     
                                                                        )
                                                                    )
                                                                ),
                                                                
                                                                column(
                                                                    width = 12, 
                                                                    box(width = NULL,
                                                                        dataTableOutput("tableDE"),
                                                                        collapsible = TRUE, 
                                                                        collapsed = FALSE, 
                                                                        title = "Label by gene", solidHeader=TRUE
                                                                        
                                                                    )
                                                                    
                                                                ),
                                                                tags$style(type='text/css', "#submit { margin-top: 25px;}")
                                                                
                                                            )
                                                        ),
                                                        tabPanel(
                                                            title="Humans", id="Humans",
                                                            fluidRow(
                                                                column(
                                                                    width = 12,
                                                                    box(
                                                                        width = NULL, collapsible = TRUE,
                                                                        title = "Enter variables", solidHeader = TRUE,
                                                                        radioButtons("deseqMG", 'Select comparison', choices = c("Acute exercise vs. baseline", 
                                                                                                                                 "2 h rest vs. baseline",
                                                                                                                                 "12 w vs. baseline",
                                                                                                                                 "12 w effect on acute exercise",
                                                                                                                                 "12 w effect on 2 h rest"),
                 
                                                                                     
                                                                        )
                                                                    )
                                                                ),
                                                                column(
                                                                    width = 12, 
                                                                    box(width = NULL,
                                                                        dataTableOutput("tableDE_MG"),
                                                                        collapsible = TRUE, 
                                                                        collapsed = FALSE, 
                                                                        title = "Label by gene", solidHeader=TRUE,

                                                                    )
                                                                ),
                                                                tags$style(type='text/css', "#submit { margin-top: 25px;}")
                                                            )
                                                            
                                                        )
                                                        
                                                 )
                                                 
                                                 
                                          ), 
                                          column(width = 6, 
                                                 box(
                                                     width = NULL, collapsible=T, 
                                                     title = "Plot", solidHeader = TRUE, status = "primary",
                                                     uiOutput("test1")
                                                 )
                                                 
                                          )
                                          
                                          
                                      )
                                      
                              ), ## Closes deseq tab
                              
                              tabItem(tabName = "int_plot",
                                      fluidRow(
                                          column(width = 6, 
                                                 tabBox(width = 12, title="", id="choose_tabInt",
                                                        tabPanel(
                                                            title="By species", id="By species",
                                                            fluidRow(
                                                                column(
                                                                    width = 12,
                                                                    box(
                                                                        width = NULL, collapsible = TRUE,
                                                                        title = "Enter variables", solidHeader = TRUE,
                                                                        splitLayout(
                                                                            selectInput("dataset_int", 'Select tissue', choices = c("Skeletal muscle",
                                                                                                                                    "WAT")),
                                                                            selectizeInput("gene_int", 'Search for your favorite gene', choices = NULL)
                   
                                                                            
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                        ),
                                                        tabPanel(
                                                          title="By tissue - mice", id = "By tissue - mice",
                                                          fluidRow(
                                                            column(
                                                              width=12,
                                                              box(
                                                                width=NULL, collapsible=TRUE, 
                                                                title = "", solidHeader = TRUE, 
                                                                splitLayout(
                                                                  selectizeInput("gene_int_tissue_HMDP", "Search for your favorite gene", choices=NULL)
                                                                )
                                                              )
                                                            )
                                                          )
                                                        ),
                                                        tabPanel(
                                                          title="By tissue - humans", id = "By tissue - humans",
                                                          fluidRow(
                                                            column(
                                                              width=12,
                                                              box(
                                                                width=NULL, collapsible=TRUE, 
                                                                title = "", solidHeader = TRUE, 
                                                                splitLayout(
                                                                  selectizeInput("gene_int_tissue_MG", "Search for your favorite gene", choices=NULL)
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                 )
                                          ),
                                          column(width = 6, 
                                                 box(
                                                     width = NULL, collapsible=T, 
                                                     title = "Plot", solidHeader = TRUE, status = "primary",
                                                     uiOutput("test2")
                                                 )
                                      )
                              )
                              
                              
                          )
                          )
    )

    
    

                dashboardPage(
                    dashboardHeader(title = "MyoGlu + HMDP Exercise studies"),
                    sidebar,
                    body
                )
    
    
}