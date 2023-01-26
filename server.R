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


# Read in datasets
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

## Create lists for different panes

 data_sets <- list("Skeletal muscle" = skm_app,
                   "Liver" = liver_app,
                   "Heart" = heart_app,
                   "GWAT" = gwat_app,
                   "BAT" = bat_app)

 data_setsMG <- list("Skeletal muscle" = skmMG_app,
                     "Subcutaneous white adipose tissue" = atMG_app)

 ds_int <- list("Skeletal muscle" = integrated,
                "WAT" = integrated_wat)

 ds_int_tissue <- list("Mice" = all_tissues_mice,
                       "Humans" = all_tissues_men)

 data_setsDeSeqMG <- list("Acute exercise vs. baseline"=df,
                          "2 h rest vs. baseline" = df2,
                          "12 w vs. baseline" = df4,
                          "12 w effect on acute exercise" = df6,
                          "12 w effect on 2 h rest" = df7)

 data_setsDeSeqHMDP <- list("Skeletal muscle" = df13,
                            "Heart" = df11,
                            "Liver" = df12,
                            "GWAT" = df10,
                            "BAT" = df14)

 # create style property for tooltip
 # background color is set so tooltip is a bit transparent
 # z-index is set so we are sure are tooltip will be on top
 style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0); ")

server <- function(input, output, session) {
    
    datasetInput <- reactive({
        pluck(data_sets[[input$dataset]]) %>% 
            filter(gene %in% input$gene)
    }) 
    
    datasetInputStrain <- reactive({
        pluck(data_sets[[input$dataset]]) %>% 
            filter(gene %in% input$gene, 
                   Strain %in% input$strain) 
    })
    
    datasetInputMG <- reactive({
        pluck(data_setsMG[[input$datasetMG]]) %>% 
            filter(gene %in% input$geneMG)
    }) 
    
    datasetInputGroupMG <- reactive({
        pluck(data_setsMG[[input$datasetMG]]) %>% 
            filter(gene %in% input$geneMG,
                   group %in% input$group)
    }) 
    
    deseqinput_HMDP <- reactive({
        pluck(data_setsDeSeqHMDP[[input$deseqHMDP]])
    }) 
    
    deseqinput_MG <- reactive({
        pluck(data_setsDeSeqMG[[input$deseqMG]])
    })
    
    datasetInt_data <- reactive({
        pluck(ds_int[[input$dataset_int]]) %>% 
            filter(gene %in% input$gene_int)
    }) 
    
    datasetIntTissue_data_HMDP <- reactive({
        all_tissues_mice %>% 
            filter(gene %in% input$gene_int_tissue_HMDP)
    }) 
    
    datasetIntTissue_data_MG <- reactive({
        all_tissues_men %>% 
            filter(gene %in% input$gene_int_tissue_MG)
    }) 
    
    
    ########################################    
    # Reactive object containing all plots #
    ########################################    
    react.vals <- reactiveValues( 
      plot1  = ggplot(), 
      plot2  = ggplot(),
      plot3 = ggplot(),
      plot4 = ggplot(),
      plot5 = ggplot(),
      plot6 = ggplot(),
      plot7 = ggplot()
    )
    
    updateSelectizeInput(session, "gene", choices=levels(as.factor(skm_app$gene)), server=TRUE)
    updateSelectizeInput(session, "geneMG", choices = levels(as.factor(skmMG_app$gene)),  server = TRUE)
    updateSelectizeInput(session, "gene_int", choices = levels(as.factor(integrated$gene)), server = TRUE)
    updateSelectizeInput(session, "gene_int_tissue_HMDP",  choices=levels(as.factor(all_tissues_mice$gene)), server=TRUE)
    updateSelectizeInput(session, "gene_int_tissue_MG", choices=levels(as.factor(all_tissues_men$gene)), server=TRUE)
    
    observe({
        if (input$choose_tab == "Mice") {
            output$test <- renderUI({
                plotOutput("plot1",height="650px", brush = "plot_brush") %>% 
                    shinycssloaders::withSpinner(type=2)
            })
        }
        else if(input$choose_tab == "Humans") {
            output$test <- renderUI({
                plotOutput("plot2",height="650px", brush = "plot_brush") %>% 
                    shinycssloaders::withSpinner(type=2)
            })
        }
    })
    
    observe({
      react.vals$plot1 <- ggplot(datasetInput(), aes_string(x="fct_rev(Group)", y="reg_counts")) +
        geom_boxplot(outlier.shape = NA, fill="lightblue")+
        geom_point(color="black", size = 3, 
                   position=position_dodge(0.75), alpha=0.6) +
        theme_minimal() +
        theme(axis.title =element_text(size=16, face="bold", color="black"),
              axis.text = element_text(size=16, face="bold", color="black"),
              plot.caption=element_text(size=14, face="italic", color="black"),
              legend.position="none") +
        scale_y_continuous(name=paste(input$gene, "(regularized log2 counts)")) +
        scale_x_discrete(name="") +
        labs(caption = "\n You can get raw counts per point by checking the boxes in the left panel \n Please see README for more details")
      
      
    })
    
    observeEvent(react.vals$plot1, { 
      output$plot1 <- renderPlot({ react.vals$plot1 
        
        if(!is.null(input$strain)) {
          react.vals$plot1 + ggrepel::geom_label_repel(data=datasetInputStrain(), nudge_x=0.5,
                                                       aes(label=paste(
                                                         Strain,"\n", signif(reg_counts,3))), size=3.75)
        } else {
          react.vals$plot1 
        }
        
      })
    })
    
    observe({
      react.vals$plot2 <- ggplot(datasetInputMG(), aes_string(x="visit", y="rpkm")) +
        theme_minimal() +
        scale_x_discrete(name="Timepoint") +
        theme(axis.title =element_text(size=16, face="bold", color="black"),
              axis.text = element_text(size=16, face="bold", color="black"),
              plot.caption=element_text(size=14, face="italic", color="black"),
              legend.position="none",
              plot.title=element_text(size=20, face="bold", color="black")) +
        scale_y_continuous(name=paste(input$geneMG, "(regularized log2 counts)"))
    })
    observeEvent(react.vals$plot2, { 
      output$plot2 <- renderPlot({  react.vals$plot2
        
        
        if(!is.null(input$group)) {
          react.vals$plot2 + 
            geom_boxplot(data=datasetInputGroupMG(),aes_string(fill="group"), outlier.shape = NA)+
            geom_point(data=datasetInputGroupMG(),aes_string(fill="group"),color="black", size = 3, 
                       position=position_jitterdodge(dodge.width = 0.75, jitter.width=0.1, seed=1), alpha=0.6) +
            theme(legend.position="bottom",
                  legend.text = element_text(size=14, face="bold")) +
            ggsci::scale_fill_npg(name="", breaks=c("D", "N"), labels=c("Overweight men", "Normal weight men"))+
            labs(caption = "\n A refers to baseline tests, B refers to post-intervention tests \n Please see README for more details",
                 title = "Plot by group")
          
          
        } else {
          react.vals$plot2 +
            geom_boxplot(outlier.shape = NA, fill="lightblue")+
            geom_point(color="black", size = 3, alpha=0.6, position=position_jitter(width=0.1, seed=1)) +
            labs(caption = "\n A refers to baseline tests, B refers to post-intervention tests \n Please see README for more details",
                 title = "Total population")
        }
        
      })
    })
    
    
    observe({
        if (input$selectall > 0) {
            if (input$selectall %% 1 == 0){
                
                updateCheckboxGroupInput(session=session, 
                                         inputId="strain",
                                         choices = levels(as.factor(skm_app$Strain)),
                                         selected = c())
                
            }}
    })
    
    observe({
        if (input$choose_tabDE == "Mice") {
            output$test1 <- renderUI({
                div(
                    style="position:relative",
                plotOutput("plot3",height="650px", brush = "plot_brush",
                           hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")) %>% 
                    shinycssloaders::withSpinner(type=2),
                uiOutput("hover_info_HMDP")
                )
            })
        }
        else if(input$choose_tabDE == "Humans") {
            output$test1 <- renderUI({
                div(
                    style = "position:relative",
                plotOutput("plot4",height="650px", brush = "plot_brush",
                           hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")) %>% 
                    shinycssloaders::withSpinner(type=2),
                uiOutput("hover_info_MG")
                )
            })
        }
    })
    
    output$hover_info_HMDP <- renderUI({
        mydata <- deseqinput_HMDP()
        hover <- input$plot_hover
        point <- nearPoints(mydata, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)
        
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<b> Gene: </b><i>", point$external_gene_name, "</i><br/>",
                          "<b> Log2 fold change: </b>", round(point$log2FoldChange,2), "<br/>",
                          "<b> P-value </b>", signif(point$pvalue,3), "<br/>",
                          "<b> Adjusted p-value:</b>" , signif(point$padj,3), "<br/>")))
        )
    })
    
    output$hover_info_MG <- renderUI({
        mydata <- deseqinput_MG()
        hover <- input$plot_hover
        point <- nearPoints(mydata, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)
        
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<b>Gene: </b><i>", point$row, "</i><br/>",
                          "<b> Log2 fold change: </b>", round(point$log2FoldChange,2), "<br/>",
                          "<b> P-value </b>", signif(point$pvalue,3), "<br/>",
                          "<b> Adjusted p-value:</b>" , signif(point$padj,3), "<br/>")))
        )
    })
    
    observe({
        mydata <- deseqinput_HMDP()
        s <- mydata[input$tableDE_rows_selected,]
        react.vals$plot3 <- ggplot(data=mydata, aes(x=log2FoldChange, y=invFDR, color=pvalue<0.05)) +
            geom_point(size=4) +
            scale_x_continuous(name = "Log2 fold change", limits=c(-6,6)) +
            scale_y_continuous(name="-log2 (p-value)")+
            scale_color_manual(values=c("#999999", "#FC4E07")) +
            theme_minimal() +
            theme(legend.position="none",
                  text=element_text(size=18))
 
    })
    
    output$tableDE <- renderDataTable({
        mydata <- deseqinput_HMDP() %>% 
            select(external_gene_name, log2FoldChange, pvalue, padj)
        datatable(mydata, options=list(pageLength = 10, language = list(search="Search for your favorite gene:"))) %>% 
                  formatRound(columns=2:4, digits=3)
    })
    

    observeEvent(react.vals$plot3,{ 
      output$plot3 <- renderPlot({  
        react.vals$plot3
        if(!is.null(input$tableDE_rows_selected)) {
          mydata <- deseqinput_HMDP()
          react.vals$plot3 + ggrepel::geom_label_repel(data=mydata[input$tableDE_rows_selected,], 
                                                       aes_string(label="external_gene_name"),
                                                       xlim = c(NA, -1), # <--- here
                                                       seed = 1, color="black", size=6)
        } else {
          react.vals$plot3
        }
      })
    })
        
    
    observe({
        mydata1 <- deseqinput_MG()
        react.vals$plot4 <- ggplot(mydata1, aes(x=log2FoldChange, y=invFDR, color=pvalue<0.05)) +
            geom_point(size=4) +
            scale_x_continuous(name = "Log2 fold change", limits=c(-7.5,7.5)) +
            scale_y_continuous(name="-log2 (p-value)")+
            scale_color_manual(values=c("#999999", "#FC4E07")) +
            theme_minimal() +
            theme(legend.position="none",
                  text=element_text(size=18)) 
        
    })
    

    
    observeEvent(react.vals$plot4,{ 
      output$plot4 <- renderPlot({  
        react.vals$plot4
        if(!is.null(input$tableDE_MG_rows_selected)) {
          mydata <- deseqinput_MG()
          react.vals$plot4 + ggrepel::geom_label_repel(data=mydata[input$tableDE_MG_rows_selected,], 
                                                       aes_string(label="row"),
                                                       xlim = c(NA, -1), # <--- here
                                                       seed = 1, color="black", size=6)
        } else {
          react.vals$plot4
        }
      })
    })
    
    
    output$tableDE_MG <- renderDataTable({
        mydata1 <- deseqinput_MG() %>% select(row, log2FoldChange, pvalue, padj)
        datatable(mydata1, options=list(pageLength = 10, language = list(search="Search for your favorite gene:"))) %>% 
            formatRound(columns=2:4, digits=3)
    })

    
    ######################################
    # Plots for the Integrated data pane #
    ######################################   
    output$plot5 <- renderPlot({
      react.vals$plot5 <- 
        ggplot(datasetInt_data(), aes_string(x="Species", y="TPM", fill="fct_rev(Group)")) +
        geom_boxplot(outlier.shape=NA) +
        geom_point(size=3, position=position_jitterdodge(dodge.width=0.75, jitter.width = 0.1, seed=1), alpha=0.6)+
        theme_minimal() +
        theme(axis.title =element_text(size=16, face="bold", color="black"),
              axis.text = element_text(size=16, face="bold", color="black"),
              legend.position="bottom",
              legend.text = element_text(size=14, face="bold")) +
        ggsci::scale_fill_npg(name="") +
        scale_x_discrete(name="") +
        scale_y_continuous(name=paste(input$gene_int, "(TPM)"))
      
      react.vals$plot5
    })
    
    
    output$plot6 <- renderPlot({
      react.vals$plot6 <- 
        ggplot(datasetIntTissue_data_HMDP(), aes_string(x="Tissue", y="TPM", fill="fct_rev(Group)")) +
        geom_boxplot(outlier.shape=NA) +
        geom_point(size=3, position=position_jitterdodge(dodge.width=0.75, jitter.width = 0.1, seed=1), alpha=0.6)+
        theme_minimal() +
        theme(axis.title =element_text(size=16, face="bold", color="black"),
              axis.text = element_text(size=16, face="bold", color="black"),
              legend.position="bottom",
              legend.text = element_text(size=14, face="bold")) +
        ggsci::scale_fill_npg(name="") +
        scale_x_discrete(name="")+
        scale_y_continuous(name=paste(input$gene_int_tissue_HMDP, "(TPM)"))
      
      react.vals$plot6
    })
    
    output$plot7 <- renderPlot({
      react.vals$plot7 <- 
        ggplot(datasetIntTissue_data_MG(), aes_string(x="Tissue", y="TPM", fill="fct_rev(Group)")) +
        geom_boxplot(outlier.shape=NA) +
        geom_point(size=3, position=position_jitterdodge(dodge.width=0.75, jitter.width = 0.2, seed=1), alpha=0.6)+
        theme_minimal() +
        theme(axis.title =element_text(size=16, face="bold", color="black"),
              axis.text = element_text(size=16, face="bold", color="black"),
              legend.position="bottom",
              legend.text = element_text(size=14, face="bold")) +
        ggsci::scale_fill_npg(name="")+
        scale_x_discrete(name="")+
        scale_y_continuous(name=paste(input$gene_int_tissue_MG, "(TPM)")) 
      
      react.vals$plot7
    })
    
    observe({
      if (input$choose_tabInt == "By species") {
        output$test2 <- renderUI({
          plotOutput("plot5",height="650px", brush = "plot_brush") %>% 
            shinycssloaders::withSpinner(type=2)
          
        })
      }
      
      else if (input$choose_tabInt == "By tissue - mice") {
        output$test2 <- renderUI({
          plotOutput("plot6",height="650px", brush = "plot_brush") %>% 
            shinycssloaders::withSpinner(type=2)
          
        })
      }
      else if(input$choose_tabInt == "By tissue - humans") {
        output$test2 <- renderUI({
          plotOutput("plot7",height="650px", brush = "plot_brush") %>% 
            shinycssloaders::withSpinner(type=2)
          
          
        })
      }
    })
  
##################################    
# Show / hide the loading screen #   
##################################
  
    hostess <- Hostess$new("loader", infinite=TRUE)
    hostess$start()
    hostess$close()
    waiter_hide()
}