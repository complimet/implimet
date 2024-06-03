## Implimet
# Shiny app
# By Anu Surendra

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(waiter)
library(tidyverse)  
library(cluster)    
library(factoextra)
library(dendextend)
library(RColorBrewer)
library(gplots)
library(viridis)
library(dplyr) 
library(ggplot2)
library(DT)
library(Hmisc)

source("/var/www/compLiMet/public_html/shiny/dev_site/implimet/src/app_general_function.r")
source("/var/www/compLiMet/public_html/shiny/dev_site/implimet/src/app_imputation_function.r")
source("/var/www/compLiMet/public_html/shiny/dev_site/implimet/src/app_generate_missing.r")



# UI
sidebar <- dashboardSidebar(
  sidebarMenu(
    div(
      align = "center",
      br(),
      tags$figure(
        tags$img(src = "logo_only.png",  # PLACEHOLDER FROM FLATICON -- original logo to be made
                 width = 100,
                 alt = ""),
      ),
      h3("ImpLiMet v1.0"),
      br()
    ),
    menuItem("Getting start", tabName = "start", icon = icon("gauge")
             ),
    menuItem("Download sample data", tabName = "samples", icon = icon("download")
             ),
    menuItem("Analyze", tabName = "analyze", icon = icon("magnifying-glass")
             ),
    menuItem("Troubleshoot", tabName = "troubleshoot", icon = icon("screwdriver-wrench")
             ),
    menuItem("Authors and citing", tabName = "cite", icon = icon("pencil")
             ),
    menuItem("Return to CompLiMet", icon = icon("house"), href = "https://complimet.ca")
  )
)

body <- dashboardBody(
  tabItems(
    
    ########################
    # GETTING STARTED PAGE #
    ########################
    tabItem(
      tabName = "start",
      box(
          column(12,
              h1("Overview of ImpLiMet"),
              p(
               "Optimal imputation method depends on the cause of missingness and the characteristics of the data. ImpLiMet enables users to impute missing data using 8 different methods and proposes the optimal imputation approach for the user’s data set if users have at least three features and six samples without any missing values in their dataset. Although focused on metabolomics and lipidomics, ImpLiMet can be used for any dataset."
              ),
              br(),
              p(
               "Users can either select an imputation method among the eight provided options (indicated below) or opt for the automated selection of the optimal method. The optimized method is determined as follows:"
              ),
              tags$ol(
                tags$li(
                  p(
                    "1.	ImpLiMet selects the largest subset of data with no missing values from the user’s dataset. There must be at least three features and six samples to optimize the imputation method.  If this criteria is not met, the user chooses their own implementation method. ",
                  )
                ),
 
                tags$li(
                  p(
                  "2.	Three missing mechanisms are then simulated from the user’s data: missing at random (MAR), missing not at random (MNAR), and missing completely at random (MCAR).",
                )
              ),
                tags$li(
                  p(
                    "3.	Eight imputation methods are used to impute missing data in these three simulations.  These imputation methods include five univariate and three multivariate methods:",
                    br(),
                    tags$span(
                      tags$ul(
                        tags$li(
                          p(
                            "5 univariate methods: ",tags$b(" mean, minimum, 1/5 minimum, median, and maximum")
                          )
                        ),
                        tags$li(
                          p(
                            "3 multi-variate methods: ",
                            tags$b("k-nearest neighbour (KNN), Random Forest (RF), and Multivariate Imputation by Chained Equations (MICE) "),
                            ". For the multivariate methods, the required parameters (number of neighbours for KNN, trees for RF, and iterations for MICE) are also optimized."
                        
                          )
                        )
                      )
                    )
                  ),
                tags$li(
                  p(
                    "The performance of each imputation method is determined by the mean absolute percentage error (MAPE).",
                  )
                ),
                tags$li(
                  p(
                    "5.	A comparison table is displayed showing the MAPE for each method. The method with the lowest MAPE across the missingness simulations is suggested as the optimal method.",
                  )
                 ),
                )
              ),
          
    
              br(),
              p(
                "6.	The effect of the chosen imputation method on the data structure is visualized by principal component analysis (PCA), comparing the impact of removing all features and all  samples with missing data to the chosen imputation method. Data are autoscaled prior to PCA."
              ),
              br()
          ),
          column(12,
            div(
              br(),
              tags$figure(
                align = "center",
                tags$img(src = "impute_figure_v4.svg",
                width = "90%",
                alt = "")
              )
            )
          ),
        width = 12
      )
    ),
    
    ###########################
    # DOWNLOAD TEMPALTES PAGE #
    ###########################
    tabItem(
      tabName = "samples",
      box(
        column(
          12,
     #     tags$b(h3("Preparing your data for Implimet")),
    #      br(),
    #      p(
    #        h4("Required input variables")
    #      ),
    #      p(
    #       tags$figure(
    #        align = "left",
    #        tags$img(src = "requiredinput.png",
    #                 width = "50%",
    #                 height = "50%",
    #                 alt = "")
    #      )
    #      ),
          br(),
          p(
            h4("Input data format and examples as a .CSV file")
          ),
          p(
          tags$ol(
                tags$li("Input data with a single feature measurement group"),
                          p(
           tags$figure(
            align = "left",
            tags$img(src = "./input_data_without_grouping.png",
                     width = "50%",
                     height = "50%",
                     alt = "")
          )
          ),
          tags$li("Input data with a single feature measurement group.",sep="/"," If the dataset includes features measured in different units by different platforms (multiple feature measurement groups), data should be formatted to indicate which groups should be considered separately for missing data simulation (i.e., which data were measured in the same units on the same platform)."),
 
                                   p(
           tags$figure(
            align = "left",
            tags$img(src = "./input_data_with_grouping.png",
                     width = "50%",
                     height = "50%",
                     alt = "")
          )
          )
                
                )
          ),
          br(),
  #        p(
  #          h4("Outputs")
  #        ),
  #        p(
  #        tags$ol(
  #              tags$li("Imputed dataset for download"),
  #                        
  #        tags$li("Table output (if optimization is chosen) example is shown below. If the user chooses the default imputed output data, the input dataset is imputed with RF with tree value = 250 as output (yellow highlight)."),
  #        br(),
  #        p(
  #         tags$figure(
  #          align = "left",
  #          tags$img(src = "output.png",
  #                   width = "50%",
  #                   height = "50%",
  #                   alt = "")
  #          )
  #        ),
  #        tags$li("PCA, t-SNE plots"),
 
          
   #     )
  #    ),

 

          
          
          
          h3("Sample Data"),
          tags$ol(
            tags$li(
              tags$a("Input data without group info", href = "./src/input_templates/Input_data_without_group_info.csv")
            ),
            tags$li(
              tags$a("Input data with group info", href = "./src/input_templates/Input_data_with_group_Info.csv")
            )
          ),
          br()
        ),
        width = 12
      )
    ),
    
    #####################################
    # ANALYZE PAGE FOR RUNNING IMPLIMET #
    #####################################
    tabItem(
      tabName = "analyze",
      tabBox(
        tabPanel(
          "Imputation",
          div( id="imputeDiv",
          useWaiter(),
          h2("Step 1"),
           checkboxInput("vgroup","Select in data input uncludes information about multiple feature measurement groups.",value = F),
          p(
            "Upload a file for imputation (*.csv)."
          ),
          fileInput(inputId = "imputationInput",
                    label = NULL,
                    width = "50%"),
          textAreaInput("caption_input", "", width="900px",rows=4),
          
          
          h2("Step 2"),

        	#numericInput("smt","Remove samples with the selected % of missing values", 100, min = 10, max = 100, step= 10,width = "50%"),
          selectInput("smt","Remove samples with the selected % of missing values", selected = "Don't remove any samples", c("Don't remove any samples",seq(10,100,10)),width = "50%"),
          
          #numericInput("fmt","Remove features with the selected % of missing values",100,min = 10, max = 100, step= 10,width = "50%"),
          selectInput("fmt","Remove features with the selected % of missing values",selected = "Don't remove any features",c("Don't remove any features",seq(10,100,10)),width = "50%"),
          
          textAreaInput("caption", "", width="900px",rows=4),
          downloadButton("export_btn_cleanedData", "Cleaned Data"),
          
     #MCC     checkboxInput("logshift","Log Shift",value = F),
          h2("Step 3"),
     p(
       "Select imputation method (Note: a minimum of 3 samples or 3 features without missing values with more values in the other dimension are required for the optimization option)"
     ),
          selectInput("imputationAlgorithm",
                      label = "Select imputation method:",
                      c("Min" = "min",
                        "Max" = "max",
                        "Median" = "median",
                        "Mean" = "mean",
                        "Optimization" = "optimization",
                        "Feature Minimum/5" = "min5",
                        "KNN" = "knn",
                        "RF" = "rf",
                        "MICE" = "mice"),
                      selected = "optimization",
                      width = "50%"),
          conditionalPanel(
            condition = "input.imputationAlgorithm == 'knn'",
            textInput("kValue", "K Value", "")
          ),
          conditionalPanel(
            condition = "input.imputationAlgorithm == 'rf'",
            textInput("treeVal", "Tree Value", "")
          ),
          conditionalPanel(
            condition = "input.imputationAlgorithm == 'mice'",
            textInput("miceIteration", "Mice Iteration", "")
          ),
          # -------Janice_add another check box--------------------
          conditionalPanel(
            condition = "input.imputationAlgorithm == 'optimization'", 
            checkboxInput("full_search_in","full parameter search",value = F)
          ),
          # -------Janice_add another check box--------------------
          useWaiter(),
          actionButton("runIMPLIMET",
                       label = "Run IMPLIMET",
                       class = "btn-warning",
                       style = "color: #fff;"),
          br(),
          conditionalPanel(
            condition = "input.condition2 == 1",
            downloadButton("export_btn", "Imputed Data")
          ),
          br(),
           conditionalPanel(
            condition = "input.condition == 1",
            DTOutput('table')
          )
        )),
         tabPanel(
          "Visualization",
          p(
            "Principal component analysis (PCA) for Samples (top) and Features (bottom)"
          ),
          tags$table(width="80%",border="0",
                     #works          tags$tr(
  #works            tags$td(span(plotOutput("clustRaw"))),
  #works           ),                     
  #works            tags$tr(
  #works               tags$td(span(plotOutput("clustImputed")))
  #works           ),
  tags$tr(
    tags$td( span(plotOutput("pcaRaw"))),  tags$td(span(plotOutput("pcaImputed"))),
  ),
  tags$tr(
    tags$td(span( plotOutput("tsneRaw"))),  tags$td(span(plotOutput("tsneImputed")))
  )
          ),
          br(),downloadButton("export_btn_visual", "Data Visualization")
        ),
        width = 12
      )
    ),
    
    #############################
    # PAGE FOR TROUBLESHOOTING #
    #############################
    tabItem(
      tabName = "troubleshoot",
      box(
        column(
          12,
          h1("Troubleshooting ImpLiMet"),
          p(
            "When troubleshooting, please review this list of common reasons for ImpLiMet failing to run. If you are still experiencing difficulties running our tool, please contact ", a("ldomic@uottawa.ca", href = "mailto: ldomic@uottawa.ca"), " for further assistance. Please include your input dataset and a description of the problem that you experienced. We will reproduce the problem and provide you with a solution."
          ),
          br(),
          h4("1. My file does not load or does not produce any results."),
          p(
            "ImpLiMet only accepts comma-delimited files as input. Tab-delimited or excel files will be read but will not produce any results. Please convert your input data into .csv format before running ImpLiMet.  Data must start from row 2 if there is no feature measurement group information or row 3 if feature measurement group information is selected. Sample information should be listed in the first column. Each feature column must have a unique label."
          ),
          br(),
          h4("2. Missing values must be empty strings (cells) or NA."),
          p(
            "ImpLiMet's focus is on missing values and  recognizes empty strings or NA as missing values. Please convert your missing value indicators to empty strings or NA."
          ),
          br(),
          h4("3. ImpLiMet accepts input datasets with any number of groups."),
          p(
            "Although any number of feature measurement groups is acceptable and will provide results keep in mind that more features in a feature measurement group you have the more accurate will be the imputation."
          ),
          br(),
          h4("4. Sample IDs have to be unique for each row."),
          p(
            "ImpLiMet does not analyze duplicated samples, so sample names have to be unique in the input. Please avoid using only numbers for sample naming. Instead, please use a combination of characters and numbers." # janice added comments
          ),
          br(),   
          h4("5. Selection of an Imputation method provides output but selection of the optimization option crashes."),
          p(
            "There must be a minimum of THREE or more complete (no missing values) columns (features) and minimum of SIX complete rows (samples) to run the optimization option. For very small datasets (e.g. dimensions 3x6) threshold should be set to 10% for full optimization as with larger thresholds dataset remaning after missingness simulation can become insufficient for imputation testing." # MCC added comments
          ),
          br()   
        ),
        width = 12
      )
    ),
    
    ################################
    # PAGE FOR AUTHORS AND CITING  #
    ################################
    tabItem(
      tabName = "cite",
      box(
        column(
          12,
          h3("Contact us"),
          p(a("ldomic@uottawa.ca", href = "mailto: ldomic@uottawa.ca")),
          br(),
          h3("Cite the use of ImpLiMet in a publication"),
          p("Huiting Ou, Anuradha Surendra, Graeme SV McDowell, Emily Hashimoto-Roth, Jianguo Xia, Steffany A.L. Bennett, Miroslava Cuperlovic-Culf IMPutation for LIpidomics and Metabolomics (IMPLIMET): an online implementation of missing data imputation."),
          br(),
          h3("Public Server"),
          p("IMPLIMET: ", a("https://complimet.ca/shiny/implimet/", href = "https://complimet.ca/shiny/implimet/")),
          br(),
          h3("Software License"),
          p(
            "IMPLIMET is free software. You can redistribute it and/or modify it under the terms of the ",
            a("GNU General Public License", href = "https://www.gnu.org/licenses/", target = "_blank"),
            " v3 (or later versions) as published by the Free Software Foundation. As per the GNU General Public License, IMPLIMET is distributed as a bioinformatic 
            lipidomic tool to assist users WITHOUT ANY WARRANTY and without any implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. All 
            limitations of warranty are indicated in the GNU General Public License."
          ),
          br()
        ),
        width = 12
      )
    )
  ),
  span(style="visibility:hidden",textInput( "condition", "", "0")),
  span(style="visibility:hidden",textInput( "condition2", "", "0")),
  span(style="visibility:hidden",textInput( "tableCol", "", "0")),
  
  ################
  # HEADER TITLE #
  ################
  tags$head(tags$script(HTML(
      '$(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass" style="float:right"><strong>Imp</strong>utation for <strong>Li</strong>pidomics and <strong>Met</strong>abolomics</span>\');
      })'
      )))
)


ui <- dashboardPage(
  dashboardHeader(title = "CompLiMet"),
  sidebar,
  body,
  skin = "black",
  tags$head(
   includeCSS("/var/www/compLiMet/public_html/shiny/dev_site/implimet/www/complimetGUI.css")
 #  includeCSS("C:/Project/Complimet/Implimet_20240603/www/complimetGUI.css")
    
  )
)

# Server
server <- function(input, output, session) {
  
  dataValues <- reactiveValues(
    ts = NULL,
    mainDir = NULL,
    algo = NULL,
    cleanedFilename = NULL,
    db_data_cleaned_logshift = NULL,
    outputFilename = NULL,
    db_data_cleaned = NULL,
    groups = NULL,
    imputedFilename = NULL,
    imputedDataframe = NULL,
    reversed_imputed_df = NULL,
    minVal = NULL,
    compoundOnly = NULL,
    pcaRaw = NULL,
    pcaImputed = NULL,
    tsneRaw = NULL,
    tsneImputed = NULL,
    clustRaw=NULL,
    clustImputed=NULL
  )
  
  observeEvent(input$imputationInput, {
  
  updateTextInput(session, "condition",
      label = "",value = "0")
      updateTextInput(session, "condition2",
      label = "",value = "0")
      
      
    req(input$imputationInput,input$fmt,input$smt)
  
    ts <- format(Sys.time(), format = "%m%d%Y%H%M%S")
    
    dir.create(file.path("/var/www/compLiMet/public_html/shiny/dev_site/implimet/www/tmp/", ts), showWarnings = FALSE)
  #    dir.create(file.path("C:/Project/Complimet/Implimet_20240603/www/tmp/", ts), showWarnings = FALSE)
    
      
   filename <- paste("/var/www/compLiMet/public_html/shiny/dev_site/implimet/www/tmp/",ts,"/",ts,"_data.csv",sep="")
  #  filename <- paste("C:/Project/Complimet/Implimet_20240603/www/tmp/",ts,"/",ts,"_data.csv",sep="")
    
    cleanedFilename <- paste(ts,"_cleaned_data.csv",sep="")
    file.copy(input$imputationInput$datapath,filename, overwrite = TRUE)
    
   
    variable_groups <- input$vgroup
    print("HELLO6787")
    compound_missing <- ifelse(tolower(input$fmt) == tolower("Don't remove any features"),110/100,as.numeric(input$fmt)/100) # column
    print("HELLO6799")
    sample_missing <- ifelse(tolower(input$smt) == tolower("Don't remove any samples"),110/100,as.numeric(input$smt)/100) # row
    raw_data <- read_in(filename, variable_groups)[[1]]
    compound_only <-  read_in(filename, variable_groups)[[2]]
    group <- read_in(filename, variable_groups)[[3]]
    
    dataValues$groups <- group

    if (isTRUE(variable_groups)) {
      unique_groups <-length(unique(as.numeric(group)))
      sample_dimension <- dim(raw_data)[1]-1
    } else {
      unique_groups <- 1
      sample_dimension <- dim(raw_data)[1]
    }


    transformed_compound_only <- log_shift(compound_only)[[1]]
    compound_only_cleaned <- clean_up(transformed_compound_only, sample_missing, compound_missing)

 updateTextAreaInput(session, "caption_input",
                        label = "",
                        value = paste("Total number of sample left: ",sample_dimension, "\nTotal number of features left: ",dim(compound_only)[2],
                                      "\nTotal number of measurement groups: ",unique_groups, "\nTotal number of missing values in the dataset: ",sum(is.na(transformed_compound_only)) ,sep=""))
    
    
    
    updateTextAreaInput(session, "caption",
      label = "",
      value = paste("The sample(s) left : ",dim(compound_only_cleaned)[1], "\n and the feature(s) left: ",dim(compound_only_cleaned)[2],sep=""))
      
      
      
    output$export_btn_cleanedData <- downloadHandler(
      filename = function() {
        paste(ts,"_Cleaneddata.csv", sep="")
      },
      content = function(file) {
        write.csv(compound_only_cleaned, file)
      }
    )
    
   unlink(paste("/var/www/compLiMet/public_html/shiny/dev_site/implimet/www/tmp/", ts,"/*", sep=""), recursive = T)

  #  unlink(paste("C:/Project/Complimet/Implimet_20240603/www/tmp/", ts,"/*", sep=""), recursive = T)
    
  
  })
  
  
  
 observeEvent(input$imputationAlgorithm, {
  
  updateTextInput(session, "condition2",
      label = "",value = "0")
  if(input$imputationAlgorithm != 'optimization'){
  
  updateTextInput(session, "condition",
      label = "",value = "0")
      
   }
  
  })
  
  
  observeEvent(input$smt, {
  
    req(input$imputationInput,input$fmt,input$smt)
  
    ts <- format(Sys.time(), format = "%m%d%Y%H%M%S")

    dir.create(file.path("/var/www/compLiMet/public_html/shiny/dev_site/implimet/www/tmp/", ts), showWarnings = FALSE)
    
    filename <- paste("/var/www/compLiMet/public_html/shiny/dev_site/implimet/www/tmp/",ts,"/",ts,"_data.csv",sep="")
#    dir.create(file.path("C:/Project/Complimet/Implimet_20240603/www/tmp/", ts), showWarnings = FALSE)
    
#    filename <- paste("C:/Project/Complimet/Implimet_20240603/www/tmp/",ts,"/",ts,"_data.csv",sep="")
    
    cleanedFilename <- paste(ts,"_cleaned_data.csv",sep="")
    file.copy(input$imputationInput$datapath,filename, overwrite = TRUE)
    
    
    variable_groups <- input$vgroup
  
    compound_missing <- ifelse(tolower(input$fmt) == tolower("Don't remove any features"),110/100,as.numeric(input$fmt)/100) # column
    print("HELLO5650")
    print(compound_missing)
    print(input$fmt)
    sample_missing <- ifelse(tolower(input$smt) == tolower("Don't remove any samples"),110/100,as.numeric(input$smt)/100) # row
    print(sample_missing)
    raw_data <- read_in(filename, variable_groups)[[1]]
    compound_only <-  read_in(filename, variable_groups)[[2]]
    transformed_compound_only <- log_shift(compound_only)[[1]]
    compound_only_cleaned <- clean_up(transformed_compound_only, sample_missing, compound_missing)
    
    
    updateTextAreaInput(session, "caption",
      label = "",
      value = paste("The sample(s) left : ",dim(compound_only_cleaned)[1], "\n and the feature(s) left: ",dim(compound_only_cleaned)[2],sep=""))
      
      
      
      output$export_btn_cleanedData <- downloadHandler(
      filename = function() {
        paste(ts,"_Cleaneddata.csv", sep="")
      },
      content = function(file) {
        write.csv(compound_only_cleaned, file)
      }
    )
    
    unlink(paste("/var/www/compLiMet/public_html/shiny/dev_site/implimet/www/tmp/", ts,"/*", sep=""), recursive = T)
#     unlink(paste("C:/Project/Complimet/Implimet_20240603/www/tmp/", ts,"/*", sep=""), recursive = T)
    
  })
  
  
  observeEvent(input$fmt, {
  
    req(input$imputationInput,input$fmt,input$smt)
  
    ts <- format(Sys.time(), format = "%m%d%Y%H%M%S")
    
   dir.create(file.path("/var/www/compLiMet/public_html/shiny/dev_site/implimet/www/tmp/", ts), showWarnings = FALSE)
 #     dir.create(file.path("C:/Project/Complimet/Implimet_20240603/www/tmp/", ts), showWarnings = FALSE)
    
    filename <- paste("/var/www/compLiMet/public_html/shiny/dev_site/implimet/www/tmp/",ts,"/",ts,"_data.csv",sep="")
 #   filename <- paste("C:/Project/Complimet/Implimet_20240603/www/tmp/",ts,"/",ts,"_data.csv",sep="")
    
    cleanedFilename <- paste(ts,"_cleaned_data.csv",sep="")
    file.copy(input$imputationInput$datapath,filename, overwrite = TRUE)
    
    
    variable_groups <- input$vgroup
  
    compound_missing <- ifelse(tolower(input$fmt) == tolower("Don't remove any features"),110/100,as.numeric(input$fmt)/100) # column
    print("HELLO9678")
    print(compound_missing)
    sample_missing <- ifelse(tolower(input$smt) == tolower("Don't remove any samples"),110/100,as.numeric(input$smt)/100) # row
    print(sample_missing)
    raw_data <- read_in(filename, variable_groups)[[1]]
    compound_only <-  read_in(filename, variable_groups)[[2]]
    transformed_compound_only <- log_shift(compound_only)[[1]]
    compound_only_cleaned <- clean_up(transformed_compound_only, sample_missing, compound_missing)
    
    
    updateTextAreaInput(session, "caption",
      label = "",
      value = paste("The sample(s) left : ",dim(compound_only_cleaned)[1], "\n and the feature(s) left: ",dim(compound_only_cleaned)[2],sep=""))
      
    output$export_btn_cleanedData <- downloadHandler(
      filename = function() {
        paste(ts,"_Cleaneddata.csv", sep="")
      },
      content = function(file) {
        write.csv(compound_only_cleaned, file)
      }
    )
    
   unlink(paste("/var/www/compLiMet/public_html/shiny/dev_site/implimet/www/tmp/", ts,"/*", sep=""), recursive = T)
 
 #     unlink(paste("C:/Project/Complimet/Implimet_20240603/www/tmp/", ts,"/*", sep=""), recursive = T)
    
  
  })
  
  
  observeEvent(input$runIMPLIMET, {
  
  
    waiter_show( 
      id = "imputeDiv",
      html = spin_fading_circles() # use a spinner
    )
    
  
  
    ts <- format(Sys.time(), format = "%m%d%Y%H%M%S")
#    dir.create(file.path("C:/Project/Complimet/Implimet_20240603/tmp/", ts), showWarnings = FALSE)
#    dataValues$mainDir <- paste0("C:/Project/Complimet/Implimet_20240603/tmp/", ts)
    
#    filename <- paste("C:/Project/Complimet/Implimet_20240603/tmp/",ts,"/",ts,"_data.csv",sep="")
    
    dir.create(file.path("/var/www/compLiMet/public_html/shiny/dev_site/implimet/www/tmp/", ts), showWarnings = FALSE)
    dataValues$mainDir <- paste0("/var/www/compLiMet/public_html/shiny/dev_site/implimet/www/tmp/", ts)
    
    filename <- paste("/var/www/compLiMet/public_html/shiny/dev_site/implimet/www/tmp/",ts,"/",ts,"_data.csv",sep="")

#    dir.create(file.path("C:/Project/Complimet/Implimet_20240603/www/tmp/", ts), showWarnings = FALSE)
#    dataValues$mainDir <- paste0("C:/Project/Complimet/Implimet_20240603/www/tmp/", ts)
    
#    filename <- paste("C:/Project/Complimet/Implimet_20240603/www/tmp/",ts,"/",ts,"_data.csv",sep="")
    
        
    cleanedFilename <- paste(ts,"_cleaned_data.csv",sep="")
    dataValues$ts <- ts
    dataValues$cleanedFilename <- cleanedFilename
    file.copy(input$imputationInput$datapath,filename, overwrite = TRUE)
    
    
    
    print(filename)
    variable_groups <- input$vgroup
    print(variable_groups)
    
    compound_missing <- ifelse(tolower(input$fmt) == tolower("Don't remove any features"),110/100,as.numeric(input$fmt)/100) # column
    sample_missing <- ifelse(tolower(input$smt) == tolower("Don't remove any samples"),110/100,as.numeric(input$smt)/100) # row
    
    print("HELLO94746")
    print(compound_missing)
    print(sample_missing)
    #optimized_method <- input$imputationAlgorithm
    
    
    # data input
    raw_data <- read_in(filename, variable_groups)[[1]]
    

      
    print("HELLO23")
    # data input
    compound_only <-  read_in(filename, variable_groups)[[2]]
    dataValues$compoundOnly <- compound_only
    print(compound_only)
        
    # variable groups
    group <- read_in(filename, variable_groups)[[3]]
    dataValues$groups <- group
    # data transformation
    transformed_compound_only <- log_shift(compound_only)[[1]]
    min_val <- log_shift(compound_only)[[2]] # get the lowest value for reverse transformation
    dataValues$minVal <- min_val
    
    
    
    # clean up the dataframe
    compound_only_cleaned <- clean_up(transformed_compound_only, sample_missing, compound_missing)
    dataValues$db_data_cleaned <- compound_only_cleaned
  
    dataValues$compoundOnly <- compound_only_cleaned #MCC added for missing rows fix April11 2024
    #MCC keep them for now dataValues$compoundOnly[is.na(dataValues$compoundOnly)] <- 0
  
     print(input$imputationAlgorithm)


 #MCC   if(input$logshift){
     #MCC      df_logshift <- log_shift(dataValues$db_data_cleaned)
     #MCC      dataValues$minVal <- df_logshift[[2]] 
     #MCC      dataValues$db_data_cleaned_logshift <- df_logshift[[1]]
     #MCC   }else{
      dataValues$db_data_cleaned_logshift <- dataValues$db_data_cleaned
      #MCC   }
    
        
    if(input$imputationAlgorithm=="min"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "minimum", var_group = dataValues$groups)
      dataValues$algo <- "Minimum"
      dataValues$imputedDataframe <- imputed_df[[1]]
    }else if(input$imputationAlgorithm=="mean"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "mean", var_group = dataValues$groups) 
      dataValues$algo <- "Mean"
      dataValues$imputedDataframe <- imputed_df[[1]]  
    }else if(input$imputationAlgorithm=="median"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "median", var_group = dataValues$groups)
      dataValues$algo <- "Median"
      dataValues$imputedDataframe <- imputed_df[[1]]
    }else if(input$imputationAlgorithm=="max"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "maximum", var_group = dataValues$groups)
      dataValues$algo <- "Maximum"
      dataValues$imputedDataframe <- imputed_df[[1]]
    }else if(input$imputationAlgorithm=="min5"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "one_fifth_minimum", var_group = dataValues$groups)
      dataValues$algo <- "One Fifth Minimum"
      dataValues$imputedDataframe <- imputed_df[[1]]
    }else if(input$imputationAlgorithm=="knn"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "KNN", k_val = as.numeric(input$kValue), var_group = dataValues$groups)
      dataValues$algo <- "KNN"
      dataValues$imputedDataframe <- imputed_df[[1]]
    }else if(input$imputationAlgorithm=="rf"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "RandomForest", tree_val = as.numeric(input$treeVal), var_group = dataValues$groups)
      dataValues$algo <- "Random Forest"
      dataValues$imputedDataframe <- imputed_df[[1]]
    }else if(input$imputationAlgorithm=="mice"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "MICE", mice_iteration = as.numeric(input$miceIteration), var_group = dataValues$groups)
      dataValues$algo <- "MICE"
      dataValues$imputedDataframe <- imputed_df[[1]]
    }else{
      # janice added the "full_search" checkbox here
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, full_search = input$full_search_in, missing_variable_percentage = compound_missing, method = "optimization", var_group = dataValues$groups)
      
      dataValues$algo <- "Algorithm Optimization"
      dataValues$imputedDataframe <- imputed_df[[1]]
      tableoutput <- imputed_df[[2]]
    
    
    
      updateTextInput(session, "condition",
        label = "",value = "1")
    
    
      dataValues$algo <-imputed_df[[3]]
      
      coloured_colored <-which(colnames(tableoutput)==imputed_df[[3]])
    
      updateTextInput(session, "tableCol",
                      label = "",value = coloured_colored )  
      
      
      
      output$table <- renderDataTable(
        
        datatable(tableoutput, options = list(
          pageLength = 3,dom = 't',initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().columns().header()[$('#tableCol').val()]).css({'background-color': '#F39C12', 'color': 'white'});",
            "}")),escape = FALSE)
        
        
      )
      
    }
    
  
    # reverse transformation
    reversed_imputed_df <- reverse_log_shift(dataValues$imputedDataframe, min_val = dataValues$minVal)
    returned_df <- add_group_info(reversed_imputed_df, dataValues$groups)
    
    print("HELLO THERE")
    
    dataValues$reversed_imputed_df <- returned_df
    
    imputedFilename <- paste(dataValues$ts,"_imputed_data.csv",sep="")
    dataValues$imputedFilename <- imputedFilename
    
    
    
     updateTextInput(session, "condition2",
      label = "",value = "1")
         
      output$export_btn <- downloadHandler(
      filename = function() {
        paste(dataValues$imputedFilename,sep="")
      },
      content = function(file) {
        write.csv(dataValues$reversed_imputed_df, file)
      }
    )
    
    
   
    # pca
    pca_raw <- plot_PCA(dataValues$compoundOnly, paste("Cleaned data"))
    pca_imputed <- plot_PCA(reversed_imputed_df, paste("",dataValues$algo,"imputation"))
    dataValues$pcaRaw <- pca_raw
    dataValues$pcaImputed <- pca_imputed
    
    
    # tsne:
#    tsne_raw <- plot_tSNE(dataValues$compoundOnly, paste("tSNE original data"))
#    tsne_imputed <- plot_tSNE(reversed_imputed_df, paste("tSNE",dataValues$algo,"imputation"))
#    dataValues$tsneRaw <- tsne_raw
#    dataValues$tsneImputed <- tsne_imputed
    
    # or pca but of features
    tsne_raw <- plot_PCA(t(dataValues$compoundOnly), paste("Cleaned data"))
    tsne_imputed <- plot_PCA(t(reversed_imputed_df), paste("",dataValues$algo,"imputation"))
    dataValues$tsneRaw <- tsne_raw
    dataValues$tsneImputed <- tsne_imputed
	
#works hist_raw <- plot_histogram(dataValues$compoundOnly, "Histogram Original data before imputation")
    #works hist_imputed <- plot_histogram(reversed_imputed_df,  "Histogram Data after imputation")
    #works     dataValues$clustRaw <- hist_raw
#works     dataValues$clustImputed <- hist_imputed

    
    
    output$pcaRaw <- renderPlot({
      dataValues$pcaRaw
    }, res = 96)
    
    
    png(file = paste(dataValues$mainDir,"/",dataValues$ts, "_pcaCleanedSamples.png", sep=""),   # The directory you want to save the file in
    width = 950, # The width of the plot in inches
    height = 950) 
    
    #print(dataValues$pcaRaw)
    
    
    #dev.off()
    
    output$pcaImputed <- renderPlot({
      dataValues$pcaImputed
    }, res = 96)
    
    
    png(file = paste(dataValues$mainDir,"/",dataValues$ts, "_pcaImputedSamples.png", sep=""),   # The directory you want to save the file in
    width = 950, # The width of the plot in inches
    height = 950) 
    
    print(dataValues$pcaImputed)
    
    
    dev.off()
    
    
    
    output$tsneRaw <- renderPlot({
      dataValues$tsneRaw
    }, res = 96)
    
    
    png(file = paste(dataValues$mainDir,"/",dataValues$ts, "_pcaCleanedFeatures.png", sep=""),   # The directory you want to save the file in
    width = 950, # The width of the plot in inches
    height = 950) 
    
    print(dataValues$tsneRaw)
    
    
    dev.off()
    
    
    output$tsneImputed <- renderPlot({
      dataValues$tsneImputed
    }, res = 96)
    
    
    png(file = paste(dataValues$mainDir,"/",dataValues$ts, "_pcaImputedFeatures.png", sep=""),   # The directory you want to save the file in
    width = 950, # The width of the plot in inches
    height = 950) 
    
    print(dataValues$tsneImputed)
    
    
    dev.off()
    
	
    #works 	  output$clustRaw <- renderPlot({
    #works 	    for (col in names(dataValues$clustRaw)) {
    #works 	      print(dataValues$clustRaw[[col]])
 #works    #works 	    }
    #works    }, res = 96)
    
	  #works     png(file = paste(dataValues$mainDir,"/",dataValues$ts, "_HistogramRaw.png", sep=""),   # The directory you want to save the file in
	  #works    width = 950, # The width of the plot in inches
	  #works    height = 950) 
    
   # print(dataValues$clustRaw)
	  #works    for (col in names(dataValues$clustRaw)) {
	  #works      print(dataValues$clustRaw[[col]])
#works     }
    
	  #works     dev.off()
    
    
    #works    output$clustImputed <- renderPlot({
    #works     for (col in names(dataValues$clustRaw)) {
    #works        print(dataValues$clustImputed[[col]])
    #works      }
    #works   }, res = 96)
    
    
    #works    png(file = paste(dataValues$mainDir,"/",dataValues$ts, "_HistogramImputed.png", sep=""),   # The directory you want to save the file in
    #works    width = 950,
    #works     height = 1000) 
    
#    print(plot_histogram(reversed_imputed_df, paste("Histogram",dataValues$algo,"imputation")))
    #works    for (col in names(dataValues$clustImputed)) {
    #works       print(dataValues$clustImputed[[col]])
    #works     }
    
    #works    dev.off()
    
    
    
    
      output$export_btn_visual <- downloadHandler(
      filename = function() {
        paste(dataValues$ts, "zip", sep=".")
      },
      content = function(fname) {
        zip(zipfile=fname,flags="-j",files=paste(dataValues$mainDir,unlist(list.files (path = dataValues$mainDir,pattern=".png")),sep="/"))
      },
      contentType = "application/zip"
    )
 
     waiter_hide()
  
  })
  

}

# Run
shinyApp(ui, server)
