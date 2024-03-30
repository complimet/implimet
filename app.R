## Implimet
# Shiny app
# By Anu Surendra

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(waiter)
source("C:/Project/Complimet/Implimet_JO_March29_2024/src/app_general_function.r")
source("C:/Project/Complimet/Implimet_JO_March29_2024/src/app_imputation_function.r")
source("C:/Project/Complimet/Implimet_JO_March29_2024/src/app_generate_missing.r")




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
      h3("Implimet v1.0"),
      br()
    ),
    menuItem("Getting started", tabName = "start", icon = icon("gauge")
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
          column(6,
              h1("Overview of Implimet"),
              p(
               "Missing data can occur in any high throughput measurement making multivariate analysis highly problematic. Optimal imputation method depends on the cause of missingness and the characteristics of the data. Implimet proposes optimal imputation method for the data set, by determining accuracy of imputation between several widely used imputation methods. Presented application can be used for any dataset."
              ),
              br(),
              p(
               "Overall, Implimet offers two data imputation options. One option is to impute by a user-selected method among the eight available methods (indicated below), and the other is to impute by an optimized method. An optimized method was determined by the following steps:"
              ),
              tags$ol(
                tags$li(
                  p(
                    "Select a complete data section from the user input dataset. Then, create three missing datasets via three missing mechanisms, which are missing at random (MAR), missing not at random, and missing completely at random (MCAR).",
                  )
                ),
                tags$li(
                  p(
                    "Impute each missing dataset with eight imputation methods including five univariate and three multivariate methods.",
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
                    "The performance of each imputation method was determined by the mean absolute percentage error (MAPE).",
                  )
                ),
                tags$li(
                  p(
                    "A comparison table is displayed showing the MAPE each method produced. The method with the lowest MAPE is the optimized method.",
                  )
                 ),
                )
              ),
              br(),
              p(
                "Data transformation was performed before imputation, and reverse transformation was undertaken on the imputed dataset to obtain the original values as imputed output."
              ),
              br(),
              p(
                "Visualization of the dataset before and after imputation is performed via principal component analysis (PCA) and t-distributed stochastic neighbour embedding (t-SNE)."
              ),
              br()
          ),
          column(6,
            div(
              br(),
              tags$figure(
                align = "center",
                tags$img(src = "impute_figure_v2.svg",
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
          tags$b(h3("Preparing your data for Implimet")),
          br(),
          p(
            h4("Required input variables")
          ),
          p(
           tags$figure(
            align = "left",
            tags$img(src = "requiredinput.png",
                     width = "50%",
                     height = "50%",
                     alt = "")
          )
          ),
          br(),
          p(
            h4("Input data format and examples")
          ),
          p(
          tags$ol(
                tags$li("Data ",tags$b("without"), "grouping information"),
                          p(
           tags$figure(
            align = "left",
            tags$img(src = "input_data_without_grouping.png",
                     width = "50%",
                     height = "50%",
                     alt = "")
          )
          ),
          tags$li("Data ",tags$b("with"), "grouping information"),
                          p(
           tags$figure(
            align = "left",
            tags$img(src = "input_data_with_grouping.png",
                     width = "50%",
                     height = "50%",
                     alt = "")
          )
          )
                
                )
          ),
          br(),
          p(
            h4("Outputs")
          ),
          p(
          tags$ol(
                tags$li("Imputed dataset for download"),
                          
          tags$li("Table output (if optimization is chosen) example is shown below. If the user chooses the default imputed output data, the input dataset is imputed with RF with tree value = 250 as output (yellow highlight)."),
          br(),
          p(
           tags$figure(
            align = "left",
            tags$img(src = "output.png",
                     width = "50%",
                     height = "50%",
                     alt = "")
            )
          ),
          tags$li("PCA and t-SNE"),
          br(),
          p(
           tags$figure(
            align = "left",
            tags$img(src = "pca.png",
                     width = "50%",
                     height = "50%",
                     alt = "")
            )
          ),
          br(),
          p(
           tags$figure(
            align = "left",
            tags$img(src = "tsne.png",
                     width = "50%",
                     height = "50%",
                     alt = "")
            )
          ),
          
        )
      ),
            br(),
          p(
            h4("Relevant information:")
          ),
          br(),
                tags$ol(
                tags$li("Optimization time: 30-40 mins with 1 thread"),
                tags$li("A completed example is shown in \"Implimet_example.rmd\"")
                ),

          
          
          
          h3("Sample Data"),
          p(
            "\"two_optimization_examples\" contains 2 optimization examples which include all variables in RData format:"
          ),
          tags$ol(
            tags$li(
              tags$a("Input data without group info", href = "./src/input_templates/1_without_group_imputed_data.csv")
            ),
            tags$li(
              tags$a("Input data with group info", href = "./src/input_templates/2_with_group_imputed_data.csv")
            )
          ),
          br()
        ),
        width = 12
      )
    ),
    
    #####################################
    # ANALYZE PAGE FOR RUNNING META-BOA #
    #####################################
    tabItem(
      tabName = "analyze",
      tabBox(
        tabPanel(
          "Imputation",
          div( id="imputeDiv",
          useWaiter(),
          h2("Step 1"),
          p(
            "Upload a file for imputation (*.csv)."
          ),
          fileInput(inputId = "imputationInput",
                    label = NULL,
                    width = "50%"),
          h2("Step 2"),
          # p("variable group"), ======= janice removed the nextline
          checkboxInput("vgroup","variable group",value = T),
         	numericInput("smt","Sample missing threshold",0.2,step= 0.1,width = "50%"),
          numericInput("fmt","Feature missing threshold",0.3, step= 0.1,width = "50%"),
          checkboxInput("logshift","Log Shift",value = F),
          h2("Step 3"),
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
            condition = "input.condition == 1",
            downloadButton("export_btn", "Imputed Data")
          ),
          br(),
           conditionalPanel(
            condition = "input.condition == 1",
            dataTableOutput('table')
          )
        )),
         tabPanel(
          "Visualization",
          tags$table(width="100%",
            tags$tr(
              tags$td(span(plotOutput("pcaRaw"))),  tags$td(span(plotOutput("pcaImputed"))),
            ),
            tags$tr(
              tags$td(span( plotOutput("tsneRaw"))),  tags$td(span(plotOutput("tsneImputed")))
            )
          )
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
          h1("Troubleshooting IMPLIMET"),
          p(
            "When troubleshooting, please review this list of common reasons for IMPLIMET failing to run. If you are still experiencing difficulties running our tool, please contact ", a("ldomic@uottawa.ca", href = "mailto: ldomic@uottawa.ca"), " for further assistance. Please include your input dataset and a description of the problem that you experienced. We will reproduce the problem and provide you with a solution."
          ),
          br(),
          h4("1. My file does not load or does not produce any results."),
          p(
            "IMPLIMET only accepts comma-delimited files as input. Tab-delimited or excel files will be read but will not produce any results. Please convert your input data into .csv format before running IMPLIMET. Data must start from row 2 - if there is no group information or row 3 - if group information is selected. Sample information should be listed in the first column with unique IDs for each sample."
          ),
          br(),
          h4("2. Missing values must be empty strings or NA."),
          p(
            "IMPLIMET's focus is on missing values and  recognizes empty strings or NA as missing values. Please convert your missing value indicators to empty strings or NA."
          ),
          br(),
          h4("3. IMPLIMET accepts input datasets with any number of groups."),
          p(
            "Although any number of groups is acceptable and will provide results keep in mind that more features in the group you have more accurate will be the imputation."
          ),
          br(),
          h4("4. Sample IDs have to be unique for each row."),
          p(
            "IMPLIMET does not analyze duplicated samples, so sample names have to be unique for input. Please avoid using only numbers for sample naming. Instead, please use a combination of character and number." # janice added comments
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
          h3("Cite the use of IMPLIMET in a publication"),
          p("Huiting Ou, Anuradha Surendra, Graeme SV McDowell, Fumihiko Matsuda, Jianguo Xia, Steffany A.L. Bennett, Miroslava Cuperlovic-Culf IMPutation for LIpidomics and Metabolomics (IMPLIMET): an online implementation of missing data imputation."),
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
  
  ################
  # HEADER TITLE #
  ################
  tags$head(tags$script(HTML(
      '$(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass">Imputation for lipidomics and metabolomics</span>\');
      })'
      )))
)


ui <- dashboardPage(
  dashboardHeader(title = "CompLiMet"),
  sidebar,
  body,
  skin = "black",
  tags$head(
   includeCSS("C:/Project/Complimet/Implimet_JO_March29_2024/www/complimetGUI.css")
  )
)

# Server
server <- function(input, output, session) {
  
  dataValues <- reactiveValues(
    ts = NULL,
    mainDir = NULL,
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
    tsneImputed = NULL
  )
  
  observeEvent(input$imputationInput, {
  
  updateTextInput(session, "condition",
      label = "",value = "0")
  
  })
  
  observeEvent(input$runIMPLIMET, {
  
  
    waiter_show( 
      id = "imputeDiv",
      html = spin_fading_circles() # use a spinner
    )
  
  
    ts <- format(Sys.time(), format = "%m%d%Y%H%M%S")
    dir.create(file.path("C:/Project/Complimet/Implimet_JO_March29_2024/tmp/", ts), showWarnings = FALSE)
    dataValues$mainDir <- paste0("C:/Project/Complimet/Implimet_JO_March29_2024/tmp/", ts)
    
    filename <- paste("C:/Project/Complimet/Implimet_JO_March29_2024/tmp/",ts,"/",ts,"_data.csv",sep="")
    cleanedFilename <- paste(ts,"_cleaned_data.csv",sep="")
    dataValues$ts <- ts
    dataValues$cleanedFilename <- cleanedFilename
    file.copy(input$imputationInput$datapath,filename, overwrite = TRUE)
    
    
    print(filename)
    variable_groups <- input$vgroup
    print(variable_groups)
    
    compound_missing <- input$fmt # column
    sample_missing <- input$smt # row
    
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
  
  
  
     print(input$imputationAlgorithm)
     
    
    if(input$logshift){
      df_logshift <- log_shift(dataValues$db_data_cleaned)
      dataValues$minVal <- df_logshift[[2]] 
      dataValues$db_data_cleaned_logshift <- df_logshift[[1]]
    }else{
      dataValues$db_data_cleaned_logshift <- dataValues$db_data_cleaned
    }
    
        
    if(input$imputationAlgorithm=="min"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "minimum", var_group = dataValues$groups)
      dataValues$imputedDataframe <- imputed_df[[1]]
    }else if(input$imputationAlgorithm=="mean"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "mean", var_group = dataValues$groups) 
      dataValues$imputedDataframe <- imputed_df[[1]]  
    }else if(input$imputationAlgorithm=="median"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "median", var_group = dataValues$groups)
      dataValues$imputedDataframe <- imputed_df[[1]]
    }else if(input$imputationAlgorithm=="max"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "maximum", var_group = dataValues$groups)
      dataValues$imputedDataframe <- imputed_df[[1]]
    }else if(input$imputationAlgorithm=="min5"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "one_fifth_minimum", var_group = dataValues$groups)
      dataValues$imputedDataframe <- imputed_df[[1]]
    }else if(input$imputationAlgorithm=="knn"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "KNN", k_val = as.numeric(input$kValue), var_group = dataValues$groups)
      dataValues$imputedDataframe <- imputed_df[[1]]
    }else if(input$imputationAlgorithm=="rf"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "RandomForest", tree_val = as.numeric(input$treeVal), var_group = dataValues$groups)
      dataValues$imputedDataframe <- imputed_df[[1]]
    }else if(input$imputationAlgorithm=="mice"){
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "MICE", mice_iteration = as.numeric(input$miceIteration), var_group = dataValues$groups)
      dataValues$imputedDataframe <- imputed_df[[1]]
    }else{
      # janice added the "full_search" checkbox here
      imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, full_search = input$full_search_in, missing_variable_percentage = compound_missing, method = "optimization", var_group = dataValues$groups)
      dataValues$imputedDataframe <- imputed_df[[1]]
      tableoutput <- imputed_df[[2]]
    
      
      output$table <- renderDataTable(tableoutput)
      
    }
    
  
    # reverse transformation
    reversed_imputed_df <- reverse_log_shift(dataValues$imputedDataframe, min_val = dataValues$minVal)
    returned_df <- add_group_info(reversed_imputed_df, dataValues$groups)
    
    print("HELLO THERE")
    
    dataValues$reversed_imputed_df <- returned_df
    
    imputedFilename <- paste(dataValues$ts,"_imputed_data.csv",sep="")
    dataValues$imputedFilename <- imputedFilename
    
         
      output$export_btn <- downloadHandler(
      filename = function() {
        paste(dataValues$imputedFilename,sep="")
      },
      content = function(file) {
        write.csv(dataValues$reversed_imputed_df, file)
      }
    )
    
    
      updateTextInput(session, "condition",
      label = "",value = "1")
    
    
  
    
    # pca
    pca_raw <- plot_PCA(dataValues$compoundOnly, "PCA Original data before imputation")
    pca_imputed <- plot_PCA(reversed_imputed_df, "PCA Data after imputation")
    dataValues$pcaRaw <- pca_raw
    dataValues$pcaImputed <- pca_imputed
    
    
    # tsne:
    tsne_raw <- plot_tSNE(dataValues$compoundOnly, "tSNE Original data before imputation")
    tsne_imputed <- plot_tSNE(reversed_imputed_df, "tSNE Data after imputation")
    dataValues$tsneRaw <- tsne_raw
    dataValues$tsneImputed <- tsne_imputed
    
    
    output$pcaRaw <- renderPlot({
      dataValues$pcaRaw
    }, res = 96)
    
    output$pcaImputed <- renderPlot({
      dataValues$pcaImputed
    }, res = 96)
    
    output$tsneRaw <- renderPlot({
      dataValues$tsneRaw
    }, res = 96)
    
    output$tsneImputed <- renderPlot({
      dataValues$tsneImputed
    }, res = 96)
    
     waiter_hide()
  
  })
  

}

# Run
shinyApp(ui, server)
