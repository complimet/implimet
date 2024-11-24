## Implimet
# Shiny app
# By Anu Surendra


automatic_install_packages = function(){
  requiredPackages = c("shiny","shinydashboard","shinyWidgets","waiter",
                          "tidyverse","cluster","factoextra","dendextend",
                          "RColorBrewer","gplots","viridis","dplyr","ggplot2",
                          "DT","Hmisc","reshape2","moments","ipc","future",
                          "promises","missRanger","mice","ggfortify",
                          "Rtsne","magrittr","tidyr","heatmap3","foreach","svglite","impute")
  
  for(p in requiredPackages){
    if(!require(p,character.only = TRUE)){
      install.packages(p)
    }
  }
  
}

getCurrentFileLocation <-  function()
{
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

automatic_install_packages()

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(waiter)
library(svglite)
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
library(reshape2)
library(moments)
library(ipc)
library(future)
library(promises)
plan(multicore) 

main.dir <- getCurrentFileLocation()
setwd(main.dir)

source("./src/app_general_function.r")
source("./src/app_imputation_function.r")
source("./src/app_generate_missing.r")
# UI
sidebar <- dashboardSidebar(
    sidebarMenu(
        div(
            align = "center",
            br(),
            tags$figure(
                tags$img(
                    src = "logo_only.png", # 
                    width = 100,
                    alt = ""
                ),
            ),
            h3("ImpLiMet v1.0"),
            br()
        ),
        menuItem("Getting started", tabName = "start", icon = icon("gauge")),
        menuItem("Download sample data", tabName = "samples", icon = icon("download")),
        menuItem("Analyze", tabName = "analyze", icon = icon("magnifying-glass")),
        menuItem("Troubleshoot", tabName = "troubleshoot", icon = icon("screwdriver-wrench")),
        menuItem("Authors and citing", tabName = "cite", icon = icon("pencil")),
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
                column(
                    12,
                    h1("Overview of ImpLiMet"),
                    p(
                        "Selecting the optimal imputation method for a dataset depends on evaluating the cause of the missingness in the data and the characteristics of the data. ImpLiMet enables users to impute missing data using 8 different methods and proposes the optimal imputation approach for the user’s data set if users have at least three features and six samples without any missing values in their dataset. ImpLiMet can be used for any dataset.
               Code is available at:", a("https://github.com/complimet/ImpLiMet", href = "https://github.com/complimet/ImpLiMet")
                    ),
                    br(),
                    p(
                        "Users can either select an imputation method among the eight provided options (indicated below) or opt for the automated selection of the optimal method for their dataset. The optimized method is determined as follows:"
                    ),
                    tags$ol(
                        tags$li(
                            p(
                                "ImpLiMet selects the largest subset of data with no missing values from the user’s dataset. There must be at least three features and six samples to optimize the imputation method.  If this criteria is not met, the user chooses their own implementation method. ",
                            )
                        ),
                        tags$li(
                            p(
                                "Three missing mechanisms are then simulated from the user’s data: missing at random (MAR), missing not at random (MNAR), and missing completely at random (MCAR).",
                            )
                        ),
                        tags$li(
                            p(
                                "Eight imputation methods are used to impute missing data in these three simulations.  These imputation methods include five univariate and three multivariate methods:",
                                br(),
                                tags$span(
                                    tags$ul(
                                        tags$li(
                                            p(
                                                "5 univariate methods: ", tags$b(" mean, minimum, 1/5 minimum, median, and maximum")
                                            )
                                        ),
                                        tags$li(
                                            p(
                                                "3 multi-variate methods: ",
                                                tags$b("k-nearest neighbour (KNN), Random Forest (RF), and Multivariate Imputation by Chained Equations (MICE)."),
                                                " For the multivariate methods, the required parameters (number of neighbours for KNN, trees for RF, and iterations for MICE) are also optimized."
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
                                    "A comparison table is displayed showing the MAPE for each method. The method with the lowest MAPE across the missingness simulations is suggested as the optimal method. If the full parameter search is selected in the optimization for RF, KNN and MICE, MAPE is calculated for a range of hyperparameters (for RF: 5 to 500 trees, for KNN N from 10 to 100, for MICE 1 to 3 iterations).  Without full search selected, only one parameter is tested (for RF, tree of 500, for KNN N=10 and for MICE run uses 2 iterations).",
                                )
                            ),
                            tags$li(
                                p(
                                    "The effect of the chosen imputation method on the data structure is assessed and visualized by histograms and principal component analysis (PCA), comparing the impact of removing all features and all samples with missing data to the chosen imputation method on data structure, including kurtosis and skewness information challenging assumptions of a normal distribution.",
                                )
                            ),
                        ),
                    ),
                    br(),
                ),
                column(
                    12,
                    div(
                        br(),
                        tags$figure(
                            align = "center",
                            tags$img(
                                src = "impute_figure_v5.svg",
                                width = "90%",
                                alt = ""
                            )
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
                    br(),
                    p(
                        h4("Input data format and examples as a .CSV file")
                    ),
                    p(
                        tags$ol(
                            tags$li("Input data without multiple feature measurement groups"),
                            tags$i("Column one must contain Sample IDs. Row one must contain feature names."),
                            p(
                                tags$figure(
                                    align = "left",
                                    tags$img(
                                        src = "./input_data_without_grouping.png",
                                        width = "50%",
                                        height = "50%",
                                        alt = ""
                                    )
                                )
                            ),
                            tags$li("Input data with multiple feature measurement groups."),
                            tags$i("If the dataset includes features measured in different units or on different platforms (multiple feature measurement groups), data should be formatted to indicate which feature measurement groups are to be considered separately for missing data simulation.  In this case, row one must contain feature names. Row 2 must contain the feature measurement group information."),
                            p(
                                tags$figure(
                                    align = "left",
                                    tags$img(
                                        src = "./input_data_with_grouping.png",
                                        width = "50%",
                                        height = "50%",
                                        alt = ""
                                    )
                                )
                            )
                        )
                    ),
                    br(),
               
                    h3("Sample Data"),
                    tags$ol(
                        tags$li(
                            tags$a("Input data without multple feature measurement groups", href = "./src/input_templates/Input_data_without_group_info.csv")
                        ),
                        tags$li(
                            tags$a("Input data with multiple feature measurement groups", href = "./src/input_templates/Input_data_with_group_Info.csv")
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
                    strong("Imputation"),
                    div(
                        id = "imputeDiv",
                        useWaiter(),
                        span(class="btn btn-default action-button btn-warning shiny-bound-input", style = "float:right;padding:10px 80px 10px 80px;display: inline-block;color: #fff;", a("User Manual", href = "Manual_Nov2024.pdf", target = "_blank",style="font-family: Montserrat;color: #fff;")),
                        h2("Step 1"),
                        checkboxInput("vgroup", "This box must be selected prior to data upload if input includes information about multiple feature measurement groups (see download sample data for information about the required input format).", value = F),
                        p(
                            "Upload a file for imputation (*.csv)."
                        ),
                        fileInput(
                            inputId = "imputationInput",
                            label = NULL,
                            width = "50%"
                        ),
                        textAreaInput("caption_input", "", width = "900px", rows = 4),
                        h2("Step 2"),

                        selectInput("smt", "Remove samples with the specified % of missing values", selected = "Don't remove any samples", c("Don't remove any samples", seq(10, 100, 10)), width = "50%"),

                        selectInput("fmt", "Remove features with the specified % of missing values", selected = "Don't remove any features", c("Don't remove any features", seq(10, 100, 10)), width = "50%"),
                        textAreaInput("caption", "", width = "900px", rows = 4),
                        downloadButton("export_btn_cleanedData", "Cleaned Data"),

                        h2("Step 3"),
                        p(
                            "Select imputation method (Note: a minimum of 6 samples or 3 features without missing values is required for the full optimization option). Final results are graphically represented in the Visualization tab above."
                        ),
                        selectInput("imputationAlgorithm",
                            label = "Select imputation method:",
                            c(
                                "Min" = "min",
                                "Max" = "max",
                                "Median" = "median",
                                "Mean" = "mean",
                                "Optimization" = "optimization",
                                "Feature Minimum/5" = "min5",
                                "KNN" = "knn",
                                "RF" = "rf",
                                "MICE" = "mice"
                            ),
                            selected = "optimization",
                            width = "50%"
                        ),
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
                            checkboxInput("full_search_in", "full parameter search (slow for large dataset)", value = F)
                        ),
                        # ------- another check box--------------------
                        useWaiter(),
                        actionButton("runIMPLIMET",
                            label = "Run IMPLIMET",
                            class = "btn-warning",
                            style = "color: #fff;"
                        ),
                        br(),
                        conditionalPanel(
                            condition = "input.condition2 == 1",
                            downloadButton("export_btn", "Imputed Data")
                        ),
                        br(),
                        conditionalPanel(
                            condition = "input.condition == 1",
			    p(
                              tags$b("Orange label indicates the minimal MAPE value across all tests and the method used for the imputation following this optimization. Blue label indicates the minimal average of MAPE values for the three missingness types. If this imputation method is preferred please select it and run ImpLiMet.")
                            ),
                            DTOutput("table")
                        )
                    )

                ),
                tabPanel(
                  strong("Visualization"),
                  div(
                        id = "visualDiv",
                        useWaitress(),
                        
                  conditionalPanel(
                            condition = "input.visualPlotDiv == 1",
                    p(
                        em("Visualization is only provided when there is more than 2 samples and features without any missing values.")
                    ), br(),
                    p(
                        "Histogram of normalized raw data and imputed data"
                    ),
                    tags$table(
                        width = "100%", border = "0",
                        tags$tr(
                            tags$td(span(plotOutput("histRaw"))), tags$td(span(plotOutput("histImputed")))
                        ), tags$tr(
                            tags$td(colspan = 2, span(plotOutput("skewnessRaw")))
                        ),
                        tags$tr(
                            tags$td(colspan = 2, span(plotOutput("kurtosisRaw")))
                        ),
                    ), br(), br(),
                    p(
                        "Principal component analysis (PCA) for Samples (top) and Features (bottom)"
                    ),
                    tags$table(
                        width = "100%", border = "0",
 
                        tags$tr(
                            tags$td(span(plotOutput("pcaRaw"))), tags$td(span(plotOutput("pcaImputed"))),
                        ),
                        tags$tr(
                            tags$td(span(plotOutput("tsneRaw"))), tags$td(span(plotOutput("tsneImputed")))
                        )
                    ),
                    br(), conditionalPanel(
                            condition = "input.visualPlotDownload == 1",
                            downloadButton("export_btn_visual", "Data Visualization"))
                ))),
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
                        "When troubleshooting, please review this list of common reasons for ImpLiMet failing to run. If you are still experiencing difficulties, please contact ", a("ldomic@uottawa.ca", href = "mailto: ldomic@uottawa.ca"), " for further assistance. Please include your input dataset and a description of the problem that you experienced. We will reproduce the problem and provide you with a solution."
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
                    h4("3. ImpLiMet accepts input datasets with any number of feature measurement groups."),
                    p(
                        "Although any number of feature measurement groups is acceptable and will provide results keep in mind that more features in a feature measurement group you have the more accurate will be the imputation."
                    ),
                    br(),
                    h4("4. Sample IDs must be unique for each row."),
                    p(
                        "ImpLiMet does not analyze duplicated samples, so sample names must be unique in the input. Please avoid using only numbers for sample naming. Instead, please use a combination of characters and numbers." # janice added comments
                    ),
                    br(),
                    h4("5. Selection of an Imputation method provides output but selection of the optimization option crashes."),
                    p(
                        "There must be a minimum of THREE or more complete (no missing values) columns (features) and minimum of SIX complete rows (samples) to run the optimization option. For very small datasets (e.g., dimensions 3x6) the sample and feature threshold must be set to 10% for full optimization." # MCC added comments
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
                    p("Ou H, Surendra A, McDowell GSV, Hashimoto-Roth E,  Xia J, Bennett SAL, Cuperlovic-Culf M (2024) Imputation for Lipidomics and Metabolomics (ImpLiMet): Online application for optimization and method selection for missing data imputation. bioRxives, June, 2024: https://doi.org/10.1101/2024.06.17.599353 "),
                    br(),
                    h3("Public Server"),
                    p("ImpLiMet: ", a("https://complimet.ca/shiny/implimet/", href = "https://complimet.ca/shiny/implimet/")),
                    br(),
                    h3("Software License"),
                    p(
                        "ImpLiMet is free software. You can redistribute it and/or modify it under the terms of the ",
                        a("GNU General Public License", href = "https://www.gnu.org/licenses/", target = "_blank"),
                        " v3 (or later versions) as published by the Free Software Foundation. As per the GNU General Public License, ImpLiMet is distributed as a bioinformatic
            tool to assist users WITHOUT ANY WARRANTY and without any implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. All
            limitations of warranty are indicated in the GNU General Public License."
                    ),
                    br()
                ),
                width = 12
            )
        )
    ),
    span(style = "visibility:hidden", textInput("condition", "", "0")),
    span(style = "visibility:hidden", textInput("condition2", "", "0")),
    span(style = "visibility:hidden", textInput("tableCol", "", "0")),
    span(style = "visibility:hidden", textInput("visualPlotDiv", "", "0")),
    span(style = "visibility:hidden", textInput("visualPlotDownload", "", "0")),

    ################
    # HEADER TITLE #
    ################
    tags$head(tags$script(HTML(
        '$(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass" style="float:right">Imputation for Lipidomics and Metabolomics</span>\');
      })'
    )))
)


ui <- dashboardPage(
    dashboardHeader(title = tags$a(id = "logomainlink", href = "https://complimet.ca/", "CompLiMet")),
    sidebar,
    body,
    skin = "black",
    tags$head(
        tags$title("Implimet"),
        includeCSS(paste(main.dir,"/www/complimetGUI.css",sep=""))
    )
)

# Server
server <- function(input, output, session) {

    options(shiny.maxRequestSize=50*1024^2) 
    if (!interactive()) sink(stderr(), type = "output")
  main.dir <- getCurrentFileLocation()
   ifelse(!dir.exists(file.path(paste(main.dir,"/www/tmp/",sep=""))), dir.create(file.path(paste(main.dir,"/www/tmp/",sep=""))), FALSE)
    
    w1 <- Waiter$new( id = "imputeDiv",
            html = spin_fading_circles())
			
			
    waitress1 <- Waitress$new("#histRaw",theme = "overlay-percent",infinite = T, hide_on_render = T)
    waitress2 <- Waitress$new("#histImputed",theme = "overlay-percent",infinite = T, hide_on_render = T)
    waitress3 <- Waitress$new("#skewnessRaw",theme = "overlay-percent",infinite = T, hide_on_render = T)
    waitress4 <- Waitress$new("#kurtosisRaw",theme = "overlay-percent",infinite = T, hide_on_render = T)
    waitress5 <- Waitress$new("#pcaRaw",theme = "overlay-percent",infinite = T, hide_on_render = T)
    waitress6 <- Waitress$new("#pcaImputed",theme = "overlay-percent",infinite = T, hide_on_render = T)
    waitress7 <- Waitress$new("#tsneRaw",theme = "overlay-percent",infinite = T, hide_on_render = T)
    waitress8 <- Waitress$new("#tsneImputed",theme = "overlay-percent",infinite = T, hide_on_render = T)
    
    

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
        clustRaw = NULL,
        clustImputed = NULL,
        tableoutput = NULL,
		process = NULL
    )

    observeEvent(input$imputationInput, {
        updateTextInput(session, "condition",
            label = "", value = "0"
        )
        updateTextInput(session, "condition2",
            label = "", value = "0"
        )
        
        updateTextInput(session, "visualPlotDiv",
                label = "", value = "0"
        )
        
         updateTextInput(session, "visualPlotDownload",
                label = "", value = "0"
        )


        req(input$imputationInput, input$fmt, input$smt)

        ts <- format(Sys.time(), format = "%m%d%Y%H%M%S")

        dir.create(file.path("./www/tmp/", ts), showWarnings = FALSE)

        filename <- paste("./www/tmp/", ts, "/", ts, "_data.csv", sep = "")
    
        cleanedFilename <- paste(ts, "_cleaned_data.csv", sep = "")
        file.copy(input$imputationInput$datapath, filename, overwrite = TRUE)


        variable_groups <- input$vgroup
        compound_missing <- ifelse(tolower(input$fmt) == tolower("Don't remove any features"), 110 / 100, as.numeric(input$fmt) / 100) # column
        sample_missing <- ifelse(tolower(input$smt) == tolower("Don't remove any samples"), 110 / 100, as.numeric(input$smt) / 100) # row
        raw_data <- read_in(filename, variable_groups)[[1]]
        compound_only <- read_in(filename, variable_groups)[[2]]
        group <- read_in(filename, variable_groups)[[3]]

        dataValues$groups <- group

        if (isTRUE(variable_groups)) {
            unique_groups <- length(unique(as.numeric(group)))
            sample_dimension <- dim(raw_data)[1] - 1
        } else {
            unique_groups <- 1
            sample_dimension <- dim(raw_data)[1]
        }


        transformed_compound_only <- log_shift(compound_only)[[1]]
        compound_only_cleaned <- clean_up(transformed_compound_only, sample_missing, compound_missing)

        updateTextAreaInput(session, "caption_input",
            label = "",
            value = paste("Total number of sample(s): ", sample_dimension, "\nTotal number of feature(s): ", dim(compound_only)[2],
                "\nTotal number of measurement groups: ", unique_groups, "\nTotal number of missing values in the dataset: ", sum(is.na(transformed_compound_only)),
                sep = ""
            )
        )



        updateTextAreaInput(session, "caption",
            label = "",
            value = paste("The sample(s) left : ", dim(compound_only_cleaned)[1], "\n and the feature(s) left: ", dim(compound_only_cleaned)[2], sep = "")
        )



        output$export_btn_cleanedData <- downloadHandler(
            filename = function() {
                paste(ts, "_Cleaneddata.csv", sep = "")
            },
            content = function(file) {
                write.csv(compound_only_cleaned, file)
            }
        )

        unlink(paste("./www/tmp/", ts, "/*", sep = ""), recursive = T)
    })



    observeEvent(input$imputationAlgorithm, {
        updateTextInput(session, "condition2",
            label = "", value = "0"
        )
        if (input$imputationAlgorithm != "optimization") {
            updateTextInput(session, "condition",
                label = "", value = "0"
            )
        }
    })


    observeEvent(input$smt, {
        req(input$imputationInput, input$fmt, input$smt)

        ts <- format(Sys.time(), format = "%m%d%Y%H%M%S")

        dir.create(file.path("./www/tmp/", ts), showWarnings = FALSE)

        filename <- paste("./www/tmp/", ts, "/", ts, "_data.csv", sep = "")
   
        cleanedFilename <- paste(ts, "_cleaned_data.csv", sep = "")
        file.copy(input$imputationInput$datapath, filename, overwrite = TRUE)


        variable_groups <- input$vgroup

        compound_missing <- ifelse(tolower(input$fmt) == tolower("Don't remove any features"), 110 / 100, as.numeric(input$fmt) / 100) # column

        sample_missing <- ifelse(tolower(input$smt) == tolower("Don't remove any samples"), 110 / 100, as.numeric(input$smt) / 100) # row
        
        raw_data <- read_in(filename, variable_groups)[[1]]
        compound_only <- read_in(filename, variable_groups)[[2]]
        transformed_compound_only <- log_shift(compound_only)[[1]]
        compound_only_cleaned <- clean_up(transformed_compound_only, sample_missing, compound_missing)


        updateTextAreaInput(session, "caption",
            label = "",
            value = paste("The sample(s) left : ", dim(compound_only_cleaned)[1], "\n and the feature(s) left: ", dim(compound_only_cleaned)[2], sep = "")
        )



        output$export_btn_cleanedData <- downloadHandler(
            filename = function() {
                paste(ts, "_Cleaneddata.csv", sep = "")
            },
            content = function(file) {
                write.csv(compound_only_cleaned, file)
            }
        )

        unlink(paste("./www/tmp/", ts, "/*", sep = ""), recursive = T)
    })


    observeEvent(input$fmt, {
        req(input$imputationInput, input$fmt, input$smt)

        ts <- format(Sys.time(), format = "%m%d%Y%H%M%S")

        dir.create(file.path("./www/tmp/", ts), showWarnings = FALSE)

        filename <- paste("./www/tmp/", ts, "/", ts, "_data.csv", sep = "")
  
        cleanedFilename <- paste(ts, "_cleaned_data.csv", sep = "")
        file.copy(input$imputationInput$datapath, filename, overwrite = TRUE)


        variable_groups <- input$vgroup

        compound_missing <- ifelse(tolower(input$fmt) == tolower("Don't remove any features"), 110 / 100, as.numeric(input$fmt) / 100) # column

        sample_missing <- ifelse(tolower(input$smt) == tolower("Don't remove any samples"), 110 / 100, as.numeric(input$smt) / 100) # row

        raw_data <- read_in(filename, variable_groups)[[1]]
        compound_only <- read_in(filename, variable_groups)[[2]]
        transformed_compound_only <- log_shift(compound_only)[[1]]
        compound_only_cleaned <- clean_up(transformed_compound_only, sample_missing, compound_missing)


        updateTextAreaInput(session, "caption",
            label = "",
            value = paste("The sample(s) left : ", dim(compound_only_cleaned)[1], "\n and the feature(s) left: ", dim(compound_only_cleaned)[2], sep = "")
        )

        output$export_btn_cleanedData <- downloadHandler(
            filename = function() {
                paste(ts, "_Cleaneddata.csv", sep = "")
            },
            content = function(file) {
                write.csv(compound_only_cleaned, file)
            }
        )

        unlink(paste("./www/tmp/", ts, "/*", sep = ""), recursive = T)
    })

    observeEvent(input$stopIMPLIMET, {


    })
    
	
	
	
	observeEvent(input$runIMPLIMET, {
        

        
        w1$show()
        
        fut <- NULL
        
        ts <- format(Sys.time(), format = "%m%d%Y%H%M%S")
     
       dir.create(file.path("./www/tmp/", ts), showWarnings = FALSE)
       dataValues$mainDir <- paste0("./www/tmp/", ts)

       filename <- paste("./www/tmp/", ts, "/", ts, "_data.csv", sep = "")

        cleanedFilename <- paste(ts, "_cleaned_data.csv", sep = "")
        dataValues$ts <- ts
        dataValues$cleanedFilename <- cleanedFilename
        file.copy(input$imputationInput$datapath, filename, overwrite = TRUE)

        variable_groups <- input$vgroup


        compound_missing <- ifelse(tolower(input$fmt) == tolower("Don't remove any features"), 110 / 100, as.numeric(input$fmt) / 100) # column
        sample_missing <- ifelse(tolower(input$smt) == tolower("Don't remove any samples"), 110 / 100, as.numeric(input$smt) / 100) # row

        # data input
        raw_data <- read_in(filename, variable_groups)[[1]]



        # data input
        compound_only <- read_in(filename, variable_groups)[[2]]
        dataValues$compoundOnly <- compound_only

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

        dataValues$compoundOnly <- compound_only_cleaned #  added for missing rows 

        dataValues$db_data_cleaned_logshift <- dataValues$db_data_cleaned


        if (input$imputationAlgorithm == "min") {
            imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "minimum", var_group = dataValues$groups)
            dataValues$algo <- "Minimum"
            dataValues$imputedDataframe <- imputed_df[[1]]
        } else if (input$imputationAlgorithm == "mean") {
            imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "mean", var_group = dataValues$groups)
            dataValues$algo <- "Mean"
            dataValues$imputedDataframe <- imputed_df[[1]]
        } else if (input$imputationAlgorithm == "median") {
            imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "median", var_group = dataValues$groups)
            dataValues$algo <- "Median"
            dataValues$imputedDataframe <- imputed_df[[1]]
        } else if (input$imputationAlgorithm == "max") {
            imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "maximum", var_group = dataValues$groups)
            dataValues$algo <- "Maximum"
            dataValues$imputedDataframe <- imputed_df[[1]]
        } else if (input$imputationAlgorithm == "min5") {
            imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "one_fifth_minimum", var_group = dataValues$groups)
            dataValues$algo <- "One Fifth Minimum"
            dataValues$imputedDataframe <- imputed_df[[1]]
        } else if (input$imputationAlgorithm == "knn") {
            imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "KNN", k_val = as.numeric(input$kValue), var_group = dataValues$groups)
            dataValues$algo <- "KNN"
            dataValues$imputedDataframe <- imputed_df[[1]]
        } else if (input$imputationAlgorithm == "rf") {
            imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "RandomForest", tree_val = as.numeric(input$treeVal), var_group = dataValues$groups)
            dataValues$algo <- "Random Forest"
            dataValues$imputedDataframe <- imputed_df[[1]]
        } else if (input$imputationAlgorithm == "mice") {
            imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, method = "MICE", mice_iteration = as.numeric(input$miceIteration), var_group = dataValues$groups)
            dataValues$algo <- "MICE"
            dataValues$imputedDataframe <- imputed_df[[1]]
        } else {
            imputed_df <- imputation(cleaned_df = dataValues$db_data_cleaned_logshift, full_search = input$full_search_in, missing_variable_percentage = compound_missing, method = "optimization", var_group = dataValues$groups)
            dataValues$algo <- "Algorithm Optimization"
            dataValues$imputedDataframe <- imputed_df[[1]]
            dataValues$tableoutput <- imputed_df[[2]]
            dataValues$algo <- imputed_df[[3]]
              
     
            tableoutput <- dataValues$tableoutput
            
             avg.tableoutput <- apply(as.data.frame(tableoutput[,-1]),2,function(x){
              mean(as.numeric(gsub("\\s.*","",x,perl=T)))
              
            })
            
          
            avg.tableoutput <- c("Average",round(avg.tableoutput,3))
            tableoutput <- rbind(tableoutput,avg.tableoutput)
            dataValues$tableoutput <- tableoutput
            
            
            updateTextInput(session, "condition",
                label = "", value = "1"
            )
            
      

            coloured_colored <- which(colnames(tableoutput) == dataValues$algo)
            
            algo2 <- min(as.numeric(tableoutput[4,-1]))
            coloured_colored2 <- which(tableoutput[4,-1] == algo2)
            
            algo3 <- NULL
            
            if(coloured_colored>6){
              
              algo3 <- min(as.numeric(gsub("\\s.*","",unlist(tableoutput[,coloured_colored]),perl=T)))
              
            }else{
              
              algo3 <- min(as.numeric(unlist(tableoutput[,coloured_colored])))
            }
            
            coloured_colored3 <- which(tableoutput[,coloured_colored] == algo3)

            
            updateTextInput(session, "tableCol",
                label = "", value = coloured_colored
            )
            
            updateTextInput(session, "tableCol2",
                            label = "", value = coloured_colored2
            )
            
            updateTextInput(session, "tableCol3",
                            label = "", value = coloured_colored3-1
            )
            

            output$table <- renderDataTable(
                datatable(as.data.frame(tableoutput), options = list(
                    pageLength = 4, dom = "t", initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().columns().header()[$('#tableCol').val()]).css({'background-color': '#F39C12', 'color': 'white'});
                         $(this.api().table().cell(3,$('#tableCol').val()).node()).css({'background-color': '#1f4caf', 'color': 'white'});
                         $(this.api().table().cell($('#tableCol3').val(),$('#tableCol').val()).node()).css({'background-color': '#F39C12', 'color': 'white'});
                        ",
                        "}"
                    )
                ), escape = FALSE)
            )        
	}


        # reverse transformation
        reversed_imputed_df <- reverse_log_shift(dataValues$imputedDataframe, min_val = dataValues$minVal)
        returned_df <- add_group_info(reversed_imputed_df, dataValues$groups)


        dataValues$reversed_imputed_df <- returned_df

        imputedFilename <- paste(dataValues$ts, "_imputed_data.csv", sep = "")
        dataValues$imputedFilename <- imputedFilename



        updateTextInput(session, "condition2",
            label = "", value = "1"
        )

        output$export_btn <- downloadHandler(
            filename = function() {
                paste(dataValues$imputedFilename, sep = "")
            },
            content = function(file) {
                write.csv(dataValues$reversed_imputed_df, file)
            }
        )


        all.cleaned.df.samples <- dataValues$compoundOnly[complete.cases(dataValues$compoundOnly), ]
        all.cleaned.df.features <- as.data.frame(t(dataValues$compoundOnly))
  
        all.cleaned.df.features <- all.cleaned.df.features[complete.cases(all.cleaned.df.features), ]

		
        h1 <- NULL
		    h2 <- NULL
		    h3 <- NULL
		    h4 <- NULL
		    h5 <- NULL
            
        waitress1$start()

        if (dim(all.cleaned.df.samples)[1] > 2 && dim(all.cleaned.df.samples)[2] > 2 && dim(all.cleaned.df.features)[1] > 2 && dim(all.cleaned.df.features)[2] > 2) {
            pca_raw <- plot_PCA(dataValues$compoundOnly, paste("Cleaned data"))
            pca_imputed <- plot_PCA(reversed_imputed_df, paste("", dataValues$algo, "imputation"))
            dataValues$pcaRaw <- pca_raw
            dataValues$pcaImputed <- pca_imputed


           

            ggsave(file = paste(dataValues$mainDir, "/", dataValues$ts, "_pcaCleanedSamples.svg", sep = ""), plot = dataValues$pcaRaw, device = "svg", dpi = "print", width = 950, height = 950, units = "px", limitsize = F, scale = 4)

            ggsave(file = paste(dataValues$mainDir, "/", dataValues$ts, "_pcaImputedSamples.svg", sep = ""), plot = dataValues$pcaImputed, device = "svg", dpi = "print", width = 950, height = 950, units = "px", limitsize = F, scale = 4)



            tsne_raw <- plot_PCA(t(dataValues$compoundOnly), paste("Cleaned data"))
            tsne_imputed <- plot_PCA(t(reversed_imputed_df), paste("", dataValues$algo, "imputation"))
            dataValues$tsneRaw <- tsne_raw
            dataValues$tsneImputed <- tsne_imputed

            ggsave(file = paste(dataValues$mainDir, "/", dataValues$ts, "_pcaCleanedFeatures.svg", sep = ""), plot = dataValues$tsneRaw, device = "svg", dpi = "print", width = 950, height = 950, units = "px", limitsize = F, scale = 4)

            compound_only.scale <- scale(dataValues$compoundOnly)

            data.m.imputed.scale <- scale(reversed_imputed_df)

            hist.x.min <- min(melt(compound_only.scale)[, 3], melt(data.m.imputed.scale)[, 3], na.rm = T) - 1
            hist.x.max <- max(melt(compound_only.scale)[, 3], melt(data.m.imputed.scale)[, 3], na.rm = T) + 1

            data.m.density <- density(melt(compound_only.scale)[, 3], na.rm = T)
            data.m.imputed.density <- density(melt(data.m.imputed.scale)[, 3], na.rm = T)

            hist.y.min <- min(data.m.density$y, data.m.imputed.density$y, na.rm = T) - 5
            hist.y.max <- max(data.m.density$y, data.m.imputed.density$y, na.rm = T) + 5



            h1 <- NULL

            h1 <- ggplot(melt(compound_only.scale), aes(x = value)) +
                geom_histogram(fill = "#1f4caf", stat = "density", position = "dodge") +
                ggtitle("Cleaned data") +
                xlab("Scaled Feature Values") +
                xlim(hist.x.min, hist.x.max) +
                theme_classic()
            



            h2 <- ggplot(melt(data.m.imputed.scale), aes(x = value)) +
                geom_histogram(fill = "#FFA500", stat = "density", position = "dodge") +
                ggtitle(paste(dataValues$algo, "imputation")) +
                xlab("Scaled Feature Values") +
                xlim(hist.x.min, hist.x.max) +
                theme_classic()



            data.m.skewness <- as.data.frame(skewness(log10(abs(dataValues$compoundOnly)), na.rm = T))
            data.m.skewness <- cbind(rownames(data.m.skewness), data.m.skewness)
            colnames(data.m.skewness) <- c("features", "skewness")
            data.m.imputed.skewness <- as.data.frame(skewness(log10(abs(reversed_imputed_df)), na.rm = T))
            data.m.imputed.skewness <- cbind(rownames(data.m.imputed.skewness), data.m.imputed.skewness)
            colnames(data.m.imputed.skewness) <- c("features", "skewness")

            skewness.y.min <- min(data.m.skewness$skewness, data.m.imputed.skewness$skewness, na.rm = T) - 1
            skewness.y.max <- max(data.m.skewness$skewness, data.m.imputed.skewness$skewness, na.rm = T) + 1


            data.skewness <- cbind(data.m.skewness, data.m.imputed.skewness$skewness)
            colnames(data.skewness) <- c("features", "Cleaned data", paste(dataValues$algo, "imputation"))

            data.m.kurtosis <- as.data.frame(kurtosis(log10(abs(dataValues$compoundOnly)), na.rm = T))
            data.m.kurtosis <- cbind(rownames(data.m.kurtosis), data.m.kurtosis)
            colnames(data.m.kurtosis) <- c("features", "kurtosis")
            data.m.imputed.kurtosis <- as.data.frame(kurtosis(log10(abs(reversed_imputed_df)), na.rm = T))
            data.m.imputed.kurtosis <- cbind(rownames(data.m.imputed.kurtosis), data.m.imputed.kurtosis)
            colnames(data.m.imputed.kurtosis) <- c("features", "kurtosis")

            kurtosis.y.min <- min(data.m.kurtosis$kurtosis, data.m.imputed.kurtosis$kurtosis, na.rm = T) - 5
            kurtosis.y.max <- max(data.m.kurtosis$kurtosis, data.m.imputed.kurtosis$kurtosis, na.rm = T) + 5

            data.kurtosis <- cbind(data.m.kurtosis, data.m.imputed.kurtosis$kurtosis)
            colnames(data.kurtosis) <- c("features", "Cleaned data", paste(dataValues$algo, "imputation"))



            h3 <- ggplot(melt(data.skewness), aes(x = features, y = value, fill = variable)) +
                geom_bar(stat = "identity", color = "black", position = position_dodge(), width = 0.8) +
                ggtitle("Skewness") +
                scale_fill_manual(values = c("#1f4caf", "#FFA500")) +
                ylim(skewness.y.min, skewness.y.max) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank())


            h5 <- ggplot(melt(data.kurtosis), aes(x = features, y = value, fill = variable)) +
                geom_bar(stat = "identity", color = "black", position = position_dodge(), width = 0.8) +
                ggtitle("kurtosis") +
                ylim(kurtosis.y.min, kurtosis.y.max) +
                scale_fill_manual(values = c("#1f4caf", "#FFA500")) +
                theme_bw() +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.title = element_blank())




            ggsave(file = paste(dataValues$mainDir, "/", dataValues$ts, "_histogramCleanedFeatures.svg", sep = ""), plot = h1, device = "svg", dpi = "print", width = 950, height = 950, units = "px", limitsize = F, scale = 4)

            ggsave(file = paste(dataValues$mainDir, "/", dataValues$ts, "_histogramImputedFeatures.svg", sep = ""), plot = h2, device = "svg", dpi = "print", width = 950, height = 950, units = "px", limitsize = F, scale = 4)



            

            ggsave(file = paste(dataValues$mainDir, "/", dataValues$ts, "_skewnessFeatures.svg", sep = ""), plot = h3, device = "svg", dpi = "print", width = 950, height = 950, units = "px", limitsize = F, scale = 4)


            ggsave(file = paste(dataValues$mainDir, "/", dataValues$ts, "_kurtosisFeatures.svg", sep = ""), plot = h5, device = "svg", dpi = "print", width = 950, height = 950, units = "px", limitsize = F, scale = 4)

            ggsave(file = paste(dataValues$mainDir, "/", dataValues$ts, "_pcaImputedFeatures.svg", sep = ""), plot = dataValues$tsneImputed, device = "svg", dpi = "print", width = 950, height = 950, units = "px", limitsize = F, scale = 4)
        }

        
        updateTextInput(session, "visualPlotDiv",
                label = "", value = "1"
        )
   
		    output$histRaw <- renderPlot(
          {
            waitress1$start()
          
            validate(need(length(h1)>0,'Visualization is only provided when there \nis more than 2 samples and features without any missing values.'))
                   
            h1
           },
           res = 96
        )
            
            
        output$histImputed <- renderPlot(
          {
            waitress2$start()       
            validate(need(length(h2)>0,'Visualization is only provided when there \nis more than 2 samples and features without any missing values.'))
                   
            h2
                   
          },
          res = 96
        )


        output$skewnessRaw <- renderPlot(
          {
            waitress3$start()
          
            validate(need(length(h3)>0,'Visualization is only provided when there \nis more than 2 samples and features without any missing values.'))
  
             
             h3
          },
          res = 96
        )

        
        output$kurtosisRaw <- renderPlot(
          {
            waitress4$start()
          
            validate(need(length(h5)>0,'Visualization is only provided when there \nis more than 2 samples and features without any missing values.'))
            
            
            h5
          },
          res = 96
        )

        
        output$pcaRaw <- renderPlot(
          {
            waitress5$start()
          
            validate(need(length(dataValues$pcaRaw)>0,'Visualization is only provided when there \nis more than 2 samples and features without any missing values.'))
            dataValues$pcaRaw
          },
          res = 96
        )

        
        output$pcaImputed <- renderPlot( 
          {
            
            waitress6$start()
          
            validate(need(length(dataValues$pcaImputed)>0,'Visualization is only provided when there \nis more than 2 samples and features without any missing values.'))
            dataValues$pcaImputed
          },
          res = 96
        )
       
        
        output$tsneRaw <- renderPlot(
          {
            waitress7$start()
          
            validate(need(length(dataValues$tsneRaw)>0,'Visualization is only provided when there \nis more than 2 samples and features without any missing values.'))
            
            dataValues$tsneRaw
          },
          res = 96
        )

            
        output$tsneImputed <- renderPlot(
          {
            waitress8$start()
          
            validate(need(length(dataValues$tsneImputed)>0,'Visualization is only provided when there \nis more than 2 samples and features without any missing values.'))
            
            dataValues$tsneImputed
          },
          res = 96
        )

        
        if(length(h1)>0 && length(h2)>0 && length(h3)>0 && length(h5)>0 && length(dataValues$pcaRaw)>0 && length(dataValues$pcaImputed)>0 && length(dataValues$tsneRaw)>0 && length(dataValues$tsneImputed)>0){
          
          updateTextInput(session, "visualPlotDownload",
                label = "", value = "1"
          )
        
        }



        output$export_btn_visual <- downloadHandler(
            filename = function() {
                paste(dataValues$ts, "zip", sep = ".")
            },
            content = function(fname) {
                zip(zipfile = fname, flags = "-j", files = paste(dataValues$mainDir, unlist(list.files(path = dataValues$mainDir, pattern = ".svg")), sep = "/"))
            },
            contentType = "application/zip"
        )
        

        w1$hide()
    })
}

# Run
shinyApp(ui, server)
