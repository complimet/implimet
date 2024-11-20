# Installation instructions 


This document provides a short description of how to use ImpLiMet on your computer.

ImpLiMet is a stand-alone application implemented using the R language (R > 4.0) and the Shiny libraries. It can be installed as any other R package on several operating systems (Windows, macOS and Linux). Download this repository and open app.R in RStudio. Install necessary libraries (see Dependancies bellow) and  just Run Application from RStudio. 

# Platforms

ImpLiMet runs under RShiny and can be easily run within RStudio on any computer platform.

# Script Overview

All scripts for ImpLiMet are written in R and are located in src or www folders. Included are:

./app.R - Shiny app creating UI, data input and output and calling of other functions
./src/app_data_manipulation.r - R code for data preparation for anlaysis
./src/app_general_function.r - R code with general data manipulation fuction
./src/app_generate_missing.r - R code for the generation of missing values for method optimization
./src/app_imputation_function.r - R code with imputation functionalities


# Dependencies

ImpLiMet requires several R packages that are available on CRAN. All required packages can be installed using this command:

install.packages(c("shiny","shinydashboard","shinyWidgets","tidyverse","cluster","factoextra","dendextend","RColorBrewer","gplots","viridis","dplyr","ggplot2","DT","Hmisc","magrittr","impute","missRanger","mice","factoextra","ggfortify","foreach","Rtsne","magrittr","dplyr","tidyr","heatmap3"))
