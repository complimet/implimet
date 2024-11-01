
Installation instructions 
This document provides a short description of how to use ImpLiMet on your computer.

Platforms

ImpLiMet runs under RShiny and can be easily user within RStudio on any computer platform.

Script Overview

All scripts for ImpLiMet are written in R and are located in src or www folders. Included are:

./src/app.R


Dependencies

ImpLiMet requires several R packages that are available on CRAN and can be installed using this command:
install.packages(c("shiny","shinydashboard","shinyWidgets","tidyverse","cluster","factoextra","dendextend","RColorBrewer","gplots","viridis","dplyr","ggplot2","DT","Hmisc","magrittr","impute","missRanger","mice","factoextra","ggfortify","foreach","Rtsne","magrittr","dplyr","tidyr","heatmap3"))



