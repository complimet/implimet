################################################
# This is the main script with function call
################################################
library(tidyverse)
library(magrittr)

# source functions
# setwd("~/Desktop/imputation")
source("~/data_analysis/imputation/web_app/Implimet/src/app_general_function.r")
source("~/data_analysis/imputation/web_app/Implimet/src/app_imputation_function.r")
source("~/data_analysis/imputation/web_app/Implimet/src/app_generate_missing.r")

#----------------------------USER INPUT------------------------------------------
# column: compounds, row: samples
filename <- "data/grouped_example_lipid.csv"

# clean up variables selection
compound_missing <- 0.1 # column
sample_missing <- 0.1 # row

# choose method: 7 choices
# optimized, min, mean, median, knn, RF, mice
optimized_method <- c("optimization")
full_search_op <- "FALSE"

# grouping input (optional) for MAR missing generation
variable_groups <- T

#-----------------------------1.START-----------------------------------------
# data input
raw_data <- read_in(filename, var_groups = variable_groups)[[1]]
# data input
compound_only <-  read_in(filename, var_groups = variable_groups)[[2]]
# variable groups
group <- read_in(filename, var_groups = variable_groups)[[3]]

# data transformation 
# transformed_compound_only <- compound_only
transformed_compound_only <- log_shift(compound_only)[[1]]
min_val <- log_shift(compound_only)[[2]]

# clean up the dataframe
compound_only_cleaned <- clean_up(transformed_compound_only, sample_missing, compound_missing)

# ````````````````````````2. ONE OF THE CASE IS RUN``````````````````````````
# # case 1: user chooses a specific imputation method
# # 8 available input: minimun, one_fifth_minium, mean, median, maximum, 
# # KNN + k_val, RandomForest + tree_val, MICE + mice_iteration

# # univariates
imputed_df <- imputation(cleaned_df = compound_only_cleaned, method = "minimum", var_group = group)
imputed_df <- imputation(cleaned_df = compound_only_cleaned, method = "one_fifth_minimum", var_group = group)
imputed_df <- imputation(cleaned_df = compound_only_cleaned, method = "mean", var_group = group)
imputed_df <- imputation(cleaned_df = compound_only_cleaned, method = "median", var_group = group)
imputed_df <- imputation(cleaned_df = compound_only_cleaned, method = "maximum", var_group = group)

# multi-variates
imputed_df <- imputation(cleaned_df = compound_only_cleaned, method = "KNN", k_val = 10, var_group = group)
imputed_df <- imputation(cleaned_df = compound_only_cleaned, method = "RandomForest", tree_val = 10, var_group = group)
imputed_df <- imputation(cleaned_df = compound_only_cleaned, method = "MICE", mice_iteration = 5, var_group = group)

# ``````````````````````````````````````````````````````````````````````````````
# # case 2: user chooses optimization
#debug(imputation)
imputed_df <- imputation(cleaned_df = compound_only_cleaned, 
                         method = optimized_method, 
                         missing_variable_percentage = compound_missing, 
                         var_group = group, full_search = full_search_op)


# ```````````````````````3. REVERSE TRANSFORMATION AND SAVE IMPUTED DATA---------------
# reverse transform the data to original values
reversed_imputed_df <- reverse_log_shift(imputed_df[[1]], min_val = min_val)
# add the group column and save the data in current working directory
returned_df <- add_group_info(reversed_imputed_df, group_info  = group)

# ---------------------- 4. VISUALIZATION ---------------------------------------------
# pca
pca_raw <- plot_PCA(compound_only, "Original data before imputation")
pca_imputed <- plot_PCA(reversed_imputed_df, "Data after imputation")

# tsne: https://datavizpyr.com/how-to-make-tsne-plot-in-r/ 
tsne_raw <- plot_tSNE(compound_only, "Original data before imputation")
tsne_imputed <- plot_tSNE(reversed_imputed_df, "Data after imputation")



