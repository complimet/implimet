# libraries
library(impute)
library(missRanger)
library(mice)
library(factoextra)
library(ggfortify)
library(Rtsne)
library(magrittr)
library(dplyr)

###### read in dataset and select variables
read_in <- function(df_input, var_groups){
  raw_data <- read.csv(df_input,row.names = 1, header = T, check.names = F)
  if (var_groups == T){
    # data cleaning
    compound_only <- raw_data %>% select_if(is.numeric) %>% slice(-1)
    variable_groups <- raw_data %>% slice_head(n = 1)
    out <- list(raw_data, compound_only, variable_groups)
  }else if(var_groups == F){
    compound_only <- raw_data %>% select_if(is.numeric) 
    out <- list(raw_data, compound_only, NULL)
  }
  return(out)
}

##### clean up the datasets
clean_up <- function(compound_only, sample_thres, compound_thres){
  
  # ======= janice updated this block ============
  # get sample and compound total numbers
  sample_num <- dim(compound_only)[1]
  compound_num <- dim(compound_only)[2]
  
  # cut point
  row_thres <- sample_thres * compound_num
  col_thres <- compound_thres * sample_num
  
  # check missing number of compounds
  # 1. removal by compound
  col_missing <- apply(compound_only, 2, function(x) return(sum(is.na(x)) <= col_thres))
  cleaned_df_compound <- compound_only %>% select_if(col_missing)
  # 2. removal by sample
  row_missing <- apply(cleaned_df_compound, 1, function(x) return(sum(is.na(x)) <= row_thres))
  cleaned_df <- cleaned_df_compound %>% filter(row_missing)
  # ======= janice updated this block ============
  
  #-----------------------
  # filtering message
  # return the names of filtered metabolite and samples
  cat("The following are filtered:\n")
  if (sum(row_missing == F) == 0){
    cat("1. All samples are kept.\n")
  }else{
    cat("1. These samples are removed: ", names(which(row_missing == F)), "\n")
  }
  
  if (sum(col_missing == F) == 0){
    cat("2. All variables are kept.")
  }else{
    cat("2. These variables are removed: ", names(which(col_missing == F)), ".\n")
  }
  #-----------------------
  return(cleaned_df)
}

##### imputation starts
# var_group: supply group information to the optimization function for MAR generation
imputation <- function(cleaned_df,  method = "optimization", full_search = FALSE, 
                       k_val = NULL, tree_val = NULL, mice_iteration = NULL, #multi-variate method required input
                       missing_variable_percentage = NULL, var_group = NULL){
  cat("imputation - full search ", full_search)
  cleaned_df <- data.frame(cleaned_df, check.names = F)
  table_output <- NULL
  if(method == "optimization") {
    # perform method comparison table with imputed dataframes
    out <- method_optimization(cleaned_df, missing_variable_percentage, var_group = var_group, full_search = full_search)
    
    # ------------------------------------------------
    # 1. print out a table for imputation methods and errors
    table_output <- out %>% select(missing_type:mape) %>% mutate(mape = round(mape, digits = 3)) %>% 
      mutate(mape = ifelse(method == "KNN", paste0(mape, " (k:", opt_value,")" ),
                           ifelse(method == "RF",  paste0(mape, " (trees:", opt_value,")" ), 
                                  ifelse(method == "MICE",  paste0(mape, " (iteration index:", opt_value,")"), mape)))) %>% 
      pivot_wider(id_cols = missing_type, names_from  = method, values_from = c(mape))
    
    print(table_output) # show output table of comparison
    # -------------------------------------------------
    # ======= janice disabled the get user input here, by default retrieve the lowest MAPE method =======
    
    # 2. get user input here with prompts -------------
    # default <- readline(prompt="Would you like to get the default: Y/N ") ## user input
    # if (default == "Y"){
      min_method_table <-  out %>% slice_min(mape)
      
      # check if multiple methods return the same error rate
      if(dim(min_method_table)[1] == 1){
        method <- min_method_table$method
        opt_val <-min_method_table$opt_value
      }else if(dim(min_method_table)[1] > 1){
        
        # if MAPE is equal, then use this priority list to choose the method
        priority_list <- factor(c("RF", "MICE", "KNN", "one_fifth_minimum", "mean", "median", "minimum", "maximum"))
        first_match <- priority_list %>% intersect(min_method_table$method) %>% first()
        matched_row <- min_method_table %>% filter(method == first_match)
        
        # assign the method and opt value
        method <- matched_row$method
        opt_val <-matched_row$opt_value
      }
      
    # }else{ # do not add quotation
    #   miss_type <- readline(prompt="Choose 1 of 3 'MCAR, MAR, MNAR': " )  ## user input
    #   ## user input
    #   imputed_method <- readline(prompt="Select 1 of 8 method: \n<minimum, one_fifth_minimum, mean, median, 
    #                              \nmaximum, KNN, RandomForest, MICE>: " )
    #   method <- out %>% filter(missing_type == miss_type & method == imputed_method) %>% pull(method)
    #   opt_val <- out %>% filter(missing_type == miss_type & method == imputed_method) %>% pull(opt_value)
    # }
    
    # print out messages
    if (method == "KNN") {
      k_val <- opt_val
      cat(method, "is applied. Optimized parameter is", opt_val)
    } else if (method == "RandomForest" | method == "RF") {
      tree_val <- opt_val
      cat(method, "is applied. Optimized parameter is", opt_val)
    } else if (method == "MICE") {
      mice_iteration <- opt_val
      cat(method, "is applied. Optimized parameter is", opt_val)
    } else {
      cat("univariate_method", method)
    }
    
  }
  
  ##### univariate methods: min, 1/5 min, max, median, mean
  if(method == "minimum"){
    imputed_df <- impute_min(compound_df = cleaned_df, type = "minimum")
  }else if (method == "one_fifth_minimum"){
    imputed_df <- impute_min(compound_df = cleaned_df, type = "one_fifth_minimum")
  }else if (method == "mean"){
    imputed_df <- impute_mean(compound_df = cleaned_df)
  }else if (method == "median"){
    imputed_df <- impute_median(compound_df = cleaned_df)
  }else if (method == "maximum"){
    imputed_df <- impute_max(compound_df = cleaned_df)
    
    ##### multivariate methods
  }else if (method == "KNN"){
    imputed_df <- impute_knn(compound_df = cleaned_df, k_val)
  }else if(method == "RandomForest" | method == "RF"){
    imputed_df <- impute_RF(compound_df = cleaned_df, tree_val)
  }else if (method == "MICE"){
    imputed_df <- impute_MICE(compound_df = cleaned_df, mice_iteration)
  }
  
  return(list(imputed_df,table_output))
}
#-----------------------------------------------------
##### start optimization
method_optimization <- function(cleaned_df, missing_percentage, var_group = NULL, full_search = FALSE){
  cat("method_opt - full search ", full_search)
  # 1. filter samples with missing data
  full_df <- cleaned_df[complete.cases(cleaned_df), ]
  
  # 2. remove samples with three methods
  df_MCAR <- generate_MCAR(full_df, missing_percent = missing_percentage)
  df_MAR  <- generate_MAR(full_df, misspercent = missing_percentage, var_group = var_group)
  df_MNAR <- generate_MNAR(full_df, misspercent = missing_percentage)
  
  # 3. impute the missing df with 8 methods
  opt_mcar <- test_all_imputation(full_df, df_MCAR, "MCAR", full_search = full_search)
  opt_mnar <- test_all_imputation(full_df, df_MNAR, "MNAR", full_search = full_search)
  opt_mar <-  test_all_imputation(full_df, df_MAR, "MAR",  full_search = full_search)
  
  # 4. generate an output
  out <- rbind(opt_mcar, opt_mnar, opt_mar)
  
  # 5. return a table with error counts
  return(out)
}

#-----------------------------------------------------
### test all imputation 
test_all_imputation <- function(full_df, missing_df, missing_type, var_group = NULL, full_search = FALSE){
  cat("test_all_imputation - full search ", full_search)
  
  # univariate
  imp_mean <- impute_mean(missing_df)
  imp_median <- impute_mean(missing_df)
  imp_max <- impute_max(missing_df)
  imp_min <- impute_min(missing_df, type = "minimum")
  imp_min_onefifth <- impute_min(missing_df, type = "one_fifth_minimum")
  
  # # univariate methods
  mean_err <- mape(imp_mean, missing_df, full_df)
  median_err <- mape(imp_median, missing_df, full_df)
  max_err <- mape(imp_max, missing_df, full_df)
  min_err <- mape(imp_min, missing_df, full_df)
  min_onefifth_err <- mape(imp_min_onefifth, missing_df, full_df)
  
  if(full_search == "FALSE"){
    
    # multi-variate
    imp_knn <- impute_knn(missing_df, k = 10); print("HELLO7891a_F")
    imp_rf <- impute_RF(missing_df, tree_val = 500); print("HELLO7891b_F")
    imp_mice <- impute_MICE(missing_df, mice_iteration = 2); print("HELLO7891c_F")
    
    # # multi-variate error
    knn_err <- mape(imp_knn, missing_df, full_df)
    rf_err <- mape(imp_rf, missing_df, full_df)
    mice_err <- mape(imp_mice, missing_df, full_df)
    
    # merge results
    imputed_collection <-  tibble(
      missing_type = c(rep(missing_type,8)),
      method = c("mean", "median", "maximum", "minimum", "one_fifth_minimum", "KNN", "RF", "MICE"),
      opt_value = as.numeric(c(rep(0,5), 10, 500, 2)),
      mape = c(mean_err, median_err, max_err, min_err, min_onefifth_err, knn_err, rf_err, mice_err),
      imputed_df = list(imp_mean, imp_median, imp_max, imp_min, imp_min_onefifth, imp_knn, imp_rf, imp_mice))
    
  }else if (full_search == "TRUE"){
    # multi-variate
    # KNN
    imp_opt_knn <- k_val_optimization(full_df, missing_df, missing_type); print("HELLO7891a_T")
    # RF
    imp_opt_tree <- tree_val_optimization(full_df, missing_df, missing_type); print("HELLO7891b_T")
    # MICE
    imp_mice_ite <- mice_iteration_opt(full_df, missing_df, missing_type); print("HELLO7891c_T")
    # merge all results
    imputed_collection_uni <-  tibble(
      missing_type = c(rep(missing_type,5)),
      method = c("mean", "median", "maximum", "minimum", "one_fifth_minimum"),
      opt_value = as.numeric(rep(0,5)),
      mape = c(mean_err, median_err, max_err, min_err, min_onefifth_err),
      imputed_df = list(imp_mean, imp_median, imp_max, imp_min, imp_min_onefifth))
    
    imputed_collection <- rbind(imputed_collection_uni, imp_opt_knn, imp_opt_tree, imp_mice_ite)
  }
  
  # return everything
  return(imputed_collection)
}

#-----------------------------------------------------
###### perform data transformation: log transform -> shift all values to positives + 0.01
log_shift <- function(dataset){
  #changed by Anu
  #dataset <- compound_only
  log_dataset <- log(dataset)
  print("HELLO")
  print(log_dataset)
  min_val <- abs(min(log_dataset, na.rm = T))
  print(min_val)
  transformed_df <- log_dataset + min_val + 0.01
  print("HELLO2")
  return(list(transformed_df, min_val))
}


# reverse data transformation
reverse_log_shift <- function(dataset, min_val){
  log <- dataset - 0.01 - min_val
  original_df <- exp(log)
  return(original_df)
  }

# add group info back to imputed data AND save the imputed data
add_group_info <- function(imputed_df, group_info){
  if(is.null(group_info) == F){
    imputed_df <- rbind(group_info[, names(imputed_df)], imputed_df)
  }
  # save the imputed df in current directory
  write.table(imputed_df, file = "imputed_data.csv", col.names = TRUE, row.names = TRUE, sep = ",", quote = FALSE)
  return(imputed_df)
}

##### PCA plotting
plot_PCA <- function(df, title){
  # PC calculation
  pca_df <- df %>% select_if(~ !any(is.na(.))) # select features without missing
  # centering and z-transformation
  pca_res <- prcomp(pca_df, scale. = T , center = T)
  p1 <- fviz_eig(pca_res)
  p2 <- autoplot(pca_res, data = pca_df, alpha = 0.7, shape = 20, scale = F) +
    theme_bw() + ggtitle(title)
  return(p2)
}

##### t-SNE plotting
plot_tSNE <- function(df, title){
  #Anu Changed, janice changed perplexity => 10
  title <- paste(title, " \n(perplexity = 10,theta = 0.5,momentum = 0.5,normalize = T,\nfinal_momentum = 0.8,eta = 200,exaggeration_factor = 12)",sep="")
  tSNE_fit <- df %>% select_if(~ !any(is.na(.))) %>% scale() %>% Rtsne(.,perplexity = 10)
  tSNE_df <- tSNE_fit$Y %>% as.data.frame(row.names = rownames(df)) %>% rename(tSNE1="V1", tSNE2="V2")
  p <- tSNE_df %>% ggplot(aes(x = tSNE1,y = tSNE2))+ geom_point() + theme_bw() + ggtitle(title)
  return(p)
}









