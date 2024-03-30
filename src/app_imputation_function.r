###### imputation functions ######
#------------- univariate ---------------
library(foreach)

# median
impute_median <- function(compound_df){
  imputed_df <- foreach(i = c(1:dim(compound_df)[2]), .combine = 'data.frame') %do% {
            metabolite_vector <- compound_df[,i]
            na_cell <- is.na(metabolite_vector)
            metabolite_vector[na_cell] <- median(metabolite_vector, na.rm = T)
            metabolite_vector
  }
  colnames(imputed_df) <- colnames(compound_df)
  rownames(imputed_df) <- rownames(compound_df)
  return(imputed_df)
}

# min: 1/5th of minimum
impute_min <- function(compound_df, type){
  if(type == "minimum"){
  imputed_df <- foreach(i = c(1:dim(compound_df)[2]), .combine = 'data.frame') %do% {
    metabolite_vector <- compound_df[,i]
    na_cell <- is.na(metabolite_vector)
    metabolite_vector[na_cell] <- min(metabolite_vector, na.rm = T)
    metabolite_vector
    }
  }
  else if(type == "one_fifth_minimum"){
    imputed_df <- foreach(i = c(1:dim(compound_df)[2]), .combine = 'data.frame') %do% {
      metabolite_vector <- compound_df[,i]
      na_cell <- is.na(metabolite_vector)
      metabolite_vector[na_cell] <- min(metabolite_vector, na.rm = T)/5
      metabolite_vector
    }
  }
  colnames(imputed_df) <- colnames(compound_df)
  rownames(imputed_df) <- rownames(compound_df)
  return(imputed_df)
}

# mean
impute_mean <-function(compound_df){
  imputed_df <- foreach(i = c(1:dim(compound_df)[2]), .combine = 'data.frame') %do% {
    metabolite_vector <- compound_df[,i]
    na_cell <- is.na(metabolite_vector)
    metabolite_vector[na_cell] <- mean(metabolite_vector, na.rm = T)
    metabolite_vector
  }
  colnames(imputed_df) <- colnames(compound_df)
  rownames(imputed_df) <- rownames(compound_df)
  return(imputed_df)
}

# max 
impute_max <- function(compound_df){
  imputed_df <- foreach(i = c(1:dim(compound_df)[2]), .combine = 'data.frame') %do% {
    metabolite_vector <- compound_df[,i]
    na_cell <- is.na(metabolite_vector)
    metabolite_vector[na_cell] <- max(metabolite_vector, na.rm = T)
    metabolite_vector
  }
  colnames(imputed_df) <- colnames(compound_df)
  rownames(imputed_df) <- rownames(compound_df)
  return(imputed_df)
}

#------------- multivariate ---------------
# knn imputation
impute_knn <- function(compound_df, k_val){
  #  knn with user selected k-value
  if(is.numeric(k_val) == F){
    # error message
    message("An error occurred: please input the number of neighbours in KNN imputation method")
    return(-1)
  }else{
    imputed_df <- impute.knn(as.matrix(compound_df), k = k_val, rowmax = 1, colmax = 1)$data %>% 
    data.frame(check.names = F) %>% mutate_if(is.character,as.numeric)
  }
  return(imputed_df)
}

# RF imputation
impute_RF <- function(compound_df, tree_val){
  #  RF with user selected tree-number
  if(is.numeric(tree_val) == F){
    # error message
    message("An error occurred: please input a number of trees in random forest")
    return(-1)
  }else{
    df <- data.frame(compound_df, check.names = T)
    imputed_df <- missRanger(df, formula = . ~ . , num.trees = tree_val, verbose = 0)
    colnames(imputed_df) <- colnames(compound_df)
  }
  return(imputed_df)
}

# MICE imputation
impute_MICE <- function(compound_df, mice_iteration){
  #  MICE with user selected round of iterations
  if(is.numeric(mice_iteration) == F){
    # error message
    message("An error has occurred: please input a number indicating iteration")
    return(-1)
  }else{
    df <- data.frame(compound_df, check.names = T)
    mice_imp_df <-  mice(data = df, m = mice_iteration, printFlag = F)
    imputed_df <- complete(mice_imp_df, mice_iteration) # the last iteration will be returned
    colnames(imputed_df) <- colnames(compound_df)
  }
  return(imputed_df)
}


##### multivariate-optimization functions #####
# ----------------------------------------------------------
##### ERROR by Mean Absolute Errors
mape <- function(imp, mis, true) {
  imp <- as.matrix(imp)
  mis <- as.matrix(mis)
  true <- as.matrix(true)
  missIndex <- which(is.na(mis))
  errvec <- abs(imp[missIndex] - true[missIndex])
  mape_out <- mean(errvec/abs(true[missIndex]))
  return(mape_out)
}

# KNN optimization functions
# check MAPE with original df, shared by RF and KNN
check_mape <- function(ori_df, missing_df, opt_val, method, missing_type){
  
  if(method == "KNN"){
    out_df <- impute.knn(as.matrix(missing_df), k = opt_val, rowmax = 1, colmax = 1)$data
    mape_val <- mape(out_df, missing_df, ori_df)

    }else if(method == "RF"){
    df <- data.frame(missing_df, check.names = T)
    out_df <- missRanger(df, formula = . ~ . , num.trees = opt_val, verbose = 0)
    colnames(out_df) <- colnames(missing_df)
    mape_val <- mape(out_df, missing_df, ori_df)
    
  }
  return(list(mape_val,out_df))
}

# ----------------------------------------------------------
# KNN k value imputation
k_optimization <- function(ori_df, missing_df, k_value_list, missing_type, test_class){
  
  imputed_collection_knn <- tibble(
    missing_type = c(missing_type),
    method = c("KNN"),
    opt_value = as.numeric(0),
    mape = as.numeric(100),
    imputed_df = list(ori_df),
  )
  
  for(i in k_value_list){
    error = check_mape(ori_df, missing_df, i, method = "KNN", missing_type = missing_type)
    if(error[[1]] < imputed_collection_knn$mape){
      imputed_collection_knn$opt_value <- i
      imputed_collection_knn$mape <- error[[1]]
      imputed_collection_knn$imputed_df <- list(data.frame(error[[2]], check.names = F))
    }
  }
  # reassigned the variable to global variable
  if(test_class == 1){
    return(imputed_collection_knn$opt_value) # this is the optimized k-value
  }else if(test_class == 2 ){
    return(imputed_collection_knn) # this returns the "imputed_collection_knn" result
  }
}

k_val_optimization <- function(ori_df, missing_df, missing_type){
  # 1. rough search
  k_values <- c(1, seq(10,100, by = 30)) #MCC increased step from 5 to 200 by into from 10 to 100 by 30
  k1 <- k_optimization(ori_df, missing_df, k_values, missing_type, test_class = 1)
  
  # 2. fine search
  k_fine <- seq(k1-4, k1+4, by = 1) %>% subset(. > 0)
  k2 <- k_optimization(ori_df,missing_df, k_fine, missing_type, test_class = 2)
  return(k2)
}

# ----------------------------------------------------------
# RF tree value optimization
tree_optimization <- function(ori_df, missing_df, tree_value_list, missing_type){
  
  imputed_collection_rf <- tibble(
    missing_type = c(missing_type),
    method = c("RF"),
    opt_value = as.numeric(0),
    mape = as.numeric(100),
    imputed_df = list(ori_df),
  )
  
  for(i in tree_value_list){
    mape = check_mape(ori_df, missing_df, i, method = "RF", missing_type = missing_type)
    if(mape[[1]] < imputed_collection_rf$mape){
      imputed_collection_rf$opt_value <- i
      imputed_collection_rf$mape <- mape[[1]]
      imputed_collection_rf$imputed_df <- list(data.frame(mape[[2]], check.names = F))
    }
  }
  # return optimized tibble
  return(imputed_collection_rf)
}


tree_val_optimization <- function(ori_df, missing_df, missing_type){
  # rough search
  tr_values <- c(5, 10, 20, seq(50,150, by = 100)) #MCC changed from 50 to 300 by 50 into from 50 to 150 by 100
  tr1 <- tree_optimization(ori_df, missing_df, tr_values, missing_type)
  return(tr1)
}

# ----------------------------------------------------------
# MICE optimization
opt_iteration <- function(x, imputed_df, missing_df, ori_df){
  imputed_df_complete <- complete(imputed_df,x)
  error <- mape(imputed_df_complete, missing_df, ori_df)
  return(error)
}

mice_iteration_opt <- function(ori_df, missing_df, missing_type){
  # number of iterations to test
  m_max <- 2 #MCC changed from 20 to 2
  
  # imputation
  df <- data.frame(missing_df, check.names = T)
  imputed_df_out <- mice(data = df, m = m_max, printFlag = F)
  
  # output plots
  error_out <- sapply(c(1:m_max), function(x) opt_iteration(x, imputed_df_out, missing_df, ori_df))
  opt_out <- data.frame("iteration" = c(1:m_max), "mape_values"= error_out)
  
  # output the line with the optimzed iteration
  optimized_MICE_iteration <- opt_out[which.min(opt_out$mape_values),]
  
  # add the optimized df
  imputed_collection_mice <- tibble(
    missing_type = c(missing_type),
    method = c("MICE"),
    opt_value = as.numeric(optimized_MICE_iteration$iteration),
    mape = as.numeric(optimized_MICE_iteration$mape_values),
    imputed_df = list(data.frame(complete(imputed_df_out,optimized_MICE_iteration$iteration), check.names = F))
    )
  return(imputed_collection_mice)
}
