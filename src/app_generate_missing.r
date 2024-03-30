##### functions to generate missing datasets #####

# Missing completely at random
generate_MCAR <- function(full_data, missing_percent){
  # check the size of the input dataset 992 x 57
  row_num <- dim(full_data)[1]
  col_num <- dim(full_data)[2]
  # generate random numbers with size 992 x 57
  random_num <- runif(n = row_num*col_num)
  # create a filter matrix
  NA_matrix <- matrix(data = random_num, nrow = row_num, ncol = col_num, byrow = F)
  NA_matrix_logical <- (NA_matrix < missing_percent)
  # replace the TRUE boxes with NA in the orginal data
  full_data[NA_matrix_logical] <- NA
  # merge with individual info
  full_data_MCAR <- full_data
  
  # return the MCAR missing dataframe
  return(full_data_MCAR)
}


#------------------------- MAR or MNAR ------------------------------------

# generate a covariate matrix
cov_matrix <- function(full_data, var_group = NULL){
  # if there is no grouping information
  if(is.null(var_group)){
    # 1. calculate TIC sum for each sample
    sample_sum_signal <- apply(full_data, 1, sum)
    # 2. check the size of the input dataset 992 x 57
    row_num <- dim(full_data)[1]
    col_num <- dim(full_data)[2]
    # 3. create a filter matrix
    TIC_sum_matrix <- matrix(data = sample_sum_signal, nrow = row_num, ncol = col_num, byrow = F)
    # 4. matrix where each cell reppresent the TIC - that particular cell
    cov_table <- TIC_sum_matrix - full_data
  }
  else{
    print("HELL902")
    # create a covariate table using the  group info
    grp_tr <- t(var_group)
    full_data_tr <- t(full_data)
    print("HELLO900")
    print(dim(grp_tr))
    print("HELLO909")
    print(dim(full_data_tr))
    merged <- merge(grp_tr, full_data_tr, by = 0) %>% data.frame(check.names = F, row.names = 1) %>% mutate(across(c(group),as.factor))
    
    # create a sum table for each sample
    group_sum <- merged %>% group_by(group) %>% summarise(across(everything(), ~sum(., na.rm = T))) %>% ungroup()
    freq <- data.frame(table(merged$group))
    df_expanded <- group_sum[rep(row.names(group_sum), freq$Freq),] %>% select(-group)
    sum_df <- df_expanded %>% t %>% data.frame(check.names = F)
    colnames(sum_df) <- colnames(full_data)
    # covariate table is created
    cov_table <-  sum_df - full_data
    
  }
  return(cov_table)
}

# generate number of missing samples
rand_vect <- function(N, M, sd = 1, pos.only = TRUE) {
  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  if (pos.only) while (any(vec < 0)) {
    negs <- vec < 0
    pos  <- vec > 0
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  return(vec)
}

# AIM: To generate missing not at random (MAR) lipid dataset
missing_index <- function(full_data, lipid_name, missing_value, missing_percentage, covariate_matrix, method){
  
  # get number of samples
  sample_size <- dim(full_data)[1]
  # get the lipid
  lipid_n <-  full_data %>% select(all_of(lipid_name))
  
  #-----------------------------------------------------
  # generate a logistic distribution for 992 samples
  a <- rlogis(sample_size)
  # get a random number
  ramdom_num <- runif(sample_size)
  # multiple a and random number
  b <- a*ramdom_num # hist(b)
  # choose the ones on the edge
  alpha <- missing_value
  # get the lower alpha/2 % data
  lower_end <- rank(b) < alpha *0.5 
  # get the upper alpha/2 % data
  upper_end <- rank(b) > (sample_size - alpha *0.5)
  #-----------------------------------------------------
  
  if(method == "MAR"){
    lipid_cov <- covariate_matrix %>% select(all_of(lipid_name)) 
    
    # filtering the missing data index
    filter_matrix <- data.frame("rank_a" = rank(a, ties.method = c("random")), a, ramdom_num, b, "missingness" = (lower_end+upper_end))
    lipid_table <- data.frame("idx" = c(1:sample_size), lipid_name = lipid_n, "lipid_cov" = c(lipid_cov[,1]),  
                              "rank_lipid_cov" = rank(lipid_cov, ties.method = c("random")), check.names = F)
    colnames(lipid_table) <- c("idx", lipid_name, "lipid_cov", "rank_lipid_cov")
    out <- merge(lipid_table, filter_matrix, by.x = "rank_lipid_cov", by.y = "rank_a") %>% arrange(idx)
    NA_list <- out %>% pull(missingness)
    
  }else if(method == "MNAR"){
    # filtering the missing data index
    filter_matrix <- data.frame("rank_a" = rank(a,ties.method = c("random")), a, ramdom_num, b, "missingness" = (lower_end+upper_end))
    lipid_table <- data.frame("idx" = c(1:sample_size),  lipid_name = lipid_n, "rank_lipid" = rank(lipid_n, ties.method = c("random")), check.names = F)
    colnames(lipid_table) <- c("idx", lipid_name, "rank_lipid")
    out <- merge(lipid_table, filter_matrix, by.x = "rank_lipid", by.y = "rank_a") %>% arrange(idx)
    NA_list <- out %>% pull(missingness)
  }
  
  # return missing index
  return(NA_list)
}


# generate MAR matrix
generate_MAR <- function(full_data, misspercent, var_group = NULL){
  # list of lipid names
  full_df_var_list <- colnames(full_data)
  
  # sample size and number of variables
  sample_size <- dim(full_data)[1]
  num_variable <- dim(full_data)[2]
  
  # generate number of missing missing
  missing_val <- rand_vect(num_variable, num_variable*as.double(misspercent)*sample_size, sd =0.3*as.double(misspercent)*sample_size)
  print(head(missing_val))
  print(head(var_group))
  covariate_matrix <- cov_matrix(full_data, var_group)
  
  # generate a missing matrix
  tmp <- c(); matrix <- matrix(data = NA, nrow = sample_size, ncol = num_variable)
  
  tryCatch(
    expr = {
      for (i in 1:num_variable){
        tmp <-  missing_index(full_data, full_df_var_list[i], missing_val[i], misspercent, covariate_matrix, "MAR")
        matrix[,i] <- tmp
      }
    },
    error = function(e){ 
      message('MAR: An error has occured, now try again. MAR')
      for (i in 1:num_variable){
        tmp <-  missing_index(full_data, full_df_var_list[i], missing_val[i], misspercent, covariate_matrix, "MAR")
        matrix[,i] <- tmp
      }
    },
    finally = {
      message('MAR: All done, quitting.')
    }
  )
  
  # replace 1 for NA
  ind <- matrix==1
  full_data[ind] <- NA
  
  # return a MAR dataset
  return(full_data)
}


generate_MNAR<- function(full_data, misspercent){
  
  # missing_index("440.4b", 0.1)
  full_df_var_list <- colnames(full_data)
  # sample size and number of variables
  sample_size <- dim(full_data)[1]
  num_variable <- dim(full_data)[2]
  
  # filter matrix
  missing_val <- rand_vect(num_variable, num_variable*as.double(misspercent)*sample_size, sd = 0.3*as.double(misspercent)*sample_size)
  
  # generate a missing matrix
  tmp <- c(); matrix <- matrix(data = NA, nrow = sample_size, ncol = num_variable)
  
  tryCatch(
    expr = {
      for (i in 1:num_variable){
        tmp <-  missing_index(full_data, full_df_var_list[i], missing_val[i], misspercent, covariate_matrix, "MNAR")
        matrix[,i] <- tmp
      }
    },
    error = function(e){ 
      message('MNAR: An error has occured, try again.')
      for (i in 1:num_variable){
        tmp <-  missing_index(full_data, full_df_var_list[i], missing_val[i], misspercent, covariate_matrix, "MNAR")
        matrix[,i] <- tmp
      }
    },
    finally = {
      message('MNAR: All done, quitting.')
    }
  )
  
  ind <- matrix==1
  full_data[ind] <- NA
  
  # return missing dataset
  return(full_data)
}










