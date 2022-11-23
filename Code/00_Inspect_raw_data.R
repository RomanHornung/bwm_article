"Get a rought first overview to the files in 'Data/Raw'.
 For each of the DFs in there, check the following:
  > do they contain the necessary blocks 'clin', 'mirna', 'mutation', 'cnv' & 'rna'
  > is 'TP53' part of the 'mutation' block (will be used as artifical response)
  > amount of features for each of the blocks
  > amount of observations for each DF
??  > amount of variables with missing values  ??
"
# [0] SetWD, load packages, define variables and fuctions                    ----
# 0-1 Set WD
setwd("/Users/frederik/Desktop/BWM-Article/")             # Mac
setwd("C:/Users/kuche/Desktop/BWM-Paper")                 # Windows
setwd("/dss/dsshome1/lxc0B/ru68kiq3/Project/BWM-Article") # LRZ-Server

# 0-2 Load packages

# 0-3 Define variables
# --1 Names of the blocks each DF should contain
necessary_blocks <- c('clin', 'mirna', 'mutation', 'cnv', 'rna')

# 0-4 Define functions
# --1 Function to get the columns that contain missing values
get_cols_w_NAs <- function(df) {
  " Check whether the passed dataframe (df) contains any columns w/ missing values
  
  Args:
    - df (data.frame) : DF with n rows and p columns, that shall be checked for 
                        NAs in any of its p columns!
    
  Return:
    - Boolean for each column, whether it cotains any NAs
  "
  results <- apply(df, 2, function(x) any(is.na(x)))
  
  if (length(which(results)) <= 0) {
    return(0)
  } else {
    return(length(results))
  }
}

# [1] Inspect each single data-frame in 'Data/Raw' & check them              ----
#     (do they contain all necessary blocks, the response variable, ...)
# 1-1 Get all file-names in 'Data/Raw'
all_files <- list.files('./Data/Raw/')

# 1-2 Loop over each file in 'all_files' and get info to them
for (curr_file in all_files) {
  
  # --1 Load the current file & print its name
  curr_data <- load(paste0("./Data/Raw/", curr_file))
  cat(paste0("\n --- Current DF: ", curr_file, " ------------------------\n\n"))
  
  # --2 Check that it contains all necessary blocks
  # --2-1 If there is 'targetvar' in it, remove it from the blocks, as we use 
  #       'TP53' (from the mutation block) as response 
  if ('targetvar' %in% curr_data) {
    curr_data <- curr_data[-which(curr_data == 'targetvar')]
  }
  
  # --2-2 Check that the DF contains all necessary blocks
  block_check <- sapply(necessary_blocks, FUN = function(x) !x %in% curr_data)
  if (any(block_check)) {
    stop(paste0('The DF does not contain all 5 necessary blocks'))
  } else {
    print(paste0('The DF contains all necessary Blocks:'))
    print(curr_data)
  }
  
  # --3 Check whether the 'mutation' block contains the 'TP53' variable (used as response)
  if (!"TP53" %in% colnames(mutation)) {
    stop(paste0("The 'mutation' block in the DF misses the response 'TP53'"))
  } else {
    print("'TP53' is part of the 'mutation' block")
  }
  
  # --4 Get the total amount of features & observations (over all blocks)
  # --4-1 Count features & observations per block
  c_nrow <- c(); c_ncol <- c()
  for (curr_block in necessary_blocks) {
    c_nrow <- c(c_nrow, nrow(eval(as.symbol(curr_block))))
    c_ncol <- c(c_ncol, ncol(eval(as.symbol(curr_block))))
  }
  
  # --4-2 Check that each block has same amount of observations
  if (length(unique(c_nrow)) > 1) {
    stop(paste0('Not all blocks have the same amount of observations'))
  }
  
  # --4-3 Print the amount of features & observations for 'curr_file'
  cat(paste0('The DF contains in total: \n > ', 
             unique(c_nrow), ' observations & \n > ', sum(c_ncol), ' features\n'))
 
  # --5 Get the amount of missing values
  # --5-1 Count the cols w/ at least on NA in each block
  cols_w_na <- c()
  for (curr_block in necessary_blocks) {
    cols_w_na <- c(cols_w_na, get_cols_w_NAs(eval(as.symbol(curr_block))))
  }
  
  # --5-2 Print results
  if (all(cols_w_na == 0)) {
    cat(paste0('For the DF all blocks are fully observed!\n'))
  } else {
    cat(paste0('The DF contains blocks w/ missing values!\n'))
  }
}


# [2] Get the average amount of observations/ features over all DFs          ----
# 2-1 Get all file-names in 'Data/Raw'
all_files <- list.files('./Data/Raw/')

# 2-2 Define a DF to store the results
all_res <- data.frame('Data-Set' = character(),
                      'nrow' = integer(),
                      'ncol_clin' = integer(),
                      'ncol_mirna' = integer(),
                      'ncol_mutation' = integer(),
                      'ncol_cnv' = integer(),
                      'ncol_rna' = integer(),
                      'fraction_pos_response' = numeric())

# 2-3 Loop over each file in 'all_files' and get infos to them
for (curr_file in all_files) {
  
  # --1 Load the current data-set
  curr_data <- load(paste0("./Data/Raw/", curr_file))
  
  # --2 Loop over all its blocks & count number of observations / features
  c_nrow <- c(); c_ncol <- c()
  for (curr_block in necessary_blocks) {
    c_nrow <- c(c_nrow, nrow(eval(as.symbol(curr_block))))
    c_ncol <- c(c_ncol, ncol(eval(as.symbol(curr_block))))
  }
  
  # --3 As all block contain the same amount of features (checked in [1])
  #     only keep the unique value & assign the file name
  c_nrow  <- unique(c_nrow)
  
  # --4 Get the fraction of positive classes in 'target_var' (TP53)
  dist_target       <- prop.table(table(mutation[,"TP53"]))
  fraction_positive <- round(dist_target[names(dist_target) == 1], 3)
  
  # --5 Add results to 'all_res'
  all_res[nrow(all_res) + 1,] <- c(curr_file, c_nrow, c_ncol, fraction_positive)
}

# 2-4 Add the average amount (e.g. cols in block 'clin') over all 13 DFs
all_res[nrow(all_res) + 1,] <- c("Average", 
                                 round(mean(as.numeric(all_res$nrow)), 2),
                                 round(mean(as.numeric(all_res$ncol_clin)), 2), 
                                 round(mean(as.numeric(all_res$ncol_mirna)), 2),
                                 round(mean(as.numeric(all_res$ncol_mutation)), 2),
                                 round(mean(as.numeric(all_res$ncol_cnv)), 2),
                                 round(mean(as.numeric(all_res$ncol_rna)), 2),
                                 round(mean(as.numeric(all_res$fraction_pos_response)), 2))

# 2-5 Inspect the DF
all_res

# 2-5 Save 'all_res' to Docs
write.csv2(all_res, './Docs/raw_data_overview/Overview_to_raw_data.csv')
