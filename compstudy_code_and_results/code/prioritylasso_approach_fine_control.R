# Set the working directory to the directory 'bwm_article' 
# of the electronic appendix:

library("this.path")
setwd(gsub("/compstudy_code_and_results/code", "", this.dir()))

# This script expects the name of the data set, the train pattern, the test
# pattern and the repetition as a command line argument
args <- commandArgs(trailingOnly = TRUE)
# if train pattern, test pattern and repetition are not specified, use 1 as
# default
if (is.na(args[2])) {
  args[2] <- 1
}
if (is.na(args[3])) {
  args[3] <- 1
}
if (is.na(args[4])) {
  args[4] <- 1
}

# [0] SetWD, load packages, define variables and functions                  ----
# 0-1 Set WD
# currently use default (as it is an Rproj)

# 0-2 Load packages
library(checkmate)
library(prioritylasso)
library(caret)
library(pROC)
library(combinat)

# Use rejection sampling for random number sampling:
RNGkind(sample.kind = "Rejection") 

# 0-3 Define variables

# 0-4 Define functions
# 0-4-1 Load functions from './compstudy_code_and_results/code/functions/create_bwm_pattern.R"
source("./compstudy_code_and_results/code/functions/create_bwm_pattern.R")

source("./compstudy_code_and_results/code/functions/prioritylasso_approach_functions.R")

# [1] Run the experiments                                                    ----
# 1-1 Initalize a empty DF to store the results
pl_res <- data.frame()

# 1-2 Define a list with the paths to the availabe DFs
df_paths <- paste0("./compstudy_code_and_results/data/", list.files("./compstudy_code_and_results/data/"))

# 1-2b determine the position of the data set within the list and use this to
# calculate the count start
# every data set has 100 runs, so multiply by 100
df_index <- which(grepl(args[1], df_paths))
# correct the count for the data set
count <- 1 + (df_index - 1) * 100
# correct for train pattern
count <- count + (as.numeric(args[2]) - 1) * 20
# correct for test pattern
count <- count + (as.numeric(args[3]) - 1) * 5
# correct for repetition
count <- count + as.numeric(args[4]) - 1

# 1-3 Create a list of seeds for each single evaluation-setting
set.seed(1234)
allseeds <- base::sample(1000:10000000, 
                         size = length(df_paths) * length(c(1, 2, 3, 4, 5)) * 
                           length(c(1, 2, 3, 4)) * length(c(1, 2, 3, 4, 5)))

# 1-4 Evaluate a RF on all the possible combinations of block-wise missingness
#     patterns in train- & test-set for all DFs in 'df_paths'. Each is evaluated
#     5-times.
# the curr_path is defined by the command line argument
curr_path <- df_paths[df_index]
first_loop_test <- TRUE
first_loop_train <- TRUE
for (curr_train_pattern in seq(from = args[2], to = 5, by = 1)) {
  if (first_loop_train) {
    start_value_test <- as.numeric(args[3])
  } else {
    start_value_test <- 1
  }
  for (curr_test_pattern in seq(from = start_value_test, to = 4, by = 1)) {
    if (first_loop_test) {
      start_value_rep <- as.numeric(args[4])
    } else {
      start_value_rep <- 1
    }
    for (curr_repetition in seq(from = start_value_rep, to = 5, by = 1)) {
      
      # Print Info to current evaluation!
      cat('-----------------------------------------------\n',
          "Current Path:          >", curr_path, '\n',
          "Current Train Pattern: >", curr_train_pattern, '\n',
          "Current Test Patter:   >", curr_test_pattern, '\n',
          "Current Repetition:    >", curr_repetition, '\n',
          "Current count:         >", count, '\n')
      
      
      # Get initial seed for the current combination evaluation-settings
      int_seed <- allseeds[count]
      count    <- count + 1
      
      # Set seed & draw points from uniform distribution
      set.seed(int_seed)    
      seeds <- round(runif(4, 0, 100000))
      
      # Use these 'seeds' to set the four necessary seeds for the evaluation:
      #     1. Seed to split data into test & train
      curr_split_seed <- seeds[1]
      
      #     2. Seed to shuffle the block order in 'train'
      curr_block_seed_train <- seeds[2]
      
      #     3. Seed to shuffle the block order in 'test'
      curr_block_seed_test <- seeds[3]
      
      #     4. Seed for the train-pattern (assignment of obs. in train to folds)
      curr_train_pattern_seed <- seeds[4]
      
      # Run the evaluation with current settings
      curr_res <- tryCatch(eval_pl_approach(path               = curr_path, 
                                            frac_train         = 0.75, 
                                            split_seed         = curr_split_seed,
                                            block_seed_train   = curr_block_seed_train, 
                                            block_seed_test    = curr_block_seed_test,
                                            train_pattern      = curr_train_pattern,
                                            train_pattern_seed = curr_train_pattern_seed, 
                                            test_pattern       = curr_test_pattern),
                           error = function(c) {
                             data.frame("path"               = curr_path, 
                                        "frac_train"         = 0.75, 
                                        "split_seed"         = curr_split_seed, 
                                        "block_seed_train"   = curr_block_seed_train,
                                        "block_seed_test"    = curr_block_seed_test, 
                                        "block_order_train_for_BWM" = '---',
                                        "block_order_test_for_BWM"  = '---',
                                        "train_pattern"      = curr_train_pattern, 
                                        "train_pattern_seed" = curr_train_pattern_seed, 
                                        "test_pattern"       = curr_test_pattern,
                                        "common_blocks"      = "---",
                                        "AUC"                = '---',
                                        "Accuracy"           = '---', 
                                        "Sensitivity"        = '---', 
                                        "Specificity"        = '---', 
                                        "Precision"          = '---', 
                                        "Recall"             = '---', 
                                        "F1"                 = '---', 
                                        "BrierScore"         = '---')
                           }
      ) 
      # Add the, 'int_seed', 'curr_repetition' & 'SingleBlock' to 'curr_res'
      curr_res$int_seed   <- int_seed
      curr_res$repetition <- curr_repetition
      curr_res$approach   <- 'PL'
      
      # Add the results of the setting to 'BW_res' & save it
      pl_res <- rbind(pl_res, curr_res)
      write.csv(pl_res, paste0('./compstudy_code_and_results/results/pl_approach/PL_Eval_', args[1], '_', args[2], '_', args[3], '_', args[4], '.csv'))
      rm(curr_res)
      gc()
    }
    first_loop_test <- FALSE
  }
  first_loop_train <- FALSE
}
