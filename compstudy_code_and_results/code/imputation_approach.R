# Set the working directory to the directory 'bwm_article' 
# of the electronic appendix:

library("this.path")
setwd(gsub("/compstudy_code_and_results/code", "", this.dir()))

"Script to evaluate the Imputation approach on data with blockwise missingness
 Split a DF to a train- & test-set, separately induce block-wise missingness
 patterns to these and evaluate the IMP-Approach on these then.
 This is done 5x for each DF in './compstudy_code_and_results/data/' for each possible combination 
 of blockwise missingess patterns in train- & test-set.

 IMP-Approach:
  > Impute the missing values in the train-set with TOBMI
    (see 'https://academic.oup.com/bioinformatics/article/35/8/1278/5092930'
     - the functions for TOBMI were supplied by Roman Hornung)
  > Remove all blocks from the train-set that are not available in the test-set
  > Fit a RF on the remaining train-set & create predicitons for the test-set
  > Evaluate the results with common metrics (AUC, Accuracy, F-1 Score, ...)
"
# [0] Set WD, load packages, define variables and functions                  ----

# 0-2 Load packages
library(checkmate)
library(randomForestSRC)
library(caret)
library(pROC)

# Use rejection sampling for random number sampling:
RNGkind(sample.kind = "Rejection") 

# 0-3 Define variables

# 0-4 Define functions
# 0-4-1 Load functions from './compstudy_code_and_results/code/functions/create_bwm_pattern.R"
source("./compstudy_code_and_results/code/functions/create_bwm_pattern.R")

source("./compstudy_code_and_results/code/functions/imputation_approach_functions.R")

# [1] Run the experiments                                                    ----
# 1-1 Initialize a empty DF to store the results
IMP_res <- data.frame()

# 1-2 Define a list with the paths to the available DFs
df_paths <- paste0("./compstudy_code_and_results/data/", list.files("./compstudy_code_and_results/data/"))

# 1-3 Create a list of seeds for each single evaluation-setting
set.seed(1234)
count    <- 1
allseeds <- base::sample(1000:10000000, 
                         size = length(df_paths) * length(c(1, 2, 3, 4, 5)) * 
                                length(c(1, 2, 3, 4)) * length(c(1, 2, 3, 4, 5)))

# 1-4 Evaluate a RF on all the possible combinations of block-wise missingness
#     patterns in train- & test-set for all DFs in 'df_paths'. Each is evaluated
#     5-times.
for (curr_path in df_paths) {
  for (curr_train_pattern in c(1, 2, 3, 4, 5)) {
    for (curr_test_pattern in c(1, 2, 3, 4)) {
      for (curr_repetition in c(1, 2, 3, 4, 5)) {
        
        # Print Info to current evaluation!
        cat('-----------------------------------------------\n',
            "Current Path:          >", curr_path, '\n',
            "Current Train Pattern: >", curr_train_pattern, '\n',
            "Current Test Patter:   >", curr_test_pattern, '\n',
            "Current Repetition:    >", curr_repetition, '\n')
        
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
        curr_res <- tryCatch(eval_imp_approach(path               = curr_path, 
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
                                          "common_blocks"      = '---',
                                          "ntree"              = '---', 
                                          "mtry"               = '---', 
                                          "min_node_size"      = '---', 
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
        curr_res$approach   <- 'Imputation'

        # Add the results of the setting to 'IMP_res' & save it
        IMP_res <- rbind(IMP_res, curr_res)
        write.csv(IMP_res, './compstudy_code_and_results/results/imp_approach/IMP_Eval.csv')
      }
    }
  }
}