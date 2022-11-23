"Script to evaluate the Single-Block approach on data with blockwise missingness
 Split a DF to a train- & test-set, separately induce block-wise missingness
 patterns to these and evaluate the SB-Approach on these then.
 This is done 5x for each DF in './Data/Raw/' for each possible combination 
 of blockwise missingess patterns in train- & test-set.

 SB-Aproach:
  > For each common observed block of train- & test-set fit a RF 
  > Evaluate each block-wise fitted RF with its OOB-AUC
  > Use the RF with the best OOB-AUC to create predicitons for the test-set then
  > Evaluate the results with common metrics (AUC, Accuracy, F-1 Score, ...)
"
# [0] SetWD, load packages, define fix variables and functions                ----
# 0-1 Set WD
setwd("/Users/frederik/Desktop/BWM-Article/")             # Mac
setwd("C:/Users/kuche/Desktop/BWM-Paper")                 # Windows
setwd("/dss/dsshome1/lxc0B/ru68kiq3/Project/BWM-Article") # Server

# 0-2 Load packages
library(checkmate)
library(randomForestSRC)
library(caret)
library(pROC)

# 0-3 Define variables

# 0-4 Define functions
# 0-4-1 Load functions from 'code/01_Create_BWM_Pattern"
source("./Code/01_Create_BWM_Pattern.R")

# 0-4-2 Function to fit a RF & return its OOB-AUC
fit_RF_get_oob_AUC <- function(data) {
  "Fit an RF (with its standard settings) to 'data' ('ytarget' must be in there &
   is used as response) - only return the oob-AUC of the fit RF.
   
   Args:
    > data (data.frame): Data with at least two columns & observations. 
                         Must contain the column 'ytarget' & no missing values.
                         
   Return:
    > Return the AUC-metric that was calculated based on the oob-observations of 
      the RF - in case this fails return 0
  "
  # [0] Check Inputs
  # 0-1 'data' has to be a DF, with at least 2 observations & w/ columns
  assert_data_frame(data, any.missing = F, min.rows = 2, min.cols = 2)
  
  # 0-2 'data' must contain 'ytarget' as column
  if (!'ytarget' %in% colnames(data)) stop("'data' must contain 'ytarget' as column")
  
  # [1] Fit RF on the data
  # 1-1 Train a RF
  # --1 Create a formula to pass to the RF 
  #     (define response & use remaining variables as features)
  formula_all <- as.formula(paste('ytarget', " ~ ."))
  
  # --2 Fit the actual RF (only use standard-settings)
  RF <- rfsrc(formula = formula_all, data = data, samptype = "swr", 
              seed = 12345678, var.used = 'all.trees')
  
  # [2] Get the AUC based on the oob-observations & return it then
  # 2-1 Get the predicted probabilities for the oob-observations
  pred_prob_oob <- RF$predicted.oob[,'1']
  
  # 2-2 Compare the predicted class-prob. with the true classes & calc the AUC
  #  -> in case RF predict all OOB as 0/ 1 error will arise -> set AUC to 0
  AUC <- tryCatch(expr = pROC::auc(data$ytarget, pred_prob_oob, quiet = T),
                  error = function(c) 0)
  
  # 2-3 Return the AUC
  return(AUC)
}

# 0-4-3 Function to fit an RF on train & predict on test then 
get_predicition <- function(train, test) {
  " Fit a RF on 'train' and create predicitons for 'test' then 

    Args:
      - train  (DF): DF that only contains variables also availabe for 'test' -
                     both must contain 'ytarget'
      - test   (DF): DF that is completly observed in all observations
                     
    Return:
      - List with: > 'pred_classes' = predicted class for each observation in 'test'
                   > 'pred_prob_pos_class' = predicted probability for each obs. 
                                             to be in class 1
                   > settings of the RF for 'mtry', 'min_node_size' & 'ntree'
  "
  # [0] Check Inputs
  # 0-1 'train' & 'test' must be dataframes w/o missing values
  assert_data_frame(train, any.missing = FALSE)
  assert_data_frame(test, min.rows = 1, any.missing = FALSE)
  
  # 0-2 'train' must not contain any col-names not available in 'test' & vic versa
  if (!all((colnames(train) %in% colnames(test)))) {
    stop("Train-Set has different features than the Test-Set!")
  }
  
  if (!all((colnames(test) %in% colnames(train)))) {
    stop("Test-Set has different features than the Train-Set!")
  }
  
  # [1] Train a RF & create predictions for the test-set
  # 1-1 Train a RF on 'train'
  # --1 Create a formula to pass to the RF 
  #     (define response & use remaining variables as features)
  formula_all <- as.formula(paste('ytarget', " ~ ."))
  
  # --2 Fit the RF (use standard-settings)
  RF <- rfsrc(formula = formula_all, data = train, samptype = "swr", 
              seed = 12345678, var.used = 'all.trees')
  
  # 1-2 Get predictions for the test-set
  predicitons <- predict(RF, test)
  
  # 1-3 Return the predicted classes & the predicted probabilities for class '1'
  #     as well as the settings of the RF
  return(list('pred_classes'        = predicitons$class,
              'pred_prob_pos_class' = predicitons$predicted[,'1'],
              'RF_ntree'            = RF$ntree,
              'RF_mtry'             = RF$mtry,
              'RF_min_node_size'    = RF$nodesize))
}

# 0-4-4 Evaluate a RF with the complete-case approach & get its metrics
eval_sb_appr <- function(path = './Data/Raw/BLCA.Rda', frac_train = 0.75, split_seed = 1312,
                         block_seed_train = 1234, block_seed_test = 1312, train_pattern = 2, 
                         train_pattern_seed = 12, test_pattern = 2) {
  "Evaluate the SB-Approach on the data 'path' points to.
   On each block that the test- & train-set have in commom, a seperate RF is trained & evaluated 
   with the out-of-bag AUC- the RFs are trained with their standard settings
   (e.g. 'ntree', 'mtry' & 'min_node_size'). The block that leads to the best out-of-bag AUC
   is then used to predict on the test-set. Evaluate the predicitons with common metrics,
   and return all results in a DF w/ all the settings for the evaluation 
   (e.g. path, seeds, train_pattern, settings for RF, best oob-block, ...)
   
   Args:
      > path               (str): Path to a dataset - must contain 'Data/Raw'
      > frac_train       (float): Fraction of observations for the train-set - ]0;1[
      > split_seed         (int): Seed for the split of the data to train & test
      > block_seed_train   (int): Seed for the shuffeling of the block-order in train
      > block_seed_test    (int): Seed for the shuffeling of the block-order in test
      > train_pattern      (int): Seed for the induction of the pattern for train
                                  (obs. are assigned to different folds!)
      > train_pattern_seed (int): Pattern to induce into train (1, 2, 3, 4, 5)
      > test_pattern       (int): Pattern to induce into test (1, 2, 3, 4)
   
   Return:
      > A DF with the settings of the experiment (path to the data, train pattern, ...), 
        hte common blocks between train- & test-set, as well as the settings of the RF 
        (ntree, mtry, ...) and the results of the evaluation (AUC; Brier-Score; Accuracy)
  "
  # [0] Check Inputs
  #     --> All arguments are checked in the functions 'get_train_test()' &
  #         'get_predicition()' that were loaded from 'Code/01_Create_BWM_Pattern.R'
  
  # [1] Load the data & prepare them for the SB-Approach
  # 1-1 Load the data from 'path', split it to test- & train & induce block-wise 
  #     missingness into both of them according to 'train_pattern' & 'test_pattern'
  train_test_bwm <- get_train_test(path = path,                             # Path to the data
                                   frac_train = frac_train,                 # Fraction of data used for Training (rest for test)
                                   split_seed = split_seed,                 # Seed for the split of the data into test- & train
                                   block_seed_train = block_seed_train,     # Seed to shuffle the block-order in train
                                   block_seed_test = block_seed_test,       # Seed to shuffle the block-order in test
                                   train_pattern = train_pattern,           # Pattern to introduce to train
                                   train_pattern_seed = train_pattern_seed, # Seed for the introduction of the BWM into train
                                   test_pattern = test_pattern)             # Pattern for the test-set
  
  # 1-2 Extract the observed blocks from test & store the corresponding DFs separately in the list 
  #     'test_blocks' (each fully observed block is an own entry in the list w/ its block-name)
  test_blocks <- list()
  for (curr_block in train_test_bwm$Test$block_names) {
    
    # --1 Which Index has the current block
    curr_block_idx <- which(train_test_bwm$Test$block_names == curr_block)
    
    # --2 Get the corresponding columns to 'curr_block'
    curr_block_cols <- which(train_test_bwm$Test$block_index == curr_block_idx)
    
    # --3 Check whether the whole block is observed (no NA's)
    curr_block_observed <- all(!is.na(train_test_bwm$Test$data[,curr_block_cols] <= 0))
    
    # --4 If the current block is fully observed, add it to 'test_blocks' & add the response as 
    #     additional column to the DF then
    if (curr_block_observed) {
      test_blocks[[curr_block]] <- train_test_bwm$Test$data[,curr_block_cols]
      test_blocks[[curr_block]]$ytarget <- train_test_bwm$Test$data$ytarget
    }
  }
  
  # 1-3 For each block in 'test_blocks', check whether train has observations in the corresponding 
  #     block. If this is the case, add the observed part of the block to 'train_blocks'
  train_blocks <- list()
  for (curr_test_block in names(test_blocks)) {
    
    # --1 Get the index of the current test-block from the train-set
    curr_train_block_idx <- which(train_test_bwm$Train$block_names == curr_test_block)
    
    # --2 Extract the corresponding columns from train for current train-block 
    curr_train_block_cols <- which(train_test_bwm$Train$block_index == curr_train_block_idx)
    
    # --3 Check whether the whole block has (at least some) observed values (no NA's), then we can use it!
    curr_train_block_partly_observed <- any(!is.na(train_test_bwm$Train$data[,curr_train_block_cols] <= 0))
    
    # --4 If the current_test_block is (partly) observed in the train, add it to 'train_blocks' &
    #     additionally add the corresponding response to the DF then
    if (curr_train_block_partly_observed) {
      
      #--4-1 Get the rows that are fully observed
      observed_rows <- which(rowSums(is.na(train_test_bwm$Train$data[,curr_train_block_cols])) == 0)
      
      # --4-2 Add the block with only observed values to 'train_blocks'
      train_blocks[[curr_test_block]] <- train_test_bwm$Train$data[observed_rows, curr_train_block_cols]
      
      # --4-3 Add the corresponding response variable
      train_blocks[[curr_test_block]]$ytarget <- train_test_bwm$Train$data$ytarget[observed_rows]
    }
  }
  
  # 1-4 If 'train_blocks' is empty the SB approach can not be applied & return an DF w/o metrics
  if (length(names(train_blocks)) <= 0) {
    return(data.frame("path"               = curr_path, 
                      "frac_train"         = 0.75, 
                      "split_seed"         = curr_split_seed, 
                      "block_seed_train"   = curr_block_seed_train,
                      "block_seed_test"    = curr_block_seed_test, 
                      "block_order_train_for_BWM" = paste(train_test_bwm$Train$block_names, collapse = ' - '),
                      "block_order_test_for_BWM"  = paste(train_test_bwm$Train$block_names, collapse = ' - '),
                      "train_pattern"      = curr_train_pattern, 
                      "train_pattern_seed" = curr_train_pattern_seed, 
                      "test_pattern"       = curr_test_pattern,
                      "common_blocks"      = "---",
                      "block_best_oob"     = "---",
                      "block_best_oob_auc" = "---",
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
                      "BrierScore"         = '---'))
  }
  
  # [2] If 'train_blocks' is not empty: Fit an RF on each block of the train-set & evaluate them 
  #     with their OOB-AUC
  # 2-1 Loop over each block in 'train_blocks', fit an RF on it & get the corresponding oob-AUC
  res_df <- data.frame('block' = character(), 
                       'auc'   = numeric())
  
  for (curr_train_block in names(train_blocks)) {
    
    # --1 Extract the data of the 'curr_train_block' from 'train_blocks'
    curr_data <- train_blocks[[curr_train_block]]
    
    # --2 Get the oob-AUC of a RF fit on 'curr_data'
    curr_AUC <- fit_RF_get_oob_AUC(data = curr_data)
    
    # --3 Collect 'block_name' & corresponding AUC
    res_df[nrow(res_df) + 1, ] <- c(curr_train_block, curr_AUC)
  }
  
  # 2-2 Get the name of the block that led to the highest oob-AUC
  best_block <- res_df$block[which(res_df$auc == max(res_df$auc))]
  
  # 2-3 In case 'best_block' has more than one entrance, randomly sample one
  #     (this might happen due to multiple RFs with the same oob-AUC in 2-1)
  set.seed(block_seed_train)
  if (length(best_block) > 1) best_block <- sample(best_block, 1)
  
  # [3] Get predictions for the test-set (based on 'best_block') & get the corresponding metrics
  # 3-1 Train a RF on the 'best_block' of train-set & use it to predict on the test-set then!
  preds_test_set <- get_predicition(train = train_blocks[[best_block]],
                                    test = test_blocks[[best_block]])
  
  # 3-2 Calculate the metrics based on the true & predicted labels
  # 3-2-1  Confusion Matrix & all corresponding metrics (Acc, F1, Precision, ....)
  metrics_1 <- caret::confusionMatrix(preds_test_set$pred_classes, 
                                      train_test_bwm$Test$data$ytarget, 
                                      positive = "1")
  
  # 3-2-2 Calculate the AUC
  AUC <- pROC::auc(train_test_bwm$Test$data$ytarget, 
                   preds_test_set$pred_prob_pos_class, quiet = T)
  
  # 3-2-3 Calculate the Brier-Score
  brier <- mean((preds_test_set$pred_prob_pos_class - as.numeric(levels(train_test_bwm$Test$data$ytarget))[train_test_bwm$Test$data$ytarget]) ^ 2)
  
  # [3] Return the results as DF
  return(data.frame("path"               = path, 
                    "frac_train"         = frac_train, 
                    "split_seed"         = split_seed, 
                    "block_seed_train"   = block_seed_train,
                    "block_seed_test"    = block_seed_test, 
                    "block_order_train_for_BWM" = paste(train_test_bwm$Train$block_names, collapse = ' - '),
                    "block_order_test_for_BWM"  = paste(train_test_bwm$Test$block_names, collapse = ' - '),
                    "train_pattern"      = train_pattern, 
                    "train_pattern_seed" = train_pattern_seed, 
                    "test_pattern"       = test_pattern, 
                    "common_blocks"      = paste(names(train_blocks), collapse = ' - '),
                    "block_best_oob"     = best_block,
                    "block_best_oob_auc" = res_df$auc[res_df$block == best_block],
                    "ntree"              = preds_test_set$RF_ntree, 
                    "mtry"               = preds_test_set$RF_mtry, 
                    "min_node_size"      = preds_test_set$RF_min_node_size, 
                    "AUC"                = AUC,
                    "Accuracy"           = metrics_1$overall['Accuracy'], 
                    "Sensitivity"        = metrics_1$byClass['Sensitivity'], 
                    "Specificity"        = metrics_1$byClass['Specificity'], 
                    "Precision"          = metrics_1$byClass['Precision'], 
                    "Recall"             = metrics_1$byClass['Recall'], 
                    "F1"                 = metrics_1$byClass['F1'], 
                    "BrierScore"         = brier))
}

# [1] Run the experiments                                                    ----
# 1-1 Initalize a empty DF to store the results
SB_res <- data.frame()

# 1-2 Define a list with the paths to the availabe DFs
df_paths <- paste0("./Data/Raw/", list.files("./Data/Raw/"))

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
        curr_res <- tryCatch(eval_sb_appr(path               = curr_path, 
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
                                          "block_best_oob"     = "---",
                                          "block_best_oob_auc" = "---",
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
        curr_res$approach   <- 'SingleBlock'
        
        # Add the results of the setting to 'SB_res' & save it
        SB_res <- rbind(SB_res, curr_res)
        write.csv(SB_res, './Docs/Evaluation_Results/SB_Approach/SB_Eval.csv')
      }
    }
  }
}