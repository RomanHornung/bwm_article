
# 0-4-2 Function to evaluate the PL-Apprach
eval_pl_approach <- function(path = './compstudy_code_and_results/data/BLCA.Rda', frac_train = 0.75, split_seed = 1312,
                             block_seed_train = 1234, block_seed_test = 1312, train_pattern = 2, 
                             train_pattern_seed = 12, test_pattern = 2) {
  "
   Evaluate the PL-Approach on the data 'path' points to. 
   TODO: add description
   Evaluate the predicitons with common metrics,and return all results in a DF w/ all the
   settings for the evaluation  (e.g. path, seeds, train_pattern, settings for RF, ...).
   In case the approach can no be applied (e.g. no common blocks in train- & test-set), return a 
   DF with the settings of the evaluation, but w/ '---' for the metrics!
   
   Args:
      > path               (str): Path to a dataset - must contain './compstudy_code_and_results/data'
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
  #         'get_predicition()' that werde loaded from './compstudy_code_and_results/code/functions/create_bwm_pattern.R'
  
  # [1] Load & prepare the data 
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
  
  # 1-2 Get the observed blocks of test- & train-set
  # --1 Get the fully observed blocks in 'test' and store their names in 'test_blocks'
  #     (because there is only one missingness pattern in the test data, a block
  #     is either completely observed or not at all)
  test_blocks <- list()
  for (curr_block in train_test_bwm$Test$block_names) {
    
    # --1 Which Index has the current block
    curr_block_idx <- which(train_test_bwm$Test$block_names == curr_block)
    
    # --2 Get the corresponding columns to 'curr_block'
    curr_block_cols <- which(train_test_bwm$Test$block_index == curr_block_idx)
    
    # --3 Check whether the whole block is observed (no NA's)
    curr_block_observed <- all(!is.na(train_test_bwm$Test$data[,curr_block_cols] <= 0))
    
    # --4 If the current block is fully observed, add it to 'test_blocks'
    if (curr_block_observed) {
      test_blocks[[curr_block]] <- train_test_bwm$Test$data[,curr_block_cols]
    }
  }
  
  # --2 For each block in 'test_blocks', check whether 'train' has observations in the corresponding 
  #     block. If this is the case, add the observed part of the block to 'train_blocks'
  train_blocks <- list()
  for (curr_test_block in names(test_blocks)) {
    
    # --1 Get the index of the current test-block from the train-set
    curr_train_block_idx <- which(train_test_bwm$Train$block_names == curr_test_block)
    
    # --2 Extract the corresponding columns from train for current train-block 
    curr_train_block_cols <- which(train_test_bwm$Train$block_index == curr_train_block_idx)
    
    # --3 Check whether the whole block has (at least some) observed values (no NA's), then we can use it!
    curr_train_block_partly_observed <- any(!is.na(train_test_bwm$Train$data[,curr_train_block_cols] <= 0))
    
    # --4 If the current_test_block is (partly) observed in the train, add it to 'train_blocks'
    if (curr_train_block_partly_observed) {
      train_blocks[[curr_test_block]] <- train_test_bwm$Train$data[, curr_train_block_cols]
    } else {
      # --5 If the block is observed in the test data, but not in the train data,
      # remove it also from the test data because then there's no advantage of
      # keeping it (because it can't be used by the pl model anyways)
      test_blocks[[curr_test_block]] <- NULL
    }
  }
  # --5 extract the response variable
 ytarget <- train_test_bwm$Train$data$ytarget
 ytarget_test <- train_test_bwm$Test$data$ytarget
 
 # --6 store names
 names_train_blocks <- names(train_blocks)
  
  # 1-3 In case 'train_blocks' is an empty list, return the result-DF, but w/o metrics!
  if (length(names(train_blocks)) <= 0) {
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
                      "common_blocks"      = '---',
                      "AUC"                = '---',
                      "Accuracy"           = '---', 
                      "Sensitivity"        = '---', 
                      "Specificity"        = '---', 
                      "Precision"          = '---', 
                      "Recall"             = '---', 
                      "F1"                 = '---', 
                      "BrierScore"         = '---'))
  }
  
  # 1-4 make a matrix out of the list
  train_matrix <- as.matrix(do.call("cbind", train_blocks))
  
  # 1-5 bring the test data blocks in the same order as the train data and make
  #     a matrix
  test_matrix <- as.matrix(do.call("cbind", test_blocks[names(train_blocks)]))
  
  # 1-6 create the block indices
  block_indices_pre <- unlist(lapply(train_blocks, ncol))
  block_indices <- list()
  for (i in seq_len(length(block_indices_pre))) {
    if (i == 1) {
      block_indices[[i]] <- seq(from = 1, to = block_indices_pre[i], by = 1)
    } else {
      block_indices[[i]] <- seq(from = max(block_indices[[i - 1]]) + 1,
                                to = max(block_indices[[i - 1]]) + block_indices_pre[i],
                                by = 1)
    }
  }
  rm(train_blocks)
  rm(test_blocks)
  
  # 1-7 store block information for later
  train_block_names <- train_test_bwm$Train$block_names
  test_block_names <- train_test_bwm$Test$block_names
  rm(train_test_bwm)
  
  # [2] Train & evaluate prioritylasso on the data
  # 2-1 Train a PL model on the 'train' data
  # maybe return block order in cvm function not only as string but in number
  # format
  block_permutations <- permn(block_indices)
  set.seed(732)
  pl_cvm_results <- cvm_prioritylasso(
    X = train_matrix,
    Y = ytarget,
    family = "binomial",
    type.measure = "deviance", # sometimes cvm.glmnet falls back to deviance,
    # therefore I use it as the default to make runs comparable
    nfolds = 5,
    mcontrol = missing.control(
      handle.missingdata = "impute.offset",
      impute.offset.cases = "available"
    ),
    blocks.list = block_permutations,
    return.x = FALSE
  )
  
  # 2-2 Get the best model & order of blocks
  best_model <- pl_cvm_results$best.model
  
  # 2-3 bring the test data into the correct block order (the same order as
  # the best model from the training data, otherwise the prediction is not
  # correct) and make the prediction
  # also check for missingness in the test data and set option accordingly
  block_order_best_model <- best_model$blocks
  if (sum(is.na(test_matrix)) > 1) {
    missing_type <- "impute.block"
  } else {
    missing_type <- "none"
  }
  predictions <- predict(best_model,
                         newdata = test_matrix[, unlist(block_order_best_model)],
                         type = "response",
                         handle.missingtestdata = missing_type,
                         include.allintercepts = TRUE)
  
  # coerce them to a vector
  predictions <- as.vector(predictions)
  
  # 2-4 Get the predicted class
  classes_predicted <- factor(as.numeric(predictions >= 0.5), levels = c(0, 1))
  
  # [3] Calculate the metrics based on the true & predicted labels
  # 3-1  Confusion Matrix & all corresponding metrics (Acc, F1, Precision, ....)
  metrics_1 <- caret::confusionMatrix(classes_predicted, 
                                      factor(ytarget_test, 
                                             levels = c(0, 1)),
                                      positive = "1")
  
  # 3-2 Calculate the AUC
  AUC <- pROC::auc(factor(ytarget_test, levels = c(0, 1)), 
                   predictions, quiet = T)
  
  # 3-3 Calculate the Brier-Score
  brier <- mean((predictions - ytarget_test)  ^ 2)
  
  # remove memory consumption
  rm(pl_cvm_results)
  rm(best_model)
  rm(train_matrix)
  rm(test_matrix)
  gc()
  
  # [4] Return the results as DF
  return(data.frame("path"               = path, 
                    "frac_train"         = frac_train, 
                    "split_seed"         = split_seed, 
                    "block_seed_train"   = block_seed_train,
                    "block_seed_test"    = block_seed_test, 
                    "block_order_train_for_BWM" = paste(train_block_names, collapse = ' - '),
                    "block_order_test_for_BWM"  = paste(test_block_names, collapse = ' - '),
                    "train_pattern"      = train_pattern, 
                    "train_pattern_seed" = train_pattern_seed, 
                    "test_pattern"       = test_pattern, 
                    "common_blocks"      = paste(names_train_blocks, collapse = ' - '),
                    "AUC"                = AUC,
                    "Accuracy"           = metrics_1$overall['Accuracy'], 
                    "Sensitivity"        = metrics_1$byClass['Sensitivity'], 
                    "Specificity"        = metrics_1$byClass['Specificity'], 
                    "Precision"          = metrics_1$byClass['Precision'], 
                    "Recall"             = metrics_1$byClass['Recall'], 
                    "F1"                 = metrics_1$byClass['F1'], 
                    "BrierScore"         = brier))
}
