
# 0-4-2 Function to evaluate the mdd-sPLS-Apprach
eval_mddspls_approach <- function(path = './compstudy_code_and_results/data/BLCA.Rda', frac_train = 0.75, split_seed = 1312,
                                  block_seed_train = 1234, block_seed_test = 1312, train_pattern = 2, 
                                  train_pattern_seed = 12, test_pattern = 2) {
  "
   Evaluate the mdd-sPLS-Approach on the data 'path' points to. 
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
  # --1 Get all test blocks (whether observed or not) in the correct order
  #     into a list
  test_blocks <- list()
  for (curr_block in train_test_bwm$Test$block_names) {
    
    # --1 Which Index has the current block
    curr_block_idx <- which(train_test_bwm$Test$block_names == curr_block)
    
    # --2 Get the corresponding columns to 'curr_block'
    curr_block_cols <- which(train_test_bwm$Test$block_index == curr_block_idx)
    
    # --3 add it to test_blocks
    test_blocks[[curr_block]] <- as.matrix(train_test_bwm$Test$data[,curr_block_cols])
  }
  
  # --2 Get all train blocks (whether observed or not) in the correct order
  #     into a list
  train_blocks <- list()
  for (curr_test_block in names(test_blocks)) {
    
    # --1 Get the index of the current test-block from the train-set
    curr_train_block_idx <- which(train_test_bwm$Train$block_names == curr_test_block)
    
    # --2 Extract the corresponding columns from train for current train-block 
    curr_train_block_cols <- which(train_test_bwm$Train$block_index == curr_train_block_idx)
    
    # --3 add it to train_blocks
    train_blocks[[curr_test_block]] <- as.matrix(train_test_bwm$Train$data[, curr_train_block_cols])
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
  
  # 1-7 store block information for later
  train_block_names <- train_test_bwm$Train$block_names
  test_block_names <- train_test_bwm$Test$block_names
  rm(train_test_bwm)
  
  # [2] Train & evaluate mdd-sPLS on the data
  # 2-1 Train a mdd-sPLS model on the 'train' data
  set.seed(2387)
  mddspls_cv <- perf_mddsPLS(Xs = train_blocks,
                             Y = as.factor(ytarget),
                             n_lambda = 10,
                             R = 1,
                             NCORES = 4,
                             mode = "logit",
                             plot_result = FALSE,
                             kfolds = 10,
                             weight = TRUE)
  
  # 2-2 Train the model with the best parameters
  best_model <- mddsPLS(Xs = train_blocks,
                        Y = as.factor(ytarget),
                        lambda = mddspls_cv$Optim$optim_para_all$Lambdas[1],
                        R = mddspls_cv$Optim$optim_para_all$R[1],
                        mode = "logit",
                        weight = TRUE)
  
  # 2-3 make predictions
  predictions <- predict(object = best_model,
                         newdata = test_blocks)$probY
  # the predictions have probabilities for both classes, only use the probability
  # for class 1
  predictions <- as.vector(predictions[, 2])
  
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
