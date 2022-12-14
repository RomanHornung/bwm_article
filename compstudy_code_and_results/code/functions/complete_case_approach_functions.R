
# 0-4-2 Get predictions for 'test' from a RF trained on 'train'
get_predicition <- function(train, test) {
  " Get predictions from a RF-Model for 'test', whereby the RF was trained on 'train'.
    Important: > All obs. in 'test' are fully observed!
               > 'train' only consits of features that are availabe in 'test'
                  & contains not a single missing value
                
    Args:
      - train  (DF): DF that only contains variables that are also availabe for 'test'.
                     Must not contain any NA values! 
      - test   (DF): DF that is completly observed for all its observations
                     
    Return:
      - List with: > 'pred_classes' = predicted class for each observation in 'test'
                   > 'pred_prob_pos_class' = predicted probability for a obs. 
                                             to be in class 1
                   > settings of the RF for 'mtry', 'min_node_size' & 'ntree'
  "
  # [0] Check Inputs
  # 0-1 'train' & 'test' must be dataframes w/o missing values
  assert_data_frame(train, any.missing = FALSE)
  assert_data_frame(test, min.rows = 1, any.missing = FALSE)
  
  # 0-2 'train' must not contain any colnames not available in 'test' & vic-versa
  if (!all((colnames(train) %in% colnames(test)))) {
    stop("Train-Set has different features than the Test-Set!")
  }
  
  if (!all((colnames(test) %in% colnames(train)))) {
    stop("Test-Set has different features than the Train-Set!")
  }
  
  # [1] Train a RF & create predictions for the test-set
  # 1-1 Train a RF on 'train'
  # --1 Create a formula to pass to the RF 
  formula_all <- as.formula(paste('ytarget', " ~ ."))
  
  # --2 Fit the actual RF (only use standard-settings)
  RF <- rfsrc(formula = formula_all, data = train, samptype = "swr", 
              seed = 12345678, var.used = 'all.trees')
  
  # 1-2 Get prediction on the test-set from the RF
  predicitons <- predict(RF, test)
  
  # [2] Return the predicted classes & the predicted probabilities for class '1'
  #     as well as the settings of the RF
  return(list('pred_classes'        = predicitons$class,
              'pred_prob_pos_class' = predicitons$predicted[,'1'],
              'RF_ntree'            = RF$ntree,
              'RF_mtry'             = RF$mtry,
              'RF_min_node_size'    = RF$nodesize))
}

# 0-4-3 Evaluate a RF with the complete-case approach & get its metrics
eval_cc_appr <- function(path = './compstudy_code_and_results/data/BLCA.Rda', frac_train = 0.75, split_seed = 1312,
                         block_seed_train = 1234, block_seed_test = 1312, train_pattern = 2, 
                         train_pattern_seed = 12, test_pattern = 2) {
  "Evaluate the CC-Approach on the data 'path' points to. 
   Load the data, split it to train- & test-set and remove all blocks from 
   the train-set that are not available in the test-set! Then remove all 
   observations from the remaining train-set that are not fully observed. 
   In case there are no obs. in the train-set, OR the obs. all have the same 
   reponse class, return a DF w/ settings to evaluation, but w/o metrics.
   In case the train-set is fine, a RF is trained (w/ standard settings 'ntree', 
   'mtry' & 'min_node_size') & evaluated on test-set then. 
   Finally return a DF with the the AUC, the Brier-Score & the standard metrics 
   Precision, Recall, Sensitivity, Specificity, F-1 Score & Accuracy + all the 
   settings for the evaluation (e.g. path, seeds, train_pattern, block_order, ...).
   
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
      > A DF with the settings of the experiment (path to the data, train pattern, ...)
        as well as the settings of the RF (ntree, mtry, ...) and the results
        of the evaluation (AUC; Brier-Score; Accuracy)
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
  
  # 1-2 Prepare the test-set
  # 1-2-1 Get the observed features from the test-set (contain no NAs)
  obs_test_fea <- names(which(colSums(is.na(train_test_bwm$Test$data)) <= 0))
  
  # 1-2-2 Drop all features from the test-set with at least one NA  
  #       --> only contains fully observed features then
  train_test_bwm$Test$data <- train_test_bwm$Test$data[,obs_test_fea]
  
  # 1-3 Prepare the train-set
  # 1-3-1 Remove all variables from the train-set, that are not available for test
  #       --> only contains features then that are available for test
  train_test_bwm$Train$data <- train_test_bwm$Train$data[,obs_test_fea]
  
  # 1-3-2 Remove all observations from the train-set with missing values
  train_test_bwm$Train$data <- train_test_bwm$Train$data[complete.cases(train_test_bwm$Train$data), ]
  
  # 1-4 Check if the Train-Set is still usable to fit a RF on it:
  #     - If it hasn't any rows left OR
  #     - all observations have the same response
  #     --> return a DF with the same lay-out as the result-DF, but w/o Metrics! 
  if (nrow(train_test_bwm$Train$data) <= 0 | length(unique(train_test_bwm$Train$data$ytarget)) == 1) {
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
  
  # --> Test- & Train-Set consist of the same columns & all obs. are fully observed in it 
  
  # [2] Train & evaluate a RF (with its standard-settings) 
  # 2-1 Get predictions for the test-set from a RF that is fitted with its 
  #     standard settings to the processed train-set
  preds_test_set <- get_predicition(train = train_test_bwm$Train$data, 
                                    test = train_test_bwm$Test$data)
  
  # 2-2 Calculate the metrics based on the true & predicted labels
  # 2-2-1  Confusion Matrix & all corresponding metrics (Acc, F1, Precision, ....)
  metrics_1 <- caret::confusionMatrix(preds_test_set$pred_classes, 
                                      train_test_bwm$Test$data$ytarget,
                                      positive = "1")
  
  # 2-2-2 Calculate the AUC
  AUC <- pROC::auc(train_test_bwm$Test$data$ytarget, 
                   preds_test_set$pred_prob_pos_class, quiet = T)
  
  # 2-2-3 Calculate the Brier-Score
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
