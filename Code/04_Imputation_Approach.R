"Script to evaluate the Imputation approach on data with blockwise missingness
 Split a DF to a train- & test-set, separately induce block-wise missingness
 patterns to these and evaluate the IMP-Approach on these then.
 This is done 5x for each DF in './Data/Raw/' for each possible combination 
 of blockwise missingess patterns in train- & test-set.

 IMP-Approach:
  > Impute the missing values in the train-set with TOBMI
    (see 'https://academic.oup.com/bioinformatics/article/35/8/1278/5092930'
     - the functions for TOBMI were supplied by Dr. Roman Hornung)
  > Remove all blocks from the train-set that are not available in the test-set
  > Fit a RF on the remaining train-set & create predicitons for the test-set
  > Evaluate the results with common metrics (AUC, Accuracy, F-1 Score, ...)
"
# [0] Set WD, load packages, define variables and functions                  ----
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

# 0-4-2 Altered TOBMI function with greater speed than the original TOMBI
#       (supplied by Dr. Hornung)
TOBMIfast <- function(x = cpg, y = exp) {
  
  # Calculating the distances among un-/complete cases using auxiliary dataset
  dist.matrix <- as.matrix(dist( x ))
  
  # Neighbors list for every uncomplete cases
  missing_num <- length(which(complete.cases(y) == F)) 
  donors <- list()
  for(i in 1:missing_num){
    tempobj <- dist.matrix[i,c(c(missing_num + 1):dim(x)[1])]
    donors[[i]] <- as.matrix(tempobj[order(tempobj, runif(length(tempobj)))][1 : floor(sqrt(dim(x)[1] - missing_num))])
    # NEW: If the Mahalanobis distance was zero, the weights were NaN. --> Replace
    #      distances of zero by the smallest observed distance greater than zero.
    # Exception: Sometimes all distances were zero. In these cases just set all 
    #            distances to 1, which corresponds to equal weights.
    if (any(donors[[i]][,1]!=0))
      donors[[i]][,1][donors[[i]][,1]==0] <- min(donors[[i]][,1][donors[[i]][,1]!=0])  
    else
      donors[[i]][,1] <- 1
  }
  
  ##Neighbors will be weighted by distance 
  donors.w<-list()    
  for(i in 1:missing_num){
    donors.w[[i]]<-(1/donors[[i]][,1])/sum((1/donors[[i]][,1]))
  }
  
  neighbourindices <- lapply(donors.w, function(x1) sapply(names(x1), function(x2) which(rownames(y)==x2)))
  
  ##Imputation process
  for(j in 1:missing_num){
    sweep(as.matrix(y[neighbourindices[[j]],]), 1, donors.w[[j]], "*")->donors.calculate
    y[j,]<-apply(donors.calculate, MARGIN = 2,sum)
  }
  
  imputed.data<-y
}

# 0-4-2 Function to do imputation a la TOBMI (supplied by Dr. Hornung)
ImputeWithTOBMI <- function(omicsdata, blockind) {
  
  rownamessafe        <- rownames(omicsdata)
  rownames(omicsdata) <- 1:nrow(omicsdata)
  
  blockscompl <- unique(blockind[apply(omicsdata, 2, function(x) !any(is.na(x)))])
  blocksres   <- setdiff(unique(blockind), blockscompl)
  
  omicsdatacompl <- omicsdata[,blockind==blockscompl]
  
  for(i in seq(along=blocksres))
    omicsdata[,blockind==blocksres[i]] <- ImputeTwo(omicsdatacompl, omicsdata[,blockind==blocksres[i]])
  
  rownames(omicsdata) <- rownamessafe
  
  return(omicsdata)
}

# 0-4-3 HelpFunction for 0-4-2 (supplied by Dr. Hornung)
ImputeTwo <- function(omicsdatacompl, omicsdatamiss) {
  
  reorderind <- order(complete.cases(omicsdatamiss))
  rereorderind <- order(reorderind)
  
  imputed <- TOBMIfast(x = omicsdatacompl[reorderind,], y = omicsdatamiss[reorderind,])
  imputed <- imputed[rereorderind,]
  
  return(imputed)
}

# 0-4-4 Fit a RF on train & get predictions on test
get_predicition <- function(train, test) {
  "Fit a RF on 'train' and create predicitons for 'test' then. 
                
    Args:
      - train  (DF): DF that only contains variables also availabe for 'test'.
                     Must not contain any NA values! 
      - test   (DF): DF that is completly observed in all observations
                     
    Return:
      - List with: > 'pred_classes' = predicted class for each observation in 'test'
                   > 'pred_prob_pos_class' = predicted probability for each obs. 
                                             to be in class 1
                   > settings of the RF for 'mtry', 'min_node_size' & 'ntree'
  "
  # [0] Check Inputs
  # 0-1 'train' & 'test' must be DFs w/o missing values
  assert_data_frame(train, any.missing = FALSE)
  assert_data_frame(test, any.missing = FALSE)
  
  # 0-2 'train' must not contain any colnames not avaible in 'test' & vic versa
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
  
  # 1-2 Get Prediction for the test-set
  predicitons <- predict(RF, test)
  
  # 1-3 Return the predicted classes & the predicted probabilities for class '1'
  #     as well as the settings of the RF
  return(list('pred_classes'        = predicitons$class,
              'pred_prob_pos_class' = predicitons$predicted[,'1'],
              'RF_ntree'            = RF$ntree,
              'RF_mtry'             = RF$mtry,
              'RF_min_node_size'    = RF$nodesize))
}

# 0-5-5 Evaluate the imputation approach
eval_imp_approach <- function(path = './Data/Raw/BLCA.Rda', frac_train = 0.75, split_seed = 1312,
                              block_seed_train = 1234, block_seed_test = 1312, train_pattern = 2, 
                              train_pattern_seed = 12, test_pattern = 2) {
  "Evaluate the Imputation-Approach on the data 'path' points to.
   The (block-wise) missing values in the train-set are imputed with TOMBI. Then all the blocks that
   are not available for the test-set are removed from the train-set.
   If the train-set is 'empty' afterwards, the approach can not be applied!
   Else a RF is trained (w/ standard settings 'ntree', 'mtry' & 'min_node_size') &
   evaluated on test-set then. 
   Finally return a DF with the the AUC, the Brier-Score & the standard metrics 
   Precision, Recall, Sensitivity, Specificity, F-1 Score & Accuracy + all the 
   settings for the evaluation (e.g. path, seeds, train_pattern, block_order, ...).
   
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
  
  # [1] Load the data & prepare them for the Imputation-Approach
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
  
  # 1-2 Impute the missing values in the train-set
  # 1-2-1 Remove the response variable 'ytarget' from the train-set (temporary for imputation)
  train_ytarget                     <- train_test_bwm$Train$data$ytarget
  train_test_bwm$Train$data$ytarget <- NULL
  
  # 1-2-2 Do the imputation of missing values in the train-set
  train_test_bwm$Train$data_imputed <- ImputeWithTOBMI(omicsdata = train_test_bwm$Train$data, 
                                                       blockind  = train_test_bwm$Train$block_index) 
  
  # 1-3 Remove the blocks from the imputed train-set that are not available for the test-set
  # 1-3-1 Get the names of the observed blocks in test
  observed_test_blocks <- c()
  for (curr_test_block in train_test_bwm$Test$block_names) {
    
    # --1 Which block-index has 'curr_test_block' 
    curr_test_block_idx <- which(train_test_bwm$Test$block_names == curr_test_block)
    
    # --2 Get the corresponding columns to 'curr_test_block'
    curr_test_block_cols <- which(train_test_bwm$Test$block_index == curr_test_block_idx)
    
    # --3 Check if 'curr_test_block' is fully observed in the test-set, if so, add it to 'observed_test_blocks'
    if (sum(is.na(train_test_bwm$Test$data[,curr_test_block_cols])) == 0) {
      observed_test_blocks <- c(observed_test_blocks, curr_test_block)
    }
  }
  
  # 1-3-2 Remove all blocks & columns from the test-set that are not fully observed 
  blocks_to_keep_ <- which(train_test_bwm$Test$block_names %in% observed_test_blocks)
  cols_to_keep_   <- which(train_test_bwm$Test$block_index %in% blocks_to_keep_)
  y_target_idx_   <- which(colnames(train_test_bwm$Test$data) == "ytarget")
  train_test_bwm$Test$data <- train_test_bwm$Test$data[,c(cols_to_keep_, y_target_idx_)]
  
  # 1-3-3 Remove all blocks & columns from the imputed train-set that are not observed in test
  blocks_to_keep <- which(train_test_bwm$Train$block_names %in% observed_test_blocks)
  cols_to_keep   <- which(train_test_bwm$Train$block_index %in% blocks_to_keep)
  train_test_bwm$Train$data_imputed <- train_test_bwm$Train$data_imputed[,cols_to_keep]
  
  # 1-4 Add the original response to the train-set
  train_test_bwm$Train$data_imputed$ytarget <- train_ytarget
  
  # 1-5 If the train-set doesn't consist of any observations, return the result
  #     DF with empty metrics! 
  if (nrow(train_test_bwm$Train$data_imputed) <= 0) {
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
                      "common_blocks"      = paste(observed_test_blocks, collapse = ' - '),
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

  # [2] Fit an RF on the imputed train-set, get predictions for the test-set & calc metrics
  # 2-1 Train a RF on the imputed train-set & use it to predict on the test-set then!
  preds_test_set <- get_predicition(train = train_test_bwm$Train$data_imputed,
                                    test = train_test_bwm$Test$data)
  
  # 2-2 Calculate the metrics based on the true & predicted labels
  # 2-2-1 Confusion Matrix & all corresponding metrics (Acc, F1, Precision, ....)
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
                    "common_blocks"      = paste(observed_test_blocks, collapse = ' - '),
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
# 1-1 Initialize a empty DF to store the results
IMP_res <- data.frame()

# 1-2 Define a list with the paths to the available DFs
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
                                          "block_seed_train"   = block_seed_train,
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
        write.csv(IMP_res, './Docs/Evaluation_Results/IMP_Approach/IMP_Eval.csv')
      }
    }
  }
}