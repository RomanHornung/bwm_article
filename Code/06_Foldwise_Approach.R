"Script to evaluate the Fold-wise approach on data with blockwise missingness
 Split a DF to a train- & test-set, separately induce block-wise missingness
 patterns to these and evaluate the FW-Approach on these then.
 This is done 5x for each DF in './Data/Raw/' for each possible combination 
 of blockwise missingess patterns in train- & test-set.
 
 FW-Approach:
  > Fit a seperate RF on each fold of the train-set
  > Prune these foldwise fitted RFs in regard to the test-set
  > Internally evaluate pruned FW-RF with their oob-AUC 
  > Each RF is then used to predict on the observations of the test-set 
  > Create an overall prediciton by calculating an weighted average of the fold-wise
    predicitons - as weights we use the oob-AUC of the pruned FW-RFs
  > Evaluate the results with common metrics (AUC, Accuracy, F-1 Score, ...)
"
# [0] SetWD, load packages, define variables and functions                  ----
# 0-1 Set WD
setwd("/Users/frederik/Desktop/BWM-Article/")             # Mac
setwd("C:/Users/kuche/Desktop/BWM-Paper")                 # Windows
setwd("/dss/dsshome1/lxc0B/ru68kiq3/Project/BWM-Article") # Server

# 0-2 Load packages
library(checkmate)
library(randomForestSRC)
library(caret)
library(pROC)
library(doParallel)

# 0-3 Define variables

# 0-4 Define functions
# 0-4-1 Load functions from 'code/01_Create_BWM_Pattern"
source("./Code/01_Create_BWM_Pattern.R")

# 0-4-2 Load functions so we can fit and prune a RF 
#       (not possible with 'randomForestSRC'-library...)
source("./code/06_1_simpleRF_adaption.R")

# 0-4-3 Check whether all trees are grown correctly
all_trees_grown_correctly <- function(trees) {
  "Check, whether 'trees', were grown correctly & if not grow these trees again,
   as long, as they are grown correctly! 
      --> Growning not correctly: No 'childNodeIDs', no split variables etc...
  
   Args:
      trees (list) : list filled with object of the class 'Tree'! 
                     For each object in there check, whether it was grown 
                     correctly (has childNodeIDs) - if not grown it again!
                     
   Return: 
      list of trees, where all of these trees were grown correctly
      --> each tree has at least one 'childNodeID'
  "
  # [0] Check Inputs  ----------------------------------------------------------
  # 0-1 All Objects in 'trees' of class 'Tree'
  trees_classes <- sapply(trees, function(x) class(x))
  if (any(!grepl("Tree", trees_classes))) {
    stop("Not all Objects in 'trees' are of class 'Tree'")
  }
  
  # [1] Get the entrance of the objects, that miss child node IDs  -------------
  wrong_trees  <- unlist(lapply(1:length(trees), 
                                FUN = function(x) {
                                  if (length(trees[[x]]$child_nodeIDs) == 0) x
                                }))
  
  # [2] Regrow the trees, that were not grown correctly  -----------------------
  #     If there are any trees not grown correctly, grow them again until all
  #     of the trees were grown correctly!
  while (length(wrong_trees) > 0) {
    
    # grow the errours trees again
    trees[wrong_trees] <- lapply(trees[wrong_trees], 
                                 function(x) {
                                   x$grow(replace = TRUE)
                                   x
                                 })
    
    # check whether any of the trees is not grown correctly!
    wrong_trees  <- unlist(lapply(1:length(trees), 
                                  FUN = function(x) {
                                    if (length(trees[[x]]$child_nodeIDs) == 0) x
                                  }))
  }
  
  # [3] Return the correclty grown trees  --------------------------------------
  return(trees)
}

# 0-4-4 Calculate the oob AUC of single Forest
get_oob_AUC <- function(trees) {
  "Calculate OOB AUC of a list of trees! 
   For this we go  through all OOB Predictions and obtain aggregated predicitons
   from all trees, that have the same observation as out-of-bag!
   In case the metrics are 'NA' [not defined] they are repalced by 0 [worst value]
    
    Args: 
      - trees (list)        : list filled w/ objects of class 'Tree'
      
    Return:
      - Average oob-Acc & oob-F1 metric for 'trees'!
  "
  # [0] Check Input ------------------------------------------------------------
  # 0-1 Make sure 'trees' is a list filled with 'Trees'
  assert_list(trees, min.len = 1, any.missing = FALSE)
  if (any(sapply(trees, function(x) 'Tree' %in% class(x)))) {
    stop("not all elements in 'trees' are of class 'Tree'")
  }
  
  # [1] Get the OOB Predicitons ------------------------------------------------
  # 1-1 Get the trees, that are usable - not pruned in the first split_variable!
  usable_trees <- sapply(1:length(trees), function(x) {
    
    # Check whether the first split_var was pruned!
    if (trees[[x]]$child_nodeIDs[[1]][1] != "pruned") {
      return(x)
    } 
  })
  
  # 1-1-1 If all the trees were pruned in the first node, we can not do
  #       any OOB predictions --> OOB-Accuracy = 0
  if (length(usable_trees) < 1) {
    return(0)
  } else {
    usable_trees <- unlist(usable_trees)
  }
  
  # 1-2 For all usable trees [not pruned in first split variable] get the IDs
  #     of the OOB observations. Then collect all and only keep unique IDs!
  oob_ids_all_trees <- sapply(usable_trees, function(x) trees[[x]]$oob_sampleIDs)
  unique_oob_ids    <- unique(unlist(oob_ids_all_trees))
  
  # 1-3 Loop over all 'unique_oob_ids' and get the oob predictions from all 
  #     trees, that have the same OOB observation!
  all_oob_preds_class0 <- c()
  for (curr_oob in unique_oob_ids) {
    
    # 1-3-1 Get all trees that have 'curr_oob' as OOB observation!
    trees_same_oob <- unlist(sapply(usable_trees, function(x) {
      if (curr_oob %in% trees[[x]]$oob_sampleIDs) x
    }))
    
    # 1-3-2 Get the feas of the observation that is OOB for 'trees_same_oob' 
    curr_oob_feas <- Data$new(data = trees[[1]]$data$data[curr_oob,])
    
    # 1-3-3 Get a Prediciton for the 'curr_oob' from all trees!
    predicted_probs <- sapply(trees_same_oob, 
                              function(x) trees[[x]]$predict(curr_oob_feas))
    
    # 1-3-4 Aggregate the predictions from the different trees and
    #       Get the probability for class 0! [First Row is class 0]
    all_oob_preds_class0 <- c(all_oob_preds_class0, 
                              sum(predicted_probs[1,]) / length(trees_same_oob))
  }
  
  # [2] Compare predicted probabilites with the true classes 
  # 2-1 Convert 'all_oob_preds_class0' to 'all_oob_preds_class1'
  all_oob_preds_class1 <- ((all_oob_preds_class0 - 1) * - 1)
  
  # 2-2 Get the AUC
  AUC <- tryCatch(expr = pROC::auc(trees[[1]]$data$data[unique_oob_ids, 1], 
                                   all_oob_preds_class1, quiet = T),
                  error = function(c) 0)
  
  # 2-3 Return it
  return(AUC)
}

# 0-4-5 Function to evaluate the FW-Approach
eval_fw_approach <- function(path = './Data/Raw/BLCA.Rda', frac_train = 0.75,
                             split_seed = 12, block_seed_train = 13, block_seed_test = 11, 
                             train_pattern = 2, train_pattern_seed = 12, test_pattern = 2) {
  "Evaluate the FW-Approach on the data 'path' points to. 
   Fit a seperate RF on each fold in the train-set - each RF is trained with
   their standard settings as from the 'rfsrc'-package (e.g. 'ntree' & 'mtry'). 
   Prune these trees according to the test-set then 
   (if a single decision-tree uses a split-variable that is not available in the
   test-set, make this node a new terminal node).
   Then the predicitionss of the fold-wise fitted RFs are aggregated to a final 
   prediciton, whereby the final prediciton equals a weighted average of the
   fold-wise predicitons. The weights in the weighted average equal the oob-auc 
   of the fold-wise fitted RFs. Evaluate the predicitons with common metrics, and
   return all results in a DF w/ all the settings for the evaluation (e.g. path, 
   seeds, train_pattern, settings for RF, ...). 
   In case an error happens, return a DF with the settings of the evaluation, 
   but w/ '---' for the metrics!
   
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
        the settings of the RF (ntree, mtry, ...) and the metric-results of the 
        evaluation (AUC; Brier-Score; Accuracy)
  "
  # [0] Check Inputs
  #     --> All arguments are checked in the functions 'get_train_test()' &
  #         'get_predicition()' [loaded from 'Code/01_Create_BWM_Pattern.R']
  
  # [1] Load & prepare the data
  # 1-1 Load the data from 'path', split it to test- & train-set, shuffle their block-order
  #     & induce block-wise missingness according to 'train_pattern' & 'test_pattern'
  train_test_bwm <- get_train_test(path = path,                             # Path to the data
                                   frac_train = frac_train,                 # Fraction of data used for Training (rest for test)
                                   split_seed = split_seed,                 # Seed for the split of the data into test- & train
                                   block_seed_train = block_seed_train,     # Seed to shuffle the block-order in train
                                   block_seed_test = block_seed_test,       # Seed to shuffle the block-order in test
                                   train_pattern = train_pattern,           # Pattern to introduce to train
                                   train_pattern_seed = train_pattern_seed, # Seed for the introduction of the BWM into train
                                   test_pattern = test_pattern)             # Pattern for the test-set
  
  # [2] Get the various folds in the data & fit a seperate RF on each of the folds
  # 2-1 Get the amount of folds in the train-data & check whether there are at least two folds
  amount_train_folds <- length(unique(train_test_bwm$Train$fold_index))
  
  if (amount_train_folds <= 1) {
    print("Train-Set only consits of a single fold! FW-Approach makes no sense in such a setting")
    return(data.frame("path"             = path, 
                      "frac_train"         = frac_train, 
                      "split_seed"         = split_seed, 
                      "block_seed_train"   = block_seed_train,
                      "block_seed_test"    = block_seed_test, 
                      "block_order_train_for_BWM" = paste(train_test_bwm$Train$block_names, collapse = ' - '),
                      "block_order_test_for_BWM"  = paste(train_test_bwm$Test$block_names, collapse = ' - '),
                      "train_pattern"      = train_pattern, 
                      "train_pattern_seed" = train_pattern_seed, 
                      "test_pattern"       = test_pattern, 
                      "AUC"                = '---',
                      "Accuracy"           = '---', 
                      "Sensitivity"        = '---', 
                      "Specificity"        = '---', 
                      "Precision"          = '---', 
                      "Recall"             = '---', 
                      "F1"                 = '---', 
                      "BrierScore"         = '---'))
  }
  
  # 2-2 Fit a seperate RandomForest to each of the available folds
  Forest <- list()
  for (j_ in 1:amount_train_folds) {
    
    # --1 Extract the data for fold 'j_' & remove all columns w/ NAs 
    curr_fold <- train_test_bwm$Train$data[which(train_test_bwm$Train$fold_index == j_),]
    curr_fold <- curr_fold[,-which(sapply(curr_fold, function(x) sum(is.na(x)) == nrow(curr_fold)))]
    
    # --2 Define formula
    formula_all <- as.formula(paste("ytarget ~ ."))
    
    # --3 Define settings for the current foldwise RF 
    #     (settings for the arguments are the same as in the 'rfsrc'-package)
    fold_RF <- simpleRF(formula           = formula_all, 
                        data              = curr_fold, 
                        num_trees         = 5,    # Same settings as for 'rfSCR' ! 500 !
                        mtry              = NULL, 
                        min_node_size     = 1,
                        replace           = TRUE) # always TRUE, as we need OOB!
    
    # --4 Grow the single trees of the just defined 'fold_RF' 
    fold_RF <- lapply(fold_RF, function(x) {
      x$grow(replace = TRUE)
      x
    })
    
    # --5 Ensure the trees are grown correctly
    fold_RF <- all_trees_grown_correctly(fold_RF)
    
    # --6 Add the grown 'fold_RF' to 'Forest'
    Forest[[j_]] <- fold_RF
  }
  
  # [3] Get predicitons from each fold-wise fitted RF on the test-set
  # 3-1 Remove all columns with NA from the test-data & check it again then
  testdata <- train_test_bwm$Test$data
  testdata <- testdata[ , colSums(is.na(testdata)) == 0]
  assert_data_frame(testdata, any.missing = F, min.rows = 1)
  
  # 3-2 Process the test-data (specific preparation for each FW-fitted RF)
  #     Convert 'testdata' to same format as the data the FW-RFs	 were originally
  #     trained with, to ensure factor levels/ features are the same ....
  tree_testsets <- list()
  for (i in 1:length(Forest)) {
    tree_testsets[[i]] <- process_test_data(tree = Forest[[i]][[1]], 
                                            test_data = testdata)
  }
  
  # 3-3 Get a prediction for every observation in 'testdata' from all FW-RFs
  #     (class & probabilites) - as the features in Train & Test can be different, 
  #     the FW-fitted Forests need to be pruned before creating a prediciton
  tree_preds_all <- list()
  tree_preds_all <- foreach(i = 1:length(Forest)) %dopar% { # par
    
    # save the predictions as 'treeX_pred'
    return(get_pruned_prediction(trees = Forest[[i]], 
                                 test_set = tree_testsets[[i]]))
  }
  
  # 3-4 Check whether any of the RFs is not usable [only NA predicitons] & rm it
  # --1 Get the trees that can not be used for prediciton
  not_usable <- sapply(seq_len(length(tree_preds_all)), function(i) {
    all(is.na(tree_preds_all[[i]]$Class))
  })
  
  # --2 Check that there are still trees existing, else no preds possible!
  if (all(not_usable)) {
    print("None of the trees are usable for predictions!")
    return(data.frame("path"             = path, 
                      "frac_train"         = frac_train, 
                      "split_seed"         = split_seed, 
                      "block_seed_train"   = block_seed_train,
                      "block_seed_test"    = block_seed_test, 
                      "block_order_train_for_BWM" = paste(train_test_bwm$Train$block_names, collapse = ' - '),
                      "block_order_test_for_BWM"  = paste(train_test_bwm$Test$block_names, collapse = ' - '),
                      "train_pattern"      = train_pattern, 
                      "train_pattern_seed" = train_pattern_seed, 
                      "test_pattern"       = test_pattern, 
                      "AUC"                = '---',
                      "Accuracy"           = '---', 
                      "Sensitivity"        = '---', 
                      "Specificity"        = '---', 
                      "Precision"          = '---', 
                      "Recall"             = '---', 
                      "F1"                 = '---', 
                      "BrierScore"         = '---'))
  }
  
  # --3 Remove the tress, that are not usable from 'Forest', 'tree_preds_all' & 'tree_testsets'
  if (any(not_usable)) {
    Forest         <- Forest[-c(which(not_usable))]
    tree_preds_all <- tree_preds_all[-c(which(not_usable))]
    tree_testsets  <- tree_testsets[-c(which(not_usable))]
  }
  
  # [4] Get the FW-RFs oob-AUC & aggregate their predicitons weighted by it
  # 4-1 Loop over all trees and prune them according to the testdata!
  #    [--> had to be added, as the pruning is not saved when running in parallel]
  for (i_ in 1:length(Forest)) {
    curr_test_set <- tree_testsets[[i_]]
    tmp <- sapply(Forest[[i_]], FUN = function(x) x$prune(curr_test_set))
  }
  
  # 4-2 Get the oob-performance of the pruned trees!
  AUC_weight <- foreach(l = seq_len(length(Forest))) %dopar% { # par
    get_oob_AUC(trees = Forest[[l]])
  }
  
  # 4-3 Get the predicitons of all FW-RFs, weight the observations & get an
  #     aggregated predicted probability  
  probs_class_1_ <- sapply(1:nrow(testdata), FUN = function(x) {
    
    # Get a probability prediciton from each [still usable] tree!
    preds_all <- sapply(seq_len(length(tree_preds_all)), function(i) {
      tree_preds_all[[i]]$Probs[[x]][1]                                 # NOT 100% sure whether probs[1] shows prob for class '1'
    })
    
    # Combine the preditions of the different trees!
    prob_class1 <- weighted.mean(preds_all, w = unlist(AUC_weight), na.rm = TRUE)
    prob_class1
  })
  
  # 4-4 Convert the probabilites to classes (all w/ prob. > 0.5 is class '1')
  preds_class_1_ <- ifelse(probs_class_1_ >= 0.5, 1, 0)
  preds_class_1_ <- factor(preds_class_1_, levels = levels(Forest[[1]][[1]]$data$data[,1]))
  
  # [5] Calculate the metrics
  # 5-1 Confusion Matrix & all corresponding metrics (Acc, F1, Precision, ....)
  metrics_1 <- caret::confusionMatrix(preds_class_1_,
                                      train_test_bwm$Test$data$ytarget,
                                      positive = "1")
  
  # 5-2 Calculate the AUC
  AUC <- pROC::auc(train_test_bwm$Test$data$ytarget, 
                   probs_class_1_, quiet = T)
  
  # 5-3 Calculate the Brier-Score
  brier <- mean((probs_class_1_ - as.numeric(levels(train_test_bwm$Test$data$ytarget))[train_test_bwm$Test$data$ytarget]) ^ 2)
  
  # [6] Return the results as DF
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
FW_res <- data.frame()

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
        curr_res2 <- tryCatch(eval_fw_approach(path               = curr_path, 
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
        
        # Add the, 'int_seed', 'curr_repetition' & 'Foldwise' to 'curr_res'
        curr_res$int_seed   <- int_seed
        curr_res$repetition <- curr_repetition
        curr_res$approach   <- 'Foldwise'
        
        # Add the results of the setting to 'FW_res' & save it
        FW_res <- rbind(FW_res, curr_res)
        write.csv(FW_res, './Docs/Evaluation_Results/FW_Approach/FW_Eval.csv')
      }
    }
  }
}