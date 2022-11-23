"Functions for R. Hornung for the implementation of a R-Package for the foldwise
 RandomForest Approach

 FW-Approach:
- Training:
  > Fit a seperate RF on each fold of the train-set
  
- Predicition:
  > Prune the foldwise fitted RFs in regard to the test-set
  > Internally evaluate the pruned FW-RF with their oob-AUC 
  > Each FW-RF is then used to predict on the observations of the test-set 
  > Create an overall prediciton by calculating an weighted average of the 
    fold-wise predicitons - as weights we use the oob-AUC of the pruned FW-RFs
  > Get weighted predicitons for each obs. in the test-set
"
# [0] SetWD, load packages, define variables and functions                  ----
# 0-1 Set WD
# setwd("/Users/frederik/Desktop/BWM-Article/")             # Mac
setwd("C:/Users/kuche/Desktop/BWM-Paper")                 # Windows
# setwd("/dss/dsshome1/lxc0B/ru68kiq3/Project/BWM-Article") # Server
# setwd("Z:/Projects/SideProjects/BlockwiseMissing/RPackage/BWM-Article")

# 0-2 Load packages
library(checkmate)
library(randomForestSRC)
library(caret)
library(pROC)
library(doParallel)

# 0-3 Define fix variables

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

# 0-4-5 Train a fold-wise RF
multisfor <- function(data, folds, num_trees = 500, mtry = NULL, min_node_size = 1) {
  " Train a seperate RF on each fold in 'data', such that the final RF
    consists of as many FW-RFs as the data contains unique folds.
    
    Args:
      data (data.frame)  : Dataframe with dimensions n*p. Must contain a binary 
                           factor-column 'ytarget' (0 = neg. class / 1 = pos. class)
      folds (vec)        : Vector of length 'n' filled with integers. Indicating
                           for each row in 'data' to which fold it belongs.
                           ! Obs. in the same fold need to be observed in the 
                             same features !
      num_trees (int)    : Amount of trees that are used to grow per foldwise RF
      mtry (int)         : Amount of variables to be checked as split-variables
                           at every split. Default = ceiling(sqrt(p))
      min_node_size (int): Max. amount of nbservations that have to be in a 
                           terminal node!
                           
    Return: 
      List with as many fold-wise fitted RFs (of class 'multisourceRF')
      as the amount of unique folds in the data
  "
  # [0] Check inputs                                                        ----
  # 0-1 'data' has to be a data.frame & contain 'ytarget' as binary factor variable
  assert_data_frame(data)
  if (!('ytarget' %in% colnames(data))) {
    stop("'data' must contain 'ytarget' as column")
  } 
  assert_factor(data$ytarget, levels = c('0', '1'))
  
  # 0-2 'folds' must be of the same length as 'data' & must only contain integers
  if (nrow(data) != length(folds)) {
    stop("'folds' does not have the same length as 'data' has rows")
  }
  assert_integer(folds)
  
  # 0-3 Ensure that all obs. in the same fold have the same features
  for (curr_fold in unique(folds)) {

    # Get the observations of 'curr_fold'
    curr_obs <- which(folds == curr_fold)
    
    # Get the observed feature for the first observation in 'curr_obs'
    obs_feas_in_curr_fold <- colnames(data[curr_obs[1],
                                           which(!is.na(data[curr_obs[1],]), 
                                                 arr.ind=TRUE)])
    
    # Compare the observed features from the remaining 'curr_obs' to 
    # 'obs_feas_in_curr_fold' & ensure all feas are available for both
    check_common_colnames <- sapply(curr_obs[2:length(curr_obs)], function(x) {
      
      # observed columns for the current obs.
      colnames_curr_obs <- colnames(data[x, which(!is.na(data[x,]), 
                                                  arr.ind = TRUE)])
      
      check1 <- all(colnames_curr_obs %in% obs_feas_in_curr_fold)
      check2 <- all(obs_feas_in_curr_fold %in% colnames_curr_obs)
      
      any(!c(check1, check2))
    })
    
    if (any(check_common_colnames)) {
      stop("Observations in the same folds, do not have the same observed features!")
    }
  }
  
  # 0-4 'num_trees' & 'min_node_size' must be integers >= 1
  assert_int(num_trees, lower = 1)
  assert_int(min_node_size, lower = 1)
  
  # 0-5 'mtry' must be an int <= amount of cols in data - if not 'NULL'
  if (is.null(mtry)) {
    mtry = as.integer(ceiling(sqrt(ncol(data))))
  } else {
    assert_int(mtry, upper = ncol(data), lower = 1)
  }
  
  # [1] Fit a a separate RF on each of the available folds in the data      ----
  # 1-1 Initialize a list to store the fold-wise fitted RFs
  Forest <- list()
  
  # 1-2 Loop over each fold & fit a random-forest to each of them
  for (j_ in unique(folds)) {
    
    # --1 Extract the data for fold 'j_' & remove all columns w/ NAs 
    curr_fold <- data[which(folds == j_),]
    curr_fold <- curr_fold[,-which(sapply(curr_fold, function(x) sum(is.na(x)) == nrow(curr_fold)))]
    
    # --2 Define formula
    formula_all <- as.formula(paste("ytarget ~ ."))
    
    # --3 Define settings for the current foldwise RF 
    #     (settings for the arguments are the same as in the 'rfsrc'-package)
    fold_RF <- simpleRF(formula           = formula_all, 
                        data              = curr_fold, 
                        num_trees         = num_trees,
                        mtry              = as.integer(mtry), 
                        min_node_size     = min_node_size,
                        replace           = TRUE) # always TRUE, as we need OOB!
    
    # --4 Grow the single trees of the just defined 'fold_RF' 
    fold_RF <- lapply(fold_RF, function(x) {
      x$grow(replace = TRUE)
      x
    })
    
    # --5 Ensure the trees are grown correctly 
    #    (e.g. might be that no split-vars were found, ...)
    fold_RF <- all_trees_grown_correctly(fold_RF)
    
    # --6 Add the grown 'fold_RF' to 'Forest'
    Forest[[j_]] <- fold_RF
    
  }
  
  class(Forest) <- "multisfor"
  
  # 1-3 Return the fold-wise fitted RF's
  return(Forest)
}

# 0-4-6 Write predict-function
predict.multisfor <- function(object, data, weighted = TRUE) {
  " Get predicitons for the 'data' from a fold-wise fitted RF ('object').
    To do so, process the test-data for each foldwise-fitted RF such that it has 
    the exact same lay-out as the data the fold-wise RF has been trained on.
    After that, prune each of the fold-wise fitted RFs in regard to the test-set
    (cut their nodes, if they use a non-availabe split-variable). Then each 
    fold-wise RF predicts the class probability for each obs. in the test-set. 
    
    Args:
      > object     (list): An object of class 'multisfor'
                           (see 'multisfor()'-function)
      > data (data.frame): Data we want to get predicitons for from our
                           FW_RFs - must not contain missing values & all
                           observations have to be observed in the same 
                           features. If the data doesn't contain features the
                           FW_RFs have been trained, no prediciton is possible
      > weighted   (bool): Shall the oob-AUC of the pruned fold-wise fitted trees
                           be used to create a weighted average of the prediciton?
                            -> else it will be an unweighted average
                                
    Return:
      > Vector with the predicted probability for each observation to belong to 
        class '1'.
  "
  # [0] Check inputs
  # 0-1 'object' has to be of class 'multisfor'
  assert_list(object)
  if (class(object) != 'multisfor') {
    stop("'object' must only contain objects of class 'multisfor'")
  }
  
  # 0-2 'data' has to be a data-frame w/o any missing values & min 1 obs.
  #        --> All obs. must be observed in the same features
  assert_data_frame(data, any.missing = F, min.rows = 1)
  
  # 0-3 'weighted' has to be a boolean
  assert_flag(weighted)
  
  # [1] Process 'data' (specific preparation for each FW-fitted RF)
  #     -> Convert 'data' to the same format of the data the FW-RFs were 
  #        originally trained with, to ensure factor levels/ features are the same ....
  tree_testsets <- list()
  for (i in 1:length(object)) {
    tree_testsets[[i]] <- process_test_data(tree      = object[[i]][[1]], 
                                            test_data = data)
  }
  
  # [2] Get a prediction for every observation in 'data' from each FW-RFs
  #     (class & probabilities) - as the features in Train & Test can be different, 
  #     the FW-fitted Forests need to be pruned before creating a prediction
  tree_preds_all <- list()
  tree_preds_all <- foreach(i = 1:length(object)) %do% { 
    
    # save the predictions as 'treeX_pred'
    get_pruned_prediction(trees    = object[[i]], 
                          test_set = tree_testsets[[i]])
  }
  
  # [3] Generate a (weighted) average of the predicitons
  # 3-1 Check whether any of the RFs is not usable [only NA predicitons] & rm it
  not_usable <- sapply(seq_len(length(tree_preds_all)), function(i) {
    all(is.na(tree_preds_all[[i]]$Class))
  })
  
  # 3-2 Check if the any of the trees are still usable after the pruning.
  #     And remove these trees that can not be used for predictions from 
  #     'tree_preds_all'
  if (all(not_usable)) {
    stop("None of the foldwise fitted RFs are usable for predictions!")
  } else if (any(not_usable)) {
    object         <- object[-c(which(not_usable))]
    tree_preds_all <- tree_preds_all[-c(which(not_usable))]
    tree_testsets  <- tree_testsets[-c(which(not_usable))]
  }
  
  # 3-3 Get the oob-metric for the remaining (& already pruned) FW-RFs
  # --3-1 Loop over all trees and prune them according to the testdata!
  for (i_ in 1:length(object)) {
    curr_test_set <- tree_testsets[[i_]]
    tmp           <- sapply(object[[i_]], FUN = function(x) x$prune(curr_test_set))
  }
  
  # --3-2 Get the oob-performance of the pruned trees!
  AUC_weight <- foreach(l = seq_len(length(object))) %do% { # par
    get_oob_AUC(trees = object[[l]])
  }
  
  # --3-3 Get the predicted probabilities for class 1 for each observation & 
  #       from each of the remaining FW-RFs
  probs_class_1_ <- sapply(1:nrow(data), FUN = function(x) {
    
    # Get a probability prediciton from each [still usable] tree!
    preds_all <- sapply(seq_len(length(tree_preds_all)), function(i) {
      tree_preds_all[[i]]$Probs[[x]][1]
    })
    
    # Combine the preditions of the different trees!
    if (weighted) {
      preds_all_w <- weighted.mean(preds_all, w = unlist(AUC_weight), na.rm = TRUE)
    } else {
      preds_all_w <- mean(preds_all, na.rm = TRUE)
    }
    
    preds_all_w
  })
  
  # [3] Return the predicted probabilities for each observation to belong 
  #     to class '1' 
  return(probs_class_1_)
}

# -------------------- FOR THE IMPLEMENTATION ----------------------------------
# (1) Generate a DF with reduced dimensions for the implementation
if (FALSE) {
  
  # 1-1 Load an exemplary Test- & Train-Set
  train_test_bwm <- get_train_test(path = './Data/Raw/BLCA.Rda',  # Path to the data
                                   frac_train = 0.75,             # Fraction of data used for Training (rest for test)
                                   split_seed = 12,               # Seed for the split of the data into test- & train
                                   block_seed_train = 13,         # Seed to shuffle the block-order in train
                                   block_seed_test = 11,          # Seed to shuffle the block-order in test
                                   train_pattern = 2,             # Pattern to introduce to train
                                   train_pattern_seed = 12,       # Seed for the introduction of the BWM into train
                                   test_pattern = 2)              # Pattern for the test-set
  
  # 1-2 Reduce the dimensions of the test- & train-data by 90%
  cols_to_keep_ex           <- colnames(train_test_bwm$Train$data)[seq(from = 1, to = 81875, by = 10)]
  cols_to_keep_ex           <- unique(c(colnames(train_test_bwm$Train$data)[1:4], cols_to_keep_ex, 'ytarget'))
  train_test_bwm$Train$data <- train_test_bwm$Train$data[cols_to_keep_ex]
  train_test_bwm$Test$data  <- train_test_bwm$Test$data[cols_to_keep_ex]
  
  # 1-3 Extract the test & train-data from the list
  datatrain <- train_test_bwm$Train$data
  datatest  <- train_test_bwm$Test$data[, colSums(is.na(train_test_bwm$Test$dat)) == 0]
  
  # 1-4 Get the assigned folds per observation
  foldstrain <- train_test_bwm$Train$fold_index
  
  # 1-5 Save the data to '
  save(datatrain, foldstrain, datatest, file="./Data/Example_Data/ExampleData_Package.Rda")
}

# (2) Load the data forthe implementaion
load("./Data/Example_Data/ExampleData_Package.Rda")


# (3) Fit a FW-RF on the train-set
fw_rfs <- multisfor(data          = datatrain, 
                    folds         = foldstrain,
                    num_trees     = 50, 
                    mtry          = 50,
                    min_node_size = 1)

# (4) Get predicted classes & class-probabilites for each obs. in the test-set
predictions <- predict.multisfor(object   = fw_rfs, 
                                 data     = datatest,
                                 weighted = T)
