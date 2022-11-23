"Script for a quality-check of the evaluation results! 

  > Ensure the settings of the evaluations for the various approaches
    were always the same (same seed, block-order, ...)
  > Ensure the approaches were evaluated correctly & do not miss results
    in certain settings due to an unexpected behavior
"
# [0] SetWD, load packages, define fix variables and functions                ----
# 0-1 Set WD
setwd("/Users/frederik/Desktop/BWM-Article/")             # Mac
setwd("C:/Users/kuche/Desktop/BWM-Paper")                 # Windows
setwd("/dss/dsshome1/lxc0B/ru68kiq3/Project/BWM-Article") # Server

# 0-2 Load packages

# 0-3 Define variables
# 0-3-1 Define paths to the evaluation-results
paths <- c("./Docs/Evaluation_Results/CC_Approach/CC_Eval.csv",
           "./Docs/Evaluation_Results/SB_Approach/SB_Eval.csv",
           "./Docs/Evaluation_Results/BW_Approach/BW_Eval.csv",
           "./Docs/Evaluation_Results/IMP_Approach/IMP_Eval.csv")

# 0-3-2 Columns that must be available in all eval-results
#       (- settings of the evaluation [seeds, block-order in train & test, ...]
#        - resulting metrics of evaluation [F1, Acc, ROC, BrierScore, ...])
nec_cols <- c('path', 'frac_train', 'split_seed', 'block_seed_train', 
              'block_seed_test', 'block_order_train_for_BWM', 
              'block_order_test_for_BWM', 'train_pattern', 'train_pattern_seed', 
              'test_pattern', 'AUC', 'Accuracy', 'Sensitivity', 'Specificity', 
              'Precision', 'Recall', 'F1', 'BrierScore', 'int_seed', 
              'repetition', 'approach')

# 0-4 Define functions
# 0-4-1 Compare two vectors & return unequal elements
check_identical <- function(a, b) {
  "Compare two vectors a & b for equality.
   Return the missmatching elements, else NULL"
  if (!identical(a, b)) {
    print(c(a[which(a != b)], b[which(b != a)]))
  }
}

# [1] Start investigation of the results                                      ----
# 1-1 Load the results from 'paths' into a list
results <- sapply(paths, function(x) read.csv(x))

# 1-2 Assign proper names
names(results) <- sapply(paths, function(x)
  strsplit(x, split = "/")[[1]][length(strsplit(x, split = "/")[[1]]) - 1]
)

# 1-3 Ensure all results have the necessary columns 'nec_cols'
if (!all(sapply(results, function(x) all(nec_cols %in% colnames(x))))) {
  stop("At least one of the DFs in 'paths' misses a column from 'nec_cols'")
} 

# 1-4 Check the evaluation data for unexpected errors 
#     --> an unexpected error leads to results w/o 'block-order'
if (any(sapply(results, function(x) '---' %in% unique(x$block_order_test_for_BWM)))) {
  
  warning("--- UNEXPECTED ERROR --- \n Unexpected Behavior during the Evaluation for the following cases: \n")
  
  # Get those dfs w/ errors
  dfs_w_errors_ <- which(sapply(results, function(x) '---' %in% unique(x$block_order_test_for_BWM)))
  
  # Get the corresponding rows w/ unexpected errors
  error_rows_ <- sapply(dfs_w_errors_, function(x) 
    which(results[[x]]$block_order_train_for_BWM == '---' | results[[x]]$block_order_test_for_BWM == '---')
  )
  
  # Get the settings for those w/ unexpected behavior
  for (curr_df in names(dfs_w_errors_)) {
    print(paste('-----', curr_df, '-----'))
    print(results[[curr_df]][error_rows_[[curr_df]], c('path', 'int_seed', 'repetition', 'approach')])
  }
}

# 1-5 Ensure all DFs have the same settings for their evaluations
# 1-5-1 Get the columns with the settings for the DFs
eval_set_cols <-c('frac_train', 'split_seed', 'block_seed_train', 'int_seed',
                  'block_seed_test', 'block_order_train_for_BWM', 'repetition')
  
# 1-5-2 Add a 'check' column to each results df with the settings for the eval
for (curr_df in names(results)) {
  results[[curr_df]]$check <- sapply(1:nrow(results[[curr_df]]), function(y) 
    paste(results[[curr_df]][y,eval_set_cols], collapse = "- ")
  )
}

# 1-5-3 Ensure 'check' is the same in each DF
#       -> print those settings, that lead to missmatches!
for (curr_df1 in names(results)) {
  for (curr_df2 in names(results)) {
    check_identical(results[[curr_df1]]$check, results[[curr_df2]]$check)
  }
}