# Set the working directory to the directory 'bwm_article' 
# of the electronic appendix:

library("this.path")
setwd(gsub("/compstudy_code_and_results/code", "", this.dir()))

# [0] SetWD, load packages, define variables and functions                  ----
# 0-1 Set WD

# Use rejection sampling for random number sampling:
RNGkind(sample.kind = "Rejection") 

# Make table of settings:

# 1-2 Define a list with the paths to the availabe DFs
df_paths <- paste0("./compstudy_code_and_results/data/", list.files("./compstudy_code_and_results/data/"))

# 1-3 Create a list of seeds for each single evaluation-setting
set.seed(1234)
count    <- 1
allseeds <- base::sample(1000:10000000, 
                         size = length(df_paths) * length(c(1, 2, 3, 4, 5)) * 
                           length(c(1, 2, 3, 4)) * length(c(1, 2, 3, 4, 5)))

scenariogrid <- expand.grid(curr_path=df_paths, curr_train_pattern=c(1, 2, 3, 4, 5),
  curr_test_pattern=c(1, 2, 3, 4),
    curr_repetition=c(1, 2, 3, 4, 5), stringsAsFactors = FALSE)
scenariogrid <- expand.grid(curr_repetition=c(1, 2, 3, 4, 5), curr_test_pattern=c(1, 2, 3, 4),
                            curr_train_pattern=c(1, 2, 3, 4, 5), curr_path=df_paths, 
                            stringsAsFactors = FALSE)
scenariogrid <- scenariogrid[,ncol(scenariogrid):1]
scenariogrid$int_seed <- allseeds

set.seed(1234)
reorderind <- sample(1:nrow(scenariogrid))
scenariogrid <- scenariogrid[reorderind,,drop=FALSE]
rownames(scenariogrid) <- NULL


scenariogrid$settingid <- 1:nrow(scenariogrid)
rownames(scenariogrid) <- 1:nrow(scenariogrid)



# Save scenariogrid, needed in evaluation of the results:

save(scenariogrid, file="./compstudy_code_and_results/results/fw_approach/scenariogrid_foldwiserf.Rda")




# Source the functions that are used in performing the calculations 
# on the cluster:

source("./compstudy_code_and_results/code/functions/foldwise_approach_functions.R")
source("./compstudy_code_and_results/code/functions/create_bwm_pattern.R")


# Load 'parallel'-package: 
library("parallel") 

RNGkind("L'Ecuyer-CMRG")
set.seed(1234)

# Perform the calculations and save the results:
Results <- mclapply(1:nrow(scenariogrid), function(z) try({evaluatesetting(z)}), mc.silent=TRUE, mc.cores = 90) 
