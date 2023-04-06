# Set the working directory to the directory 'bwm_article' 
# of the electronic appendix (outcomment the following line
# and replace 'pathtobwm_article' by the path to 'bwm_article'
# on your computer):

setwd("Z:/Projects/SideProjects/BlockwiseMissing/WIREs/FirstRevision/Code/bwm_article")


if (FALSE) {
  
  # In this part, the raw results obtained for the different approaches are read in
  # pre-processed and save in a file "resall" (note that this part can be skipped
  # because the file "resall" is available in directory "bwm_article/compstudy_code_and_results/results"):
  
  
  resfd <- read.csv("./compstudy_code_and_results/results/fd_approach/FD_Eval.csv")
  
  resbw <- read.csv("./compstudy_code_and_results/results/bw_approach/BW_Eval.csv")
  
  rescc <- read.csv("./compstudy_code_and_results/results/cc_approach/CC_Eval.csv")
  
  resimp <- read.csv("./compstudy_code_and_results/results/imp_approach/IMP_Eval.csv")
  
  
  tempfiles <- list.files("./compstudy_code_and_results/results/MDDSPLS_Approach")
  
  tempdatasets <- list()
  for(i in seq(along=tempfiles)) {
    tempdatasets[[i]] <- read.csv(paste0("./compstudy_code_and_results/results/mddspls_approach/", tempfiles[i]))
  }
  
  resmddspls <- do.call("rbind", tempdatasets)
  
  
  
  tempfiles <- list.files("./compstudy_code_and_results/results/PL_Approach")
  
  tempfiles <- setdiff(tempfiles, c("PL_Eval_ESCA_2_4_3.csv", "PL_Eval_ESCA_4_4_3.csv", "PL_Eval_ESCA_old.csv", "PL_Eval_PAAD_4_4_5.csv", "PL_Eval_PAAD_old.csv"))
  
  
  tempdatasets <- list()
  for(i in seq(along=tempfiles)) {
    tempdatasets[[i]] <- read.csv(paste0("./compstudy_code_and_results/results/pl_approach/", tempfiles[i]))
    # Some datasets include the variable "X.1", which only contains the values 1, 2, 3, ... and in
    # therefore not useful. --> Remove this variable:
    if("X.1" %in% names(tempdatasets[[i]]))
      tempdatasets[[i]]$X.1 <- NULL
  }
  
  respl <- do.call("rbind", tempdatasets)
  
  
  
  ressb <- read.csv("./compstudy_code_and_results/results/sb_approach/SB_Eval.csv")
  
  
  
  

  load("./compstudy_code_and_results/results/fw_approach/scenariogrid_foldwiseRF.Rda")
  
  
  tempfiles <- list.files("./compstudy_code_and_results/results/fw_approach")
  tempfiles <- setdiff(tempfiles, c("Results_foldwiseRF.Rda", "scenariogrid_foldwiserf.Rda", "resall.Rda"))
  
  inds <- as.numeric(gsub(".Rda", "", gsub("FW_Eval_", "", tempfiles)))
  reorderind <- order(inds)
  inds <- inds[reorderind]
  tempfiles <- tempfiles[reorderind]
  
  scenariogrid$done <- FALSE
  scenariogrid$done[inds] <- TRUE
  
  Results <- list()
  for(i in seq(along=tempfiles)) {
    load(paste0("./compstudy_code_and_results/results/fw_approach/", tempfiles[i]))
    curr_res$AUC <- ifelse(curr_res$AUC=="---", NA, as.numeric(curr_res$AUC)) 
    curr_res$Accuracy <- ifelse(curr_res$Accuracy=="---", NA, as.numeric(curr_res$Accuracy)) 
    curr_res$Sensitivity <- ifelse(curr_res$Sensitivity=="---", NA, as.numeric(curr_res$Sensitivity)) 
    curr_res$Specificity <- ifelse(curr_res$Specificity=="---", NA, as.numeric(curr_res$Specificity)) 
    curr_res$Precision <- ifelse(curr_res$Precision=="---", NA, as.numeric(curr_res$Precision)) 
    curr_res$Recall <- ifelse(curr_res$Recall=="---", NA, as.numeric(curr_res$Recall)) 
    curr_res$F1 <- ifelse(curr_res$F1=="---", NA, as.numeric(curr_res$F1)) 
    curr_res$BrierScore <- ifelse(curr_res$BrierScore=="---", NA, as.numeric(curr_res$BrierScore)) 
    Results[[i]] <- curr_res
  }
  

  
  resfw <- do.call("rbind", Results)
  
  resfw$path <- as.character(resfw$path)
  resfw$block_order_train_for_BWM <- as.character(resfw$block_order_train_for_BWM)
  resfw$block_order_test_for_BWM <- as.character(resfw$block_order_test_for_BWM)
  
  resfw$split_seed <- as.integer(resfw$split_seed) 
  resfw$block_seed_train <- as.integer(resfw$block_seed_train) 
  resfw$block_seed_test <- as.integer(resfw$block_seed_test) 
  resfw$train_pattern <- as.integer(resfw$train_pattern) 
  resfw$train_pattern_seed <- as.integer(resfw$train_pattern_seed) 
  resfw$test_pattern <- as.integer(resfw$test_pattern) 
  resfw$repetition <- as.integer(resfw$repetition)
  
  
  
  temp1 <- paste(resfw$path, resfw$train_pattern, resfw$test_pattern, resfw$repetition, sep="_")
  temp2 <- paste(rescc$path, rescc$train_pattern, rescc$test_pattern, rescc$repetition, sep="_")
  
  datatemp <- rescc[!(temp2 %in% temp1),]
  datatemp$AUC <- NA 
  datatemp$Accuracy <- NA 
  datatemp$Sensitivity <- NA 
  datatemp$Specificity <- NA 
  datatemp$Precision <- NA 
  datatemp$Recall <- NA 
  datatemp$F1 <- NA 
  datatemp$BrierScore <- NA 
  
  datatemp$AUC <- as.numeric(datatemp$AUC) 
  datatemp$Accuracy <- as.numeric(datatemp$Accuracy) 
  datatemp$Sensitivity <- as.numeric(datatemp$Sensitivity) 
  datatemp$Specificity <- as.numeric(datatemp$Specificity) 
  datatemp$Precision <- as.numeric(datatemp$Precision) 
  datatemp$Recall <- as.numeric(datatemp$Recall) 
  datatemp$F1 <- as.numeric(datatemp$F1) 
  datatemp$BrierScore <- as.numeric(datatemp$BrierScore)
  
  datatemp <- datatemp[,names(datatemp) %in% names(resfw)]
  
  datatemp$approach <- "Foldwise"
  
  
  resfwsafe <- resfw
  resfw <- rbind(resfwsafe, datatemp)
  
  resfw$doneFW <- c(rep(TRUE, nrow(resfwsafe)), rep(FALSE, nrow(datatemp)))
  
  
  
  
  # For two data.frames some variables, which should be metric, are characters
  # because they contain the entries "---" instead of NA.
  # --> Convert these into numeric variables:
  
  rescc$Accuracy <- as.numeric(rescc$Accuracy)
  rescc$AUC <- as.numeric(rescc$AUC)
  rescc$BrierScore <- as.numeric(rescc$BrierScore)
  rescc$F1 <- as.numeric(rescc$F1)
  rescc$Precision <- as.numeric(rescc$Precision)
  rescc$Recall <- as.numeric(rescc$Recall)
  rescc$Sensitivity <- as.numeric(rescc$Sensitivity)
  rescc$Specificity <- as.numeric(rescc$Specificity)
  
  respl$Accuracy <- as.numeric(respl$Accuracy)
  respl$AUC <- as.numeric(respl$AUC)
  respl$BrierScore <- as.numeric(respl$BrierScore)
  respl$F1 <- as.numeric(respl$F1)
  respl$Precision <- as.numeric(respl$Precision)
  respl$Recall <- as.numeric(respl$Recall)
  respl$Sensitivity <- as.numeric(respl$Sensitivity)
  respl$Specificity <- as.numeric(respl$Specificity)
  
  rescc$ntree <- as.numeric(rescc$ntree)
  rescc$mtry <- as.numeric(rescc$mtry)
  rescc$min_node_size <- as.numeric(rescc$min_node_size)
  
  datlist <- list(resbw, rescc, resimp, resmddspls, respl, ressb, resfw, resfd)
  names(datlist) <- c("bw", "cc", "imp", "mddspls", "pl", "sb", "fw", "fd")
  
  
  
  
  library("dplyr")
  
  resall <- bind_rows(datlist)
  
  
  resall$method <- factor(rep(c("BlwRF", "ComplcRF", "ImpRF", "MddsPLS", "PrLasso", "SingleBlRF", "MultisRF", "FulldataRF"),
                              times=sapply(datlist, nrow)), levels=c("ComplcRF", "SingleBlRF", "BlwRF", "MultisRF", "ImpRF", "MddsPLS", "PrLasso", "FulldataRF"))
  
  
  
  delnames <- c("X", "frac_train", "split_seed", "block_seed_train", "block_seed_test",
                "train_pattern_seed", "int_seed", "approach")
  resall <- resall[,!(names(resall) %in% delnames)]
  
  names(resall)[names(resall)=="path"] <- "dataset"

  resall$dataset <- gsub(".Rda", "", gsub("/BWM-Article/Data/Raw/", "", resall$dataset))
  resall$dataset <- gsub(".Rda", "", gsub("./Data/Raw/", "", resall$dataset))
  resall$dataset <- gsub(".Rda", "", gsub("./compstudy_code_and_results/data/", "", resall$dataset))
  
  
  save(resall, file="./compstudy_code_and_results/results/resall.Rda")
  
}



# Load the processed results:

load("./compstudy_code_and_results/results/resall.Rda")




# Check missingness patterns in the datasets:
#############################################


tapply(resall$AUC, resall$method, function(x) sum(is.na(x)))



restemp <- resall[resall$method=="MultisRF",]

restemp2 <- restemp[restemp$doneFW,]

combs <- paste(restemp2$train_pattern, restemp2$test_pattern, sep="_")
nnas <- tapply(restemp2$AUC, combs, function(x) sum(is.na(x)))
nnnas <- tapply(restemp2$AUC, combs, function(x) sum(!is.na(x)))

sort(nnas)

# --> If there are no missing values in the training data, the
#     method does not output a prediction. That is why for the training set
#     missingness pattern (trbmp) 1 no results where obtained.
#     The reason why for test set missingness pattern (tebmp) 1 very
#     few results were obtained is the following: In tebmp 1 only the
#     clinical data is available in the test data. Starting with the
#     first splits, Foldwise-RFs prune all trees removing all splits
#     for which variables are used that do not occur in the test set.
#     Because the clinical covariates are so few in comparison to
#     the omics features it is very unlikely that the first splits
#     use clinical covariates. As a consequence, if the test set
#     contains only clinical covariates the likelihood is high that
#     all trees are removed from all forests. And if all trees were
#     removed no predictions are performed. This explains the large
#     frequencies of NAs for tebmp 1 in the case of the Foldwise-RF.
#     There are also some cases where there are no results for
#     tebmp 2. Here, probably the same mechanism as described as above
#     was at work, which lead to the removal of all trees.


restemp1 <- restemp[!restemp$doneFW,]

combs <- paste(restemp1$train_pattern, restemp1$test_pattern, sep="_")
nnas <- tapply(restemp1$AUC, combs, function(x) sum(is.na(x)))
nnnas <- tapply(restemp1$AUC, combs, function(x) sum(!is.na(x)))

sort(nnas)

# --> Results did not finish only for training pattern 5 (the one with
# the many folds).



# Complete Case Approach:

restemp <- resall[resall$method=="ComplcRF",]

combs <- paste(restemp$train_pattern, restemp$test_pattern, sep="_")
nnas <- tapply(restemp$AUC, combs, function(x) sum(is.na(x)))
nnnas <- tapply(restemp$AUC, combs, function(x) sum(!is.na(x)))

sort(nnas)

# --> If the validation data set does not contain missing
# values, none of the variables in the training data patterns
# are removed, which is why in that case there are no complete
# cases in the training data sets and the complete case approach
# is not applicable.
# For the combination training set pattern 4 and test set
# pattern 3, there are also no complete cases possible in
# the test data.
# For other training and test set pattern combinations it depends
# on the arrangement of the blocks in the training and test
# set patterns whether or not there are complete cases possible
# in the training data and consequently whether or not the 
# complete case approach is applicable.


# Priority Lasso:

restemp <- resall[resall$method=="PrLasso",]

combs <- paste(restemp$train_pattern, restemp$test_pattern, restemp$dataset, sep="_")
nnas <- tapply(restemp$AUC, combs, function(x) sum(is.na(x)))
nnnas <- tapply(restemp$AUC, combs, function(x) sum(!is.na(x)))

sort(nnas)

# --> With one exceptions, the errors all occurred for the
# dataset ESCA. There were 10 errors in total.



table(is.na(resall$AUC), is.na(resall$Accuracy))
table(is.na(resall$AUC), is.na(resall$BrierScore))


round(tapply(resall$AUC, resall$method, function(x) mean(is.na(x)))*100, 1)


# Verify that Compl-RF and Foldwise-RF delivered no results in different cases:

resallwide <- reshape(resall[,c("train_pattern", "test_pattern", "repetition",
                                "dataset", "method", "AUC")], 
                      idvar = c("train_pattern", "test_pattern", "repetition",
                                                                        "dataset"), timevar = "method", direction = "wide")

table(is.na(resallwide$AUC.ComplcRF), is.na(resallwide$AUC.MultisRF))

# --> Yes, with one exception.







################################################################################
# Global comparison without distinguishing between different trbmp and tebmp vales
################################################################################

resallsafe <- resall
resall <- resall[resall$method!="FulldataRF",]

# Perform the analysis under the exclusion of all repetitions for which at least
# one method resulted in an error:

# Remove these cases:

tempvar <- paste(resall$train_pattern, resall$test_pattern, resall$repetition, resall$dataset, sep="_")

tempvarun <- unique(tempvar)
good <- sapply(tempvarun, function(x) all(!is.na(resall$AUC[tempvar==x])))

restemp <- resall[tempvar %in% tempvarun[good],]



# Boxplots of the raw values:

library("plyr")
resallsum <- ddply(restemp, .variables=c("train_pattern", "test_pattern", "method", "dataset"), .fun=summarise, AUC=mean(AUC, na.rm=TRUE),
                   AUC=mean(AUC, na.rm=TRUE),
                   Accuracy=mean(Accuracy, na.rm=TRUE),
                   Sensitivity=mean(Sensitivity, na.rm=TRUE),
                   Specificity=mean(Specificity, na.rm=TRUE),
                   Precision=mean(Precision, na.rm=TRUE),
                   Recall=mean(Recall, na.rm=TRUE),
                   F1=mean(F1, na.rm=TRUE),
                   BrierScore=mean(BrierScore, na.rm=TRUE))

names(resallsum)

sort(tapply(resallsum$AUC, resallsum$method, mean))
sort(tapply(resallsum$Accuracy, resallsum$method, mean))
sort(tapply(resallsum$BrierScore, resallsum$method, mean), decreasing = TRUE)

plot(sort(tapply(resallsum$AUC, resallsum$method, mean)))
plot(sort(tapply(resallsum$Accuracy, resallsum$method, mean)))
plot(sort(tapply(resallsum$BrierScore, resallsum$method, mean), decreasing = TRUE))


ggdata <- reshape(resallsum, varying=c("AUC", "Accuracy", "BrierScore"), 
                  v.names="value", 
                  timevar="measure", times=c("AUC", "ACC", "Brier"),
                  direction="long")
ggdata$measure <- factor(ggdata$measure, levels=c("Brier", "AUC", "ACC"))


length(tempvarun)

ggdata$helpvar <- 0
tempvar <- paste(ggdata$train_pattern, ggdata$test_pattern, ggdata$dataset, sep="_")
tempvarun <- unique(tempvar)
colorvals <- runif(length(tempvarun))
for(i in seq(along=tempvarun))
  ggdata$helpvar[tempvar==tempvarun[i]] <- colorvals[i]


set.seed(1234)
ggdatasub <- ggdata[tempvar %in% sample(tempvarun, size=50),]


library("ggplot2")
p <- ggplot(data=ggdata, aes(x=method, y=value)) + theme_bw() + facet_wrap(~measure, nrow=3, scales="free_y") +
  geom_line(data=ggdatasub, aes(group=interaction(train_pattern, test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_global_values.pdf", width=7, height=10)






# Boxplots of the ranks:



resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                "dataset", "method", "AUC")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                        "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("AUC", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("AUC", names(resallwide))] <- resranks

resalllongAUC <- reshape(resallwide, varying=colnames(resranks), 
                  v.names="rank", 
                  timevar="method", times=gsub("AUC.", "", colnames(resranks)),
                  direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "Accuracy")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                         "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("Accuracy", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("Accuracy", names(resallwide))] <- resranks

resalllongAccuracy <- reshape(resallwide, varying=colnames(resranks), 
                         v.names="rank", 
                         timevar="method", times=gsub("Accuracy.", "", colnames(resranks)),
                         direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "BrierScore")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                              "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("BrierScore", names(resallwide))], 1, function(x) rank(x)))

resallwide[,grep("BrierScore", names(resallwide))] <- resranks

resalllongBrierScore <- reshape(resallwide, varying=colnames(resranks), 
                              v.names="rank", 
                              timevar="method", times=gsub("BrierScore.", "", colnames(resranks)),
                              direction="long")



resalllong <- rbind(resalllongAccuracy, resalllongAUC, resalllongBrierScore)
resalllong$measure <- rep(c("ACC", "AUC", "Brier"), times=c(nrow(resalllongAccuracy), 
                                                            nrow(resalllongAUC), 
                                                            nrow(resalllongBrierScore)))
resalllong$measure <- factor(resalllong$measure, levels=c("Brier", "AUC", "ACC"))

library("plyr")
resalllongsum <- ddply(resalllong, .variables=c("train_pattern", "test_pattern", "method", "dataset", "measure"), 
                       .fun=summarise, rank=mean(rank, na.rm=TRUE))

resalllongsum$method <- factor(resalllongsum$method, levels=c("ComplcRF", "SingleBlRF", "BlwRF", "MultisRF", "ImpRF", "MddsPLS", "PrLasso"))

library("ggplot2")
p <- ggplot(data=resalllongsum, aes(x=method, y=rank)) + theme_bw() + facet_wrap(~measure, nrow=3, scales="free_y") +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/Figure_3.eps", width=7, height=10)


sort(tapply(resalllongsum$rank[resalllongsum$measure=="Brier"], resalllongsum$method[resalllongsum$measure=="Brier"], mean))
plot(sort(tapply(resalllongsum$rank[resalllongsum$measure=="Brier"], resalllongsum$method[resalllongsum$measure=="Brier"], mean)))

sort(tapply(resalllongsum$rank[resalllongsum$measure=="AUC"], resalllongsum$method[resalllongsum$measure=="AUC"], mean))
plot(sort(tapply(resalllongsum$rank[resalllongsum$measure=="AUC"], resalllongsum$method[resalllongsum$measure=="AUC"], mean)))

sort(tapply(resalllongsum$rank[resalllongsum$measure=="ACC"], resalllongsum$method[resalllongsum$measure=="ACC"], mean))
plot(sort(tapply(resalllongsum$rank[resalllongsum$measure=="ACC"], resalllongsum$method[resalllongsum$measure=="ACC"], mean)))






# Perform the above analysis under the exclusion of ComplcRF (because ComplcRF
# did not deliver results in certain settings):


restemp <- resall[resall$method!="ComplcRF",]

tempvar <- paste(restemp$train_pattern, restemp$test_pattern, restemp$repetition, restemp$dataset, sep="_")

tempvarun <- unique(tempvar)
good <- sapply(tempvarun, function(x) all(!is.na(restemp$AUC[tempvar==x])))

restemp <- restemp[tempvar %in% tempvarun[good],]



# Boxplots of the raw values:

library("plyr")
resallsum <- ddply(restemp, .variables=c("train_pattern", "test_pattern", "method", "dataset"), .fun=summarise, AUC=mean(AUC, na.rm=TRUE),
                   AUC=mean(AUC, na.rm=TRUE),
                   Accuracy=mean(Accuracy, na.rm=TRUE),
                   Sensitivity=mean(Sensitivity, na.rm=TRUE),
                   Specificity=mean(Specificity, na.rm=TRUE),
                   Precision=mean(Precision, na.rm=TRUE),
                   Recall=mean(Recall, na.rm=TRUE),
                   F1=mean(F1, na.rm=TRUE),
                   BrierScore=mean(BrierScore, na.rm=TRUE))

names(resallsum)

sort(tapply(resallsum$AUC, resallsum$method, mean))
sort(tapply(resallsum$Accuracy, resallsum$method, mean))
sort(tapply(resallsum$BrierScore, resallsum$method, mean), decreasing = TRUE)

plot(sort(tapply(resallsum$AUC, resallsum$method, mean)))
plot(sort(tapply(resallsum$Accuracy, resallsum$method, mean)))
plot(sort(tapply(resallsum$BrierScore, resallsum$method, mean), decreasing = TRUE))


ggdata <- reshape(resallsum, varying=c("AUC", "Accuracy", "BrierScore"), 
                  v.names="value", 
                  timevar="measure", times=c("AUC", "ACC", "Brier"),
                  direction="long")
ggdata$measure <- factor(ggdata$measure, levels=c("Brier", "AUC", "ACC"))

ggdata$helpvar <- 0
tempvar <- paste(ggdata$train_pattern, ggdata$test_pattern, ggdata$dataset, sep="_")
tempvarun <- unique(tempvar)
colorvals <- runif(length(tempvarun))
for(i in seq(along=tempvarun))
  ggdata$helpvar[tempvar==tempvarun[i]] <- colorvals[i]


set.seed(1234)
ggdatasub <- ggdata[tempvar %in% sample(tempvarun, size=50),]


library("ggplot2")
p <- ggplot(data=ggdata, aes(x=method, y=value)) + theme_bw() + facet_wrap(~measure, nrow=3, scales="free_y") +
  geom_line(data=ggdatasub, aes(group=interaction(train_pattern, test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_global_NoComplcRF_values.pdf", width=7, height=10)






# Boxplots of the ranks:



resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "AUC")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                         "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("AUC", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("AUC", names(resallwide))] <- resranks

resalllongAUC <- reshape(resallwide, varying=colnames(resranks), 
                         v.names="rank", 
                         timevar="method", times=gsub("AUC.", "", colnames(resranks)),
                         direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "Accuracy")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                              "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("Accuracy", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("Accuracy", names(resallwide))] <- resranks

resalllongAccuracy <- reshape(resallwide, varying=colnames(resranks), 
                              v.names="rank", 
                              timevar="method", times=gsub("Accuracy.", "", colnames(resranks)),
                              direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "BrierScore")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                                "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("BrierScore", names(resallwide))], 1, function(x) rank(x)))

resallwide[,grep("BrierScore", names(resallwide))] <- resranks

resalllongBrierScore <- reshape(resallwide, varying=colnames(resranks), 
                                v.names="rank", 
                                timevar="method", times=gsub("BrierScore.", "", colnames(resranks)),
                                direction="long")



resalllong <- rbind(resalllongAccuracy, resalllongAUC, resalllongBrierScore)
resalllong$measure <- rep(c("ACC", "AUC", "Brier"), times=c(nrow(resalllongAccuracy), 
                                                            nrow(resalllongAUC), 
                                                            nrow(resalllongBrierScore)))

library("plyr")
resalllongsum <- ddply(resalllong, .variables=c("train_pattern", "test_pattern", "method", "dataset", "measure"), 
                       .fun=summarise, rank=mean(rank, na.rm=TRUE))
resalllongsum$measure <- factor(resalllongsum$measure, levels=c("Brier", "AUC", "ACC"))

resalllongsum$method <- factor(resalllongsum$method, levels=c("ComplcRF", "SingleBlRF", "BlwRF", "MultisRF", "ImpRF", "MddsPLS", "PrLasso"))

library("ggplot2")
p <- ggplot(data=resalllongsum, aes(x=method, y=rank)) + theme_bw() + facet_wrap(~measure, nrow=3, scales="free_y") +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_global_NoComplcRF_ranks.pdf", width=7, height=10)








# Perform the above analysis under the exclusion of MultisRF (because MultisRF
# did not deliver results in certain settings):


restemp <- resall[resall$method!="MultisRF",]

tempvar <- paste(restemp$train_pattern, restemp$test_pattern, restemp$repetition, restemp$dataset, sep="_")

tempvarun <- unique(tempvar)
good <- sapply(tempvarun, function(x) all(!is.na(restemp$AUC[tempvar==x])))

restemp <- restemp[tempvar %in% tempvarun[good],]



# Boxplots of the raw values:

library("plyr")
resallsum <- ddply(restemp, .variables=c("train_pattern", "test_pattern", "method", "dataset"), .fun=summarise, AUC=mean(AUC, na.rm=TRUE),
                   AUC=mean(AUC, na.rm=TRUE),
                   Accuracy=mean(Accuracy, na.rm=TRUE),
                   Sensitivity=mean(Sensitivity, na.rm=TRUE),
                   Specificity=mean(Specificity, na.rm=TRUE),
                   Precision=mean(Precision, na.rm=TRUE),
                   Recall=mean(Recall, na.rm=TRUE),
                   F1=mean(F1, na.rm=TRUE),
                   BrierScore=mean(BrierScore, na.rm=TRUE))

names(resallsum)

sort(tapply(resallsum$AUC, resallsum$method, mean))
sort(tapply(resallsum$Accuracy, resallsum$method, mean))
sort(tapply(resallsum$BrierScore, resallsum$method, mean), decreasing = TRUE)

plot(sort(tapply(resallsum$AUC, resallsum$method, mean)))
plot(sort(tapply(resallsum$Accuracy, resallsum$method, mean)))
plot(sort(tapply(resallsum$BrierScore, resallsum$method, mean), decreasing = TRUE))


ggdata <- reshape(resallsum, varying=c("AUC", "Accuracy", "BrierScore"), 
                  v.names="value", 
                  timevar="measure", times=c("AUC", "ACC", "Brier"),
                  direction="long")
ggdata$measure <- factor(ggdata$measure, levels=c("Brier", "AUC", "ACC"))

ggdata$helpvar <- 0
tempvar <- paste(ggdata$train_pattern, ggdata$test_pattern, ggdata$dataset, sep="_")
tempvarun <- unique(tempvar)
colorvals <- runif(length(tempvarun))
for(i in seq(along=tempvarun))
  ggdata$helpvar[tempvar==tempvarun[i]] <- colorvals[i]


set.seed(1234)
ggdatasub <- ggdata[tempvar %in% sample(tempvarun, size=50),]


library("ggplot2")
p <- ggplot(data=ggdata, aes(x=method, y=value)) + theme_bw() + facet_wrap(~measure, nrow=3, scales="free_y") +
  geom_line(data=ggdatasub, aes(group=interaction(train_pattern, test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_global_NoMultisRF_values.pdf", width=7, height=10)






# Boxplots of the ranks:



resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "AUC")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                         "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("AUC", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("AUC", names(resallwide))] <- resranks

resalllongAUC <- reshape(resallwide, varying=colnames(resranks), 
                         v.names="rank", 
                         timevar="method", times=gsub("AUC.", "", colnames(resranks)),
                         direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "Accuracy")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                              "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("Accuracy", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("Accuracy", names(resallwide))] <- resranks

resalllongAccuracy <- reshape(resallwide, varying=colnames(resranks), 
                              v.names="rank", 
                              timevar="method", times=gsub("Accuracy.", "", colnames(resranks)),
                              direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "BrierScore")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                                "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("BrierScore", names(resallwide))], 1, function(x) rank(x)))

resallwide[,grep("BrierScore", names(resallwide))] <- resranks

resalllongBrierScore <- reshape(resallwide, varying=colnames(resranks), 
                                v.names="rank", 
                                timevar="method", times=gsub("BrierScore.", "", colnames(resranks)),
                                direction="long")



resalllong <- rbind(resalllongAccuracy, resalllongAUC, resalllongBrierScore)
resalllong$measure <- rep(c("ACC", "AUC", "Brier"), times=c(nrow(resalllongAccuracy), 
                                                            nrow(resalllongAUC), 
                                                            nrow(resalllongBrierScore)))

library("plyr")
resalllongsum <- ddply(resalllong, .variables=c("train_pattern", "test_pattern", "method", "dataset", "measure"), 
                       .fun=summarise, rank=mean(rank, na.rm=TRUE))
resalllongsum$measure <- factor(resalllongsum$measure, levels=c("Brier", "AUC", "ACC"))

resalllongsum$method <- factor(resalllongsum$method, levels=c("ComplcRF", "SingleBlRF", "BlwRF", "MultisRF", "ImpRF", "MddsPLS", "PrLasso"))

library("ggplot2")
p <- ggplot(data=resalllongsum, aes(x=method, y=rank)) + theme_bw() + facet_wrap(~measure, nrow=3, scales="free_y") +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_global_NoMultisRF_ranks.pdf", width=7, height=10)












#######################################################
# Comparison of the methods separated by trbmp pattern
#######################################################


# Perform the analysis under the exclusion of all repetitions for which at least
# one method resulted in an error:

# Remove these cases:

restemp <- resall

tempvar <- paste(restemp$train_pattern, restemp$test_pattern, restemp$repetition, restemp$dataset, sep="_")

tempvarun <- unique(tempvar)
good <- sapply(tempvarun, function(x) all(!is.na(restemp$AUC[tempvar==x])))

restemp <- restemp[tempvar %in% tempvarun[good],]



# Boxplots of the raw values:

library("plyr")
resallsum <- ddply(restemp, .variables=c("train_pattern", "test_pattern", "method", "dataset"), .fun=summarise, AUC=mean(AUC, na.rm=TRUE),
                   AUC=mean(AUC, na.rm=TRUE),
                   Accuracy=mean(Accuracy, na.rm=TRUE),
                   Sensitivity=mean(Sensitivity, na.rm=TRUE),
                   Specificity=mean(Specificity, na.rm=TRUE),
                   Precision=mean(Precision, na.rm=TRUE),
                   Recall=mean(Recall, na.rm=TRUE),
                   F1=mean(F1, na.rm=TRUE),
                   BrierScore=mean(BrierScore, na.rm=TRUE))

ggdata <- reshape(resallsum, varying=c("AUC", "Accuracy", "BrierScore"), 
                  v.names="value", 
                  timevar="measure", times=c("AUC", "ACC", "Brier"),
                  direction="long")

library("ggplot2")


ggdata$train_pattern <- paste0("Trbmp: ", ggdata$train_pattern)

ggdata$helpvar <- 0
tempvar <- paste(ggdata$test_pattern, ggdata$dataset, sep="_")
tempvarun <- unique(tempvar)
colorvals <- runif(length(tempvarun))
for(i in seq(along=tempvarun))
  ggdata$helpvar[tempvar==tempvarun[i]] <- colorvals[i]


p <- ggplot(data=ggdata[ggdata$measure=="Brier",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_line(aes(group=interaction(test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_Brier_values.pdf", width=10, height=7)


p <- ggplot(data=ggdata[ggdata$measure=="ACC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_line(aes(group=interaction(test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_ACC_values.pdf", width=10, height=7)

p <- ggplot(data=ggdata[ggdata$measure=="AUC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_line(aes(group=interaction(test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_AUC_values.pdf", width=10, height=7)






# Boxplots of the ranks:



resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "AUC")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                         "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("AUC", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("AUC", names(resallwide))] <- resranks

resalllongAUC <- reshape(resallwide, varying=colnames(resranks), 
                         v.names="rank", 
                         timevar="method", times=gsub("AUC.", "", colnames(resranks)),
                         direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "Accuracy")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                              "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("Accuracy", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("Accuracy", names(resallwide))] <- resranks

resalllongAccuracy <- reshape(resallwide, varying=colnames(resranks), 
                              v.names="rank", 
                              timevar="method", times=gsub("Accuracy.", "", colnames(resranks)),
                              direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "BrierScore")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                                "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("BrierScore", names(resallwide))], 1, function(x) rank(x)))

resallwide[,grep("BrierScore", names(resallwide))] <- resranks

resalllongBrierScore <- reshape(resallwide, varying=colnames(resranks), 
                                v.names="rank", 
                                timevar="method", times=gsub("BrierScore.", "", colnames(resranks)),
                                direction="long")



resalllong <- rbind(resalllongAccuracy, resalllongAUC, resalllongBrierScore)
resalllong$measure <- rep(c("ACC", "AUC", "Brier"), times=c(nrow(resalllongAccuracy), 
                                                            nrow(resalllongAUC), 
                                                            nrow(resalllongBrierScore)))

library("plyr")
resalllongsum <- ddply(resalllong, .variables=c("train_pattern", "test_pattern", "method", "dataset", "measure"), 
                       .fun=summarise, rank=mean(rank, na.rm=TRUE))



resalllongsum$train_pattern <- paste0("Trbmp: ", resalllongsum$train_pattern)

resalllongsum$method <- factor(resalllongsum$method, levels=c("ComplcRF", "SingleBlRF", "BlwRF", "MultisRF", "ImpRF", "MddsPLS", "PrLasso"))



library("ggplot2")

p <- ggplot(data=resalllongsum[resalllongsum$measure=="Brier",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/Figure_4.eps", width=10, height=7)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="ACC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_ACC_ranks.pdf", width=10, height=7)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="AUC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_AUC_ranks.pdf", width=10, height=7)










# Perform the above analysis under the exclusion of ComplcRF (because ComplcRF
# did not deliver results in certain settings):


restemp <- resall[resall$method!="ComplcRF",]

tempvar <- paste(restemp$train_pattern, restemp$test_pattern, restemp$repetition, restemp$dataset, sep="_")

tempvarun <- unique(tempvar)
good <- sapply(tempvarun, function(x) all(!is.na(restemp$AUC[tempvar==x])))

restemp <- restemp[tempvar %in% tempvarun[good],]



# Boxplots of the raw values:

library("plyr")
resallsum <- ddply(restemp, .variables=c("train_pattern", "test_pattern", "method", "dataset"), .fun=summarise, AUC=mean(AUC, na.rm=TRUE),
                   AUC=mean(AUC, na.rm=TRUE),
                   Accuracy=mean(Accuracy, na.rm=TRUE),
                   Sensitivity=mean(Sensitivity, na.rm=TRUE),
                   Specificity=mean(Specificity, na.rm=TRUE),
                   Precision=mean(Precision, na.rm=TRUE),
                   Recall=mean(Recall, na.rm=TRUE),
                   F1=mean(F1, na.rm=TRUE),
                   BrierScore=mean(BrierScore, na.rm=TRUE))

ggdata <- reshape(resallsum, varying=c("AUC", "Accuracy", "BrierScore"), 
                  v.names="value", 
                  timevar="measure", times=c("AUC", "ACC", "Brier"),
                  direction="long")

library("ggplot2")


ggdata$train_pattern <- paste0("Trbmp: ", ggdata$train_pattern)

ggdata$helpvar <- 0
tempvar <- paste(ggdata$test_pattern, ggdata$dataset, sep="_")
tempvarun <- unique(tempvar)
colorvals <- runif(length(tempvarun))
for(i in seq(along=tempvarun))
  ggdata$helpvar[tempvar==tempvarun[i]] <- colorvals[i]


p <- ggplot(data=ggdata[ggdata$measure=="Brier",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_line(aes(group=interaction(test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_NoComplcRF_Brier_values.pdf", width=10, height=7)


p <- ggplot(data=ggdata[ggdata$measure=="ACC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_line(aes(group=interaction(test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_NoComplcRF_ACC_values.pdf", width=10, height=7)

p <- ggplot(data=ggdata[ggdata$measure=="AUC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_line(aes(group=interaction(test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_NoComplcRF_AUC_values.pdf", width=10, height=7)





# Boxplots of the ranks:



resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "AUC")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                         "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("AUC", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("AUC", names(resallwide))] <- resranks

resalllongAUC <- reshape(resallwide, varying=colnames(resranks), 
                         v.names="rank", 
                         timevar="method", times=gsub("AUC.", "", colnames(resranks)),
                         direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "Accuracy")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                              "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("Accuracy", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("Accuracy", names(resallwide))] <- resranks

resalllongAccuracy <- reshape(resallwide, varying=colnames(resranks), 
                              v.names="rank", 
                              timevar="method", times=gsub("Accuracy.", "", colnames(resranks)),
                              direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "BrierScore")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                                "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("BrierScore", names(resallwide))], 1, function(x) rank(x)))

resallwide[,grep("BrierScore", names(resallwide))] <- resranks

resalllongBrierScore <- reshape(resallwide, varying=colnames(resranks), 
                                v.names="rank", 
                                timevar="method", times=gsub("BrierScore.", "", colnames(resranks)),
                                direction="long")



resalllong <- rbind(resalllongAccuracy, resalllongAUC, resalllongBrierScore)
resalllong$measure <- rep(c("ACC", "AUC", "Brier"), times=c(nrow(resalllongAccuracy), 
                                                            nrow(resalllongAUC), 
                                                            nrow(resalllongBrierScore)))

library("plyr")
resalllongsum <- ddply(resalllong, .variables=c("train_pattern", "test_pattern", "method", "dataset", "measure"), 
                       .fun=summarise, rank=mean(rank, na.rm=TRUE))



resalllongsum$train_pattern <- paste0("Trbmp: ", resalllongsum$train_pattern)

resalllongsum$method <- factor(resalllongsum$method, levels=c("ComplcRF", "SingleBlRF", "BlwRF", "MultisRF", "ImpRF", "MddsPLS", "PrLasso"))



ui <- resalllongsum[resalllongsum$measure=="AUC",]

sapply(unique(ui$train_pattern), function(x) sum(ui$method=="BlwRF" & ui$train_pattern==x))


library("ggplot2")

p <- ggplot(data=resalllongsum[resalllongsum$measure=="Brier",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_NoComplcRF_Brier_ranks.pdf", width=10, height=7)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="ACC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_NoComplcRF_ACC_ranks.pdf", width=10, height=7)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="AUC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_NoComplcRF_AUC_ranks.pdf", width=10, height=7)









# Perform the above analysis under the exclusion of MultisRF (because MultisRF
# did not deliver results in certain settings):


restemp <- resall[resall$method!="MultisRF",]

tempvar <- paste(restemp$train_pattern, restemp$test_pattern, restemp$repetition, restemp$dataset, sep="_")

tempvarun <- unique(tempvar)
good <- sapply(tempvarun, function(x) all(!is.na(restemp$AUC[tempvar==x])))

restemp <- restemp[tempvar %in% tempvarun[good],]



# Boxplots of the raw values:

library("plyr")
resallsum <- ddply(restemp, .variables=c("train_pattern", "test_pattern", "method", "dataset"), .fun=summarise, AUC=mean(AUC, na.rm=TRUE),
                   AUC=mean(AUC, na.rm=TRUE),
                   Accuracy=mean(Accuracy, na.rm=TRUE),
                   Sensitivity=mean(Sensitivity, na.rm=TRUE),
                   Specificity=mean(Specificity, na.rm=TRUE),
                   Precision=mean(Precision, na.rm=TRUE),
                   Recall=mean(Recall, na.rm=TRUE),
                   F1=mean(F1, na.rm=TRUE),
                   BrierScore=mean(BrierScore, na.rm=TRUE))

ggdata <- reshape(resallsum, varying=c("AUC", "Accuracy", "BrierScore"), 
                  v.names="value", 
                  timevar="measure", times=c("AUC", "ACC", "Brier"),
                  direction="long")

library("ggplot2")


ggdata$train_pattern <- paste0("Trbmp: ", ggdata$train_pattern)

ggdata$helpvar <- 0
tempvar <- paste(ggdata$test_pattern, ggdata$dataset, sep="_")
tempvarun <- unique(tempvar)
colorvals <- runif(length(tempvarun))
for(i in seq(along=tempvarun))
  ggdata$helpvar[tempvar==tempvarun[i]] <- colorvals[i]


p <- ggplot(data=ggdata[ggdata$measure=="Brier",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_line(aes(group=interaction(test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_NoMultisRF_Brier_values.pdf", width=10, height=7)


p <- ggplot(data=ggdata[ggdata$measure=="ACC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_line(aes(group=interaction(test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_NoMultisRF_ACC_values.pdf", width=10, height=7)

p <- ggplot(data=ggdata[ggdata$measure=="AUC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_line(aes(group=interaction(test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_NoMultisRF_AUC_values.pdf", width=10, height=7)





# Boxplots of the ranks:



resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "AUC")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                         "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("AUC", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("AUC", names(resallwide))] <- resranks

resalllongAUC <- reshape(resallwide, varying=colnames(resranks), 
                         v.names="rank", 
                         timevar="method", times=gsub("AUC.", "", colnames(resranks)),
                         direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "Accuracy")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                              "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("Accuracy", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("Accuracy", names(resallwide))] <- resranks

resalllongAccuracy <- reshape(resallwide, varying=colnames(resranks), 
                              v.names="rank", 
                              timevar="method", times=gsub("Accuracy.", "", colnames(resranks)),
                              direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "BrierScore")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                                "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("BrierScore", names(resallwide))], 1, function(x) rank(x)))

resallwide[,grep("BrierScore", names(resallwide))] <- resranks

resalllongBrierScore <- reshape(resallwide, varying=colnames(resranks), 
                                v.names="rank", 
                                timevar="method", times=gsub("BrierScore.", "", colnames(resranks)),
                                direction="long")



resalllong <- rbind(resalllongAccuracy, resalllongAUC, resalllongBrierScore)
resalllong$measure <- rep(c("ACC", "AUC", "Brier"), times=c(nrow(resalllongAccuracy), 
                                                            nrow(resalllongAUC), 
                                                            nrow(resalllongBrierScore)))

library("plyr")
resalllongsum <- ddply(resalllong, .variables=c("train_pattern", "test_pattern", "method", "dataset", "measure"), 
                       .fun=summarise, rank=mean(rank, na.rm=TRUE))



resalllongsum$train_pattern <- paste0("Trbmp: ", resalllongsum$train_pattern)

resalllongsum$method <- factor(resalllongsum$method, levels=c("ComplcRF", "SingleBlRF", "BlwRF", "MultisRF", "ImpRF", "MddsPLS", "PrLasso"))

library("ggplot2")

ui <- resalllongsum[resalllongsum$measure=="AUC",]

sapply(unique(ui$train_pattern), function(x) sum(ui$method=="BlwRF" & ui$train_pattern==x))

p <- ggplot(data=resalllongsum[resalllongsum$measure=="Brier",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_NoMultisRF_Brier_ranks.pdf", width=10, height=7)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="ACC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_NoMultisRF_ACC_ranks.pdf", width=10, height=7)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="AUC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_NoMultisRF_AUC_ranks.pdf", width=10, height=7)










#######################################################
# Comparison of the methods separated by tebmp pattern
#######################################################


# Perform the analysis under the exclusion of all repetitions for which at least
# one method resulted in an error:

# Remove these cases:

restemp <- resall

tempvar <- paste(restemp$train_pattern, restemp$test_pattern, restemp$repetition, restemp$dataset, sep="_")

tempvarun <- unique(tempvar)
good <- sapply(tempvarun, function(x) all(!is.na(restemp$AUC[tempvar==x])))

restemp <- restemp[tempvar %in% tempvarun[good],]



# Boxplots of the raw values:

library("plyr")
resallsum <- ddply(restemp, .variables=c("train_pattern", "test_pattern", "method", "dataset"), .fun=summarise, AUC=mean(AUC, na.rm=TRUE),
                   AUC=mean(AUC, na.rm=TRUE),
                   Accuracy=mean(Accuracy, na.rm=TRUE),
                   Sensitivity=mean(Sensitivity, na.rm=TRUE),
                   Specificity=mean(Specificity, na.rm=TRUE),
                   Precision=mean(Precision, na.rm=TRUE),
                   Recall=mean(Recall, na.rm=TRUE),
                   F1=mean(F1, na.rm=TRUE),
                   BrierScore=mean(BrierScore, na.rm=TRUE))

ggdata <- reshape(resallsum, varying=c("AUC", "Accuracy", "BrierScore"), 
                  v.names="value", 
                  timevar="measure", times=c("AUC", "ACC", "Brier"),
                  direction="long")

library("ggplot2")


ggdata$test_pattern <- paste0("Tebmp: ", ggdata$test_pattern)

ggdata$helpvar <- 0
tempvar <- paste(ggdata$train_pattern, ggdata$dataset, sep="_")
tempvarun <- unique(tempvar)
colorvals <- runif(length(tempvarun))
for(i in seq(along=tempvarun))
  ggdata$helpvar[tempvar==tempvarun[i]] <- colorvals[i]


p <- ggplot(data=ggdata[ggdata$measure=="Brier",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_line(aes(group=interaction(train_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_Brier_values.pdf", width=10, height=7)


p <- ggplot(data=ggdata[ggdata$measure=="ACC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_line(aes(group=interaction(train_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_ACC_values.pdf", width=10, height=7)

p <- ggplot(data=ggdata[ggdata$measure=="AUC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_line(aes(group=interaction(train_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_AUC_values.pdf", width=10, height=7)





# Boxplots of the ranks:



resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "AUC")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                         "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("AUC", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("AUC", names(resallwide))] <- resranks

resalllongAUC <- reshape(resallwide, varying=colnames(resranks), 
                         v.names="rank", 
                         timevar="method", times=gsub("AUC.", "", colnames(resranks)),
                         direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "Accuracy")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                              "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("Accuracy", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("Accuracy", names(resallwide))] <- resranks

resalllongAccuracy <- reshape(resallwide, varying=colnames(resranks), 
                              v.names="rank", 
                              timevar="method", times=gsub("Accuracy.", "", colnames(resranks)),
                              direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "BrierScore")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                                "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("BrierScore", names(resallwide))], 1, function(x) rank(x)))

resallwide[,grep("BrierScore", names(resallwide))] <- resranks

resalllongBrierScore <- reshape(resallwide, varying=colnames(resranks), 
                                v.names="rank", 
                                timevar="method", times=gsub("BrierScore.", "", colnames(resranks)),
                                direction="long")



resalllong <- rbind(resalllongAccuracy, resalllongAUC, resalllongBrierScore)
resalllong$measure <- rep(c("ACC", "AUC", "Brier"), times=c(nrow(resalllongAccuracy), 
                                                            nrow(resalllongAUC), 
                                                            nrow(resalllongBrierScore)))

library("plyr")
resalllongsum <- ddply(resalllong, .variables=c("train_pattern", "test_pattern", "method", "dataset", "measure"), 
                       .fun=summarise, rank=mean(rank, na.rm=TRUE))



resalllongsum$test_pattern <- paste0("tebmp: ", resalllongsum$test_pattern)

resalllongsum$method <- factor(resalllongsum$method, levels=c("ComplcRF", "SingleBlRF", "BlwRF", "MultisRF", "ImpRF", "MddsPLS", "PrLasso"))



ui <- resalllongsum[resalllongsum$measure=="Brier",]

sapply(unique(ui$test_pattern), function(x) sum(ui$method=="BlwRF" & ui$test_pattern==x))

ui$test_pattern

library("ggplot2")

p <- ggplot(data=resalllongsum[resalllongsum$measure=="Brier",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/Figure_5.eps", width=10, height=7)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="ACC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_ACC_ranks.pdf", width=10, height=7)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="AUC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_AUC_ranks.pdf", width=10, height=7)







# Perform the above analysis under the exclusion of ComplcRF (because ComplcRF
# did not deliver results in certain settings):


restemp <- resall[resall$method!="ComplcRF",]

tempvar <- paste(restemp$train_pattern, restemp$test_pattern, restemp$repetition, restemp$dataset, sep="_")

tempvarun <- unique(tempvar)
good <- sapply(tempvarun, function(x) all(!is.na(restemp$AUC[tempvar==x])))

restemp <- restemp[tempvar %in% tempvarun[good],]



# Boxplots of the raw values:

library("plyr")
resallsum <- ddply(restemp, .variables=c("train_pattern", "test_pattern", "method", "dataset"), .fun=summarise, AUC=mean(AUC, na.rm=TRUE),
                   AUC=mean(AUC, na.rm=TRUE),
                   Accuracy=mean(Accuracy, na.rm=TRUE),
                   Sensitivity=mean(Sensitivity, na.rm=TRUE),
                   Specificity=mean(Specificity, na.rm=TRUE),
                   Precision=mean(Precision, na.rm=TRUE),
                   Recall=mean(Recall, na.rm=TRUE),
                   F1=mean(F1, na.rm=TRUE),
                   BrierScore=mean(BrierScore, na.rm=TRUE))

ggdata <- reshape(resallsum, varying=c("AUC", "Accuracy", "BrierScore"), 
                  v.names="value", 
                  timevar="measure", times=c("AUC", "ACC", "Brier"),
                  direction="long")

library("ggplot2")


ggdata$test_pattern <- paste0("Tebmp: ", ggdata$test_pattern)

ggdata$helpvar <- 0
tempvar <- paste(ggdata$train_pattern, ggdata$dataset, sep="_")
tempvarun <- unique(tempvar)
colorvals <- runif(length(tempvarun))
for(i in seq(along=tempvarun))
  ggdata$helpvar[tempvar==tempvarun[i]] <- colorvals[i]


p <- ggplot(data=ggdata[ggdata$measure=="Brier",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_line(aes(group=interaction(train_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_NoComplcRF_Brier_values.pdf", width=10, height=7)


p <- ggplot(data=ggdata[ggdata$measure=="ACC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_line(aes(group=interaction(train_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_NoComplcRF_ACC_values.pdf", width=10, height=7)

p <- ggplot(data=ggdata[ggdata$measure=="AUC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_line(aes(group=interaction(train_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_NoComplcRF_AUC_values.pdf", width=10, height=7)





# Boxplots of the ranks:



resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "AUC")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                         "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("AUC", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("AUC", names(resallwide))] <- resranks

resalllongAUC <- reshape(resallwide, varying=colnames(resranks), 
                         v.names="rank", 
                         timevar="method", times=gsub("AUC.", "", colnames(resranks)),
                         direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "Accuracy")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                              "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("Accuracy", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("Accuracy", names(resallwide))] <- resranks

resalllongAccuracy <- reshape(resallwide, varying=colnames(resranks), 
                              v.names="rank", 
                              timevar="method", times=gsub("Accuracy.", "", colnames(resranks)),
                              direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "BrierScore")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                                "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("BrierScore", names(resallwide))], 1, function(x) rank(x)))

resallwide[,grep("BrierScore", names(resallwide))] <- resranks

resalllongBrierScore <- reshape(resallwide, varying=colnames(resranks), 
                                v.names="rank", 
                                timevar="method", times=gsub("BrierScore.", "", colnames(resranks)),
                                direction="long")



resalllong <- rbind(resalllongAccuracy, resalllongAUC, resalllongBrierScore)
resalllong$measure <- rep(c("ACC", "AUC", "Brier"), times=c(nrow(resalllongAccuracy), 
                                                            nrow(resalllongAUC), 
                                                            nrow(resalllongBrierScore)))

library("plyr")
resalllongsum <- ddply(resalllong, .variables=c("train_pattern", "test_pattern", "method", "dataset", "measure"), 
                       .fun=summarise, rank=mean(rank, na.rm=TRUE))



resalllongsum$test_pattern <- paste0("tebmp: ", resalllongsum$test_pattern)

resalllongsum$method <- factor(resalllongsum$method, levels=c("ComplcRF", "SingleBlRF", "BlwRF", "MultisRF", "ImpRF", "MddsPLS", "PrLasso"))



ui <- resalllongsum[resalllongsum$measure=="Brier",]

sapply(unique(ui$test_pattern), function(x) sum(ui$method=="BlwRF" & ui$test_pattern==x))


library("ggplot2")

p <- ggplot(data=resalllongsum[resalllongsum$measure=="Brier",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_NoComplcRF_Brier_ranks.pdf", width=10, height=7)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="ACC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_NoComplcRF_ACC_ranks.pdf", width=10, height=7)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="AUC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_NoComplcRF_AUC_ranks.pdf", width=10, height=7)








# Perform the above analysis under the exclusion of MultisRF (because MultisRF
# did not deliver results in certain settings):


restemp <- resall[resall$method!="MultisRF",]

tempvar <- paste(restemp$train_pattern, restemp$test_pattern, restemp$repetition, restemp$dataset, sep="_")

tempvarun <- unique(tempvar)
good <- sapply(tempvarun, function(x) all(!is.na(restemp$AUC[tempvar==x])))

restemp <- restemp[tempvar %in% tempvarun[good],]



# Boxplots of the raw values:

library("plyr")
resallsum <- ddply(restemp, .variables=c("train_pattern", "test_pattern", "method", "dataset"), .fun=summarise, AUC=mean(AUC, na.rm=TRUE),
                   AUC=mean(AUC, na.rm=TRUE),
                   Accuracy=mean(Accuracy, na.rm=TRUE),
                   Sensitivity=mean(Sensitivity, na.rm=TRUE),
                   Specificity=mean(Specificity, na.rm=TRUE),
                   Precision=mean(Precision, na.rm=TRUE),
                   Recall=mean(Recall, na.rm=TRUE),
                   F1=mean(F1, na.rm=TRUE),
                   BrierScore=mean(BrierScore, na.rm=TRUE))

ggdata <- reshape(resallsum, varying=c("AUC", "Accuracy", "BrierScore"), 
                  v.names="value", 
                  timevar="measure", times=c("AUC", "ACC", "Brier"),
                  direction="long")

library("ggplot2")


ggdata$test_pattern <- paste0("Tebmp: ", ggdata$test_pattern)

ggdata$helpvar <- 0
tempvar <- paste(ggdata$train_pattern, ggdata$dataset, sep="_")
tempvarun <- unique(tempvar)
colorvals <- runif(length(tempvarun))
for(i in seq(along=tempvarun))
  ggdata$helpvar[tempvar==tempvarun[i]] <- colorvals[i]


p <- ggplot(data=ggdata[ggdata$measure=="Brier",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_line(aes(group=interaction(train_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_NoMultisRF_Brier_values.pdf", width=10, height=7)


p <- ggplot(data=ggdata[ggdata$measure=="ACC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_line(aes(group=interaction(train_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_NoMultisRF_ACC_values.pdf", width=10, height=7)

p <- ggplot(data=ggdata[ggdata$measure=="AUC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_line(aes(group=interaction(train_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_NoMultisRF_AUC_values.pdf", width=10, height=7)





# Boxplots of the ranks:



resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "AUC")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                         "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("AUC", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("AUC", names(resallwide))] <- resranks

resalllongAUC <- reshape(resallwide, varying=colnames(resranks), 
                         v.names="rank", 
                         timevar="method", times=gsub("AUC.", "", colnames(resranks)),
                         direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "Accuracy")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                              "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("Accuracy", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("Accuracy", names(resallwide))] <- resranks

resalllongAccuracy <- reshape(resallwide, varying=colnames(resranks), 
                              v.names="rank", 
                              timevar="method", times=gsub("Accuracy.", "", colnames(resranks)),
                              direction="long")


resallwide <- reshape(restemp[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "BrierScore")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                                "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("BrierScore", names(resallwide))], 1, function(x) rank(x)))

resallwide[,grep("BrierScore", names(resallwide))] <- resranks

resalllongBrierScore <- reshape(resallwide, varying=colnames(resranks), 
                                v.names="rank", 
                                timevar="method", times=gsub("BrierScore.", "", colnames(resranks)),
                                direction="long")



resalllong <- rbind(resalllongAccuracy, resalllongAUC, resalllongBrierScore)
resalllong$measure <- rep(c("ACC", "AUC", "Brier"), times=c(nrow(resalllongAccuracy), 
                                                            nrow(resalllongAUC), 
                                                            nrow(resalllongBrierScore)))

library("plyr")
resalllongsum <- ddply(resalllong, .variables=c("train_pattern", "test_pattern", "method", "dataset", "measure"), 
                       .fun=summarise, rank=mean(rank, na.rm=TRUE))



resalllongsum$test_pattern <- paste0("tebmp: ", resalllongsum$test_pattern)

resalllongsum$method <- factor(resalllongsum$method, levels=c("ComplcRF", "SingleBlRF", "BlwRF", "MultisRF", "ImpRF", "MddsPLS", "PrLasso"))


ui <- resalllongsum[resalllongsum$measure=="Brier",]

sapply(unique(ui$test_pattern), function(x) sum(ui$method=="BlwRF" & ui$test_pattern==x))


library("ggplot2")

p <- ggplot(data=resalllongsum[resalllongsum$measure=="Brier",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_NoMultisRF_Brier_ranks.pdf", width=10, height=7)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="ACC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_NoMultisRF_ACC_ranks.pdf", width=10, height=7)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="AUC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~test_pattern, nrow=2) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_tebmp_NoMultisRF_AUC_ranks.pdf", width=10, height=7)









################################################################################
# Comparison of the methods separated by combination of trbmp and tebmp pattern
################################################################################

resall <- resallsafe

# Perform the analysis under the exclusion of all repetitions for which at least
# one method resulted in an error:

# Remove these cases:

restemp <- resall

tempvar <- paste(restemp$train_pattern, restemp$test_pattern, restemp$repetition, restemp$dataset, sep="_")

tempvarun <- unique(tempvar)
good <- sapply(tempvarun, function(x) all(!is.na(restemp$AUC[tempvar==x])))

restemp <- restemp[tempvar %in% tempvarun[good],]



# Boxplots of the raw values:

library("plyr")
resallsum <- ddply(restemp, .variables=c("train_pattern", "test_pattern", "method", "dataset"), .fun=summarise, AUC=mean(AUC, na.rm=TRUE),
                   AUC=mean(AUC, na.rm=TRUE),
                   Accuracy=mean(Accuracy, na.rm=TRUE),
                   Sensitivity=mean(Sensitivity, na.rm=TRUE),
                   Specificity=mean(Specificity, na.rm=TRUE),
                   Precision=mean(Precision, na.rm=TRUE),
                   Recall=mean(Recall, na.rm=TRUE),
                   F1=mean(F1, na.rm=TRUE),
                   BrierScore=mean(BrierScore, na.rm=TRUE))


ggdata <- reshape(resallsum, varying=c("AUC", "Accuracy", "BrierScore"), 
                  v.names="value", 
                  timevar="measure", times=c("AUC", "ACC", "Brier"),
                  direction="long")

library("ggplot2")

ggdata$helpvar <- 0
tempvar <- paste(ggdata$train_pattern, ggdata$test_pattern, ggdata$dataset, sep="_")
tempvarun <- unique(tempvar)
colorvals <- runif(length(tempvarun))
for(i in seq(along=tempvarun))
  ggdata$helpvar[tempvar==tempvarun[i]] <- colorvals[i]

ggdata$train_pattern <- paste0("Trbmp: ", ggdata$train_pattern)
ggdata$test_pattern <- paste0("Tebmp: ", ggdata$test_pattern)
ggdata$train_pattern <- factor(ggdata$train_pattern, levels=paste0("Trbmp: ", 1:5))
ggdata$test_pattern <- factor(ggdata$test_pattern, levels=paste0("Tebmp: ", 1:4))


p <- ggplot(data=ggdata[ggdata$measure=="Brier",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern+test_pattern,nrow=5,drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_line(aes(group=interaction(train_pattern, test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_Brier_values.pdf", width=10, height=10)


p <- ggplot(data=ggdata[ggdata$measure=="ACC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern+test_pattern,nrow=5,drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_line(aes(group=interaction(train_pattern, test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_ACC_values.pdf", width=10, height=10)


p <- ggplot(data=ggdata[ggdata$measure=="AUC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern+test_pattern,nrow=5,drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_line(aes(group=interaction(train_pattern, test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_AUC_values.pdf", width=10, height=10)




# Boxplots of the ranks:

restemp2 <- restemp[restemp$method!="FulldataRF",]

resallwide <- reshape(restemp2[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "AUC")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                         "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("AUC", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("AUC", names(resallwide))] <- resranks

resalllongAUC <- reshape(resallwide, varying=colnames(resranks), 
                         v.names="rank", 
                         timevar="method", times=gsub("AUC.", "", colnames(resranks)),
                         direction="long")


resallwide <- reshape(restemp2[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "Accuracy")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                              "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("Accuracy", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("Accuracy", names(resallwide))] <- resranks

resalllongAccuracy <- reshape(resallwide, varying=colnames(resranks), 
                              v.names="rank", 
                              timevar="method", times=gsub("Accuracy.", "", colnames(resranks)),
                              direction="long")


resallwide <- reshape(restemp2[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "BrierScore")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                                "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("BrierScore", names(resallwide))], 1, function(x) rank(x)))

resallwide[,grep("BrierScore", names(resallwide))] <- resranks

resalllongBrierScore <- reshape(resallwide, varying=colnames(resranks), 
                                v.names="rank", 
                                timevar="method", times=gsub("BrierScore.", "", colnames(resranks)),
                                direction="long")



resalllong <- rbind(resalllongAccuracy, resalllongAUC, resalllongBrierScore)
resalllong$measure <- rep(c("ACC", "AUC", "Brier"), times=c(nrow(resalllongAccuracy), 
                                                            nrow(resalllongAUC), 
                                                            nrow(resalllongBrierScore)))

library("plyr")
resalllongsum <- ddply(resalllong, .variables=c("train_pattern", "test_pattern", "method", "dataset", "measure"), 
                       .fun=summarise, rank=mean(rank, na.rm=TRUE))

resalllongsum$train_pattern <- paste0("Trbmp: ", resalllongsum$train_pattern)
resalllongsum$test_pattern <- paste0("Tebmp: ", resalllongsum$test_pattern)
resalllongsum$train_pattern <- factor(resalllongsum$train_pattern, levels=paste0("Trbmp: ", 1:5))
resalllongsum$test_pattern <- factor(resalllongsum$test_pattern, levels=paste0("Tebmp: ", 1:4))

resalllongsum$method <- factor(resalllongsum$method, levels=c("ComplcRF", "SingleBlRF", "BlwRF", "MultisRF", "ImpRF", "MddsPLS", "PrLasso"))


library("ggplot2")

p <- ggplot(data=resalllongsum[resalllongsum$measure=="Brier",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern+test_pattern, nrow=5, drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/Figure_6.eps", width=10, height=10)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="ACC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern+test_pattern, nrow=5, drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_ACC_ranks.pdf", width=10, height=10)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="AUC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern+test_pattern, nrow=5, drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_AUC_ranks.pdf", width=10, height=10)








# Perform the above analysis under the exclusion of ComplcRF (because ComplcRF
# did not deliver results in certain settings):


restemp <- resall[resall$method!="ComplcRF",]

tempvar <- paste(restemp$train_pattern, restemp$test_pattern, restemp$repetition, restemp$dataset, sep="_")

tempvarun <- unique(tempvar)
good <- sapply(tempvarun, function(x) all(!is.na(restemp$AUC[tempvar==x])))

restemp <- restemp[tempvar %in% tempvarun[good],]



# Boxplots of the raw values:

library("plyr")
resallsum <- ddply(restemp, .variables=c("train_pattern", "test_pattern", "method", "dataset"), .fun=summarise, AUC=mean(AUC, na.rm=TRUE),
                   AUC=mean(AUC, na.rm=TRUE),
                   Accuracy=mean(Accuracy, na.rm=TRUE),
                   Sensitivity=mean(Sensitivity, na.rm=TRUE),
                   Specificity=mean(Specificity, na.rm=TRUE),
                   Precision=mean(Precision, na.rm=TRUE),
                   Recall=mean(Recall, na.rm=TRUE),
                   F1=mean(F1, na.rm=TRUE),
                   BrierScore=mean(BrierScore, na.rm=TRUE))


ggdata <- reshape(resallsum, varying=c("AUC", "Accuracy", "BrierScore"), 
                  v.names="value", 
                  timevar="measure", times=c("AUC", "ACC", "Brier"),
                  direction="long")

library("ggplot2")

ggdata$helpvar <- 0
tempvar <- paste(ggdata$train_pattern, ggdata$test_pattern, ggdata$dataset, sep="_")
tempvarun <- unique(tempvar)
colorvals <- runif(length(tempvarun))
for(i in seq(along=tempvarun))
  ggdata$helpvar[tempvar==tempvarun[i]] <- colorvals[i]

ggdata$train_pattern <- paste0("Trbmp: ", ggdata$train_pattern)
ggdata$test_pattern <- paste0("Tebmp: ", ggdata$test_pattern)
ggdata$train_pattern <- factor(ggdata$train_pattern, levels=paste0("Trbmp: ", 1:5))
ggdata$test_pattern <- factor(ggdata$test_pattern, levels=paste0("Tebmp: ", 1:4))


p <- ggplot(data=ggdata[ggdata$measure=="Brier",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern+test_pattern,nrow=5,drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_line(aes(group=interaction(train_pattern, test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoComplcRF_Brier_values.pdf", width=10, height=10)


p <- ggplot(data=ggdata[ggdata$measure=="ACC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern+test_pattern,nrow=5,drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_line(aes(group=interaction(train_pattern, test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoComplcRF_ACC_values.pdf", width=10, height=10)


p <- ggplot(data=ggdata[ggdata$measure=="AUC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern+test_pattern,nrow=5,drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_line(aes(group=interaction(train_pattern, test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoComplcRF_AUC_values.pdf", width=10, height=10)






tempdata <- ggdata[ggdata$measure=="Brier",]
tempdata <- ddply(tempdata, .variables=c("train_pattern", "test_pattern", "method"), .fun=summarise, value=mean(value, na.rm=TRUE))

p <- ggplot(data=tempdata, aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern+test_pattern,nrow=5,drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_line(aes(group=interaction(train_pattern, test_pattern))) + 
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoComplcRF_Brier_means.pdf", width=10, height=10)



tempdata <- ggdata[ggdata$measure=="ACC",]
tempdata <- ddply(tempdata, .variables=c("train_pattern", "test_pattern", "method"), .fun=summarise, value=mean(value, na.rm=TRUE))

p <- ggplot(data=tempdata, aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern+test_pattern,nrow=5,drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_line(aes(group=interaction(train_pattern, test_pattern))) + 
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoComplcRF_ACC_means.pdf", width=10, height=10)



tempdata <- ggdata[ggdata$measure=="AUC",]
tempdata <- ddply(tempdata, .variables=c("train_pattern", "test_pattern", "method"), .fun=summarise, value=mean(value, na.rm=TRUE))

p <- ggplot(data=tempdata, aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern+test_pattern,nrow=5,drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_line(aes(group=interaction(train_pattern, test_pattern))) + 
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoComplcRF_AUC_means.pdf", width=10, height=10)











# Boxplots of the ranks:

restemp2 <- restemp[restemp$method!="FulldataRF",]

resallwide <- reshape(restemp2[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "AUC")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                         "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("AUC", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("AUC", names(resallwide))] <- resranks

resalllongAUC <- reshape(resallwide, varying=colnames(resranks), 
                         v.names="rank", 
                         timevar="method", times=gsub("AUC.", "", colnames(resranks)),
                         direction="long")


resallwide <- reshape(restemp2[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "Accuracy")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                              "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("Accuracy", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("Accuracy", names(resallwide))] <- resranks

resalllongAccuracy <- reshape(resallwide, varying=colnames(resranks), 
                              v.names="rank", 
                              timevar="method", times=gsub("Accuracy.", "", colnames(resranks)),
                              direction="long")


resallwide <- reshape(restemp2[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "BrierScore")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                                "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("BrierScore", names(resallwide))], 1, function(x) rank(x)))

resallwide[,grep("BrierScore", names(resallwide))] <- resranks

resalllongBrierScore <- reshape(resallwide, varying=colnames(resranks), 
                                v.names="rank", 
                                timevar="method", times=gsub("BrierScore.", "", colnames(resranks)),
                                direction="long")



resalllong <- rbind(resalllongAccuracy, resalllongAUC, resalllongBrierScore)
resalllong$measure <- rep(c("ACC", "AUC", "Brier"), times=c(nrow(resalllongAccuracy), 
                                                            nrow(resalllongAUC), 
                                                            nrow(resalllongBrierScore)))

library("plyr")
resalllongsum <- ddply(resalllong, .variables=c("train_pattern", "test_pattern", "method", "dataset", "measure"), 
                       .fun=summarise, rank=mean(rank, na.rm=TRUE))

resalllongsum$train_pattern <- paste0("Trbmp: ", resalllongsum$train_pattern)
resalllongsum$test_pattern <- paste0("Tebmp: ", resalllongsum$test_pattern)
resalllongsum$train_pattern <- factor(resalllongsum$train_pattern, levels=paste0("Trbmp: ", 1:5))
resalllongsum$test_pattern <- factor(resalllongsum$test_pattern, levels=paste0("Tebmp: ", 1:4))

resalllongsum$method <- factor(resalllongsum$method, levels=c("ComplcRF", "SingleBlRF", "BlwRF", "MultisRF", "ImpRF", "MddsPLS", "PrLasso"))


p <- ggplot(data=resalllongsum[resalllongsum$measure=="Brier",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern+test_pattern, nrow=5, drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p


library("ggplot2")

p <- ggplot(data=resalllongsum[resalllongsum$measure=="Brier",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern+test_pattern, nrow=5, drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoComplcRF_Brier_ranks.pdf", width=10, height=10)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="ACC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern+test_pattern, nrow=5, drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoComplcRF_ACC_ranks.pdf", width=10, height=10)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="AUC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern+test_pattern, nrow=5, drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoComplcRF_AUC_ranks.pdf", width=10, height=10)










# Perform the above analysis under the exclusion of MultisRF (because MultisRF
# did not deliver results in certain settings):


restemp <- resall[resall$method!="MultisRF",]

tempvar <- paste(restemp$train_pattern, restemp$test_pattern, restemp$repetition, restemp$dataset, sep="_")

tempvarun <- unique(tempvar)
good <- sapply(tempvarun, function(x) all(!is.na(restemp$AUC[tempvar==x])))

restemp <- restemp[tempvar %in% tempvarun[good],]



# Boxplots of the raw values:

library("plyr")
resallsum <- ddply(restemp, .variables=c("train_pattern", "test_pattern", "method", "dataset"), .fun=summarise, AUC=mean(AUC, na.rm=TRUE),
                   AUC=mean(AUC, na.rm=TRUE),
                   Accuracy=mean(Accuracy, na.rm=TRUE),
                   Sensitivity=mean(Sensitivity, na.rm=TRUE),
                   Specificity=mean(Specificity, na.rm=TRUE),
                   Precision=mean(Precision, na.rm=TRUE),
                   Recall=mean(Recall, na.rm=TRUE),
                   F1=mean(F1, na.rm=TRUE),
                   BrierScore=mean(BrierScore, na.rm=TRUE))


ggdata <- reshape(resallsum, varying=c("AUC", "Accuracy", "BrierScore"), 
                  v.names="value", 
                  timevar="measure", times=c("AUC", "ACC", "Brier"),
                  direction="long")

library("ggplot2")

ggdata$helpvar <- 0
tempvar <- paste(ggdata$train_pattern, ggdata$test_pattern, ggdata$dataset, sep="_")
tempvarun <- unique(tempvar)
colorvals <- runif(length(tempvarun))
for(i in seq(along=tempvarun))
  ggdata$helpvar[tempvar==tempvarun[i]] <- colorvals[i]

ggdata$train_pattern <- paste0("Trbmp: ", ggdata$train_pattern)
ggdata$test_pattern <- paste0("Tebmp: ", ggdata$test_pattern)
ggdata$train_pattern <- factor(ggdata$train_pattern, levels=paste0("Trbmp: ", 1:5))
ggdata$test_pattern <- factor(ggdata$test_pattern, levels=paste0("Tebmp: ", 1:4))


p <- ggplot(data=ggdata[ggdata$measure=="Brier",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern+test_pattern,nrow=5,drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_line(aes(group=interaction(train_pattern, test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoMultisRF_Brier_values.pdf", width=10, height=10)


p <- ggplot(data=ggdata[ggdata$measure=="ACC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern+test_pattern,nrow=5,drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_line(aes(group=interaction(train_pattern, test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoMultisRF_ACC_values.pdf", width=10, height=10)


p <- ggplot(data=ggdata[ggdata$measure=="AUC",], aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern+test_pattern,nrow=5,drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_line(aes(group=interaction(train_pattern, test_pattern, dataset), color=helpvar), alpha = 0.3) + geom_boxplot(fill=NA) +
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoMultisRF_AUC_values.pdf", width=10, height=10)




tempdata <- ggdata[ggdata$measure=="Brier",]
tempdata <- ddply(tempdata, .variables=c("train_pattern", "test_pattern", "method"), .fun=summarise, value=mean(value, na.rm=TRUE))

p <- ggplot(data=tempdata, aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern+test_pattern,nrow=5,drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_line(aes(group=interaction(train_pattern, test_pattern))) + 
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoMultisRF_Brier_means.pdf", width=10, height=10)



tempdata <- ggdata[ggdata$measure=="ACC",]
tempdata <- ddply(tempdata, .variables=c("train_pattern", "test_pattern", "method"), .fun=summarise, value=mean(value, na.rm=TRUE))

p <- ggplot(data=tempdata, aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern+test_pattern,nrow=5,drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_line(aes(group=interaction(train_pattern, test_pattern))) + 
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoMultisRF_ACC_means.pdf", width=10, height=10)



tempdata <- ggdata[ggdata$measure=="AUC",]
tempdata <- ddply(tempdata, .variables=c("train_pattern", "test_pattern", "method"), .fun=summarise, value=mean(value, na.rm=TRUE))

p <- ggplot(data=tempdata, aes(x=method, y=value)) + theme_bw() + facet_wrap(~train_pattern+test_pattern,nrow=5,drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_line(aes(group=interaction(train_pattern, test_pattern))) + 
  theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
        axis.text.y=element_text(size=14, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=16),
        strip.text.x = element_text(size = 16), legend.position = "none") + ylab("mean across repetitions") +
  scale_color_continuous(type = "viridis")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoMultisRF_AUC_means.pdf", width=10, height=10)











# Boxplots of the ranks:

restemp2 <- restemp[restemp$method!="FulldataRF",]

resallwide <- reshape(restemp2[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "AUC")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                         "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("AUC", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("AUC", names(resallwide))] <- resranks

resalllongAUC <- reshape(resallwide, varying=colnames(resranks), 
                         v.names="rank", 
                         timevar="method", times=gsub("AUC.", "", colnames(resranks)),
                         direction="long")


resallwide <- reshape(restemp2[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "Accuracy")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                              "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("Accuracy", names(resallwide))], 1, function(x) rank(-x)))

resallwide[,grep("Accuracy", names(resallwide))] <- resranks

resalllongAccuracy <- reshape(resallwide, varying=colnames(resranks), 
                              v.names="rank", 
                              timevar="method", times=gsub("Accuracy.", "", colnames(resranks)),
                              direction="long")


resallwide <- reshape(restemp2[,c("train_pattern", "test_pattern", "repetition",
                                 "dataset", "method", "BrierScore")], idvar = c("train_pattern", "test_pattern", "repetition",
                                                                                "dataset"), timevar = "method", direction = "wide")

resranks <- t(apply(resallwide[,grep("BrierScore", names(resallwide))], 1, function(x) rank(x)))

resallwide[,grep("BrierScore", names(resallwide))] <- resranks

resalllongBrierScore <- reshape(resallwide, varying=colnames(resranks), 
                                v.names="rank", 
                                timevar="method", times=gsub("BrierScore.", "", colnames(resranks)),
                                direction="long")



resalllong <- rbind(resalllongAccuracy, resalllongAUC, resalllongBrierScore)
resalllong$measure <- rep(c("ACC", "AUC", "Brier"), times=c(nrow(resalllongAccuracy), 
                                                            nrow(resalllongAUC), 
                                                            nrow(resalllongBrierScore)))

library("plyr")
resalllongsum <- ddply(resalllong, .variables=c("train_pattern", "test_pattern", "method", "dataset", "measure"), 
                       .fun=summarise, rank=mean(rank, na.rm=TRUE))

resalllongsum$train_pattern <- paste0("Trbmp: ", resalllongsum$train_pattern)
resalllongsum$test_pattern <- paste0("Tebmp: ", resalllongsum$test_pattern)
resalllongsum$train_pattern <- factor(resalllongsum$train_pattern, levels=paste0("Trbmp: ", 1:5))
resalllongsum$test_pattern <- factor(resalllongsum$test_pattern, levels=paste0("Tebmp: ", 1:4))

resalllongsum$method <- factor(resalllongsum$method, levels=c("ComplcRF", "SingleBlRF", "BlwRF", "MultisRF", "ImpRF", "MddsPLS", "PrLasso"))

library("ggplot2")

p <- ggplot(data=resalllongsum[resalllongsum$measure=="Brier",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern+test_pattern, nrow=5, drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoMultisRF_Brier_ranks.pdf", width=10, height=10)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="ACC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern+test_pattern, nrow=5, drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoMultisRF_ACC_ranks.pdf", width=10, height=10)


p <- ggplot(data=resalllongsum[resalllongsum$measure=="AUC",], aes(x=method, y=rank)) + theme_bw() + facet_wrap(~train_pattern+test_pattern, nrow=5, drop=FALSE, labeller = label_wrap_gen(multi_line=FALSE)) +
  geom_boxplot() +   theme(axis.text.x=element_text(size=16, angle=45, vjust=1, hjust=1, colour="black"), 
                           axis.text.y=element_text(size=14, colour="black"),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=16),
                           strip.text.x = element_text(size = 16)) + ylab("mean rank")
p

ggsave("./evaluation_code_and_results/figures_and_table3/SuppFigure_trbmp_tebmp_NoMultisRF_AUC_ranks.pdf", width=10, height=10)
