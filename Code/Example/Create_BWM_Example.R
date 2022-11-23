"
Script from Dr. Hornung with an example of how to create a BWM-Pattern into a 
raw (multi-omics) data-set that originally doesn't miss any values.
   --> Resulting DFs are in 'Data/Example_Data/ExampleData.Rda'
"
# Set WD
setwd("C:/Users/kuche/Desktop/BWM-Paper")

# Load the original raw dataset (w/0 missing values):
load("./Code/Data/TCGA/LGG.Rda")

# Processed Data can be loaded via [-> 5 Train- & Test-Sets]
# load('./Data/Example_Data/ExampleData.Rda')

# [1] Process the data                                                      ----
# 1-1 Select target-variable (TP53from 'Mutation'-Block)
ytarget <- mutation[,"TP53"]

# 1-2 Assign the various multi-omics blocks to 'block'-variables
#     (except for 'Mutation', as we use a feature from it as response)
block1 <- clin
block2 <- cnv
block3 <- mirna
block4 <- rna


# 1-3 Merge the blocks to create a single DF & create a block index
#     (response-variable is added as 'ytarget') to the data
dataset         <- data.frame(cbind(block1, block2, block3, block4))
dataset$ytarget <- ytarget
blockind        <- rep(1:4, times = c(ncol(block1), ncol(block2), ncol(block3), ncol(block4)))

# 1-4 Split the data into train and test (75% Train / 25% Test)
set.seed(1234)
trainind  <- sort(sample(1:nrow(dataset), size = round(0.75*nrow(dataset))))
testind   <- setdiff(1:nrow(dataset), trainind)
datatrain <- dataset[trainind,]
datatest  <- dataset[testind,]


# [2] Induce block-Wise missigness to the Training-Data                     ----
#     Make five versions of the training dataset, where each corresponds to one 
#     training data missing pattern in Figure 1 of the article (s. Docs)
# 2-1 First Version (fully observed, no induction of BWM needed)
datatrain1 <- datatrain

# 2-2 Second Version (with bwm-missingness)
# --1 Mix up the original row-order
datatrain2 <- datatrain[sample(1:nrow(datatrain)),] 

# --2 Amount of observations with missing values for the given pattern (2 folds)
n1 <- round(nrow(datatrain)/2) 

# --3 Assign the observations to one of the two folds
folds <- rep(1:2, times=c(n1, nrow(datatrain) - n1)) 

# --4 Set values of block 3 to NA for observations in fold1 & values of block 2 
#     & 4 to NA for observations in fold2
datatrain2[folds==1, blockind==3] <- NA
datatrain2[folds==2, blockind==2] <- NA
datatrain2[folds==2, blockind==4] <- NA

# 2-3 Third Version
# --1 Mix up the original row-order
datatrain3 <- datatrain[sample(1:nrow(datatrain)),]

# --2 Amount of observations with missing values for the given pattern (2 folds)
n1 <- round(nrow(datatrain)/2)

# --3  Assign the observations to one of the two folds
folds <- rep(1:2, times=c(n1, nrow(datatrain) - n1))

# --4 Set the values of block 4 to NA for all observations in fold 1 & set the 
#     values of block 3 to NA for all observations in fold 2
datatrain3[folds==1, blockind==4] <- NA
datatrain3[folds==2, blockind==3] <- NA


# 2-4 Fourth Version
# --1 Mix up the original row-order
datatrain4 <- datatrain[sample(1:nrow(datatrain)),]

# --2 Amount of observations with missing values for the given pattern (3 folds)
n1 <- round(nrow(datatrain)/3)
n2 <- round(nrow(datatrain)/3)

# --3  Assign the observations to one of the three folds
folds <- rep(1:3, times=c(n1, n2, nrow(datatrain) - n1 - n2))

# --4 Set the values of block 3& 4 to NA for all observations in fold 1, all 
#     values of block 2 & 4 to NA for all observations in fold 2 & set the values
#     of block 2 & 3 to NA for all observations in fold 3
datatrain4[folds==1, blockind==3] <- NA
datatrain4[folds==1, blockind==4] <- NA
datatrain4[folds==2, blockind==2] <- NA
datatrain4[folds==2, blockind==4] <- NA
datatrain4[folds==3, blockind==2] <- NA
datatrain4[folds==3, blockind==3] <- NA


# 2-5 Fifth Version
# --1 Mix up original row-order
datatrain5 <- datatrain[sample(1:nrow(datatrain)),]

# --2 Amount of observations with missing values for the given pattern (7 folds)
folds <- rep(1:7, each=round(nrow(datatrain)/8))

# --3 Assign the observations to one of the seven folds 
folds <- c(folds, rep(8, times = nrow(datatrain) - length(folds)))

# --4 Set the values of the various blocks to NA 
datatrain5[folds==2, blockind==2] <- NA
datatrain5[folds==3, blockind==3] <- NA
datatrain5[folds==4, blockind==4] <- NA
datatrain5[folds==5, blockind==3] <- NA
datatrain5[folds==5, blockind==4] <- NA
datatrain5[folds==6, blockind==2] <- NA
datatrain5[folds==6, blockind==4] <- NA
datatrain5[folds==7, blockind==2] <- NA
datatrain5[folds==7, blockind==3] <- NA
datatrain5[folds==8, blockind==2] <- NA
datatrain5[folds==8, blockind==3] <- NA
datatrain5[folds==8, blockind==4] <- NA ]


# [3] Induce block-wise missingness to the test-data                        ---- 
# 3-1 First Version - with observed values only in block 1
datatest1 <- datatest
datatest1[,blockind %in% c(2,3,4)] <- NA

# 3-2 Second Version - with observed values only in block 1 & 2
datatest2 <- datatest
datatest2[,blockind %in% c(3,4)] <- NA

# 3-3 Third Version - with observed values only in block 1, 2 & 3
datatest3 <- datatest
datatest3[,blockind==4] <- NA

# 3-4 Fourth Version - with no missing values
datatest4 <- datatest


# [4] Save the resuting Train- & Test-data with missing values
save(datatrain1, datatrain2, datatrain3, datatrain4, datatrain5,
     datatest1, datatest2, datatest3, datatest4, 
     file = "./Code/Example_Data/ExampleData.Rda")