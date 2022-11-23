" Example-Script on how to use TOMBI for the imputation on multi-omics data -  
  this script was provided by Roman Hornung 
"
# (0) Set WD & load packages                                                 ----
setwd("/Users/frederik/Desktop/BWM-Article/")             # Mac
setwd("C:/Users/kuche/Desktop/BWM-Paper")                 # Windows
setwd("/dss/dsshome1/lxc0B/ru68kiq3/Project/BWM-Article") # Server

library(tictoc)

# (1) Define Functions                                                       ----
# 1-1 Original TOMBI function:
TOBMI <- function(x = cpg, y = exp) {
  
  ##Calculating the distances among un-/complete cases using auxiliary dataset
  dist.matrix <- as.matrix(dist( x ))
  
  ##Neighbors list for every uncomplete cases
  missing_num <- length(which(complete.cases(y) == F)) 
  donors <- list()
  for(i in 1:missing_num){
    donors[[i]] <- as.matrix(sort(dist.matrix[i,c(c(missing_num + 1):dim(x)[1])])[1 : floor(sqrt(dim(x)[1] - missing_num))])
  }
  
  ##Neighbors will be weighted by distance 
  donors.w<-list()		
  for(i in 1:missing_num){
    donors.w[[i]]<-(1/donors[[i]][,1])/sum((1/donors[[i]][,1]))
  }
  
  ##Imputation process
  for(j in 1:missing_num){
    as.data.frame(donors.w[[j]])->donors.pool
    row.names(donors.pool)->donors.pool$id
    y$id <- row.names(y)
    merge(donors.pool,y,by='id')->donors.candidates
    donors.candidates[,2] * donors.candidates[,3:dim(donors.candidates)[2]]->donors.calculate
    y[j,-dim(y)[2]]<-apply(donors.calculate, MARGIN = 2,sum)
  }
  imputed.data<-y[,-dim(y)[2]]
}

# 1-2 Altered TOBMI function with greater speed than the original function 
#    (produces the same result as the original TOBMI function)
#     First implemenation (before 22.12.21) had error, which was corrected by Dr. Hornung
TOBMIfast <- function(x = cpg, y = exp) {
  
  ##Calculating the distances among un-/complete cases using auxiliary dataset
  dist.matrix <- as.matrix(dist( x ))
  
  ##Neighbors list for every uncomplete cases
  missing_num <- length(which(complete.cases(y) == F)) 
  donors <- list()
  for(i in 1:missing_num){
    tempobj <- dist.matrix[i,c(c(missing_num + 1):dim(x)[1])]
    donors[[i]] <- as.matrix(tempobj[order(tempobj, runif(length(tempobj)))][1 : floor(sqrt(dim(x)[1] - missing_num))])
    ## NEW: If the Mahalanobis distance was zero, the weights were NaN. --> Replace
    ## distances of zero by the smallest observed distance greater than zero.
    ## Expection: Sometimes all distances were zero. In these cases just set all distances
    ## to 1, which corresponds to equal weights.
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



# 1-3 Function to do the TOMBI-Imputation on our data:
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

# 1-4 HelpFunction for 1-3
ImputeTwo <- function(omicsdatacompl, omicsdatamiss) {
  
  reorderind <- order(complete.cases(omicsdatamiss))
  rereorderind <- order(reorderind)
  
  imputed <- TOBMIfast(x = omicsdatacompl[reorderind,], y = omicsdatamiss[reorderind,])
  imputed <- imputed[rereorderind,]
  
  return(imputed)
}

# (2) Prepare the data for the imputation                                    ----
# 2-1 Load a raw multi-omics DF ('ExampleData.Rda' was created with it & we need
#     to know which variable belongs to which block for the imputation)
load("./Data/Raw/LGG.Rda")

# 2-2 Select Target Variable & assign the blocks
ytarget <- mutation[,"TP53"]
block1  <- clin
block2  <- cnv
block3  <- mirna
block4  <- rna

# 2-3 Create a full data-set (from the single blocks) and create a block identifier
dataset         <- data.frame(cbind(block1, block2, block3, block4))
dataset$ytarget <- ytarget
blockind        <- rep(1:4, times = c(ncol(block1), ncol(block2), ncol(block3), 
                                      ncol(block4)))

# (3) Do the actual imputation                                               ----
# 3-1 Load the example data, to try the approach on it
load("./Data/Example_Data/ExampleData.Rda")

# 3-2 Process the data
# --1 Extract the response & delete the corresponding column in the df
omicsdata         <- datatrain2
dim(omicsdata); sum(is.na(omicsdata)) # Has missing values (except for datatrain1) & the following dimensions
ytarget           <- datatrain1$ytarget
omicsdata$ytarget <- NULL
dim(omicsdata); sum(is.na(omicsdata)) # Still missing values and the features are reduced by 1

# 3-3 Apply the imputation with TOMBI 
#     (original data & index of blocks for the variables are needed)
tic()
omicsdata2imp <- ImputeWithTOBMI(omicsdata, blockind)
toc()

# --> 421 seconds for 'datatrain4'
# --> 120 seconds for 'datatrain3'
# --> 289 seconds for 'datatrain2'
# --> 001 seconds for 'datatrain1'

# 3-4 Check whether the imputation was successful
any(is.na(omicsdata)); sum(is.na(omicsdata)) # -> should contain NAs
any(is.na(omicsdata2imp)); sum(is.na(omicsdata2imp))  # -> should not contain any NAs
                                                      # --> Seems to be working
