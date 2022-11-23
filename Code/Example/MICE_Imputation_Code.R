"
Example script on how to use the 'MICE' for the imputation of missing values.
Goal of the script is to estimate the needed time when imputing missing values 
in such high dimensional data!
"
# (0) Set WD & load packages
setwd("/Users/frederik/Desktop/BWM-Article/")             # Mac
setwd("C:/Users/kuche/Desktop/BWM-Paper")                 # Windows
setwd("/dss/dsshome1/lxc0B/ru68kiq3/Project/BWM-Article") # Server

library(tictoc)
library(mice)

# (1) Load the data
load('./Data/Example_Data/ExampleData.Rda')

# (2) Run MICE Imputation & take the time
tic()
impobj      <- mice(datatrain2, m = 1, printFlag = FALSE)
dataimputed <- complete(impobj, action=1)
a = toc()

# --- PLAY AROUND EXAMPLE -------------------------------------------------------
'
# Load example DF and induce missing values
df_example                      <- iris
df_example$Petal.Length[25:75]  <- NA
df_example$Sepal.Length[82:124] <- NA
df_example$Sepal.Width[1:34]    <- NA

tic()
impobj      <- mice(df_example, m = 1, printFlag = FALSE)
dataimputed <- complete(impobj, action=1)
a = toc()
'




