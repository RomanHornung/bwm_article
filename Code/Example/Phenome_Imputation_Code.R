"Script to try out imputation with the 'PhenomeImpute'-Package"

# (0) Set WD & load packages
setwd("/Users/frederik/Desktop/BWM-Article/")             # Mac
setwd("C:/Users/kuche/Desktop/BWM-Paper")                 # Windows
setwd("/dss/dsshome1/lxc0B/ru68kiq3/Project/BWM-Article") # Server

# install.packages("polycor")
# install.packages("ltm")
# install.packages("scrime")
# install.packages("sensitivity")
# install.packages("mlogit")
# install.packages("missForest")
# install.packages('psych')
# install.packages('cluster')
# install.packages("./Docs/PhenomeImpute/PhenomeImpute_1.0.tar.gz", type="source", repos=NULL)

library(PhenomeImpute)
library(tictoc)

# (1) Load the data
load('./Data/Example_Data/ExampleData.Rda')

tic()
impobj      <- PhenomeImpute(Data = as.matrix(datatrain2), 
                             Type = rep("con", ncol(datatrain2)), k = 5, n.re = 5)
dataimputed <- impobj[[1]]
toc()



# --- PLAY AROUND EXAMPLE -------------------------------------------------------

# Load example DF and induce missing values
df_example                      <- iris
df_example$Petal.Length[25:75]  <- NA
df_example$Sepal.Length[82:124] <- NA
df_example$Sepal.Width[1:34]    <- NA

tic()
impobj <- PhenomeImpute(Data = as.matrix(df_example), 
                        Type = rep("con", ncol(df_example)), k = 5, n.re = 5)
dataimputed <- impobj[[1]]
toc()

