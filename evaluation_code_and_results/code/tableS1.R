# Set the working directory to the directory 'bwm_article' 
# of the electronic appendix (outcomment the following line
# and replace 'pathtobwm_article' by the path to 'bwm_article'
# on your computer):

## setwd("pathtobwm_article/bwm_article")


# Load the data sets and, for each datda set, save the clinical block:

filelist <- list.files("./compstudy_code_and_results/data/")
df_paths <- paste0("./compstudy_code_and_results/data/", filelist)

clinblocks <- list()

for(i in seq(along=df_paths)) {
  
  load(df_paths[i])
 
  clinblocks[[i]] <-  clin
  
  cat("\n")
  cat("\n")
  cat(paste0("Dataset: ", i), "\n")
  cat("\n")
  cat("\n")
   
}



# Make a matrix that indicates for each data set which variables are
# available for which data set:

datanames <- gsub(".Rda", "", filelist)

varnames <- c("age", "gender", "race", "T_positive", "N_positive", 
              "M_positive", "er_status", "pr_status", "her2_positive",
              "her2_equivocal", "smoking_longer15", "smoking_shorter15")

avmat <- matrix(nrow=length(datanames), ncol=length(varnames), data=0)

for(i in seq(along=datanames)) {
  for(j in seq(along=varnames)) {
    if(varnames[j] %in% names(clinblocks[[i]]))
      avmat[i,j] <- 1
  }  
}

rownames(avmat) <- datanames
colnames(avmat) <- varnames

avmat <- cbind(label=datanames, avmat)

# Keep only those variables that are not restricted to a specific cancer type:
avmat <- avmat[,1:7]




# Save the table as a TeX table:

library("xtable")
print(xtable(avmat, type = "latex"), include.rownames=FALSE, 
      file = "./evaluation_code_and_results/figures_and_table3/tableS1.tex")
