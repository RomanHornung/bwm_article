# Set the working directory to the directory 'bwm_article' 
# of the electronic appendix (outcomment the following line
# and replace 'pathtobwm_article' by the path to 'bwm_article'
# on your computer):

## setwd("pathtobwm_article/bwm_article")
setwd("D:/Projects/SideProjects/BlockwiseMissing/GitHub_Code/bwm_article")


datasetfiles <- list.files("./compstudy_code_and_results/data/", full.names = TRUE)
pathname <- gsub("BLCA.Rda", "", grep("BLCA", datasetfiles, value=TRUE))

datasets <- gsub(".Rda", "", gsub(pathname, "", datasetfiles))
datasets

table1 <- data.frame(dataset=datasets)
table1$n <- NA
table1$prop_tp53 <- NA
table1$clin <- NA
table1$cnv <- NA
table1$mirna <- NA
table1$rna <- NA

for(i in seq(along=datasetfiles)) {
  
  load(datasetfiles[i])
  
  table1$n[i] <- nrow(clin)
  table1$prop_tp53[i] <- round(mean(mutation[,"TP53"]==1), 2)
  table1$clin[i] <- ncol(clin)
  table1$cnv[i] <- ncol(cnv)
  table1$mirna[i] <- ncol(mirna)
  table1$rna[i] <- ncol(rna)

  cat(paste0("Iteration: ", i, " of ", length(datasetfiles), "."), "\n")
  
}

table1
rownames(table1) <- NULL

library("xtable")
print(xtable(table1, type = "latex"), include.rownames=FALSE, 
      file = "./evaluation_code_and_results/figures_and_table1/table1.tex")
