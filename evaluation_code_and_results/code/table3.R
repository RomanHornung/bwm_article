# Set the working directory to the directory 'bwm_article' 
# of the electronic appendix:

library("this.path")
setwd(gsub("/evaluation_code_and_results/code", "", this.dir()))


# Load each data set, extract the numbers of variables for each block
# and save these in a data.frame:

datasetfiles <- list.files("./compstudy_code_and_results/data/", full.names = TRUE)
pathname <- gsub("BLCA.Rda", "", grep("BLCA", datasetfiles, value=TRUE))

datasets <- gsub(".Rda", "", gsub(pathname, "", datasetfiles))
datasets

table3 <- data.frame(dataset=datasets)
table3$n <- NA
table3$prop_tp53 <- NA
table3$clin <- NA
table3$cnv <- NA
table3$mirna <- NA
table3$rna <- NA

for(i in seq(along=datasetfiles)) {
  
  load(datasetfiles[i])
  
  table3$n[i] <- nrow(clin)
  table3$prop_tp53[i] <- round(mean(mutation[,"TP53"]==1), 2)
  table3$clin[i] <- ncol(clin)
  table3$cnv[i] <- ncol(cnv)
  table3$mirna[i] <- ncol(mirna)
  table3$rna[i] <- ncol(rna)

  cat(paste0("Iteration: ", i, " of ", length(datasetfiles), "."), "\n")
  
}

table3
rownames(table3) <- NULL




# Save the table as a TeX table:

library("xtable")
print(xtable(table3, type = "latex"), include.rownames=FALSE, 
      file = "./evaluation_code_and_results/figures_and_table3/table3.tex")
