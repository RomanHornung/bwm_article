# Electronic appendix to the article 'Prediction approaches for partly missing multi-omics covariate data: A literature review and an empirical comparison study'

Roman Hornung, Frederik Ludwigs, Jonas Hagenberg, Anne-Laure Boulesteix

This repository was written largely by Frederik Ludwigs and Jonas Hagenberg.

## R version and packages:

- Program: R, version 4.0.3

- Used R packages:

  - 'caret', version: 6.0-84
  - 'checkmate', version: 1.9.4
  - 'combinat', version: 0.0-8
  - 'ddsPLS', version: 1.1.7
  - 'dplyr', version: 0.8.5
  - 'ggplot2', version: 3.2.1
  - 'plyr', version: 1.8.5
  - 'prioritylasso', version: 0.3.0
  - 'pROC', version: 1.15.3 
  - 'randomForestSRC', version: 2.9.2 
  - 'ROCR', version: 1.0-7
  - 'xtable', version: 1.8-4

## Availability of the data sets used and structure of the repository:

- We provide all R codes used in the analysis. However,
  the pre-processed data sets (downloaded from The Cancer Genome
  Atlas (TCGA) database) cannot be provided in this repository due to their sizes.
  These can be downloaded from figshare using the following link:
  
  https://doi.org/10.6084/m9.figshare.22304050.v1
  
- The R package `ddsPLs` (version 1.1.7) is available in the root of this
  repository. It can be installed as follows

  ```r
  install.packages("pathtothetargzfile/ddsPLS_1.1.7.tar.gz", repose=NULL, type="source")
  ```
  
- This repository contains two subdirectories, `compstudy_code_and_results` and `evaluation_code_and_results`:

  - `compstudy_code_and_results` contains the code and raw results of the empirical comparison study. The downloaded data sets from figshare (see above) have to be stored in the data subdirectory of `compstudy_code_and_results` in order to reproduce the empirical comparison study.

  - `evaluation_code_and_results` contains the code for the evaluation of the raw results of the empirical comparison study in order to reproduce the final figures and Table 3 presented in the paper and supplement. More specifically, `evaluation.R` allows all figures to be reproduced, `table3.R` reproduces Table 3, and `tableS1.R` reproduces Table S1. Note that this does not require the entire empirical comparison study to be reproduced, as the raw results are included in this repository and, as mentioned above, the evaluation code uses the raw results.
