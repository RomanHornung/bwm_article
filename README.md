# BMW-Paper
**Prediction approaches for partly missing multi-omics covariate data: A literature review and an empirical comparison study**   
Roman Hornung, Frederik Ludwigs, Jonas Hagenberg, Anne-Laure Boulesteix

This repository was written largely by Frederik Ludwigs and Jonas Hagenberg.

## Project description
This project compares different approaches capable to deal with block-wise missingness in Multi-Omics data.  
The approaches are either based on the random-forest or on the penalised-regression approach - this repository focuses on the random-forest based approaches.     

## Block-wise missingness:
- Block-wise missingness is a special type of missingness that appears frequently in the context of Multi-Omics data - for example when concatenating data from multiple clinical studies. Even though the different studies have the same target variable, the observed features can still differ! The concatenation of such datasets results then in a DF with block-wise missingness!  

- Regular model fitting on data with block-wise missingness is for most approaches not directly possible, such that either the method needs to be adjusted or the data processed! 

- Besides the train-data, also the test-data can consist of block-wise missingness. Therefore the approaches must be able to deal with block-wise missing data in the test- as well as in the train-data.  

### Example for data with blockwise missingness:
Data with blockwise missingness always consists of different **folds** and **blocks**.  
  - A **block** describes a set of covariates containing all features collected based on a characteristic  
    &#8594; All covariates that are related in content - the given example has three blocks:  
     - **Physical properties:**     Weight, Height
     - **Educational properties:**  Income, Education
     - **Biological properties:**   g1, ..., g100
  - A **fold** represents a set of observations with the same observed blocks.  
    &#8594; All observations with the same observed features - the given example has three folds:   
     - **Fold1:** All observations with observed Physical & Educational properties
     - **Fold2:** All observations with observed Educational & Biological properties
     - **Fold3:** All observations with observed Physical & Biological properties
  
| ID  | Weight  | Height  | Income  | Education   | g1      | ...   | g100    | Y   |
|---- |-------- |-------- |-------- |-----------  |-------  |-----  |-------  |---  |
| 1   | 65.4    | 187     | 2.536   | Upper       |         |       |         | 1   |
| 2   | 83.9    | 192     | 1.342   | Lower       |         |       |         | 0   |
| 3   | 67.4    | 167     | 5.332   | Upper       |         |       |         | 1   |
| 4   |         |         | 743     | Lower       | -0.42   | ...   | 1.43    | 1   |
| 5   |         |         | 2.125   | Lower       | 0.52    | ...   | -1.37   | 0   |
| 6   | 105.2   | 175     |         |             | -1.53   | ...   | 2.01    | 0   |
| 7   | 71.5    | 173     |         |             | 0.93    | ...   | 0.53    | 0   |
| 8   | 73.0    | 169     |         |             | 0.31    | ...   | -0.07   | 1   |
  
## Data   
* The data comes from the The Cancer Genome Atlas *(TCGA)* and each dataset consits of multiple omics-blocks
* The provided data doesn't contain any missing values, such that the blockwise-missingness needs to be induced manually   
* Each data-set uses the 'TP53'-Mutation as response and consits of four further omics-blocks 'clinical', 'copy number variation', 'miRNA' & 'RNA'

## Evaluation Results  
For each approach, there is a seperate folder in 'Docs/Evaluation_Results/' that contains the results of the evaluation.  
It contains all the settings of the evaluation *(e.g. seeds, train-pattern, test-pattern, ...)* and the metrics *(e.g. Accuracy, F1, BrierScore, ...)*.  
If a **known problem** occurs *(e.g. CC-Approach: no common blocks in train & test)* there can be no metrics, but the block-order in test- & train-set will be included.  
In case an **unknown problem** occurs the block-order in test- & train-set will not be included in the results! So this can be used to track down unexpected behavior!  

## Code  
This section contains short descriptions to the scripts in 'Code/' - there is an logical order in these scripts!  

#### [1] 00_Inspect_raw_data.R
    - Get a overview to the files in 'Data/Raw'
      - do they contain all necessary blocks (clin, mirna, mutation, cnv, rna)
      - is the response 'TP53' part of the 'mutation' block
      - get the amount of features per block & total observations for the DF  
      - check if there are any variables with missing values
    - Collect the amount of features per block for each DF, get the amount of
      observations, the fraction of pos. reponse-classes and collect it all in
      a DF - resulting DF saved to 'Docs/raw_data_overview'  

#### [2] 01_Create_BWM_Pattern.R
    - All files in data/raw are fully observed & do not contain missing values  
    - Define functions, to load the data, split it to test- & train-Set & induce the different BWM-Pattern  
    - Based on the resulting data, the various approaches can be evaluated then  

#### [3] 02_Complete_Case_Approach.R
    - Evaluate the Complete-Case approach on data with BWM  
    - Remove all blocks from the train-set that are not available in the test-set
    - Only keep completly observed cases in the train-set & train a RF with it
    - Use this RF to predict on the test-set then & collect common metrics  
    - Results of the evaluation are stored in 'Docs/Evaluation_Results/CC_Approach'

#### [4] 03_Single_Block_Approach.R 
    - Evaluate the Single_Block approach on the data with BWM   
    - Fit a seperate RF on each block that train- & test-set have in common
    - Evaluate each of these RFs with the oob-AUC 
    - Use the RF with the highest oob-AUC to predict on the test-set set then & collect common metrics   
    - Results of the evaluation are stored in 'Docs/Evaluation_Results/SB_Approach'

#### [5] 04_Imputation_Approach.R 
    - Evaluate the Imputation approach on the data with BWM 
    - Impute the missing values in the train-set with the TOBMI-Imputation method
    - Remove all blocks from the imputed data that are not available in the test-set
    - Train a RF on the remaining train-set 
    - Use this RF to predict on the test-set then & collect common metrics   
    - Results of the evaluation are stored in 'Docs/Evaluation_Results/IMP_Approach'

#### [6] 05_Blockwise_Approach.R 
    - Evaluate the Blockmwise approach on the data with BWM 
    - Fit a RF seperatly on each block that train- & test-set have in common 
    - Evaluate each RF internally with the oob-AUC
    - Predict on the test-set, by creating a weighted average of the predicitons
      from the block-wise fitted RFs 
    - Use the oob-AUC for weighting the predicitons of the block-wise fitted RFs 
    - Based on predicted & true classes, calculate common metrics   
    - Results of the evaluation are stored in 'Docs/Evaluation_Results/BW_Approach'

#### [7] 06_Foldwise_Approach.R 
    - Evaluate the Foldmwise approach on the data with BWM 
    - Implementation for the specific functions etc. is in '06_1_simpleRF_adaption'
    - Fit a RF seperatly on each fold of the train-set
    - Prune each foldwise fitted RF in regards to the test-set 
    - Evaluate each of the pruned RF internally with the oob-AUC
    - Predict on the test-set, by creating a weighted average of the predicitons
      from the fold-wise fitted RFs 
    - Use the oob-AUC for weighting the predicitons of the fold-wise fitted RFs 
    - Based on predicted & true classes, calculate common metrics   
    - Results of the evaluation are stored in 'Docs/Evaluation_Results/FW_Approach'

#### [8] 07_check_eval_results.R  
    - Check the qulaity of th evaluation results 
    - Ensure there are not unexpected results due to unexpected behavior

#### [9] 08_FW_Approach_as_package.R
    - Basisis for the implemention of the foldwise-RF approach as R-package 

## Folder-Structure  
```
├── README.md <- Top-level README for devs working with this repository
│ 
├── Data <- All the data for this repository
│   │   
│   ├─── raw          <- 13 Multi-Omics DFs from TCGA     
│   └─── Example_Data <- Examplary data needed for the implementations of the approaches  
│  
├── Docs <- Sources, Results and everything else documenting the repository  
│   │  
│   ├─── Article_Versions   <- Different Versions of the article 
│   ├─── raw_data_overview  <- Overview to amount of rows & features per block for each DF in Data/Raw
│   └─── Evaluation_Results <- Results of the evaluation for all approaches (sub-folder for each approach)  
│
└── Code <- Code of the repository
    │
    ├── 00_Inspect_raw_data.R
    ├── 01_Create_BWM_Pattern.R
    ├── 02_Complete_Case_Approach.R
    ├── 03_Single_Block_Approach.R 
    ├── 04_Imputation_Approach.R 
    ├── 05_Blockwise_Approach.R 
    ├── 06_Foldwise_Approach.R 
    ├── 06_1_simpleRF_adaption.R
    ├── 07_check_eval_results.R
    ├── 08_FW_Approach_as_package.R
    └── Example  <- Templates, Examples & scripts to reproduce bugs
```