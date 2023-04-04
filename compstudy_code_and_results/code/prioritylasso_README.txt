For priority-LASSO, reproducing the results is more complex than for the other methods.
The procedure is described below.



###################################################################################
# Create a Singularity container image
###################################################################################

The evaluation of the different approaches was carried out by two people.
Priority-LASSO and mdd-sPLS were evaluated by Jonas Hagenberg and 
the other approaches by Frederik Ludwigs. To ensure that both people used the same
R configurations, Jonas Hagenberg used a so-called Singularity container.

The file "r_4_0_3_with_ddspls_prioritylasso.def" is a Singularity definition file. 
This file contains instructions for building a Singularity container image, including 
the required version of R and any required R packages. 
To use this definition file, you must first install Singularity on your machine.
Once Singularity is installed, you can build a Singularity container image using the 
command:

singularity build container_name.sif r_4_0_3_with_ddspls_prioritylasso.def

Replace container_name.sif with a desired name for your Singularity container image. 
This command creates a container image file with the specified name and the environment 
defined in the definition file.

Once the container image has been created, you can run R scripts inside the container 
using the following command:

singularity exec container_name.sif Rscript your_script.R

where "your_script.R" would be the name of the respective R script.

Note that an alternative to all of the above would be to simply install
R version 4.0.3 on your computer and use the R packages listed in the
file "r_4_0_3_with_ddspls_prioritylasso.def".



###################################################################################
# Running the R code
###################################################################################

The results are obtained separately for each data set.

1) The R script "prioritylasso_approach.R" must be run from the command line. 
To use this script, you should give the data set name as a command line argument. 
For example, the results for the BLCA data set can be obtained as follows:

singularity exec container_name.sif Rscript prioritylasso_approach.R "BLCA"

(If you are not using a Singularity container, it would simply be 
Rscript prioritylasso_approach.R "BLCA")

This will save the file "PL_Eval_BLCA.csv" in the directory 
"bwm_article/compstudy_code_and_results/results/pl_approach".

2) Errors occurred in some repetitions of the data sets "ESCA" and "PAAD". 
This resulted in the "prioritylasso_approach.R" script not being completed.
In these cases, the "prioritylasso_approach_fine_control.R" script was used to
to continue the calculations after the iterations that caused the errors.
For example, for the PAAD data set an error occurred for curr_train_pattern==4 & 
curr_test_pattern==4 & curr_repetition==4. Therefore, prioritylasso_approach.R
stopped at this iteration. We then used "prioritylasso_approach_fine_control.R" 
to restart the calculations at curr_train_pattern==4 & curr_test_pattern==4 & curr_repetition==5
to compute the rest of the results (there were no errors in the rest of the loop).
This is done as follows

singularity exec container_name.sif Rscript prioritylasso_approach_fine_control.R "PAAD" 4 4 5

(i.e. singularity exec container_name.sif Rscript prioritylasso_approach_fine_control.R 
dataset_name curr_train_pattern curr_test_pattern curr_repetition). 
(If you are not using a singularity container, it would just be 
Rscript prioritylasso_approach_fine_control.R "PAAD" 4 4 5)

This will produce the file "PL_Eval_PAAD_4_4_5.csv" in the directory 
"bwm_article/compstudy_code_and_results/results/pl_approach".

3) As described in the last step, there were errors for the data sets "ESCA" and 
"PAAD", which is why the results for these data sets were saved in several csv
files (see (2) for details).
The R script "prioritylasso_approach_combine_results.R" can be used to merge
these csv files into a single csv file containing the results for all replicates.
Before running this file, "PL_Eval_PAAD.csv" must be renamed to "PL_Eval_PAAD_old.csv" 
and "PL_Eval_ESCA.csv" must be renamed to "PL_Eval_ESCA_old.csv".
