# Set the working directory to the directory 'bwm_article' 
# of the electronic appendix:

library("this.path")
setwd(gsub("/compstudy_code_and_results/code", "", this.dir()))

library(dplyr)

esca_1 <- read.csv("./compstudy_code_and_results/results/pl_approach/PL_Eval_ESCA_old.csv")
esca_2 <- read.csv("./compstudy_code_and_results/results/pl_approach/PL_Eval_ESCA_2_4_3.csv")
esca_3 <- read.csv("./compstudy_code_and_results/results/pl_approach/PL_Eval_ESCA_4_4_3.csv")

blca <- read.csv("./compstudy_code_and_results/results/pl_approach/PL_Eval_BLCA.csv")

fill_1 <- blca %>% 
  filter(train_pattern == 2, test_pattern == 4, repetition == 2) %>% 
  mutate(across(-c(X, path, train_pattern, test_pattern, repetition, approach), ~NA),
         path = "./compstudy_code_and_results/data/ESCA.Rda")

fill_2 <- blca %>% 
  filter(train_pattern == 4, test_pattern == 4, repetition == 2) %>% 
  mutate(across(-c(X, path, train_pattern, test_pattern, repetition, approach), ~NA),
         path = "./compstudy_code_and_results/data/ESCA.Rda")

esca_total <- rbind(esca_1, fill_1, esca_2, fill_2, esca_3)
write.csv(esca_total, "./compstudy_code_and_results/results/pl_approach/PL_Eval_ESCA.csv")

paad_1 <- read.csv("./compstudy_code_and_results/results/pl_approach/PL_Eval_PAAD_old.csv")
paad_2 <- read.csv("./compstudy_code_and_results/results/pl_approach/PL_Eval_PAAD_4_4_5.csv")

fill_3 <- blca %>% 
  filter(train_pattern == 4, test_pattern == 4, repetition == 4) %>% 
  mutate(across(-c(X, path, train_pattern, test_pattern, repetition, approach), ~NA),
         path = "./compstudy_code_and_results/data/PAAD.Rda")

paad_total <- rbind(paad_1, fill_3, paad_2)
write.csv(paad_total, "./compstudy_code_and_results/results/pl_approach/PL_Eval_PAAD.csv")
