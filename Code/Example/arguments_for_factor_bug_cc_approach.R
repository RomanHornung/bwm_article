curr_path = "./Data/Raw/ESCA.Rda"
curr_train_pattern = 5
curr_test_pattern = 3
curr_repetition   = 1

set.seed(3097050)    
seeds <- round(runif(4, 0, 100000))

# Use these 'seeds' to set the four necessary seeds for the evaluation:
#     1. Seed to split data into test & train
curr_split_seed <- seeds[1]

#     2. Seed to shuffle the block order in 'train'
curr_block_seed_train <- seeds[2]

#     3. Seed to shuffle the block order in 'test'
curr_block_seed_test <- seeds[3]

#     4. Seed for the train-pattern (assignment of obs. in train to folds)
curr_train_pattern_seed <- seeds[4]




path               = curr_path
frac_train         = 0.75
split_seed         = curr_split_seed
block_seed_train   = curr_block_seed_train 
block_seed_test    = curr_block_seed_test
train_pattern      = curr_train_pattern
train_pattern_seed = curr_train_pattern_seed 
test_pattern       = curr_test_pattern