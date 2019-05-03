# step 8 ------------------------------------------------------------------
# train random forest model using training points (attribution)


# set main path -----------------------------------------------------------

data_path <- "~/disturbr/data/"
code_path <- '~/disturbr/code/'
output_path <- "~/disturbr/data/"

# read in environmental variables -----------------------------------------

# should be same number of tasks as scenes
sceneidx <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
sceneid <- list('p47_r27', 'p45_r30', 'p35_r32', 'p27_r27', 'p12_r28', 'p14_r32', 'p16_r37')[[sceneidx]]

args <- (commandArgs(TRUE))
training_blocks <- as.character(args[1]) # i.e., "357"
training_blocks <- unlist(strsplit(training_blocks, split = ""))
n_blocks <- as.numeric(args[2])

# load packages -----------------------------------------------------------

library(tidyverse)
library(rpart)
library(randomForest)
library(caret)
library(raster)
library(ggmap)
library(parallel)
library(data.table)
source(paste(code_path, 'slope_functions.R', sep = ''))
source(paste(code_path, 'attribution_model_func.R', sep = ''))
source(paste(code_path, 'attribution_model_setup.R', sep = ''))
source(paste(code_path, 'classify_attribution_image.R', sep = ''))

# find models -------------------------------------------------------------

spatial_results <- attribution_model_setup(training_blocks = training_blocks, 
                                           n_blocks = n_blocks, 
                                           sceneid = sceneid,
                                           model_type = 'spatial', 
                                           data_path = data_path, 
                                           output_path = output_path, 
                                           code_path = code_path)
temporal_results <- attribution_model_setup(training_blocks = training_blocks, 
                                            n_blocks = n_blocks, 
                                            sceneid = sceneid,
                                            model_type = 'temporal', 
                                            data_path = data_path, 
                                            output_path = output_path, 
                                            code_path = code_path)

print(paste('Finished with models for scene ', sceneid, '.', sep = ''))


