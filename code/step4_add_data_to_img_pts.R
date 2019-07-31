# step 3 ------------------------------------------------------------------
# add potential spatial and temporal variables to all pixels in image


# set main path -----------------------------------------------------------

data_path <- "~/disturbr/data/"
code_path <- '~/disturbr/code/'
output_path <- "~/disturbr/data/"

# array job arguments -----------------------------------------------------

# should be same number of tasks as blocks
block <- as.character(Sys.getenv('SLURM_ARRAY_TASK_ID'))
n_cores <- 4

args <- (commandArgs(TRUE))
sceneid <- as.character(args[1]) # i.e., p47_r27

# names of data files -----------------------------------------------------

ind_img_data <- paste('image_data/', sceneid, '/data_all_pix_', sceneid, '_1984_2000_block', block, '.csv', sep = '')
raw_img_data <- paste('image_data/', sceneid, '/all_img_data_uncondensed_', sceneid, '_block', block, '_1984_2000.csv', sep = '')
all_img_vars <- paste('image_vars/', sceneid, '/complete_img_with_vars_', sceneid, '_block', block, '_1984_2000.csv', sep = '')

# load packages -----------------------------------------------------------

library(tidyverse)
library(rpart)
library(randomForest)
library(caret)
library(raster)
library(ggmap)
library(parallel)
library(strucchange)
library(data.table)
source(paste(code_path, '/slope_functions.R', sep = ''))
source(paste(code_path, '/add_img_data_funcs.R', sep = ''))

# set google api key ------------------------------------------------------

api_key <- #insert as character

# read in image data ------------------------------------------------------

print(paste('Reading in data for block ', block, '.', sep = ''))

all_data <- fread(paste(data_path, ind_img_data, sep = ''), header = TRUE, sep = ',')
all_data$year <- as.numeric(all_data$year)
all_data <- data.table(all_data)
gc()

print('Done reading in data.')

# add variables to data -----------------------------------------------

print('Adding basic variables...')
plot_data <- add_basic_vars(data = all_data, out_data_name = 'plot_data')

# data calculated with splits for faster processing
splits_data <- split_data(data = all_data, plot_data = plot_data, num_splits = 1000)
list_splits <- splits_data[[1]]
plot_data <- splits_data[[2]]
all_data <- splits_data[[3]]

print('Adding TS slope variables...')
plot_data <- add_TS_slope(list_of_splits = list_splits, plot_data = plot_data, num_cores = n_cores)
print('Adding RPART variables...')
plot_data <- add_rpart_vars(list_of_splits = list_splits, plot_data = plot_data, num_cores = n_cores)
print('Adding raw slope variables...')
plot_data <- add_raw_slope_vars(list_of_splits = list_splits, plot_data = plot_data, num_cores = n_cores)

# write out here to make sure that everything is ok before calculating spatial variables
write.table(plot_data, paste(output_path, all_img_vars, sep = ''), row.names = FALSE, col.names = TRUE, sep = ',')

# add loess variables -----------------------------------------------------

# add empty vars for each point in dataframe
slope_data <- data.table(longitude = plot_data$longitude,
                         latitude = plot_data$latitude,
                         splits = plot_data$splits,
                         min_slope = 0,
                         max_slope = 0,
                         avg_slope = 0,
                         var_slope = 0,
                         date_min = 0,
                         smooth_range = 0,
                         pre_var = 0,
                         post_var = 0,
                         var_ratio = 0,
                         dist_date = 0,
                         dur = 0,
                         mag = 0)

# run function
print('Adding loess variables...')
plot_data <- add_loess_vars(data = all_data, plot_data = plot_data,
                            slope_data = slope_data, num_splits = max(all_data$splits), num_cores = n_cores)

# write out
write.table(plot_data, paste(output_path, all_img_vars, sep = ''), row.names = FALSE, col.names = TRUE, sep = ',')

# get spatial variables ---------------------------------------------------

#plot_data <- fread(paste(output_path, all_img_vars, sep = ''), sep = ',', header = TRUE)

# change variables that might contain Inf, -Inf, or NA
plot_data$var_ratio[is.na(plot_data$var_ratio)] <- 1
plot_data$mag[is.na(plot_data$mag)] <- 0
plot_data$dur[is.na(plot_data$dur)] <- 0

# run function
print('Adding spatial variables...')
plot_data <- add_spatial_vars(data = all_data, plot_data = plot_data, 
                              slope_data = slope_data, num_splits = max(all_data$splits), num_cores = n_cores)
  
# write out
write.table(plot_data, paste(output_path, all_img_vars, sep = ''), row.names = FALSE, col.names = TRUE, sep = ',')