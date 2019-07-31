# step 5 ------------------------------------------------------------------
# classify sample points within detected disturbance areas

# set main path -----------------------------------------------------------

data_path <- '~/disturbr/data/'
code_path <- '~/disturbr/code/'

# data names --------------------------------------------------------------

# will want to run once for spatial, once for temporal (50 new pts each), for each scene
# adding 50 new sample points to spatial, and then finding proportion sampled and
# ensuring same sample proportion for temporal (12/14/2018)

img_file <- 'images/yellowstone/summer_ndvi_yellowstone_2000_2016_30m.tif'
classified_data <- 'image_vars/yellowstone/complete_img_with_attvars_spatial_yellowstone_block7_2000_2016.csv'
img_data <- 'image_data/yellowstone/data_all_pix_yellowstone_2000_2016_block7.csv'
training_points <- 'training_points/img_training_attribution_yellowstone_block7.csv'
det_training_points <-   '/training_points/img_training_yellowstone_block7_2000_2016.csv'

# new training name
temp_training_points <- 'training_points/img_training_attribution_yellowstone_block7.csv'
# load temp predictions
temp_classified_data <- 'image_vars/yellowstone/complete_img_with_attvars_yellowstone_block7.csv'

# load packages -----------------------------------------------------------

library(tidyverse)
library(rpart)
library(raster)
library(ggmap)
library(data.table)
source(paste(code_path, 'slope_functions.R', sep = ''))
source(paste(code_path, 'classify_training_pts_func.R', sep = ''))
source(paste(code_path, 'add_training_pts_func.R', sep = ''))

# set google api key ------------------------------------------------------

api_key <- #insert as character
register_google(key = api_key)
  
# classify detection points --------------------------------------------

# for temporal model, first check how many points overlap with spatial model dataset
# number of new sample points indicates which need to be trained (1 - # new)
new_training_path <- new_training(data_path = data_path, img_data = img_data,
                                  var_data = temp_classified_data,
                                  other_var_data = classified_data,
                                  old_training_points = training_points,
                                  new_training_points = temp_training_points,
                                  index_name = 'ind', start_yr = 2000, end_yr = 2016)

# DO NOT want to remove the 0's for attribution training - they're important (the function will check for this)
# this also saves the data to defined location as well

# for spatial training, prev_add = TRUE, restart = FALSE, training_points = training_points, var_data = classified_data
# for temporal training, prev_add = FALSE, restart = TRUE, training_points = new_training_path, var_data = temp_classified_data
sample_points <- classify_points(data_path = data_path, img_file = img_file,
                                 var_data = classified_data, img_data = img_data,
                                 training_points = training_points, prev_training = det_training_points,
                                 type = 'attribution', prev_add = TRUE,
                                 index_name = 'ind', start_yr = 2000, end_yr = 2016,
                                 valid_values = seq(0, 9), sample_num = 50,
                                 restart = FALSE, start_ind = 1, remove_bad = FALSE)
# re-run:

# codes:
# 1 = false positive or unsure
# 2 = biotic
# 3 = fire
# 4 = harvest
# 5 = wind
# 6 = ice
# 7 = flood, water
# 8 = land use change (e.g., construction, mining)
# 9 = landslide, avalanche
# 0 = unknown disturbance or remove
