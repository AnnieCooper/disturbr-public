# step 2b ------------------------------------------------------------------
# classify sample training points for disturbance detection


# set main path -----------------------------------------------------------

data_path <- "~/disturbr/data/"
code_path <- '~/disturbr/code/'

# data names --------------------------------------------------------------

img_file <- 'images/yellowstone/summer_ndvi_yellowstone_2000_2016_30m.tif'
img_data <- 'image_data/yellowstone/data_all_pix_yellowstone_2000_2016_block5.csv'
training_points <- 'training_points/img_training_yellowstone_block5_2000_2016.csv'

# load packages -----------------------------------------------------------

library(tidyverse)
library(rpart)
library(raster)
library(ggmap)
library(data.table)
source(paste(code_path, 'classify_training_pts_func.R', sep = ''))

# set google api key ------------------------------------------------------

api_key <- #insert as character

# define codes for disturbance classification -----------------------------

# in an effort to limit training, we will do more points, but only do them once
# we will intersect these points with those that were labeled as disturbed and use that
# for attribution training (note that we will likely need lots more points as a result!)

# statistically speaking...can only use random points, not clusters, and for attribution
# we want roughly equal numbers for each type of disturbance (not always possible, but try)

# codes: 
# 0 = remove (bad for training)
# 1 = no apparent dist (or false positive for attribution)
# 2 = biotic
# 3 = fire
# 4 = harvest
# 5 = wind
# 6 = ice
# 7 = flooding
# 8 = land use change (e.g., construction, mining)
# 9 = landslide, avalanche

# run function ------------------------------------------------------------

# we will be doing 200 for each block x 3 blocks per scene = 600 total training points!

# this also saves the data to defined location as well
sample_points <- classify_points(data_path = data_path, img_file = img_file, img_data = img_data, 
                                 training_points = training_points, type = 'detection',
                                 index_name = 'ind', start_yr = 2000, end_yr = 2016,
                                 valid_values = seq(0, 9), sample_num = 100,
                                  restart = TRUE, start_ind = 76, remove_bad = FALSE,
                                 api_key = api_key)
