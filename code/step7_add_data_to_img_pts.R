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

all_img_vars <- paste('image_vars/', sceneid, '/complete_img_with_vars_', sceneid, '_block', block, '.csv', sep = '')
classified_img_data_spat <- paste('detection/', '/classified_img_data_spatial_', sceneid, '_block', block, '.csv', sep = '')
classified_img_data_temp <- paste('detection/', '/classified_img_data_temporal_', sceneid, '_block', block, '.csv', sep = '')
output_new_vars_spat <- paste('image_vars/', sceneid, '/complete_img_with_attvars_spatial_', sceneid, '_block', block, '.csv', sep = '')
output_new_vars_temp <- paste('image_vars/', sceneid, '/complete_img_with_attvars_temporal_', sceneid, '_block', block, '.csv', sep = '')

# load packages -----------------------------------------------------------

library(tidyverse)
library(raster)
library(ggmap)
library(parallel)
library(data.table)
source(paste(code_path, '/slope_functions.R', sep = ''))
source(paste(code_path, '/add_img_data_funcs.R', sep = ''))

# read in image data ------------------------------------------------------

print(paste('Reading in data for block ', block, '.', sep = ''))

splits_data <- fread(paste(output_path, all_img_vars, sep = ''))
splits_data$longitude <- signif(splits_data$longitude, digits = 7)
splits_data$latitude <- signif(splits_data$latitude, digits = 7)
img_data_spat <- fread(paste(output_path, classified_img_data_spat, sep = ''), header = T, sep = ',')
img_data_temp <- fread(paste(output_path, classified_img_data_temp, sep = ''), header = T, sep = ',')
img_data_spat$longitude <- signif(img_data_spat$longitude, digits = 7)
img_data_spat$latitude <- signif(img_data_spat$latitude, digits = 7)
img_data_temp$longitude <- signif(img_data_temp$longitude, digits = 7)
img_data_temp$latitude <- signif(img_data_temp$latitude, digits = 7)

print('Done reading in data.')

# add splits to classified data -------------------------------------------

img_data_spat <- splits_data[img_data_spat, , on = c('longitude', 'latitude')]
img_data_spat <- img_data_spat[,(names(img_data_spat)[grep('^i', names(img_data_spat))]) := NULL]
img_data_temp <- splits_data[img_data_temp, , on = c('longitude', 'latitude')]
img_data_temp <- img_data_temp[,(names(img_data_temp)[grep('^i', names(img_data_temp))]) := NULL]

# add variables to data -----------------------------------------------

# run function
print('Adding attribution variables...')

img_data_spat <- add_att_vars(img_data = img_data_spat, num_cores = n_cores)
write.table(img_data_spat, paste(output_path, output_new_vars_spat, sep = ''), row.names = FALSE, col.names = TRUE, sep = ',')
img_data_temp <- add_att_vars(img_data = img_data_temp, num_cores = n_cores)
write.table(img_data_temp, paste(output_path, output_new_vars_temp, sep = ''), row.names = FALSE, col.names = TRUE, sep = ',')

print(paste('Finished adding attribution variables for scene ', sceneid, '.', sep = ''))
