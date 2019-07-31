# step 2 ------------------------------------------------------------------
# get raw image data for all points


# set main path -----------------------------------------------------------

data_path <- "~/disturbr/data/"
code_path <- '~/disturbr/code/'

# get job submission variable arguments -----------------------------------

# arguments:
# sceneid (e.g., p47_r27)
# idxname (e.g., ndvi)
args <- (commandArgs(TRUE))

sceneid <- as.character(args[1])
idxname <- as.character(args[2])

# data names --------------------------------------------------------------

img_file <- paste('images/', sceneid, '/summer_', idxname, '_', sceneid, '_1984_2000_30m.tif', sep = '')
forest_file <- paste('images/', sceneid, '/tree_cover_', sceneid, '_2000_30m.tif', sep = '')
out_raw <- paste('image_data/', sceneid, '/img_data_uncondensed_', sceneid, '_1984_2000.csv', sep = '')
index_img_data <- paste('image_data/', sceneid, '/data_all_pix_', sceneid, '_1984_2000.csv', sep = '')
out_fill_img <- paste('images/', sceneid, '/fill_image_', sceneid, '_1984_2000.tif', sep = '')

# load packages -----------------------------------------------------------

library(dplyr)
library(raster)
library(data.table)
source(paste(code_path, 'create_img_data_func.R', sep = ''))

# run function ------------------------------------------------------------

create_img_data(data_path = data_path, 
                img_file = img_file,
                forest_file = forest_file,
                out_fill_img = out_fill_img,
                out_raw = out_raw,
                index_img_data = index_img_data,
                num_blocks = 9, 
                plot = FALSE, 
                start_yr = 1984, 
                end_yr = 2000, 
                for_cov_min = 20, 
                index_name = 'ind')
