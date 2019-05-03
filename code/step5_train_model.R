# step 4 ------------------------------------------------------------------
# train random forest model using training points


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

# names of data files -----------------------------------------------------

# initialize lists
training_points <- list()
mod_data_spat <- paste('/importance/detection_model_importance_spatial_', sceneid, '.csv', sep = '')
mod_data_temp <- paste('/importance/detection_model_importance_temporal_', sceneid, '.csv', sep = '')
all_img_vars <- list()
fill_image <- list()
output_raster_spat <- list()
output_raster_temp <- list()
classified_data_spat <- list()
classified_data_temp <- list()

i <- 1
# training file names
while (i <= length(training_blocks)) {
  training_points[[i]] <- paste('/training_points/img_training_', sceneid, '_block', training_blocks[i], '.csv', sep = '')
  i <- i + 1
}
i <- 1
# all file names
while (i <= n_blocks) {
  all_img_vars[[i]] <- paste('complete_img_with_vars_', sceneid, '_block', i, '.csv', sep = '')
  fill_image[[i]] <- paste('images/', sceneid, '/fill_image_', sceneid, '_block', i, '.tif', sep = '')
  output_raster_spat[[i]] <- paste('/detection/disturbance_date_img_spatial_', sceneid, '_block', i, '.tif', sep = '')
  classified_data_spat[[i]] <- paste('/detection/classified_img_data_spatial_', sceneid, '_block', i, '.csv', sep = '')
  output_raster_temp[[i]] <- paste('/detection/disturbance_date_img_temporal_', sceneid, '_block', i, '.tif', sep = '')
  classified_data_temp[[i]] <- paste('/detection/classified_img_data_temporal_', sceneid, '_block', i, '.csv', sep = '')
  i <- i + 1
}

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
source(paste(code_path, 'detection_model_func.R', sep = ''))
source(paste(code_path, 'classify_detection_image.R', sep = ''))

# import all necessary data -----------------------------------------------

# training point locations and classes
i <- 1
while (i <= length(training_blocks)) {
  temp <- fread(paste(data_path, training_points[[i]], sep = ''))
  temp$longitude <- signif(temp$longitude, digits = 10)
  temp$latitude <- signif(temp$latitude, digits = 10)

  # clean up training point disturbance classes
  temp <- temp[temp$disturbance != 0, ] # 0 class stands for unclear (remove)
  temp$disturbance <- as.factor(temp$disturbance)

  if (i == 1) {
    pt_locs <- temp
  } else {
    pt_locs <- rbind(pt_locs, temp)
  }

  i <- i + 1
}

# variables for all img points (training blocks)
i <- 1
while (i <= length(training_blocks)) {
  temp <- fread(paste(output_path, 'image_vars/', sceneid, '/', all_img_vars[as.numeric(training_blocks[i])], sep = ''))
  temp$longitude <- signif(temp$longitude, digits = 10)
  temp$latitude <- signif(temp$latitude, digits = 10)
  temp <- temp[, splits := NULL]

  if (i == 1) {
    img_data <- temp
  } else {
    img_data <- rbind(img_data, temp)
  }

  i <- i + 1
}

# variables for all img points (training blocks)
img_data_blocks <- list()
orig_data <- list()
i <- 1
while (i <= n_blocks) {
  img_data_blocks[[i]] <- fread(paste(output_path, 'image_vars/', sceneid, '/', all_img_vars[i], sep = ''))
  img_data_blocks[[i]]$longitude <- signif(img_data_blocks[[i]]$longitude, digits = 7)
  img_data_blocks[[i]]$latitude <- signif(img_data_blocks[[i]]$latitude, digits = 7)
  img_data_blocks[[i]] <- img_data_blocks[[i]][, splits := NULL]

  # save unscaled data for use later on
  orig_data[[i]] <- img_data_blocks[[i]]
  orig_data[[i]] <- orig_data[[i]][, splits := NULL]

  i <- i + 1
}

# join to create single dataframe with vars for all training points
data <- img_data[pt_locs, , on = c('longitude', 'latitude', 'avg_index')]
data <- as.data.frame(data)
data <- distinct(data, longitude, latitude, avg_index, .keep_all = TRUE)

# re-order columns so disturbance is third, followed by all of the training variables
setcolorder(data, c(1:2, ncol(data), 3:(ncol(data) - 1)))

# change values so that anything 2 or above is disturbance
data$disturbance <- as.numeric(as.character(data$disturbance))
data$disturbance[data$disturbance >= 2] <- 2

# report disturbed/undisturbed counts
cat('Disturbed pixels: ', sum(data$disturbance == 2),
      '\nUndisturbed pixels: ', sum(data$disturbance == 1),
    '\nPercent disturbed: ', (sum(data$disturbance == 2) / nrow(data)) * 100, '%',
    '\n')

# codes:
# 1 = undisturbed
# 2 = disturbance

# scale data --------------------------------------------------------------

# does not seem to be necessary, but this is good practice
#cols <- names(data)[4:ncol(data)]
#data[, (cols) := lapply(.SD, scale), .SDcols = cols]

# search for clustering with variables ------------------------------------

# convert all NA, Inf, and -Inf values
data$variance[is.na(data$variance)] <- 0
data$mad[is.na(data$mad)] <- 0
data$range[is.na(data$range)] <- 0
data$min_index[is.na(data$min_index)] <- 0
data$max_index[is.na(data$max_index)] <- 0
data$slope[is.na(data$slope)] <- 0
data$min_tree_slope[is.na(data$min_tree_slope)] <- 0
data$max_tree_slope[is.na(data$max_tree_slope)] <- 0
data$date_min_slope[is.na(data$date_min_slope)] <- 2000
data$avg_raw_slope[is.na(data$avg_raw_slope)] <- 0
data$var_raw_slope[is.na(data$var_raw_slope)] <- 0
data$min_raw_slope[is.na(data$min_raw_slope)] <- 0
data$min_slope[is.na(data$min_slope)] <- 0
data$max_slope[is.na(data$max_slope)] <- 0
data$avg_slope[is.na(data$avg_slope)] <- 0
data$var_slope[is.na(data$var_slope)] <- 0
data$date_min[is.na(data$date_min)] <- 2000
data$smooth_range[is.na(data$smooth_range)] <- 0
data$s_break_year_range[is.na(data$s_break_year_range)] <- 0
data$s_mag_avg[is.na(data$s_mag_avg)] <- 0
data$s_mag_range[is.na(data$s_mag_range)] <- 0
data$s_dur_avg[is.na(data$s_dur_avg)] <- 0
data$s_dur_range[is.na(data$s_dur_range)] <- 0
data$pre_var[is.na(data$pre_var)] <- 0
data$post_var[is.na(data$post_var)] <- 0
data$var_ratio[is.na(data$var_ratio)] <- 1
data$mag[is.na(data$mag)] <- 0
data$dur[is.na(data$dur)] <- 0
data$s_mag_ratio[is.na(data$s_mag_ratio)] <- 0
data$s_dur_ratio[is.na(data$s_dur_ratio)] <- 0
data$s_dur_ratio[data$s_dur_ratio == Inf] <- 0
data$s_dur_ratio[data$s_dur_ratio == -Inf] <- 0
data$dist_date[is.na(data$dist_date)] <- 0
data$s_break_year_range[data$s_break_year_range == Inf] <- 0
data$s_min_year_ranges[is.na(data$s_min_year_ranges)] <- 0
data$s_var_ratio_ratio[is.na(data$s_var_ratio_ratio)] <- 1
data$s_var_ratio_range[is.na(data$s_var_ratio_range)] <- 0
data$mad_mean_ratio[is.na(data$mad_mean_ratio)] <- 1
data$s_avg_slope_range[is.na(data$s_avg_slope_range)] <- 0
data$s_max_slope_range[is.na(data$s_max_slope_range)] <- 0
data$s_min_slope_range[is.na(data$s_min_slope_range)] <- 0
data$s_var_ratio_avg[is.na(data$s_var_ratio_avg)] <- 1
data$s_avg_slope_ratio[is.na(data$s_avg_slope_ratio)] <- 1
data$s_avg_slope_avg[is.na(data$s_avg_slope_avg)] <- 0
data$s_max_slope_ratio[is.na(data$s_max_slope_ratio)] <- 1
data$s_max_slope_avg[is.na(data$s_max_slope_avg)] <- 0
data$s_min_slope_ratio[is.na(data$s_min_slope_ratio)] <- 1
data$s_min_slope_avg[is.na(data$s_min_slope_avg)] <- 0
data$s_var_ratio_avg[data$s_var_ratio_avg == Inf] <- 1
data$s_var_ratio_avg[data$s_var_ratio_avg == -Inf] <- 1
data$s_var_ratio_range[data$s_var_ratio_range == Inf] <- 0
data$var_ratio[data$var_ratio == Inf] <- 1
data$var_ratio[data$var_ratio == -Inf] <- 1

# convert back to data table format
data <- as.data.table(data)

# random forest training --------------------------------------------------

# divide datasets into training and testing subsets
print("Creating spatial and temporal model...")
spat_model <- create_detection_model(data, p_train = 0.6, p_test = 0.4,
                                   length_importance = 20, output_path = output_path, imp_filename = mod_data_spat,
                                   plot_importance = FALSE, spatial = TRUE)
print("Finished creating spatial and temporal model.")
print("Creating temporal model...")
temp_model <- create_detection_model(data, p_train = 0.6, p_test = 0.4,
                                     length_importance = 20, output_path = output_path, imp_filename = mod_data_temp,
                                     plot_importance = FALSE, spatial = FALSE)
print("Finished creating temporal model.")

# apply to img data -------------------------------------------------------

for (b in 1:n_blocks) {
  print(paste("Beginning model application for block ", b, ".", sep = ""))

  # convert NA variables to 0
  img_data_blocks[[b]]$variance[is.na(img_data_blocks[[b]]$variance)] <- 0
  img_data_blocks[[b]]$mad[is.na(img_data_blocks[[b]]$mad)] <- 0
  img_data_blocks[[b]]$range[is.na(img_data_blocks[[b]]$range)] <- 0
  img_data_blocks[[b]]$min_index[is.na(img_data_blocks[[b]]$min_index)] <- 0
  img_data_blocks[[b]]$max_index[is.na(img_data_blocks[[b]]$max_index)] <- 0
  img_data_blocks[[b]]$slope[is.na(img_data_blocks[[b]]$slope)] <- 0
  img_data_blocks[[b]]$min_tree_slope[is.na(img_data_blocks[[b]]$min_tree_slope)] <- 0
  img_data_blocks[[b]]$max_tree_slope[is.na(img_data_blocks[[b]]$max_tree_slope)] <- 0
  img_data_blocks[[b]]$date_min_slope[is.na(img_data_blocks[[b]]$date_min_slope)] <- 2000
  img_data_blocks[[b]]$avg_raw_slope[is.na(img_data_blocks[[b]]$avg_raw_slope)] <- 0
  img_data_blocks[[b]]$var_raw_slope[is.na(img_data_blocks[[b]]$var_raw_slope)] <- 0
  img_data_blocks[[b]]$min_raw_slope[is.na(img_data_blocks[[b]]$min_raw_slope)] <- 0
  img_data_blocks[[b]]$min_slope[is.na(img_data_blocks[[b]]$min_slope)] <- 0
  img_data_blocks[[b]]$max_slope[is.na(img_data_blocks[[b]]$max_slope)] <- 0
  img_data_blocks[[b]]$avg_slope[is.na(img_data_blocks[[b]]$avg_slope)] <- 0
  img_data_blocks[[b]]$var_slope[is.na(img_data_blocks[[b]]$var_slope)] <- 0
  img_data_blocks[[b]]$date_min[is.na(img_data_blocks[[b]]$date_min)] <- 2000
  img_data_blocks[[b]]$smooth_range[is.na(img_data_blocks[[b]]$smooth_range)] <- 0
  img_data_blocks[[b]]$s_break_year_range[is.na(img_data_blocks[[b]]$s_break_year_range)] <- 0
  img_data_blocks[[b]]$s_mag_avg[is.na(img_data_blocks[[b]]$s_mag_avg)] <- 0
  img_data_blocks[[b]]$s_mag_range[is.na(img_data_blocks[[b]]$s_mag_range)] <- 0
  img_data_blocks[[b]]$s_dur_avg[is.na(img_data_blocks[[b]]$s_dur_avg)] <- 0
  img_data_blocks[[b]]$s_dur_range[is.na(img_data_blocks[[b]]$s_dur_range)] <- 0
  img_data_blocks[[b]]$pre_var[is.na(img_data_blocks[[b]]$pre_var)] <- 0
  img_data_blocks[[b]]$post_var[is.na(img_data_blocks[[b]]$post_var)] <- 0
  img_data_blocks[[b]]$var_ratio[is.na(img_data_blocks[[b]]$var_ratio)] <- 1
  img_data_blocks[[b]]$mag[is.na(img_data_blocks[[b]]$mag)] <- 0
  img_data_blocks[[b]]$dur[is.na(img_data_blocks[[b]]$dur)] <- 0
  img_data_blocks[[b]]$s_mag_ratio[is.na(img_data_blocks[[b]]$s_mag_ratio)] <- 0
  img_data_blocks[[b]]$s_dur_ratio[is.na(img_data_blocks[[b]]$s_dur_ratio)] <- 0
  img_data_blocks[[b]]$s_dur_ratio[img_data_blocks[[b]]$s_dur_ratio == Inf] <- 0
  img_data_blocks[[b]]$s_dur_ratio[img_data_blocks[[b]]$s_dur_ratio == -Inf] <- 0
  img_data_blocks[[b]]$dist_date[is.na(img_data_blocks[[b]]$dist_date)] <- 0
  img_data_blocks[[b]]$s_break_year_range[img_data_blocks[[b]]$s_break_year_range == Inf] <- 0
  img_data_blocks[[b]]$s_min_year_ranges[is.na(img_data_blocks[[b]]$s_min_year_ranges)] <- 0
  img_data_blocks[[b]]$s_var_ratio_ratio[is.na(img_data_blocks[[b]]$s_var_ratio_ratio)] <- 1
  img_data_blocks[[b]]$s_var_ratio_range[is.na(img_data_blocks[[b]]$s_var_ratio_range)] <- 0
  img_data_blocks[[b]]$mad_mean_ratio[is.na(img_data_blocks[[b]]$mad_mean_ratio)] <- 1
  img_data_blocks[[b]]$s_avg_slope_range[is.na(img_data_blocks[[b]]$s_avg_slope_range)] <- 0
  img_data_blocks[[b]]$s_max_slope_range[is.na(img_data_blocks[[b]]$s_max_slope_range)] <- 0
  img_data_blocks[[b]]$s_min_slope_range[is.na(img_data_blocks[[b]]$s_min_slope_range)] <- 0
  img_data_blocks[[b]]$s_var_ratio_avg[is.na(img_data_blocks[[b]]$s_var_ratio_avg)] <- 1
  img_data_blocks[[b]]$s_avg_slope_ratio[is.na(img_data_blocks[[b]]$s_avg_slope_ratio)] <- 1
  img_data_blocks[[b]]$s_avg_slope_avg[is.na(img_data_blocks[[b]]$s_avg_slope_avg)] <- 0
  img_data_blocks[[b]]$s_max_slope_ratio[is.na(img_data_blocks[[b]]$s_max_slope_ratio)] <- 1
  img_data_blocks[[b]]$s_max_slope_avg[is.na(img_data_blocks[[b]]$s_max_slope_avg)] <- 0
  img_data_blocks[[b]]$s_min_slope_ratio[is.na(img_data_blocks[[b]]$s_min_slope_ratio)] <- 1
  img_data_blocks[[b]]$s_min_slope_avg[is.na(img_data_blocks[[b]]$s_min_slope_avg)] <- 0
  img_data_blocks[[b]]$s_var_ratio_avg[img_data_blocks[[b]]$s_var_ratio_avg == Inf] <- 1
  img_data_blocks[[b]]$s_var_ratio_avg[img_data_blocks[[b]]$s_var_ratio_avg == -Inf] <- 1
  img_data_blocks[[b]]$s_var_ratio_range[img_data_blocks[[b]]$s_var_ratio_range == Inf] <- 0
  img_data_blocks[[b]]$var_ratio[img_data_blocks[[b]]$var_ratio == Inf] <- 1
  img_data_blocks[[b]]$var_ratio[img_data_blocks[[b]]$var_ratio == -Inf] <- 1
  img_data_blocks[[b]]$s_mag_ratio[img_data_blocks[[b]]$s_mag_ratio == Inf] <- 1
  img_data_blocks[[b]]$s_mag_ratio[img_data_blocks[[b]]$s_mag_ratio == -Inf] <- 1
  img_data_blocks[[b]]$s_mag_ratio[is.nan(img_data_blocks[[b]]$s_mag_ratio)] <- 0
    
  # predict disturbance with validation data
  preds_spat <- as.numeric(predict(spat_model, img_data_blocks[[b]]))
  preds_temp <- as.numeric(predict(temp_model, img_data_blocks[[b]]))

  # how many points do the spatial and temporal models disagree on?
  cat("Number of pixels with disagreement: ", sum(preds_spat != preds_temp),
      "\nPercentage of pixels with disagreement: ", sum(preds_spat != preds_temp) / length(preds_spat),
      "\n")

  # join img predictions to unscaled data to get date -----------------------


  classified_image_spat <- create_pred_image(data = img_data_blocks[[b]], orig_data = orig_data[[b]],
                                             pred_values = preds_spat, data_path = data_path,
                  fill_image = fill_image[[b]], plot = FALSE, output_path = output_path,
                  out_data = classified_data_spat[[b]], out_raster = output_raster_spat[[b]])
  classified_image_temp <- create_pred_image(data = img_data_blocks[[b]], orig_data = orig_data[[b]],
                                             pred_values = preds_temp, data_path = data_path,
                                           fill_image = fill_image[[b]], plot = FALSE, output_path = output_path,
                                           out_data = classified_data_temp[[b]], out_raster = output_raster_temp[[b]])
  print(paste("Finished with model application for block ", b, ".", sep = ""))
}
