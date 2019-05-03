# step 9 ------------------------------------------------------------------
# sample from disturbed and undisturbed points for validation

# load packages -----------------------------------------------------------

library(tidyverse)
library(data.table)

# set main path -----------------------------------------------------------

data_path <- "~/disturbr/data/attribution/"
output_path <- '~/disturbr/data/validation/'

# scene
scene <- 'p12_r28'
model_type <- 'spatial'
blocks = c(3, 2, 7)

# sample data -------------------------------------------------------------

# sample within each selected block
for (i in 1:length(blocks)) {
  
  # name files
  att_data <- paste('classified_img_data_', model_type, '_', scene, '_block', blocks[i], '.csv', sep = '')
  output_dist_samples <- paste('attributed_sample_data_disturbed_', model_type, '_', scene, '_block', blocks[i], '.csv', sep = '')
  output_norm_samples <- paste('attributed_sample_data_undisturbed_', model_type, '_', scene, '_block', blocks[i], '.csv', sep = '')
  
  # load data
  print(paste('Loading data for block ', blocks[i], '.', sep = ''))
  data <- fread(paste(data_path, att_data, sep = ''), header = T, sep = ',')
  dist_data <- data[!is.na(data$predicted),]
  other_data <- data[is.na(data$predicted),]

  # load previously sampled data
  prev_dist <- fread(paste(output_path, output_dist_samples, sep = ''), header = T, sep = ',')
  prev_other <- fread(paste(output_path, output_norm_samples, sep = ''), header = T, sep = ',')
  
  # figure out if anything needs to be added
  add_dist <- 0
  add_other <- 0
  if (nrow(prev_dist) < 50 & nrow(dist_data) >= 50) {
    add_dist <- 50 - nrow(prev_dist)
  }
  if (nrow(prev_dist) < 50 & nrow(dist_data) < 50 & nrow(prev_dist) != nrow(dist_data)) {
    add_dist <- nrow(dist_data) - nrow(prev_dist)
  }
  if (nrow(prev_other) < 50 & nrow(other_data) >= 50) {
    add_other <- 50 - nrow(prev_other)
  }
  if (nrow(prev_other) < 50 & nrow(other_data) < 50 & nrow(prev_other) != nrow(other_data)) {
    add_other <- nrow(other_data) - nrow(prev_other)
  }
  
  set.seed(1595)
  samp_dist <- sample(seq(1, nrow(dist_data)), add_dist, replace = F) 
  samp_dist <- dist_data[samp_dist, ]
  
  samp_other <- sample(seq(1, nrow(other_data)), add_other, replace = F)
  samp_other <- other_data[samp_other, ]
  
  # ensure oob sampling
  if (blocks[i] %in% c(3, 5, 7)) {
    dist_training <- fread(paste('~/disturbr/data/training_points/img_training_', scene, '_block', blocks[i], '.csv', sep = ''))
    if (model_type == 'spatial') {
      att_training <- fread(paste('~/disturbr/data/training_points/img_training_attribution_', scene, '_block', blocks[i], '.csv', sep = ''))
    } else {
      att_training <- fread(paste('~/disturbr/data/training_points/img_training_temporal_attribution_', scene, '_block', blocks[i], '.csv', sep = ''))
    }
    
    samp_dist$longitude <- signif(samp_dist$longitude, digits = 7)
    samp_dist$latitude <- signif(samp_dist$latitude, digits = 7)
    samp_other$longitude <- signif(samp_other$longitude, digits = 7)
    samp_other$latitude <- signif(samp_other$latitude, digits = 7) 
    dist_training$longitude <- signif(dist_training$longitude, digits = 7)
    dist_training$latitude <- signif(dist_training$latitude, digits = 7)
    att_training$longitude <- signif(att_training$longitude, digits = 7)
    att_training$latitude <- signif(att_training$latitude, digits = 7)
    if (nrow(prev_dist) > 0) {
      prev_dist$longitude <- signif(prev_dist$longitude, digits = 7)
      prev_dist$latitude <- signif(prev_dist$latitude, digits = 7)
    }
    if (nrow(prev_other) > 0) {
      prev_other$longitude <- signif(prev_other$longitude, digits = 7)
      prev_other$latitude <- signif(prev_other$latitude, digits = 7)
    }
    
    # remove previously trained points
    # not a significant number of points, so don't need to replace (for now)
    extras <- list()
    for (j in 1:nrow(samp_dist)) { # returns error message if dist was not detected
      tryCatch({extras[j] <- which(samp_dist$longitude == dist_training$longitude[j] &
                                     samp_dist$latitude == dist_training$latitude[j] &
                                     samp_dist$avg_index == dist_training$avg_index[j])},
               error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    print(paste('Removing ', length(extras), ' points from disturbance sample due to overlap with disturbance model training.', sep = ''))
    if (length(extras) >= 1) {
      samp_dist <- samp_dist[-unlist(extras),]
    }
    extras <- list()
    for (j in 1:nrow(samp_dist)) { # returns error message if dist was not detected
      tryCatch({extras[j] <- which(samp_dist$longitude == att_training$longitude[j] &
                                     samp_dist$latitude == att_training$latitude[j] &
                                     samp_dist$avg_index == att_training$avg_index[j])},
               error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    print(paste('Removing ', length(extras), ' points from disturbance sample due to overlap with attribution model training.', sep = ''))
    if (length(extras) >= 1) {
      samp_dist <- samp_dist[-unlist(extras),]
    }
    extras <- list()
    for (j in 1:nrow(samp_other)) { # returns error message if dist was not detected
      tryCatch({extras[j] <- which(samp_other$longitude == dist_training$longitude[j] &
                                     samp_other$latitude == dist_training$latitude[j] &
                                     samp_other$avg_index == dist_training$avg_index[j])},
               error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    print(paste('Removing ', length(extras), ' points from non-disturbance sample due to overlap with disturbance model training.', sep = ''))
    if (length(extras) >= 1) {
      samp_other <- samp_other[-unlist(extras),]
    }
    extras <- list()
    for (j in 1:nrow(samp_other)) { # returns error message if dist was not detected
      tryCatch({extras[j] <- which(samp_other$longitude == att_training$longitude[j] &
                                     samp_other$latitude == att_training$latitude[j] &
                                     samp_other$avg_index == att_training$avg_index[j])},
               error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    print(paste('Removing ', length(extras), ' points from non-disturbance sample due to overlap with attribution model training.', sep = ''))
    if (length(extras) >= 1) {
      samp_other <- samp_other[-unlist(extras),]
    }
    
    # remove any duplicated validation points
    extras <- list()
    for (j in 1:nrow(samp_other)) { # returns error message if dist was not detected
      tryCatch({extras[j] <- which(samp_other$longitude == prev_other$longitude[j] &
                                     samp_other$latitude == prev_other$latitude[j] &
                                     samp_other$avg_index == prev_other$avg_index[j])},
               error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    print(paste('Removing ', length(extras), ' points from non-disturbance sample due to overlap with previous validation.', sep = ''))
    if (length(extras) >= 1) {
      samp_other <- samp_other[-unlist(extras),]
    }
    extras <- list()
    for (j in 1:nrow(samp_dist)) { # returns error message if dist was not detected
      tryCatch({extras[j] <- which(samp_dist$longitude == prev_dist$longitude[j] &
                                     samp_dist$latitude == prev_dist$latitude[j] &
                                     samp_dist$avg_index == prev_dist$avg_index[j])},
               error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    print(paste('Removing ', length(extras), ' points from disturbance sample due to overlap with previous validation.', sep = ''))
    if (length(extras) >= 1) {
      samp_dist <- samp_dist[-unlist(extras),]
    }
  }
  
  # limit to rows already in validation dataframe
  if (nrow(samp_dist) > 0) {
    samp_dist <- data.frame(latitude = samp_dist$latitude, longitude = samp_dist$longitude, 
                            date_min_slope = samp_dist$date_min_slope, date_min = samp_dist$date_min, 
                            dist_date = samp_dist$dist_date, preds = samp_dist$preds, 
                            predicted = samp_dist$predicted, year = NA, actual_dist = NA,
                            date_min_slope_diff = NA, date_min_diff = NA, dist_date_diff = NA)
    samp_dist <- rbind(prev_dist, samp_dist)
  } else {
    samp_dist <- prev_dist
  }
  if (nrow(samp_other) > 0) {
    samp_other <- data.frame(latitude = samp_other$latitude, longitude = samp_other$longitude, 
                            date_min_slope = samp_other$date_min_slope, date_min = samp_other$date_min, 
                            dist_date = samp_other$dist_date, preds = samp_other$preds, 
                            predicted = samp_other$predicted, year = NA, actual_dist = NA)
    samp_other <- rbind(prev_other, samp_other)
  } else {
    samp_other <- prev_other
  }
  
  print(paste('Writing validation points for block ', blocks[i], '.', sep = ''))
  fwrite(samp_dist, paste(output_path, output_dist_samples, sep = ''), row.names = F, col.names = T, sep = ',')
  fwrite(samp_other, paste(output_path, output_norm_samples, sep = ''), row.names = F, col.names = T, sep = ',')
}
