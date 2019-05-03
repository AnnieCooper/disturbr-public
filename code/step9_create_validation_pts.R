# step 9 ------------------------------------------------------------------
# sample from disturbed and undisturbed points for validation

# load packages -----------------------------------------------------------

library(tidyverse)
library(data.table)

# set main path -----------------------------------------------------------

data_path <- "~/disturbr/data/attribution/"
output_path <- '~/disturbr/data/attribution/validation/'

# scene
scene <- 'p45_r30'
model_type <- 'spatial'

# randomly sample 3 blocks for validation ---------------------------------

blocks <- sample(seq(1, 9), size = 3)
print(blocks)

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
  
  # figure out proportion data disturbed
  dist_data <- data[!is.na(data$predicted),]
  other_data <- data[is.na(data$predicted),]
  prop_dist <- floor((nrow(dist_data) / nrow(other_data)) * 100)
  prop_other <- 100 - prop_dist
  print(paste('Proportion disturbed: ', prop_dist / 100, sep = ''))
  
  set.seed(1595)
  samp_dist <- sample(seq(1, nrow(dist_data)), prop_dist, replace = F) 
  samp_dist <- dist_data[samp_dist, ]
  
  samp_other <- sample(seq(1, nrow(other_data)), prop_other, replace = F)
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
  }
  
  print(paste('Writing validation points for block ', blocks[i], '.', sep = ''))
  fwrite(samp_dist, paste(output_path, output_dist_samples, sep = ''), row.names = F, col.names = T, sep = ',')
  fwrite(samp_other, paste(output_path, output_norm_samples, sep = ''), row.names = F, col.names = T, sep = ',')
}
