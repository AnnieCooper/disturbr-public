# Compare detection from temporal and spatial models ----------------------

# Written by AC Smith on 26 Apr 2019
# Last edited by AC Smith on 26 Apr 2019

# load packages -----------------------------------------------------------

library(raster)
library(tidyverse)
library(data.table)

# load data ---------------------------------------------------------------

data_path <- "~/disturbr/data/detection/"
output_path <- '~/disturbr/data/detection/validation/'

# scene
scenes <- c('p45_r30', 'p47_r27', 'p35_r32', 'p27_r27', 'p12_r28', 'p16_r37', 'p14_r32')
models <- c('temporal', 'spatial')
blocklist <- list(c(5, 1, 4), c(2, 6, 3), c(7, 9, 2), c(1, 4, 6), c(3, 2, 7), c(5, 4, 1), c(1, 7, 8))

# results df
results <- data.frame(scene = NA, block = NA, model_type = NA, n_dist = NA, 
                      n_undist = NA, n_truedist = NA, n_trueundist = NA)

for (i in 1:length(scenes)){
  scene <- scenes[i]
  blocks <- blocklist[[i]]
  for (model_type in models) {
    for (j in 1:length(blocks)) {
      # name files
      temp_data <- paste('disturbance_date_img_', model_type, '_', scene, '_block', blocks[j], '.tif', sep = '')
      
      # load data
      print(paste('Loading data for ', scene, ' block ', blocks[j], '.', sep = ''))
      data <- raster(paste(data_path, temp_data, sep = ''), sep = ',')
      
      # convert to data table with 1s for undisturbed, 2s for disturbed
      df <- data.table(longitude = coordinates(data)[, 1], latitude = coordinates(data)[, 2], val = getValues(data))
      df[!is.na(val), val := 2, ]
      df[is.na(val), val := 1, ]
      
      # load attribution validation data
      att_dist <- fread(paste('~/disturbr/data/validation/attributed_sample_data_disturbed_spatial_', scene, '_block', blocks[j], '.csv', sep = ''))
      att_undist <- fread(paste('~/disturbr/data/validation/attributed_sample_data_undisturbed_spatial_', scene, '_block', blocks[j], '.csv', sep = ''))
      
      # make sure long/lat are the same
      df$longitude <- signif(df$longitude, digits = 7)
      df$latitude <- signif(df$latitude, digits = 7)
      att_dist$longitude <- signif(att_dist$longitude, digits = 7)
      att_dist$latitude <- signif(att_dist$latitude, digits = 7)
      att_undist$longitude <- signif(att_undist$longitude, digits = 7)
      att_undist$latitude <- signif(att_undist$latitude, digits = 7)
      
      # grab matching points in detection results
      det_dist <- df %>%
        inner_join(att_dist, by = c('longitude', 'latitude')) %>%
        mutate(actual_dist = replace(actual_dist, actual_dist >= 2, 2)) %>%
        filter(actual_dist > 0)
      det_undist <- df %>%
        inner_join(att_undist, by = c('longitude', 'latitude')) %>%
        mutate(actual_dist = replace(actual_dist, actual_dist >= 2, 2)) %>%
        filter(actual_dist > 0)
      
      # print number where disturbed is disturbed, undisturbed is undisturbed
      cat('Results for ', model_type, ' model...', '\n')
      cat('Percentage correct (disturbed): ', nrow(det_dist[det_dist$actual_dist == det_dist$val, ]) / nrow(det_dist), '\n')
      cat('Percentage correct (undisturbed): ', nrow(det_undist[det_undist$actual_dist == det_undist$val, ]) / nrow(det_undist), '\n')
      
      temp <- data.frame(scene = scene, block = blocks[j], model_type = model_type, n_dist = nrow(det_dist), 
                         n_undist = nrow(det_undist), 
                         n_truedist = nrow(det_dist[det_dist$actual_dist == det_dist$val, ]), 
                         n_trueundist = nrow(det_undist[det_undist$actual_dist == det_undist$val, ]))
      
      results <- rbind(results, temp)
    }
  }
}

# clean up and export -----------------------------------------------------

results <- results[-1, ]

write.csv(results, '~/disturbr/data/validation/detection_models_spat_vs_temp_validation.csv', row.names = FALSE)

# calculate accuracy values -----------------------------------------------

results <- read.csv('~/disturbr/data/validation/detection_models_spat_vs_temp_validation.csv', header = TRUE, stringsAsFactors = FALSE)
results$crate <- NA
results$orate <- NA
results$kappa <- NA

for (i in 1:length(scenes)) {
  cat('Working on scene ', scenes[i], '.\n')
  sceneid <- scenes[i]
  for (model_type in models) {
    cat('...on model type -- ', model_type, '.\n')
    
    # import file
    temp <- results[results$scene == sceneid & results$model_type == model_type, ]
    blocks <- temp$block
    
    # initiate lists
    crate <- list()
    orate <- list()
    kappa <- list()
      
    # commission and omission rates
    crate[1] <- (sum(temp$n_dist) - sum(temp$n_truedist)) / sum(temp$n_dist)
    orate[1] <- (sum(temp$n_undist) - sum(temp$n_trueundist)) / sum(temp$n_undist)
    
    # kappa accuracy
    a <- sum(temp$n_truedist)
    b <- sum(temp$n_dist) - sum(temp$n_truedist)
    c <- sum(temp$n_undist) - sum(temp$n_trueundist)
    d <- sum(temp$n_undist)
    
    e <- a + c
    f <- a + b
    g <- b + d
    h <- c + d
    
    n <- sum(a, b, c, d)
    
    k <- (e / n) * (f / n)
    l <- (g / n) * (h / n)
    p_agree <- (a + d) / n
    
    kappa[1] <- (p_agree - (k + l)) / (1 - (k + l))
      
    newrow <- data.frame(scene = sceneid, block = 'all', model_type = model_type, 
                         n_dist = sum(temp$n_dist), n_undist = sum(temp$n_undist),
                         n_truedist = sum(temp$n_truedist), n_trueundist = sum(temp$n_trueundist),
                         crate = crate[[1]], orate = orate[[1]], kappa = kappa[[1]])
    
    results <- rbind(results, newrow)
    for (x in 1:length(blocks)) {
      blocktemp <- temp[temp$block == blocks[x], ]
      
      # commission and omission rates
      crate[1] <- (blocktemp$n_dist - blocktemp$n_truedist) / blocktemp$n_dist
      orate[1] <- (blocktemp$n_undist - blocktemp$n_trueundist) / blocktemp$n_undist
      
      # kappa accuracy
      a <- blocktemp$n_truedist
      b <- blocktemp$n_dist - blocktemp$n_truedist
      c <- blocktemp$n_undist - blocktemp$n_trueundist
      d <- blocktemp$n_undist
      
      e <- a + c
      f <- a + b
      g <- b + d
      h <- c + d
      
      n <- sum(a, b, c, d)
      
      k <- (e / n) * (f / n)
      l <- (g / n) * (h / n)
      p_agree <- (a + d) / n
      
      kappa[1] <- (p_agree - (k + l)) / (1 - (k + l))
      
      results$crate[results$scene == sceneid & 
                      results$model_type == model_type & 
                      results$block == blocks[x]] <- crate[[1]]
      results$orate[results$scene == sceneid & 
                      results$model_type == model_type & 
                      results$block == blocks[x]] <- orate[[1]]
      results$kappa[results$scene == sceneid & 
                      results$model_type == model_type & 
                      results$block == blocks[x]] <- kappa[[1]]
    }
  }
}

# final write out
write.csv(results, '~/disturbr/data/validation/detection_models_spat_vs_temp_validation.csv', row.names = FALSE)



