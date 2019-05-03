new_training <- function(data_path, img_data, var_data, other_var_data, old_training_points, 
                            new_training_points, index_name, start_yr, end_yr) {
  ## 'type' represents the model that you are training data for. There are two options: 'detection' or 'attribution'
  ## 'var_data' is only applicable for the attribution training. It should be left NULL for detection training.
  
  try(if(missing(data_path)) stop ('Must provide path for file outputs.'))

  # import image data -------------------------------------------------------
  
  # write in data
  all_data <- fread(paste(data_path, img_data, sep = ''), sep = ',', header = TRUE)
  gc()
  
  # read in old points (e.g., spatial model)
  spat_data <- fread(paste(data_path, old_training_points, sep = ''))
  
  # load classified data
  data <- fread(paste(data_path, var_data, sep = ''))
  spat_class_data <- fread(paste(data_path, other_var_data, sep = ''))
  
  # change long, lat digits to match
  data$longitude <- signif(data$longitude, digits = 7)
  data$latitude <- signif(data$latitude, digits = 7)
  spat_data$longitude <- signif(spat_data$longitude, digits = 7)
  spat_data$latitude <- signif(spat_data$latitude, digits = 7)
  all_data$longitude <- signif(all_data$longitude, digits = 7)
  all_data$latitude <- signif(all_data$latitude, digits = 7)
  
  # count overlap of training with temp predictions
  # total rows of spat_data
  n_spat <- nrow(spat_data)
  # find overlap
  dist_data <- data %>%
    filter(preds > 1) %>%
    as.data.table
  overlap <- dist_data[spat_data, nomatch = 0, on = c('longitude', 'latitude')]
  overlap <- overlap[spat_data, on = c('longitude', 'latitude'), disturbance := i.disturbance]
  n_overlap <- nrow(overlap)
  print(paste('Number of overlapping training points: ', n_overlap, sep = ''))
  
  # find proportion disturbed in both classified datasets
  prop_spat <- spat_class_data %>%
    filter(preds > 1) %>%
    nrow / nrow(spat_class_data)
  print(paste('Proportion disturbed for spatial: ', prop_spat, sep = ''))
  prop_temp <- data %>%
    filter(preds > 1) %>%
    nrow / nrow(data)
  print(paste('Proportion disturbed for temporal: ', prop_temp, sep = ''))
  goal_n <- (n_spat / prop_spat) * prop_temp # number sample / number disturbed for spatial
  print(paste('Target number of sample points: ', goal_n, sep = ''))

  # get necessary sample size (proportional to number discovered)
  s_size <- floor(goal_n - n_overlap)
  print(paste('Number of new sample points: ', s_size, sep = ''))
  if (s_size <= 0) {
    print('No additional training necessary.')
    # write out
    write.table(spat_data, paste(data_path, new_training_points, sep = ''), sep = ',', col.names = TRUE, row.names = FALSE)
    
    return(new_training_points)
    return(spat_data[sample(seq(1, nrow(spat_data), goal_n)),])
  } else {
    print('Finding additional training points...')
    lon_lat_data <- dist_data
    
    # get previously-classified data
    extras <- list()
    for (i in 1:nrow(overlap)) { # returns error message if dist was not detected
      tryCatch({extras[i] <- which(lon_lat_data$longitude == overlap$longitude[i] &
                                     lon_lat_data$latitude == overlap$latitude[i] &
                                     lon_lat_data$avg_index == overlap$avg_index[i])},
               error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    null_train <- which(sapply(extras, FUN = function(x) is.null(x)))
    extra_data <- lon_lat_data[unlist(extras),]
    if (length(null_train) > 0) {
      extra_data$disturbance <- overlap$disturbance[-null_train]
    } else {
      extra_data$disturbance <- overlap$disturbance
    }
    
    set.seed(5320)
    # remove points that have been sampled previously
    lon_lat_data <- lon_lat_data[-unlist(extras),]
    
    samp_pts <- sample(seq(1, nrow(lon_lat_data)), s_size, replace = F)
    samp_pts <- lon_lat_data[samp_pts, ]
    
    n_yrs <- (end_yr - start_yr) + 1
    # get year and ndvi data for those points
    samp_data <- data.table(longitude = rep(samp_pts$longitude, each = n_yrs), 
                            latitude = rep(samp_pts$latitude, each = n_yrs),
                            year = rep(seq(start_yr, end_yr), times = s_size),
                            ind = rep(as.numeric(NA), s_size * n_yrs))
    
    # join data tables by long, lat, and year
    samp_data <- all_data[samp_data, .(longitude, latitude, year, ind), on = c('longitude', 'latitude', 'year')]
    
    # create empty disturbance category to be filled
    samp_pts$disturbance <- as.numeric(NA)
    
    # add in previously-classified data
    samp_pts <- rbind(samp_pts, extra_data)
    
    # write out
    write.table(samp_pts, paste(data_path, new_training_points, sep = ''), sep = ',', col.names = TRUE, row.names = FALSE)
    
    return(new_training_points)
  }
}
