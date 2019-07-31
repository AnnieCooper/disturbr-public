# function to add slope, etc. data to images ------------------------------------

# add in basic variables from time series ---------------------------------
### This should all be C
add_basic_vars <- function(data, out_data_name = 'plot_data') {
  start <- Sys.time()
  out_data <- data[, .(avg_index = mean(ind),
                            variance = var(ind),
                            mad = mad(ind),
                            range = range(ind)[2] - range(ind)[1],
                            min_index = min(ind),
                            max_index = max(ind)), by = c('longitude', 'latitude')]
  out_data$mad_mean_ratio <- out_data$mad / out_data$avg_index
  assign(out_data_name, out_data)
  end <- Sys.time()
  print(paste('Total time to add basic TS variables: ', (end - start), sep = ''))
  
  return(out_data)
}

split_data <- function(data, plot_data, num_splits = 100) {
  # split data table to more efficiently calculate slopes
  # add index to img_data(single long/lat), join with larger dataset, then subset
  split_len <- round(nrow(plot_data) / num_splits)
  plot_data$splits <- rep(seq(1, num_splits), each = split_len, len = nrow(plot_data))
  data <- plot_data[data, .(longitude, latitude, year, ind, splits), on = c('longitude', 'latitude')]
  
  list_splits <- split(data, by = 'splits')
  
  # make sure that all years are there
  cat('Finished calculating splits. \nPlease check that all years have data in the table below: \n')
  print(data[data$longitude == data$longitude[1] & data$latitude == data$latitude[1]])
  
  return(list(list_splits, plot_data, data))
}

### This should be done in C
get_slopes <- function(i, data) {
  temp <- data.table(data[[i]])
  temp <- temp[, .(slope = coef(lm(ind ~ year))[2]), by = c('longitude', 'latitude')]
  
  return(temp)
}

add_TS_slope <- function(list_of_splits, plot_data, num_cores = NULL) {
  if (missing(num_cores)) {
    num_cores <- detectCores() - 1
  }
  start <- Sys.time()
  cores <- num_cores
  cl <- makeCluster(cores, type = 'SOCK')
  clusterEvalQ(cl, library(data.table))
  slope <- parSapply(cl, names(list_of_splits), FUN = get_slopes, data = list_of_splits)
  num_splits <- length(list_of_splits)
  stopCluster(cl)
  end <- Sys.time()
  print(paste('Total time to calculate slopes: ', (end - start), sep = ''))
  
  plot_data$slope <- as.numeric(NA)
  cols <- seq(1, (num_splits * 3), 3)
  for (i in cols) {
    long <- slope[[i]]
    lat <- slope[[i + 1]]
    slopes <- slope[[i + 2]]
    temp <- data.table(longitude = long, latitude = lat, slope = slopes)
    
    plot_data <- plot_data[temp, slope := slopes, on = c('longitude', 'latitude')]
  }
  
  # remove extraneous tables
  rm(slope, long, lat, slopes, temp)
  gc()
  
  print('Finished adding slopes to plot_data.')
  return(plot_data)
  
}

# add rpart variables -----------------------------------------------------

add_rpart_vars <- function(list_of_splits, plot_data, num_cores = NULL) {
  if (missing(num_cores)) {
    num_cores <- detectCores() - 1
  }
  get_rpart_vars <- function(i, data) {
    temp <- data.table(data[[i]])
    temp <- temp[, .(min_tree_slope = calc_tree_slopes(data = data.table(x = year, y = ind), x = year, y = ind)[1],
                     max_tree_slope = calc_tree_slopes(data = data.table(x = year, y = ind), x = year, y = ind)[2],
                     date_min = calc_tree_slopes(data = data.table(x = year, y = ind), x = year, y = ind)[3]), by = c('longitude', 'latitude')]
    
    return(temp)
  }
  
  start <- Sys.time()
  cores <- num_cores
  cl <- makeCluster(cores, type = 'SOCK')
  clusterEvalQ(cl, library(data.table))
  clusterExport(cl, c('calc_tree_slopes'))
  rpart_vars <- parSapply(cl, names(list_of_splits), FUN = get_rpart_vars, data = list_of_splits)
  num_splits <- length(list_of_splits)
  stopCluster(cl)
  end <- Sys.time()
  print(paste('Total time to calculate slopes: ', (end - start), sep = ''))

  # add holder columns
  plot_data$min_tree_slope <- as.numeric(NA)
  plot_data$max_tree_slope <- as.numeric(NA)
  plot_data$date_min_slope <- as.numeric(NA)
  cols <- seq(1, (num_splits * 5), 5)
  for (i in cols) {
    long <- rpart_vars[[i]]
    lat <- rpart_vars[[i + 1]]
    min_tree_slopes <- rpart_vars[[i + 2]]
    max_tree_slopes <- rpart_vars[[i + 3]]
    date_min_slopes <- rpart_vars[[i + 4]]
    
    temp <- data.table(longitude = long, latitude = lat, min_tree_slope = min_tree_slopes, max_tree_slope = max_tree_slopes, date_min_slope = date_min_slopes)
    
    plot_data <- plot_data[temp, min_tree_slope := min_tree_slopes, on = c('longitude', 'latitude')]
    plot_data <- plot_data[temp, max_tree_slope := max_tree_slopes, on = c('longitude', 'latitude')]
    plot_data <- plot_data[temp, date_min_slope := date_min_slopes, on = c('longitude', 'latitude')]
  }
  gc()
  
  cat('Finished adding RPART variables to plot_data: \n')
  return(plot_data)
}

add_raw_slope_vars <- function(list_of_splits, plot_data, num_cores = NULL) {
  
  if (missing(num_cores)) {
    num_cores <- detectCores() - 1
  }
  
  get_raw_slope_vars <- function(i, data) {
    temp <- data.table(data[[i]])
    temp <- temp[, .(avg_raw_slope = calc_raw_slopes(x = year, y = ind)[1],
                     var_raw_slope = calc_raw_slopes(x = year, y = ind)[2],
                     min_raw_slope = calc_raw_slopes(x = year, y = ind)[3]), by = c('longitude', 'latitude')]
    
    return(temp)
  }
  
  start <- Sys.time()
  cores <- num_cores
  cl <- makeCluster(cores, type = 'SOCK')
  clusterEvalQ(cl, library(data.table))
  clusterExport(cl, c('calc_raw_slopes'))
  raw_vars <- parSapply(cl, names(list_splits), FUN = get_raw_slope_vars, data = list_splits)
  num_splits <- length(list_of_splits)
  stopCluster(cl)
  end <- Sys.time()
  print(paste('Total time to calculate slopes: ', (end - start), sep = ''))

  plot_data$avg_raw_slope <- as.numeric(NA)
  plot_data$var_raw_slope <- as.numeric(NA)
  plot_data$min_raw_slope <- as.numeric(NA)
  cols <- seq(1, (num_splits * 5), 5)
  for (i in cols) {
    long <- raw_vars[[i]]
    lat <- raw_vars[[i + 1]]
    avg_raw_slopes <- raw_vars[[i + 2]]
    var_raw_slopes <- raw_vars[[i + 3]]
    min_raw_slopes <- raw_vars[[i + 4]]
    
    temp <- data.table(longitude = long, latitude = lat, avg_raw_slope = avg_raw_slopes, var_raw_slope = var_raw_slopes, min_raw_slope = min_raw_slopes)
    
    plot_data <- plot_data[temp, avg_raw_slope := avg_raw_slopes, on = c('longitude', 'latitude')]
    plot_data <- plot_data[temp, var_raw_slope := var_raw_slopes, on = c('longitude', 'latitude')]
    plot_data <- plot_data[temp, min_raw_slope := min_raw_slopes, on = c('longitude', 'latitude')]
  }
  gc()  
  
  cat('Finished adding raw slope variables to plot_data: \n')
  return(plot_data)
}


# the following are the more complicated functions ------------------------

# function in which results for loess time series variables are calculated
add_slope_results <- function(i, plot_data, data, slope_data) {
  # limit data to the point in question
  long <- plot_data$longitude[i]
  lat <- plot_data$latitude[i]
  df <- data[which(dplyr::near(data$longitude, long)), ]
  df <- df[which(dplyr::near(df$latitude, lat)), ]
  
  # calculate all slope results
  loess_results <- calc_slope_stats(df, plot = FALSE)
  
  # add results in to the master dataframe
  slope_data$avg_slope[slope_data$longitude == long & slope_data$latitude == lat] <- loess_results[1]
  slope_data$var_slope[slope_data$longitude == long & slope_data$latitude == lat] <- loess_results[2]
  slope_data$min_slope[slope_data$longitude == long & slope_data$latitude == lat] <- loess_results[3]
  slope_data$max_slope[slope_data$longitude == long & slope_data$latitude == lat] <- loess_results[4]
  slope_data$date_min[slope_data$longitude == long & slope_data$latitude == lat] <- loess_results[5]
  slope_data$smooth_range[slope_data$longitude == long & slope_data$latitude == lat] <- loess_results[6]
  slope_data$pre_var[slope_data$longitude == long & slope_data$latitude == lat] <- loess_results[7]
  slope_data$post_var[slope_data$longitude == long & slope_data$latitude == lat] <- loess_results[8]
  slope_data$var_ratio[slope_data$longitude == long & slope_data$latitude == lat] <- loess_results[9] 
  slope_data$dist_date[slope_data$longitude == long & slope_data$latitude == lat] <- loess_results[10] 
  slope_data$dur[slope_data$longitude == long & slope_data$latitude == lat] <- loess_results[11] 
  slope_data$mag[slope_data$longitude == long & slope_data$latitude == lat] <- loess_results[12] 
  
  return(slope_data[slope_data$longitude == long & slope_data$latitude == lat, ])
}

add_loess_vars <- function(data, plot_data, slope_data, num_splits, num_cores = NULL) {
  
  if (missing(num_cores)) {
    num_cores <- detectCores() - 1
  }
  
  cores <- num_cores

  for (i in 1:num_splits) {
    temp_data <- data[data$splits == i,]
    temp_plot_data <- plot_data[plot_data$splits == i,]
    temp_slope_data <- slope_data[plot_data$splits == i,]
    
    # make cluster and load functions/packages
    try(stopCluster(cl)) # just in case; this should produce an error
    ###Add type=FORK to makeCluster argument.
    cl <- makeCluster(cores, type = 'SOCK')
    clusterEvalQ(cl, library(dplyr))
    clusterEvalQ(cl, library(strucchange))
    clusterEvalQ(cl, library(data.table))
    clusterExport(cl, c('calc_slope_stats'))
    
    # get list of rows (pixels)
    inds <- as.list(seq(1, nrow(temp_plot_data)))
    
    # apply function to calculate variables
    start <- Sys.time()
    results <- parSapply(cl, unlist(inds), add_slope_results, 
                         plot_data = temp_plot_data, data = temp_data, slope_data = temp_slope_data)
    end <- Sys.time()
    print(paste('Total time to calculate loess variables for split #', i, ': ', (end - start), sep = ''))
    
    # stop cluster
    stopCluster(cl)

    # make sure everthing is a data table, not a data frame
    plot_data <- as.data.table(plot_data)
    temp_plot_data <- as.data.table(temp_plot_data)
    
    # call longitude, etc. as unlist(test[1,])
    # convert back to df
    temp_plot_data$min_slopes <- unlist(results['min_slope', ])
    temp_plot_data$max_slopes <- unlist(results['max_slope', ])
    temp_plot_data$avg_slopes <- unlist(results['avg_slope', ])
    temp_plot_data$var_slopes <- unlist(results['var_slope', ])
    temp_plot_data$date_mins <- unlist(results['date_min', ])
    temp_plot_data$smooth_ranges <- unlist(results['smooth_range', ])
    temp_plot_data$pre_vars <- unlist(results['pre_var', ])
    temp_plot_data$post_vars <- unlist(results['post_var', ])
    temp_plot_data$var_ratios <- unlist(results['var_ratio', ])
    temp_plot_data$dist_dates <- unlist(results['dist_date', ])
    temp_plot_data$durs <- unlist(results['dur', ])
    temp_plot_data$mags <- unlist(results['mag', ])
    
    plot_data <- plot_data[temp_plot_data, min_slope := min_slopes, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, max_slope := max_slopes, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, avg_slope := avg_slopes, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, var_slope := var_slopes, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, date_min := date_mins, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, smooth_range := smooth_ranges, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, pre_var := pre_vars, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, post_var := post_vars, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, var_ratio := var_ratios, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, dist_date := dist_dates, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, dur := durs, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, mag := mags, on = c('longitude', 'latitude')] 
    
    print(paste('Finished with split #:', i, sep = ' '))
  }
  
  print('Finished with all splits.')
  return(plot_data)
}

# function to add spatial variables
add_spat_data <- function(i, data) {
  # retrieve data for point
  long <- data$longitude[i]
  lat <- data$latitude[i]
  df <- data[which(dplyr::near(data$longitude, long)), ]
  df <- df[which(dplyr::near(df$latitude, lat)), ]
  
  # apply function to get stats
  results <- calc_spatial_stats(df, data)
  
  return(results)
}

add_spatial_vars <- function(data, plot_data, slope_data, num_splits, num_cores = NULL) {
  
  if (missing(num_cores)) {
    num_cores <- detectCores() - 1
  }
  cores <- num_cores
  
  for (i in 1:num_splits) {
    temp_plot_data <- plot_data[plot_data$splits == i,]
    
    # make cluster and load functions/packages
    try(stopCluster(cl)) # just in case; this should produce an error
    ###Add type=FORK to makeCluster argument.
    cl <- makeCluster(cores, type = 'SOCK')
    clusterEvalQ(cl, library(dplyr))
    clusterEvalQ(cl, library(strucchange))
    clusterEvalQ(cl, library(data.table))
    clusterExport(cl, c('calc_spatial_stats'))
    
    # get list of rows (pixels)
    inds <- as.list(seq(1, nrow(temp_plot_data)))
    
    # apply function to calculate variables
    start <- Sys.time()
    results <- parSapply(cl, unlist(inds), add_spat_data, data = temp_plot_data)
    end <- Sys.time()
    print(paste('Total time to calculate spatial variables for split #', i, ': ', (end - start), sep = ''))
    # stop cluster
    stopCluster(cl)
    
    # make sure everything is a data table, not a data frame
    temp_plot_data <- as.data.table(temp_plot_data)
    plot_data <- as.data.table(plot_data)
    
    # call longitude, etc. as unlist(test[1,])
    # convert back to df
    s_min_year_ranges <- unlist(results['s_min_year_range', ])
    s_break_year_ranges <- unlist(results['s_break_year_range', ])
    s_min_slope_avgs <- unlist(results['s_min_slope_avg', ])
    s_min_slope_ranges <- unlist(results['s_min_slope_range', ])
    s_min_slope_ratios <- unlist(results['s_min_slope_ratio', ])
    s_max_slope_avgs <- unlist(results['s_max_slope_avg', ])
    s_max_slope_ranges <- unlist(results['s_max_slope_range', ])
    s_max_slope_ratios <- unlist(results['s_max_slope_ratio', ])
    s_avg_slope_avgs <- unlist(results['s_avg_slope_avg', ])
    s_avg_slope_ranges <- unlist(results['s_avg_slope_range', ])
    s_avg_slope_ratios <- unlist(results['s_avg_slope_ratio', ])
    s_var_ratio_avgs <- unlist(results['s_var_ratio_avg', ])
    s_var_ratio_ranges <- unlist(results['s_var_ratio_range', ])
    s_var_ratio_ratios <- unlist(results['s_var_ratio_ratio', ])
    s_mag_avgs <- unlist(results['s_mag_avg', ])
    s_mag_ranges <- unlist(results['s_mag_range', ])
    s_mag_ratios <- unlist(results['s_mag_ratio', ])
    s_dur_avgs <- unlist(results['s_dur_avg', ])
    s_dur_ranges <- unlist(results['s_dur_range', ])
    s_dur_ratios <- unlist(results['s_dur_ratio', ])
    
    plot_data <- plot_data[temp_plot_data, s_min_year_ranges := s_min_year_ranges, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_break_year_range := s_break_year_ranges, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_min_slope_avg := s_min_slope_avgs, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_min_slope_range := s_min_slope_ranges, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_min_slope_ratio := s_min_slope_ratios, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_max_slope_avg := s_max_slope_avgs, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_max_slope_range := s_max_slope_ranges, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_max_slope_ratio := s_max_slope_ratios, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_avg_slope_avg := s_avg_slope_avgs, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_avg_slope_range := s_avg_slope_ranges, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_avg_slope_ratio := s_avg_slope_ratios, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_var_ratio_avg := s_var_ratio_avgs, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_var_ratio_range := s_var_ratio_ranges, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_var_ratio_ratio := s_var_ratio_ratios, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_mag_avg := s_mag_avgs, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_mag_range := s_mag_ranges, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_mag_ratio := s_mag_ratios, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_dur_avg := s_dur_avgs, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_dur_range := s_dur_ranges, on = c('longitude', 'latitude')] 
    plot_data <- plot_data[temp_plot_data, s_dur_ratio := s_dur_ratios, on = c('longitude', 'latitude')] 
    
    
    print(paste('Finished with split #: ', i, sep = ''))
  }
  
  print('Finished with all splits.')
  return(plot_data)
}



add_dist_results <- function(i, data) {
  # limit data to the point in question
  long <- data$longitude[i]
  lat <- data$latitude[i]
  df <- data[which(dplyr::near(data$longitude, long)), ]
  df <- df[which(dplyr::near(df$latitude, lat)), ]
  
  # calculate all slope results
  results <- attribution_stats(df, data)
  
  return(results)
}

add_att_vars <- function(img_data, num_cores = NULL) {
  
  if (missing(num_cores)) {
    num_cores <- detectCores() - 1
  }
  cores <- num_cores
  
  num_splits <- max(img_data$splits)
  
  for (i in 1:num_splits) {
    temp_img_data <- img_data[img_data$splits == i,]
    
    # make cluster and load functions/packages
    try(stopCluster(cl)) # just in case; this should produce an error
    ###Add type=FORK to makeCluster argument.
    cl <- makeCluster(cores, type = 'SOCK')
    clusterEvalQ(cl, library(dplyr))
    clusterEvalQ(cl, library(strucchange))
    clusterEvalQ(cl, library(data.table))
    clusterExport(cl, c('attribution_stats'))
    
    # get list of rows (pixels)
    temp_img_data <- temp_img_data[preds == 2]
    if (nrow(temp_img_data) != 0) {
      inds <- as.list(seq(1, nrow(temp_img_data)))

      # apply function to calculate variables
      start <- Sys.time()
      results <- parSapply(cl, unlist(inds), add_dist_results, data = temp_img_data)
      end <- Sys.time()
      print(end - start)
      # stop cluster
      stopCluster(cl)
      
      # call longitude, etc. as unlist(test[1,])
      # convert back to df
      d_dist_date_ranges <- as.numeric(unlist(results['d_dist_date_range', ]))
      d_dist_date_meds <- as.numeric(unlist(results['d_dist_date_med', ]))
      d_num_pixs <- as.numeric(unlist(results['d_num_pix', ]))
      d_dur_ranges <- as.numeric(unlist(results['d_dur_range', ]))
      d_dur_meds <- as.numeric(unlist(results['d_dur_med', ]))
      d_mag_ranges <- as.numeric(unlist(results['d_mag_range', ]))
      d_mag_meds <- as.numeric(unlist(results['d_mag_med', ]))
      d_avg_ind_ranges <- as.numeric(unlist(results['d_avg_ind_range', ]))
      d_avg_ind_meds <- as.numeric(unlist(results['d_avg_ind_med', ]))
      d_slope_ranges <- as.numeric(unlist(results['d_slope_range', ]))
      d_slope_meds <- as.numeric(unlist(results['d_slope_med', ]))
      d_min_slope_ranges <- as.numeric(unlist(results['d_min_slope_range', ]))
      d_min_slope_meds <- as.numeric(unlist(results['d_min_slope_med', ]))
      d_max_slope_ranges <- as.numeric(unlist(results['d_max_slope_range', ]))
      d_max_slope_meds <- as.numeric(unlist(results['d_max_slope_med', ]))
      d_date_min_ranges <- as.numeric(unlist(results['d_date_min_range', ]))
      d_date_min_meds <- as.numeric(unlist(results['d_date_min_med', ]))
      
      img_data <- img_data[temp_img_data, d_dist_date_range := d_dist_date_ranges, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_dist_date_med := d_dist_date_meds, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_num_pix := d_num_pixs, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_dur_range := d_dur_ranges, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_dur_med := d_dur_meds, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_mag_range := d_mag_ranges, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_mag_med := d_mag_meds, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_avg_ind_range := d_avg_ind_ranges, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_avg_ind_med := d_avg_ind_meds, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_slope_range := d_slope_ranges, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_slope_med := d_slope_meds, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_min_slope_range := d_min_slope_ranges, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_min_slope_med := d_min_slope_meds, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_max_slope_range := d_max_slope_ranges, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_max_slope_med := d_max_slope_meds, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_date_min_range := d_date_min_ranges, on = c('longitude', 'latitude')] 
      img_data <- img_data[temp_img_data, d_date_min_med := d_date_min_meds, on = c('longitude', 'latitude')]
    } 
    
    print(paste('Finished with split #:', i, sep = ' '))
  }  
  
  print('Finished with all splits.')
  return(img_data)
}
