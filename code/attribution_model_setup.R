attribution_model_setup <- function(training_blocks, n_blocks, sceneid,
                                    model_type, data_path, output_path, code_path, start_year, end_year) {
  
  # initialize lists
  training_points <- list()
  mod_data_spat <- paste('/importance/attribution_model_importance_spatial_', sceneid, '_', start_year, '_', end_year, '.csv', sep = '')
  mod_data_temp <- paste('/importance/attribution_model_importance_temporal_', sceneid, '.csv', sep = '')
  all_img_vars <- list()
  model_img_vars <- list()
  fill_image <- list()
  output_class_raster <- list()
  output_date_raster <- list()
  classified_data <- list()

  if (model_type == 'spatial') {
    i <- 1
    # training file names
    while (i <= length(training_blocks)) {
      training_points[[i]] <- paste('/training_points/img_training_attribution_', sceneid, '_block', training_blocks[i], '.csv', sep = '')
      i <- i + 1
    }
    i <- 1
    # all file names
    while (i <= n_blocks) {
      all_img_vars[[i]] <- paste('complete_img_with_attvars_spatial_', sceneid, '_block', i, '_', start_year, '_', end_year, '.csv', sep = '')
      model_img_vars[[i]] <- paste('complete_img_with_attvars_spatial_', sceneid, '_block', i, '_2000_2016.csv', sep = '')
      fill_image[[i]] <- paste('images/', sceneid, '/fill_image_', sceneid, '_', start_year, '_', end_year, '_block', i, '.tif', sep = '')
      output_class_raster[[i]] <- paste('/attribution/disturbance_class_img_spatial_', sceneid, '_block', i, '_', start_year, '_', end_year, '.tif', sep = '')
      output_date_raster[[i]] <- paste('/attribution/disturbance_date_img_spatial_', sceneid, '_block', i, '_', start_year, '_', end_year, '.tif', sep = '')
      classified_data[[i]] <- paste('/attribution/classified_img_data_spatial_', sceneid, '_block', i, '_', start_year, '_', end_year, '.csv', sep = '')
      i <- i + 1
    }
  } else {
    i <- 1
    # training file names
    while (i <= length(training_blocks)) {
      training_points[[i]] <- paste('/training_points/img_training_temporal_attribution_', sceneid, '_block', training_blocks[i], '.csv', sep = '')
      i <- i + 1
    }
    i <- 1
    # all file names
    while (i <= n_blocks) {
      all_img_vars[[i]] <- paste('complete_img_with_attvars_temporal_', sceneid, '_block', i, '_', start_year, '_', end_year, '.csv', sep = '')
      fill_image[[i]] <- paste('images/', sceneid, '/fill_image_', sceneid, '_', start_year, '_', end_year, '_block', i, '.tif', sep = '')
      output_class_raster[[i]] <- paste('/attribution/disturbance_class_img_temporal_', sceneid, '_block', i, '_', start_year, '_', end_year, '.tif', sep = '')
      output_date_raster[[i]] <- paste('/attribution/disturbance_date_img_temporal_', sceneid, '_block', i, '_', start_year, '_', end_year, '.tif', sep = '')
      classified_data[[i]] <- paste('/attribution/classified_img_data_temporal_', sceneid, '_block', i, '_', start_year, '_', end_year, '.csv', sep = '')
      i <- i + 1
    }
  }
  
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
    temp <- fread(paste(output_path, 'image_vars/', sceneid, '/', model_img_vars[as.numeric(training_blocks[i])], sep = ''))
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
  
  # variables for all img points (all blocks)
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
    
    ### need to import classified
    
    i <- i + 1
  }
  
  # join to create single dataframe with vars for all training points
  data <- img_data[pt_locs, , on = c('longitude', 'latitude', 'avg_index')]
  data <- as.data.frame(data)
  data <- distinct(data, longitude, latitude, avg_index, .keep_all = TRUE)
  
  # included because of re-run of var creation, this makes it so that we 
  # don't have to re-run spatial training creation (checked to make sure it's not 
  # many points for each scene). Temporal training will not differ because
  # we did not have to re-run var creation for temporal vars.
  data <- data[data$preds > 1, ] 
  # co = 1 pt/110 pts
  # sc = 1 pt/169 pts
  # pa = 5 pts/164 pts (only 1 actually disturbed)
  # wa = 10 pts/249 pts (only 2 actually disturbed)
  # mn = 23 pts/157 pts (18 disturbed)
  # or = 9 pts/191 pts (only 5 actually disturbed)
  # me = 3 pts/172 pts (only 2 actually disturbed)
  
  data <- distinct(data, longitude, latitude, avg_index, .keep_all = TRUE)
  
  # re-order columns so disturbance is third, followed by all of the training variables
  setcolorder(data, c(1:2, ncol(data), 3:(ncol(data) - 1)))
  
  # change values to numeric
  data$disturbance <- as.numeric(as.character(data$disturbance))
  
  # report disturbed/undisturbed counts
  cat('Disturbed pixels in training: ', sum(data$disturbance >= 2), 
      '\nUndisturbed pixels in training: ', sum(data$disturbance == 1), 
      '\nPercent disturbed in training: ', (sum(data$disturbance >= 2) / nrow(data)) * 100, '%',
      '\n')
  
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
  data$d_dist_date_range[is.na(data$d_dist_date_range)] <- 0
  data$d_dist_date_med[is.na(data$d_dist_date_med)] <- 2000
  data$d_dist_date_range[data$d_dist_date_range == -Inf] <- 0
  data$d_num_pix[is.na(data$d_num_pix)] <- 0
  data$d_dur_range[is.na(data$d_dur_range)] <- 0
  data$d_dur_med[is.na(data$d_dur_med)] <- 0
  data$d_mag_range[is.na(data$d_mag_range)] <- 0
  data$d_mag_med[is.na(data$d_mag_med)] <- 0
  data$d_avg_ind_range[is.na(data$d_avg_ind_range)] <- 0
  data$d_avg_ind_med[is.na(data$d_avg_ind_med)] <- 0
  data$d_slope_range[is.na(data$d_slope_range)] <- 0
  data$d_slope_med[is.na(data$d_slope_med)] <- 0
  data$d_min_slope_range[is.na(data$d_min_slope_range)] <- 0
  data$d_min_slope_med[is.na(data$d_min_slope_med)] <- 0
  data$d_max_slope_range[is.na(data$d_max_slope_range)] <- 0
  data$d_max_slope_med[is.na(data$d_max_slope_med)] <- 0
  data$d_date_min_range[is.na(data$d_date_min_range)] <- 0
  data$d_date_min_med[is.na(data$d_date_min_med)] <- 0

  # convert back to data table format
  data <- as.data.table(data)
  
  # random forest training --------------------------------------------------
  
  # divide datasets into training and testing subsets
  print(paste("Creating ", model_type, " model...", sep = ''))
  if (model_type == 'spatial') {
    model <- create_attribution_model(data, p_train = 0.6, p_test = 0.4, 
                                         length_importance = 20, output_path = output_path, imp_filename = mod_data_spat,
                                         plot_importance = FALSE, spatial = TRUE)
  } else {
    model <- create_attribution_model(data, p_train = 0.6, p_test = 0.4, 
                                           length_importance = 20, output_path = output_path, imp_filename = mod_data_temp,
                                           plot_importance = FALSE, spatial = FALSE)
  }
  print(paste("Finished creating ", model_type, " model.", sep = ''))
  
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
    img_data_blocks[[b]]$d_dist_date_range[is.na(img_data_blocks[[b]]$d_dist_date_range)] <- 0
    img_data_blocks[[b]]$d_dist_date_range[img_data_blocks[[b]]$d_dist_date_range == -Inf] <- 0
    img_data_blocks[[b]]$d_dist_date_med[is.na(img_data_blocks[[b]]$d_dist_date_med)] <- 2000
    img_data_blocks[[b]]$d_num_pix[is.na(img_data_blocks[[b]]$d_num_pix)] <- 0
    img_data_blocks[[b]]$d_dur_range[is.na(img_data_blocks[[b]]$d_dur_range)] <- 0
    img_data_blocks[[b]]$d_dur_med[is.na(img_data_blocks[[b]]$d_dur_med)] <- 0
    img_data_blocks[[b]]$d_mag_range[is.na(img_data_blocks[[b]]$d_mag_range)] <- 0
    img_data_blocks[[b]]$d_mag_med[is.na(img_data_blocks[[b]]$d_mag_med)] <- 0
    img_data_blocks[[b]]$d_avg_ind_range[is.na(img_data_blocks[[b]]$d_avg_ind_range)] <- 0
    img_data_blocks[[b]]$d_avg_ind_med[is.na(img_data_blocks[[b]]$d_avg_ind_med)] <- 0
    img_data_blocks[[b]]$d_slope_range[is.na(img_data_blocks[[b]]$d_slope_range)] <- 0
    img_data_blocks[[b]]$d_slope_med[is.na(img_data_blocks[[b]]$d_slope_med)] <- 0
    img_data_blocks[[b]]$d_min_slope_range[is.na(img_data_blocks[[b]]$d_min_slope_range)] <- 0
    img_data_blocks[[b]]$d_min_slope_med[is.na(img_data_blocks[[b]]$d_min_slope_med)] <- 0
    img_data_blocks[[b]]$d_max_slope_range[is.na(img_data_blocks[[b]]$d_max_slope_range)] <- 0
    img_data_blocks[[b]]$d_max_slope_med[is.na(img_data_blocks[[b]]$d_max_slope_med)] <- 0
    img_data_blocks[[b]]$d_date_min_range[is.na(img_data_blocks[[b]]$d_date_min_range)] <- 0
    img_data_blocks[[b]]$d_date_min_med[is.na(img_data_blocks[[b]]$d_date_min_med)] <- 0
    
    # predict disturbance with validation data
    temp <- img_data_blocks[[b]][img_data_blocks[[b]]$preds > 1,]
    predicted_values <- as.numeric(as.character(predict(model, temp)))
    table(predicted_values)

    # replace disturbance pixels with predicted values
    preds <- img_data_blocks[[b]]$preds
    preds[preds > 1] <- predicted_values
    
    # print out # disturbed and total
    cat('Number pixels predicted disturbed: ', sum(preds > 1), 
        '\nTotal pixels predicted disturbed: ', length(preds), 
        '\nPercent disturbed: ', (sum(preds > 1) / length(preds)) * 100, '%',
        '\n')
    
    # join img predictions to unscaled data to get date -----------------------
    
    classified_image_spat <- create_pred_image(data = img_data_blocks[[b]], orig_data = orig_data[[b]],
                                               pred_values = preds, data_path = data_path,
                                               fill_image = fill_image[[b]], plot = FALSE, output_path = output_path, 
                                               out_data = classified_data[[b]], 
                                               out_class_raster = output_class_raster[[b]], out_date_raster = output_date_raster[[b]]) 
    print(paste("Finished with model application for block ", b, ".", sep = ""))
  }
}