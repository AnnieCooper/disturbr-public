classify_points <- function(data_path, img_file, img_data, var_data = NULL, training_points, 
                            prev_training = NULL, prev_add = FALSE,
                            index_name, type = 'detection', start_yr, end_yr, valid_values = c(0, 1, 2), sample_num, 
                            restart = FALSE, start_ind = 1, remove_bad = TRUE) {
  ## 'type' represents the model that you are training data for. There are two options: 'detection' or 'attribution'
  ## 'var_data' is only applicable for the attribution training. It should be left NULL for detection training.
  
  try(if(missing(data_path)) stop ('Must provide path for file outputs.'))
  try(if(missing(img_file)) stop ('Must provide path to data image.'))
  
  # import image data -------------------------------------------------------

  # first band of the ndvi img is the time series average
  img <- brick(paste(data_path, img_file, sep = ''))
  
  # write in data
  all_data <- fread(paste(data_path, img_data, sep = ''), sep = ',', header = TRUE)
  gc()
  
  if (type == 'attribution') {
    # variable information
    data <- fread(paste(data_path, var_data, sep = ''))
    det_training <- fread(paste(data_path, prev_training, sep = ''))
    gc()
    
    # change long, lat digits to match
    data$longitude <- signif(data$longitude, digits = 7)
    data$latitude <- signif(data$latitude, digits = 7)
    all_data$longitude <- signif(all_data$longitude, digits = 7)
    all_data$latitude <- signif(all_data$latitude, digits = 7)
    det_training$longitude <- signif(det_training$longitude, digits = 7)
    det_training$latitude <- signif(det_training$latitude, digits = 7)
  }
  # get sample points from img ----------------------------------------------
  
  if (type == 'detection') {
    div = 25000
  }
  if (type == 'attribution') {
    div = 30000
  }
  
  if (type == 'attribution') {
    # get only disturbed pixels
    pred_data <- data[data$preds > 1, ]
    lon_lat_data <- pred_data
  }
  
  if (prev_add == TRUE) {
    if (type == 'detection') {
      # figure out .01% of data
      lon_lat_data <- all_data[ , .(avg_index = mean(ind)), by = c('longitude', 'latitude')]
    }
    
    # add in previously-classified points
    if (type == 'attribution') {
      det_training <- det_training[det_training$disturbance > 1,]
      extras <- list()
      for (i in 1:nrow(det_training)) { # returns error message if dist was not detected
        tryCatch({extras[i] <- which(lon_lat_data$longitude == det_training$longitude[i] &
                                       lon_lat_data$latitude == det_training$latitude[i] &
                                       lon_lat_data$avg_index == det_training$avg_index[i])},
                 error = function(e){cat("ERROR :", conditionMessage(e), "\n")})
      }
      if (nrow(det_training) != length(extras)) {
        extras[nrow(det_training)] <- list(NULL)
      }
      null_train <- which(sapply(extras, FUN = function(x) is.null(x)))
      if (length(null_train) > 0) {
        extra_data <- lon_lat_data[unlist(extras),]
        extra_data$disturbance <- det_training$disturbance[-null_train]
      } 
    }
    
    try(if(missing(sample_num)) sample_num <- round(nrow(lon_lat_data) / div))
    s_size <- sample_num
    print(paste('Sample size is ', s_size, '.', sep = ''))
    
    # get coordinates of sample points
    if (type == 'detection') {set.seed(275)}
    if (type == 'attribution') {
      set.seed(5320)
      # remove points that have been sampled previously
      lon_lat_data <- lon_lat_data[-unlist(extras),]
    }
    samp_pts <- sample(seq(1, nrow(lon_lat_data)), s_size, replace = F)
    samp_pts <- lon_lat_data[samp_pts, ]
  } 
  
  # read in points if you didn't finish
  if (restart == TRUE) {
    samp_pts <- fread(paste(data_path, training_points, sep = ''), sep = ',', header = TRUE)
  }
  
  if (prev_add == FALSE) {
    s_size <- nrow(samp_pts)
  }
  
  n_yrs <- (end_yr - start_yr) + 1
  # get year and ndvi data for those points
  samp_data <- data.table(longitude = rep(samp_pts$longitude, each = n_yrs), 
                          latitude = rep(samp_pts$latitude, each = n_yrs),
                          year = rep(seq(start_yr, end_yr), times = s_size),
                          ind = rep(as.numeric(NA), s_size * n_yrs))
  
  # join data tables by long, lat, and year
  samp_data <- all_data[samp_data, .(longitude, latitude, year, ind), on = c('longitude', 'latitude', 'year')]
  
  # create empty disturbance category to be filled
  if (restart == FALSE) {
    samp_pts$disturbance <- as.numeric(NA)
  }
  
  # clean up
  rm(all_data)
  gc()
  
  # classify sample points --------------------------------------------------
  
  # iteratively go through, plot, and decide disturbance status
  i <- start_ind
  while (i <= nrow(samp_data[samp_data$year == start_yr, ])) {
    print(paste('Classifying pixel #', i, '.', sep = ''))
    # get only data from full dataset where coordinates match those of the sample point
    long <- samp_pts$longitude[i]
    lat <- samp_pts$latitude[i]
    df <- samp_data[which(near(samp_data$longitude, long)), ]
    df <- df[which(near(df$latitude, lat)), ]
    # convert to simple dataframe
    temp <- data.table(x = df$year, y = df$ind)
    if (sum(is.na(temp$y)) >= 1) {
      d <- 0
      samp_pts$disturbance[which((samp_pts$longitude == long) & (samp_pts$latitude == lat))] <- d
      i <-  i + 1
    } else {
      
      # get smoothed values of the time series for the point
      fit <- loess(temp$y ~ temp$x)
      
      # find the pixel number for the sample point
      raster_id <- which(signif(coordinates(img)[, 1], 7) == long &
                           signif(coordinates(img)[, 2], 7) == lat)
      
      # create df where all coordinates are alongside the index coordinates for 
      # calculating distances
      minlat <- lat - 0.05
      maxlat <- lat + 0.05
      minlong <- long - 0.05
      maxlong <- long + 0.05
      temp_img_data <- lon_lat_data[lon_lat_data$latitude > minlat & lon_lat_data$latitude < maxlat &
                                      lon_lat_data$longitude > minlong & lon_lat_data$longitude < maxlong,]
      
      # cut image data to those within certain range
      dist_data <- data.table(x = temp_img_data$longitude, y = temp_img_data$latitude, 
                              id.x = long, id.y = lat,
                              dist = as.numeric(NA))
      if (type == 'attribution') {
        dist_data <- data.table(x = temp_img_data$longitude, y = temp_img_data$latitude, 
                                id.x = long, id.y = lat, dist_date = temp_img_data$predicted,
                                dist = as.numeric(NA))
      }
      
      # calculate distances for each row
      dist_data$dist <- apply(dist_data, 1, function(row) 
        sqrt(((row[1] - row[3]) ^ 2) + ((row[2] - row[4]) ^ 2)))
      
      # find nearest 48 (plus the actual point itself) = 600m x 600m window (20x20 pixels), 
      # or just nearest points if an edge point
      nearest <- arrange(dist_data, dist)[1:399, c(1, 2)]
      names(nearest) <- c('longitude', 'latitude')
      
      if (type == 'attribution') {
        # find nearest 48 (plus the actual point itself) = 600m x 600m window, 
        # or just nearest points if an edge point
        nearest <- arrange(dist_data, dist)[1:399, c(1, 2, 5, 6)]
        nearest_dist <- nearest[!is.na(nearest$dist_date),]
        names(nearest) <- c('longitude', 'latitude', 'dist_date', 'dist')
        print(paste('Distance to nearest point:', nearest_dist$dist[2], sep = ' '))
      }
      
      # plot the time series within the 700x700m window
      new_ext <- extent(min(nearest$longitude, na.rm = TRUE), max(nearest$longitude, na.rm = TRUE),
                        min(nearest$latitude, na.rm = TRUE), max(nearest$latitude, na.rm = TRUE))
      cropped <- crop(img, new_ext)
      plot(cropped, zlim = c(min(minValue(cropped)), max(maxValue(cropped))))
      Sys.sleep(2)
      readline(prompt = 'Press [enter] to continue to image...')
      
      # show satellite image of area to further confirm guess
      map <- ggmap(get_googlemap(center = c(long, lat), zoom = 19, 
                                 maptype = 'satellite', key = api_key))
      plot(map)
      Sys.sleep(2)
      readline(prompt = 'Press [enter] to continue to ts...')
      
      # take user input based on plotted time series + loess curve
      plot(temp$y ~ temp$x)
      lines(fit$fitted ~ temp$x)
      d <- readline(prompt = "Enter disturbance status: ")
      d <- as.numeric(d)
      try(if (is.na(d) | !(d %in% valid_values)) stop(paste("Unrecognized value. Please re-run function starting at index ", i, ".", sep = '')))

      # enter user input as disturbance decision in dataframe
      samp_pts$disturbance[which((samp_pts$longitude == long) & (samp_pts$latitude == lat))] <- d
      
      i <-  i + 1
      
      # write out in case of random stop during loop
      write.table(samp_pts, paste(data_path, training_points, sep = ''), sep = ',', col.names = TRUE, row.names = FALSE)
    }
  }
  
  # check distribution
  cat('Sample point classifications:\n')
  print(table(samp_pts$disturbance))
  
  # remove bad samples (use only when completely done training)
  if (remove_bad == TRUE & type == 'detection') {
    samp_pts <- samp_pts[samp_pts$disturbance != 0, ]
  }
  
  # add in the previously-classified points (if attribution)
  if (type == 'attribution' & prev_add == TRUE) {
    if (length(null_train) > 0) {
      samp_pts <- rbind(samp_pts, extra_data)
    } else {
      samp_pts <- samp_pts
    }
  }
  
  # write out
  write.table(samp_pts, paste(data_path, training_points, sep = ''), sep = ',', col.names = TRUE, row.names = FALSE)
  
  return(samp_pts)
}

# 110