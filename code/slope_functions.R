# function to get slope data from rpart model (tree)
calc_tree_slopes <- function(data, x, y, plot = FALSE) {
  tree <- rpart::rpart(y ~ x, data = data, control = rpart::rpart.control(minsplit = 4, cp = 0.3))
  # get sequence of years on which to predict (in case not all are in df)
  s <- seq(min(x), max(x), by = 1)
  if (plot == TRUE) {
    plot(x, y)
    lines(s, predict(tree, data.frame(x = s)))
  } 
  # predict rpart model for all years
  points <- predict(tree, data.frame(x = s))
  # get slope of all line segments
  slopes <- diff(points) / diff(x)
  # find year at which the minimum slope occurs
  date_min <- x[min(which(slopes == min(slopes)), na.rm = T)]
  
  return(c(min(slopes), max(slopes), date_min))
}

# calculates slopes for segments of raw time series
### This should be in C
calc_raw_slopes <- function(x, y) {
  # calculate slope at all segements
  slopes <- diff(y) / diff(x)
  # calculate slopes from each point to first point in series
  overall_slope <- (y - y[1]) / (x - x[1])
  # find median slope of all segments
  avg_slope <- median(slopes, na.rm = T)
  # find variance of slopes from all segments
  var_slope <- var(slopes, na.rm = T)
  # get the minimum slope to the first point
  lowest_slope <- min(overall_slope, na.rm = T)
  
  return(c(avg_slope, var_slope, lowest_slope))
}

# calculate statistics based on loess curve
calc_slope_stats <- function(df, plot = FALSE) {
  # create new, smaller dataframe
  temp_data <- data.table(x = df$year, y = df$ind)
  # create smoothed model of time series
  loess_mod <- loess(y ~ x, data = temp_data, span = 0.75)
  # pull out predicted values (from loess) for each  year
  temp_data$fit <- fitted(loess_mod)
  if (plot == TRUE) {
    plot(temp_data$x, temp_data$y)
    lines(temp_data$x, temp_data$fit)
  }
  
  # find minimum slope of loess by calculating slope many times with a 
  # small sample of years
  slopes <- rollapply(temp_data$fit, width = 3, FUN = function(x) summary(lm(x ~ seq(1, 3)))$coef[2])
  
  # calculate basic slope stats based on loess slopes
  avg_slope <- median(slopes, na.rm = TRUE)
  var_slope <- var(slopes, na.rm = TRUE)
  lowest_slope <- min(slopes, na.rm = TRUE)
  highest_slope <- max(slopes, na.rm = TRUE)
  date_min <- temp_data$x[which(slopes == lowest_slope)]
  date_max <- temp_data$x[which(slopes == highest_slope)]
  low <- min(temp_data$fit[temp_data$x >= date_min], na.rm = TRUE)
  date_min <- max(temp_data$x[temp_data$fit == low & temp_data$x >= date_min], na.rm = TRUE)
  
  # determine the point at which the loess curve has a breakpoint
  breaks_fit <- breakpoints(temp_data$y ~ temp_data$x, h = 0.2, breaks = 2)
  # find start of minimum slope
  first <- breaks_fit[[1]][1]
  
  # this should approximate the end of the minimum slope
  second <- which(temp_data$x == date_min)
  
  # calculate disturbance vars (mag, dur, date)
  dist_date <- temp_data$x[first]
  mag <- temp_data$fit[second] - temp_data$fit[first] # 0 if NA
  dur <- temp_data$x[second] - temp_data$x[first] # 0 if NA
  
  if (is.na(first)) {
    dist_date <- NA
    mag <- NA
    dur <- NA
  }
  
  # variance before/after the disturbance minimum
  pre_var <- (sd(temp_data$y[temp_data$x < dist_date], na.rm = T) / mean(temp_data$y[temp_data$x < dist_date], na.rm = T)) * 100
  post_var <- (sd(temp_data$y[temp_data$x >= dist_date], na.rm = T) / mean(temp_data$y[temp_data$x >= dist_date], na.rm = T)) * 100
  var_ratio <- pre_var / post_var
  
  # calculate the true range of the data
  smooth_range <- range(temp_data$fit, na.rm = T)[2] - range(temp_data$fit, na.rm = T)[1]
  
  return(c(avg_slope, var_slope, lowest_slope, highest_slope, date_min, smooth_range, pre_var, post_var, var_ratio, 
           dist_date, dur, mag))  
}

# calculate stats from both rpart model and raw time series
calc_tree_stats <- function(df, plot = FALSE) {
  temp_data <- data.table(x = df$year, y = df$ind)
  # estimate values using an rpart model
  tree <- rpart::rpart(y ~ x, data = temp_data, control = rpart::rpart.control(minsplit = 4, cp = 0.3))
  # calculate statistics based on the rpart model
  if (plot == TRUE) {
    tree_slopes <- calc_tree_slopes(tree, temp_data$x, temp_data$y, plot = TRUE)
  } else {
    tree_slopes <- calc_tree_slopes(tree, temp_data$x, temp_data$y)
  }
  # calculate statistics based on the raw time series
  raw_slope_vars <- calc_raw_slopes(temp_data$x, temp_data$y)
  
  return(c(tree_slopes, raw_slope_vars))
}

# calculate spatial variables
calc_spatial_stats <- function(df, data) {
  # find index where df coords = data coords
  index <- which(data$longitude == df$longitude[1] & data$latitude == df$latitude[1])
  
  # shrink full lat/long dataset so it takes less time
  minlat <- data$latitude[index] - 0.2
  maxlat <- data$latitude[index] + 0.2
  minlong <- data$longitude[index] - 0.2
  maxlong <- data$longitude[index] + 0.2
  temp_img_data <- data[data$latitude > minlat & data$latitude < maxlat &
                              data$longitude > minlong & data$longitude < maxlong,]
  
  # create df where all coordinates are alongside the index coordinates
  dist_data <- data.table(x = temp_img_data$longitude, y = temp_img_data$latitude, 
                      id.x = data$longitude[index], id.y = data$latitude[index],
                      dist = as.numeric(NA))
  
  # calculate distances for each row
  dist_data$dist <- apply(dist_data, 1, function(row) 
    sqrt(((row[1] - row[3]) ^ 2) + ((row[2] - row[4]) ^ 2)))
  
  # find nearest 63 (plus the actual point itself) = 240m x 240m window, 
  # or just nearest points if an edge point
  nearest <- data.table(arrange(dist_data, dist)[1:64, c(1, 2)])
  names(nearest) <- c('longitude', 'latitude')
  
  # join with data containing vars
  temp <- data[nearest, , on = c('longitude', 'latitude')]
  
  ### calculate spatial vars
  ### This could be in C
  df$s_min_year_range <- abs(range(temp$date_min, na.rm = T)[2] - range(temp$date_min, na.rm = T)[1])
  df$s_break_year_range <- abs(range(temp$dist_date, na.rm = T)[2] - range(temp$dist_date, na.rm = T)[1])
  df$s_min_slope_avg <- mean(temp$min_slope, na.rm = T)
  df$s_min_slope_range <- abs(range(temp$min_slope, na.rm = T)[2] - range(temp$min_slope, na.rm = T)[1])
  df$s_min_slope_ratio <- df$min_slope / df$s_min_slope_avg
  df$s_max_slope_avg <- mean(temp$max_slope, na.rm = T)
  df$s_max_slope_range <- abs(range(temp$max_slope, na.rm = T)[2] - range(temp$max_slope, na.rm = T)[1])
  df$s_max_slope_ratio <- df$max_slope / df$s_max_slope_avg
  df$s_avg_slope_avg <- mean(temp$avg_slope, na.rm = T)
  df$s_avg_slope_range <- abs(range(temp$avg_slope, na.rm = T)[2] - range(temp$avg_slope, na.rm = T)[1])
  df$s_avg_slope_ratio <- df$avg_slope / df$s_avg_slope_avg
  df$s_var_ratio_avg <- mean(temp$var_ratio, na.rm = T) # remember that var_ratio = 1 stands for no minimum slope
  df$s_var_ratio_range <- abs(range(temp$var_ratio, na.rm = T)[2] - range(temp$var_ratio, na.rm = T)[1])
  df$s_var_ratio_ratio <- df$var_ratio / df$s_var_ratio_avg
  df$s_mag_avg <- mean(temp$mag, na.rm = T)
  df$s_mag_range <- abs(range(temp$mag, na.rm = T)[2] - range(temp$mag, na.rm = T)[1])
  df$s_mag_ratio <- df$mag / df$s_mag_avg #if NaN, this should 1 (0/0)
  df$s_dur_avg <- mean(temp$dur, na.rm = T)
  df$s_dur_range <- abs(range(temp$dur, na.rm = T)[2] - range(temp$dur, na.rm = T)[1])
  df$s_dur_ratio <- df$dur / df$s_dur_avg
  
  return(df)
}

# extra spatial stats based on initial classification
attribution_stats <- function(df, data) {
  # find index where df coords = data coords
  index <- which(data$longitude == df$longitude[1] & data$latitude == df$latitude[1])
  
  # shrink full lat/long dataset so it takes less time
  minlat <- data$latitude[index] - 0.2
  maxlat <- data$latitude[index] + 0.2
  minlong <- data$longitude[index] - 0.2
  maxlong <- data$longitude[index] + 0.2
  temp_img_data <- data[data$latitude > minlat & data$latitude < maxlat &
                          data$longitude > minlong & data$longitude < maxlong,]
  
  # create df where all coordinates are alongside the index coordinates
  dist_data <- data.table(x = temp_img_data$longitude, y = temp_img_data$latitude, 
                          id.x = data$longitude[index], id.y = data$latitude[index],
                          dist = as.numeric(NA))
  
  # calculate distances for each row
  dist_data$dist <- apply(dist_data, 1, function(row) 
    sqrt(((row[1] - row[3]) ^ 2) + ((row[2] - row[4]) ^ 2)))
  
  # find nearest 63 points (plus the actual point itself) = 240m x 240m window, 
  # or just nearest points if an edge point
  nearest <- data.table(arrange(dist_data, dist)[1:64, c(1, 2)])
  names(nearest) <- c('longitude', 'latitude')
  
  # join with data containing vars
  temp <- data[nearest, , on = c('longitude', 'latitude')]
  
  # limit to disturbed pixels
  temp <- temp[temp$preds > 1]
  
  # range and median in disturbance year
  ### This could be in C
  df$d_dist_date_range <- range(temp$dist_date, na.rm = T)[2] - range(temp$dist_date, na.rm = T)[1]
  df$d_dist_date_med <- median(temp$dist_date, na.rm = T)
  # number of pixels in 500x500 window (or nearest 24 pix)
  df$d_num_pix <- nrow(temp)
  # range in duration
  df$d_dur_range <- range(temp$dur, na.rm = T)[2] - range(temp$dur, na.rm = T)[1]
  df$d_dur_med <- median(temp$dur, na.rm = T)
  # range in magnitude
  df$d_mag_range <- range(temp$mag, na.rm = T)[2] - range(temp$mag, na.rm = T)[1]
  df$d_mag_med <- median(temp$mag, na.rm = T)
  # range in avg_ind
  df$d_avg_ind_range <- range(temp$avg_ind, na.rm = T)[2] - range(temp$avg_ind, na.rm = T)[1]
  df$d_avg_ind_med <- median(temp$avg_ind, na.rm = T)
  # range in slope
  df$d_slope_range <- range(temp$slope, na.rm = T)[2] - range(temp$slope, na.rm = T)[1]
  df$d_slope_med <- median(temp$slope, na.rm = T)
  # range in min_slope
  df$d_min_slope_range <- range(temp$min_slope, na.rm = T)[2] - range(temp$min_slope, na.rm = T)[1]
  df$d_min_slope_med <- median(temp$min_slope, na.rm = T)
  # range in max_slope
  df$d_max_slope_range <- range(temp$max_slope, na.rm = T)[2] - range(temp$max_slope, na.rm = T)[1]
  df$d_max_slope_med <- median(temp$max_slope, na.rm = T)
  # range in date_min (low point, not where the slope starts)
  df$d_date_min_range <- range(temp$date_min, na.rm = T)[2] - range(temp$date_min, na.rm = T)[1]
  df$d_date_min_med <- median(temp$date_min, na.rm = T)
  
  return(df)
}