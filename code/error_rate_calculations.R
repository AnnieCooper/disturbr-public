# commission, omission, and kappa rates -----------------------------------


# load packages -----------------------------------------------------------

library(purrr)
library(dplyr)
library(stringr)

# functions ---------------------------------------------------------------

accuracies <- function(sceneid, byblock = FALSE) {
  # list files
  distlist <- list.files(path = '/home/annie/Documents/satellite-field-comp/data/validation/',
                         pattern = paste('attributed_sample_data_disturbed_spatial_', sceneid, 
                                         '_block', sep = ''),
                         full.names = TRUE)
  otherlist <- list.files(path = '/home/annie/Documents/satellite-field-comp/data/validation/',
                         pattern = paste('attributed_sample_data_undisturbed_spatial_', sceneid, 
                                         '_block', sep = ''),
                         full.names = TRUE)
  
  # initiate lists
  crate <- list()
  orate <- list()
  kappa <- list()
  frate <- list()
  
  if (byblock == FALSE) {
    # combine without bad validation points (0s)
    distdata <- map_dfr(distlist, read.csv, header = TRUE, stringsAsFactors = FALSE)
    otherdata <- map_dfr(otherlist, read.csv, header = TRUE, stringsAsFactors = FALSE)
    distdata <- distdata[distdata$actual_dist != 0, ]
    otherdata <- otherdata[otherdata$actual_dist != 0, ]
    
    # commission and omission rates
    crate[1] <- nrow(distdata[distdata$actual_dist == 1, ]) / nrow(distdata)
    orate[1] <- nrow(otherdata[otherdata$actual_dist != 1, ]) / nrow(otherdata)
    frate[1] <- nrow(distdata[distdata$actual_dist != distdata$predicted & distdata$actual_dist != 1, ]) / nrow(distdata[distdata$actual_dist > 1, ])
    
    # kappa accuracy
    a <- nrow(distdata[distdata$actual_dist != 1, ])
    b <- nrow(distdata[distdata$actual_dist == 1, ])
    c <- nrow(otherdata[otherdata$actual_dist != 1, ])
    d <- nrow(otherdata[otherdata$actual_dist == 1, ])
    
    e <- a + c
    f <- a + b
    g <- b + d
    h <- c + d
    
    n <- sum(a, b, c, d)
    
    i <- (e / n) * (f / n)
    j <- (g / n) * (h / n)
    p_agree <- (a + d) / n
    
    kappa[1] <- (p_agree - (i + j)) / (1 - (i + j))
    
    cat(paste('Commission Rate: ', crate[1], '\n',
          'Omission Rate: ', orate[1], '\n',
          'Kappa: ', kappa[1], '\n',
          'False Dist. Rate: ', frate[1], sep = ''))
    
    return(list(crate, orate, kappa, frate))
  } else {
    for (x in 1:length(distlist)) {
      block <- str_sub(distlist[[x]], -5, -5)
      # combine without bad validation points (0s)
      distdata <- map_dfr(distlist[[x]], read.csv, header = TRUE, stringsAsFactors = FALSE)
      otherdata <- map_dfr(otherlist[[x]], read.csv, header = TRUE, stringsAsFactors = FALSE)
      distdata <- distdata[distdata$actual_dist != 0, ]
      otherdata <- otherdata[otherdata$actual_dist != 0, ]
      
      # commission and omission rates
      crate[x] <- nrow(distdata[distdata$actual_dist == 1, ]) / nrow(distdata)
      orate[x] <- nrow(otherdata[otherdata$actual_dist != 1, ]) / nrow(otherdata)
      frate[x] <- nrow(distdata[distdata$actual_dist != distdata$predicted & distdata$actual_dist != 1, ]) / nrow(distdata[distdata$actual_dist > 1, ])
      
      # kappa accuracy
      a <- nrow(distdata[distdata$actual_dist != 1, ])
      b <- nrow(distdata[distdata$actual_dist == 1, ])
      c <- nrow(otherdata[otherdata$actual_dist != 1, ])
      d <- nrow(otherdata[otherdata$actual_dist == 1, ])
      
      e <- a + c
      f <- a + b
      g <- b + d
      h <- c + d
      
      n <- sum(a, b, c, d)
      
      i <- (e / n) * (f / n)
      j <- (g / n) * (h / n)
      p_agree <- (a + d) / n
      
      kappa[x] <- (p_agree - (i + j)) / (1 - (i + j))
      
      cat(paste('Validation Block: ', block, '\n',
                'Commission Rate: ', crate[x], '\n',
                'Omission Rate: ', orate[x], '\n',
                'n Disturbed: ', nrow(distdata), '\n',
                'n Undisturbed: ', nrow(otherdata), '\n',
                'Kappa: ', kappa[x], '\n',
                'False Dist. Rate: ', frate[x], '\n\n', sep = ''))
    }
    return(list(crate, orate, kappa, frate))
  }
}

# run function ------------------------------------------------------------

accuracy_wa <- accuracies('p12_r28', byblock = TRUE)


# year function -----------------------------------------------------------

year_acc <- function(sceneid, byblock = FALSE) {
  
  # list files
  distlist <- list.files(path = '/home/annie/Documents/satellite-field-comp/data/validation/',
                         pattern = paste('attributed_sample_data_disturbed_spatial_', sceneid, 
                                         '_block', sep = ''),
                         full.names = TRUE)
  
  # read in, calculate diffs, write out
  for (i in 1:length(distlist)) {
    fpath <- distlist[i]
  
    temp <- read.csv(distlist[i], header = TRUE, stringsAsFactors = FALSE)
    temp$year[temp$year == 0] <- NA
    temp$date_min_slope_diff <- abs(temp$year - temp$date_min_slope)
    temp$date_min_diff <- abs(temp$year - temp$date_min)
    temp$dist_date_diff <- abs(temp$year - temp$dist_date)
    
    write.csv(temp, fpath, row.names = FALSE)
  }

  if (byblock == FALSE) {
    # combine without bad validation points (0s)
    distdata <- map_dfr(distlist, read.csv, header = TRUE, stringsAsFactors = FALSE)
    distdata <- distdata[distdata$actual_dist > 1, ]
    
    dmsd <- distdata %>% group_by(date_min_slope_diff) %>% summarize(count = n())
    dmd <- distdata %>% group_by(date_min_diff) %>% summarize(count = n())
    ddd <- distdata %>% group_by(dist_date_diff) %>% summarize(count = n())
    
    cat(paste('Date Min Slope:', '\n', 
              '0: ', sum(dmsd$count[dmsd$date_min_slope_diff == 0] / nrow(distdata), na.rm = TRUE), '\n',
              '1: ', sum(dmsd$count[dmsd$date_min_slope_diff <= 1] / nrow(distdata), na.rm = TRUE), '\n',
              '2: ', sum(dmsd$count[dmsd$date_min_slope_diff <= 2] / nrow(distdata), na.rm = TRUE), '\n',
              '3: ', sum(dmsd$count[dmsd$date_min_slope_diff <= 3] / nrow(distdata), na.rm = TRUE), '\n',
              '4: ', sum(dmsd$count[dmsd$date_min_slope_diff <= 4] / nrow(distdata), na.rm = TRUE), '\n',
              '5: ', sum(dmsd$count[dmsd$date_min_slope_diff <= 5] / nrow(distdata), na.rm = TRUE), '\n',
              '10: ', sum(dmsd$count[dmsd$date_min_slope_diff <= 10] / nrow(distdata), na.rm = TRUE), '\n', 
              'Date Min:', '\n', 
              '0: ', sum(dmd$count[dmd$date_min_diff == 0] / nrow(distdata), na.rm = TRUE), '\n',
              '1: ', sum(dmd$count[dmd$date_min_diff <= 1] / nrow(distdata), na.rm = TRUE), '\n',
              '2: ', sum(dmd$count[dmd$date_min_diff <= 2] / nrow(distdata), na.rm = TRUE), '\n',
              '3: ', sum(dmd$count[dmd$date_min_diff <= 3] / nrow(distdata), na.rm = TRUE), '\n',
              '4: ', sum(dmd$count[dmd$date_min_diff <= 4] / nrow(distdata), na.rm = TRUE), '\n',
              '5: ', sum(dmd$count[dmd$date_min_diff <= 5] / nrow(distdata), na.rm = TRUE), '\n',
              '10: ', sum(dmd$count[dmd$date_min_diff <= 10] / nrow(distdata), na.rm = TRUE), '\n',
              'Dist Date:', '\n', 
              '0: ', sum(ddd$count[ddd$dist_date_diff == 0] / nrow(distdata), na.rm = TRUE), '\n',
              '1: ', sum(ddd$count[ddd$dist_date_diff <= 1] / nrow(distdata), na.rm = TRUE), '\n',
              '2: ', sum(ddd$count[ddd$dist_date_diff <= 2] / nrow(distdata), na.rm = TRUE), '\n',
              '3: ', sum(ddd$count[ddd$dist_date_diff <= 3] / nrow(distdata), na.rm = TRUE), '\n',
              '4: ', sum(ddd$count[ddd$dist_date_diff <= 4] / nrow(distdata), na.rm = TRUE), '\n',
              '5: ', sum(ddd$count[ddd$dist_date_diff <= 5] / nrow(distdata), na.rm = TRUE), '\n',
              '10: ', sum(ddd$count[ddd$dist_date_diff <= 10] / nrow(distdata), na.rm = TRUE), '\n', sep = ''))
  } else {
    for (x in 1:length(distlist)) {
      block <- str_sub(distlist[[x]], -5, -5)
      # combine without bad validation points (0s)
      distdata <- map_dfr(distlist[[x]], read.csv, header = TRUE, stringsAsFactors = FALSE)
      distdata <- distdata[distdata$actual_dist > 1, ]
      
      dmsd <- distdata %>% group_by(date_min_slope_diff) %>% summarize(count = n())
      dmd <- distdata %>% group_by(date_min_diff) %>% summarize(count = n())
      ddd <- distdata %>% group_by(dist_date_diff) %>% summarize(count = n())

      cat(paste('Block: ', block, '\n', 
                'Date Min Slope:', '\n', 
                '0: ', sum(dmsd$count[dmsd$date_min_slope_diff == 0] / nrow(distdata), na.rm = TRUE), '\n',
                '1: ', sum(dmsd$count[dmsd$date_min_slope_diff <= 1] / nrow(distdata), na.rm = TRUE), '\n',
                '2: ', sum(dmsd$count[dmsd$date_min_slope_diff <= 2] / nrow(distdata), na.rm = TRUE), '\n',
                '3: ', sum(dmsd$count[dmsd$date_min_slope_diff <= 3] / nrow(distdata), na.rm = TRUE), '\n',
                '4: ', sum(dmsd$count[dmsd$date_min_slope_diff <= 4] / nrow(distdata), na.rm = TRUE), '\n',
                '5: ', sum(dmsd$count[dmsd$date_min_slope_diff <= 5] / nrow(distdata), na.rm = TRUE), '\n',
                '10: ', sum(dmsd$count[dmsd$date_min_slope_diff <= 10] / nrow(distdata), na.rm = TRUE), '\n', 
                'Date Min:', '\n', 
                '0: ', sum(dmd$count[dmd$date_min_diff == 0] / nrow(distdata), na.rm = TRUE), '\n',
                '1: ', sum(dmd$count[dmd$date_min_diff <= 1] / nrow(distdata), na.rm = TRUE), '\n',
                '2: ', sum(dmd$count[dmd$date_min_diff <= 2] / nrow(distdata), na.rm = TRUE), '\n',
                '3: ', sum(dmd$count[dmd$date_min_diff <= 3] / nrow(distdata), na.rm = TRUE), '\n',
                '4: ', sum(dmd$count[dmd$date_min_diff <= 4] / nrow(distdata), na.rm = TRUE), '\n',
                '5: ', sum(dmd$count[dmd$date_min_diff <= 5] / nrow(distdata), na.rm = TRUE), '\n',
                '10: ', sum(dmd$count[dmd$date_min_diff <= 10] / nrow(distdata), na.rm = TRUE), '\n',
                'Dist Date:', '\n', 
                '0: ', sum(ddd$count[ddd$dist_date_diff == 0] / nrow(distdata), na.rm = TRUE), '\n',
                '1: ', sum(ddd$count[ddd$dist_date_diff <= 1] / nrow(distdata), na.rm = TRUE), '\n',
                '2: ', sum(ddd$count[ddd$dist_date_diff <= 2] / nrow(distdata), na.rm = TRUE), '\n',
                '3: ', sum(ddd$count[ddd$dist_date_diff <= 3] / nrow(distdata), na.rm = TRUE), '\n',
                '4: ', sum(ddd$count[ddd$dist_date_diff <= 4] / nrow(distdata), na.rm = TRUE), '\n',
                '5: ', sum(ddd$count[ddd$dist_date_diff <= 5] / nrow(distdata), na.rm = TRUE), '\n',
                '10: ', sum(ddd$count[ddd$dist_date_diff <= 10] / nrow(distdata), na.rm = TRUE), '\n', sep = '')) 
      }

  }
}

