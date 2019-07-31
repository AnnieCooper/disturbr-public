# function to create image data tables ------------------------------------

create_img_data <- function(data_path, img_file, forest_file, out_fill_img, out_raw, index_img_data,
                            num_blocks = 9, plot = TRUE, start_yr, end_yr, for_cov_min = 20, index_name = 'index') {
  ## 'num_blocks' is the number of blocks that the image is split into for easier processing, 
  ## and should be a square number
  
  try(if(missing(start_yr)) stop('Must provide a start year value.'))
  try(if(missing(end_yr)) stop('Must provide an end year value.'))
  try(if(is.numeric(start_yr) == FALSE) stop('Start year must be numeric.'))
  try(if(is.numeric(end_yr) == FALSE) stop('End year must be numeric.'))
  try(if(is.numeric(for_cov_min) == FALSE) stop('End year must be numeric.'))
  try(if(missing(data_path)) stop ('Must provide path for file outputs.'))
  try(if(missing(img_file)) stop ('Must provide path to data image.'))
  try(if(missing(forest_file)) stop ('Must provide path to forest cover image.'))
  try(if(missing(out_fill_img)) stop ('Must provide path for output image.'))
  try(if(missing(out_raw)) stop ('Must provide path for raw output data.'))
  try(if(missing(index_img_data)) stop ('Must provide path for output index data.'))
  
  # open image --------------------------------------------------------------
  
  # first band of the ndvi img is the time series average
  img <- brick(paste(data_path, img_file, sep = ''))
  forest_img <- raster(paste(data_path, forest_file, sep = ''))
  print('Finished reading in images.')
  # create dataframe --------------------------------------------------------
  
  # create empty dataframe to hold ndvi data
  img_data <- data.table(longitude = coordinates(img)[, 1],
                         latitude = coordinates(img)[, 2])
  
  # split into blocks for easier processing
  try(if(sqrt(num_blocks) %% sqrt(num_blocks) != 0) stop("num_blocks must be a square number"))
      
  num_lines <- sqrt(num_blocks) + 1
  long_splits <- seq(min(img_data$longitude), max(img_data$longitude), length.out = num_lines)
  lat_splits <- seq(min(img_data$latitude), max(img_data$latitude), length.out = num_lines)
  grid <- expand.grid(long_splits, lat_splits) 
  print('Finished retrieving coordinates.')
  # check image to make sure points are good
  if (plot == TRUE) {
    plot(forest_img)
    points(grid)
  }

  i <- 1
  j <- 1
  block_number <- 1
  while (i < num_lines) {
    
    lat_min <- i
    lat_max <- i + 1

    while (j < num_lines) {
      
      lon_min <- j
      lon_max <- j + 1
      
      # cut image to block area
      img_block <- crop(img, extent(long_splits[lon_min] - 0.1, long_splits[lon_max] + 0.1, lat_splits[lat_min] - 0.1, lat_splits[lat_max] + 0.1))
      forest_block <- crop(forest_img, extent(long_splits[lon_min] - 0.1, long_splits[lon_max] + 0.1, lat_splits[lat_min] - 0.1, lat_splits[lat_max] + 0.1))
      print(paste('Finished cropping image to block ', block_number, '.', sep = ''))
      gc()
      
      if (plot == TRUE) {
        # plot again to check
        plot(forest_block)
      }

      # create image data for block
      img_data_block <- data.table(longitude = coordinates(img_block)[, 1],
                           latitude = coordinates(img_block)[, 2])

      # retrieve values from raster brick and forest cover raster
      values <- data.table(getValues(img_block))
      forested <- data.table(getValues(forest_block))
      names <- paste(index_name, as.character(seq(start_yr, end_yr)), sep = '')
      print(paste('Finished extracting values for block ', block_number, '.', sep = ''))
      # add to dataframe (1st two columns are coordinates)
      img_data_block <- cbind(img_data_block, values)
      rm(values)
      gc()
      names(img_data_block)[3:((end_yr - start_yr) + 3)] <- names

      # add forest cover information to table
      img_data_block$forested <- forested$V1
      rm(forested)
      gc()
      print(paste('Finished creating block ', block_number, ' data frame.', sep = ''))
      # remove pixels where forest cover is <20%
      cols <- names(img_data_block)[3:((end_yr - start_yr) + 3)]
      img_data_block$na <- apply(img_data_block[, ..cols], 1, function(row) sum(is.na(row)))

      # write out image with 1's where you do the analysis
      true_coords <- data.table(longitude = coordinates(forest_block)[, 1],
                              latitude = coordinates(forest_block)[, 2],
                              value = as.numeric(NA))
      used <- img_data_block
      used$value <- as.numeric(NA)
      used[forested >= for_cov_min & na < 1, value := 1]
      used[forested < for_cov_min & na >= 1, value := NA]
      used <- used[, .(longitude, latitude, value)]
      true_coords$value <- used[, .(value)]
      crs_img <- crs(forest_block)
      coordinates(true_coords) <- ~ longitude + latitude
      gridded(true_coords) <- TRUE
      used_points <- raster(true_coords)
      projection(used_points) <- crs_img

      writeRaster(used_points, paste(data_path, strsplit(out_fill_img, '[.]')[[1]][1], '_block', block_number, '.tif', sep = ''), overwrite = T)

      # remove unnecessary values from data table
      img_data_block <- img_data_block[forested >= for_cov_min & na < 1, ]
      img_data_block <- img_data_block[, forested := NULL]
      img_data_block <- img_data_block[, na := NULL]

      # write out this form of the data table
      write.table(img_data_block, paste(data_path, strsplit(out_raw, '[.]')[[1]][1], '_block', block_number, '.csv', sep = ''), sep = ',', row.names = FALSE, col.names = TRUE)

      # remove unnecessary objects
      rm(names, forest_block)
      gc()

      # convert all index (e.g., ndvi) columns to year and index value
      all_data <- melt(img_data_block, measure.vars = grep(paste('^', index_name, sep = ''),
                                                         names(img_data_block)), variable.name = 'year',
                                                         value.name = index_name)
      all_data$year <- as.numeric(gsub(paste('^', index_name, sep = ''), '', all_data$year))

      # remove img data for memory
      rm(img_data_block)
      gc()

      # write out ndvi data
      write.table(all_data, paste(data_path, strsplit(index_img_data, '[.]')[[1]][1], '_block', block_number, '.csv', sep = ''), sep = ',', row.names = FALSE, col.names = TRUE)

      rm(all_data)
      gc()
      
      # move to the next column in the row
      j <- j + 1
      block_number <- block_number + 1
    }
    
    # move on to the row
    i <- i + 1
    j <- 1
  }
  
 print('Done creating data.')
}