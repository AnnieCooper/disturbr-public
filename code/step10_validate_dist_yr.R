# step 10 ------------------------------------------------------------------
# validate disturbance year for disturbed points

# load packages -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(raster)
library(ggmap)

# set main path -----------------------------------------------------------

data_path <- "~/disturbr/data/"
output_path <- '~/disturbr/data/validation/'

scene <- 'p14_r32'
blocks <- c(1, 7, 8)
block <- blocks[3]
model_type <- 'spatial'

# set google api key ------------------------------------------------------

api_key <- #insert as character

# names of data files -----------------------------------------------------

img_file <- paste('images/', scene, '/summer_ndvi_', scene, '_2000_2016_30m.tif', sep = '')
ndvi_img_data <- paste('image_data/', scene, '/data_all_pix_', scene, '_block', block, '.csv', sep = '')
dist_samples <- paste('attributed_sample_data_disturbed_', model_type, '_', scene, '_block', block, '.csv', sep = '')

# load data ---------------------------------------------------------------

data <- fread(paste(output_path, dist_samples, sep = ''), header = T, sep = ',')

# import image data and prep ----------------------------------------------

# first band of the ndvi img is the time series average
img <- brick(paste(data_path, img_file, sep = ''))

# read in ndvi data
all_data <- fread(paste(data_path, ndvi_img_data, sep = ''), sep = ',', header = TRUE)

# get list of all points
lon_lat_data <- all_data[ , .(avg_ind = mean(ind)), by = c('longitude', 'latitude')]

# change long, lat digits to match
data$longitude <- signif(data$longitude, digits = 7)
data$latitude <- signif(data$latitude, digits = 7)
all_data$longitude <- signif(all_data$longitude, digits = 7)
all_data$latitude <- signif(all_data$latitude, digits = 7)

# get year and ndvi data for those points
samp_data <- data.table(longitude = rep(data$longitude, each = 17), 
                        latitude = rep(data$latitude, each = 17),
                        year = rep(seq(2000, 2016), times = nrow(data)),
                        ind = rep(as.numeric(NA), nrow(data) * 17))

# join data tables by long, lat, and year
samp_data <- all_data[samp_data, .(longitude, latitude, year, ind), on = c('longitude', 'latitude', 'year')]

# create empty disturbance category to be filled - comment out if year val is partially complete
# data[, year := NULL]
# data$year <- as.numeric(NA)

# write minimum year ------------------------------------------------------

# iteratively go through, plot, and decide disturbance year (when it starts to decrease/year before)
i <- 1
while (i <= nrow(samp_data[samp_data$year == 2000, ])) {
  # get only data from full dataset where coordinates match those of the sample point
  long <- data$longitude[i]
  lat <- data$latitude[i]
  df <- samp_data[which(near(samp_data$longitude, long)), ]
  df <- df[which(near(df$latitude, lat)), ]
  # convert to simple dataframe
  temp <- data.table(x = df$year, y = df$ind)
  if (sum(is.na(temp$y)) >= 1) {
    d <- 0
    data$disturbance[which((data$longitude == long) & (data$latitude == lat))] <- d
    i <-  i + 1
  } else {
    
    # get smoothed values of the time series for the point
    fit <- loess(temp$y ~ temp$x)
    
    # find the pixel number for the sample point
    raster_id <- which(coordinates(img)[, 1] == long &
                         coordinates(img)[, 2] == lat)
    
    # create df where all coordinates are alongside the index coordinates for 
    # calculating distances
    minlat <- lat - 0.2
    maxlat <- lat + 0.2
    minlong <- long - 0.2
    maxlong <- long + 0.2
    temp_img_data <- lon_lat_data[lon_lat_data$latitude > minlat & lon_lat_data$latitude < maxlat &
                                    lon_lat_data$longitude > minlong & lon_lat_data$longitude < maxlong,]
    
    # cut image data to those within certain range
    dist_data <- data.table(x = temp_img_data$longitude, y = temp_img_data$latitude, 
                            id.x = long, id.y = lat,
                            dist = as.numeric(NA))
    
    # calculate distances for each row
    dist_data$dist <- apply(dist_data, 1, function(row) 
      sqrt(((row[1] - row[3]) ^ 2) + ((row[2] - row[4]) ^ 2)))
    
    # find nearest 48 (plus the actual point itself) = 700m x 700m window, 
    # or just nearest points if an edge point
    nearest <- arrange(dist_data, dist)[1:49, c(1, 2)]
    names(nearest) <- c('longitude', 'latitude')
    
    # print out point info
    print(data[i,])
    
    # plot the time series within the 700x700m window
    new_ext <- extent(min(nearest$longitude), max(nearest$longitude),
                      min(nearest$latitude), max(nearest$latitude))
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
    d <- readline(prompt = "Enter disturbance year: ")
    d <- as.numeric(d)
    
    # enter user input as disturbance decision in dataframe
    data$year[which((data$longitude == long) & (data$latitude == lat))] <- d
    
    # write out in case of interruption
    write.table(data, paste(output_path, dist_samples, sep = ''), sep = ',', col.names = TRUE, row.names = FALSE)
    
    i <-  i + 1
  }
}

# check distribution
table(data$year)

# write out
write.table(data, paste(output_path, dist_samples, sep = ''), sep = ',', col.names = TRUE, row.names = FALSE)
