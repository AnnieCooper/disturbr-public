# patch analysis of detected areas

# Written by LAC Smith on 13 Feb 2019
# Last edited by LAC Smith on 13 Feb 2019

# import packages ---------------------------------------------------------

library(data.table)
library(tidyverse)
library(raster)
library(ggplot2)
library(gridExtra)
library(maps)
library(mapdata)
library(scales)
library(ggmap)
library(swatches)
library(ggsci)
library(viridis)
library(sp)
library(sf)
library(ggfortify)
library(landscapemetrics)
library(SDMTools)

# set paths ---------------------------------------------------------------

data_path <- "~/Documents/satellite-field-comp/data/"
code_path <- '~/Documents/satellite-field-comp/code/final/'
output_path <- '~/Documents/satellite-field-comp/figs/'


# crop data ---------------------------------------------------------------

##### Oregon
# import all landsat scene bounds, convert to same CRS as rasters
landsat_tiles <- st_read('~/Documents/satellite-field-comp/data/other/WRS2_descending.shp')
landsat_tiles <- st_transform(landsat_tiles, crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

# for now, just do one block at a time (WA)
date_path <- 'attribution/disturbance_date_img_spatial_p45_r30_block1.tif'
date_path2 <- 'attribution/disturbance_date_img_spatial_p45_r30_block2.tif'
date_path3 <- 'attribution/disturbance_date_img_spatial_p45_r30_block3.tif'
date_path4 <- 'attribution/disturbance_date_img_spatial_p45_r30_block4.tif'
date_path5 <- 'attribution/disturbance_date_img_spatial_p45_r30_block5.tif'
date_path6 <- 'attribution/disturbance_date_img_spatial_p45_r30_block6.tif'
date_path7 <- 'attribution/disturbance_date_img_spatial_p45_r30_block7.tif'
date_path8 <- 'attribution/disturbance_date_img_spatial_p45_r30_block8.tif'
date_path9 <- 'attribution/disturbance_date_img_spatial_p45_r30_block9.tif'

type_path <- 'attribution/disturbance_class_img_spatial_p45_r30_block1.tif'
type_path2 <- 'attribution/disturbance_class_img_spatial_p45_r30_block2.tif'
type_path3 <- 'attribution/disturbance_class_img_spatial_p45_r30_block3.tif'
type_path4 <- 'attribution/disturbance_class_img_spatial_p45_r30_block4.tif'
type_path5 <- 'attribution/disturbance_class_img_spatial_p45_r30_block5.tif'
type_path6 <- 'attribution/disturbance_class_img_spatial_p45_r30_block6.tif'
type_path7 <- 'attribution/disturbance_class_img_spatial_p45_r30_block7.tif'
type_path8 <- 'attribution/disturbance_class_img_spatial_p45_r30_block8.tif'
type_path9 <- 'attribution/disturbance_class_img_spatial_p45_r30_block9.tif'

# read in rasters
date1 <- raster(paste(data_path, date_path, sep = ''))
date2 <- raster(paste(data_path, date_path2, sep = ''))
date3 <- raster(paste(data_path, date_path3, sep = ''))
date4 <- raster(paste(data_path, date_path4, sep = ''))
date5 <- raster(paste(data_path, date_path5, sep = ''))
date6 <- raster(paste(data_path, date_path6, sep = ''))
date7 <- raster(paste(data_path, date_path7, sep = ''))
date8 <- raster(paste(data_path, date_path8, sep = ''))
date9 <- raster(paste(data_path, date_path9, sep = ''))

type1 <- raster(paste(data_path, type_path, sep = ''))
type2 <- raster(paste(data_path, type_path2, sep = ''))
type3 <- raster(paste(data_path, type_path3, sep = ''))
type4 <- raster(paste(data_path, type_path4, sep = ''))
type5 <- raster(paste(data_path, type_path5, sep = ''))
type6 <- raster(paste(data_path, type_path6, sep = ''))
type7 <- raster(paste(data_path, type_path7, sep = ''))
type8 <- raster(paste(data_path, type_path8, sep = ''))
type9 <- raster(paste(data_path, type_path9, sep = ''))

date_img <- merge(date1, date2, date3, date4, date5, date6, date7, date8, date9)
type_img <- merge(type1, type2, type3, type4, type5, type6, type7, type8, type9)

# create landsat scene bounds poly
poly_or <- landsat_tiles$geometry[landsat_tiles$PATH == 45 & landsat_tiles$ROW == 30]
poly_or <- as(poly_or, 'Spatial')
bounds_or <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 45 & landsat_tiles$ROW == 30]))

# clip the little bit outside of the landsat scene
date_img <- mask(date_img, poly_or)

# crop to smaller area
small_ext <- extent(-122.8, -122.3, 42.75, 43.25)
date_img <- crop(date_img, small_ext)
type_img <- crop(type_img, small_ext)

# convert the raster to points for plotting
date_pts <- rasterToPoints(date_img)
type_pts <- rasterToPoints(type_img)

# make the points a dataframe for ggplot
date_df <- data.frame(date_pts)
type_df <- data.frame(type_pts)

# make appropriate column headings
colnames(date_df) <- c('Longitude', 'Latitude', 'Year')
colnames(type_df) <- c('Longitude', 'Latitude', 'Disturbance')

# merge into single dataframe with both year and type
df <- inner_join(date_df, type_df, by = c('Longitude', 'Latitude'))

# get map
state <- map_data('state')
state <- state[state$region %in% c('oregon'),]

# disturbance type is a factor
df$Disturbance <- as.factor(df$Disturbance)
df$Year <- as.integer(df$Year)

gc()

# calculate patches -------------------------------------------------------

test <- get_patches(landscape = date_img, class = 'all', directions = 4, to_disk = FALSE, return_raster = TRUE)
test2 <- PatchStat(mat = test[[12]], cellsize = 30, latlon = TRUE)
writeRaster(test[[12]], '/home/annie/Desktop/deleteme.tif')
