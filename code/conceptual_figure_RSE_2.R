# conceptual figure #2 ----------------------------------------------------

# written by LAC Smith 28 Feb 2019
# last edited by LAC Smith 11 Mar 2019

# harvest example locations
# WA = 47.30154167, -123.425
# ME = 46.07035278, -69.65611111
# SC = 33.07871944, -80.52555556

# load packages -----------------------------------------------------------

library(dplyr)
library(raster)
library(data.table)
library(ggmap)
library(rasterVis)

# functions ---------------------------------------------------------------

# fill raster
fill_raster <- function(template, data, varname, newext) {
  data <- as.data.frame(data)
  true_coords <- data.table(longitude = coordinates(template)[, 1],
                            latitude = coordinates(template)[, 2],
                            value = getValues(template))  
  data <- data.frame(longitude = data$longitude, 
                                        latitude = data$latitude,
                                        value = data[, grep(paste('^', varname, '$', sep = ''), names(data))])
  true_coords$value[!is.na(true_coords$value)] <- data$value
  crs_img <- crs(template)
  coordinates(true_coords) <- ~ longitude + latitude
  gridded(true_coords) <- TRUE
  used_points <- raster(true_coords)
  projection(used_points) <- crs_img
  used_points <- crop(used_points, newext)
  
  return(used_points)
}

# paths -------------------------------------------------------------------

data_path <- '~/disturbr/data/'

# load data ---------------------------------------------------------------

# raster of disturbance type
type_wa <- raster(paste(data_path, 'attribution/disturbance_class_img_spatial_p47_r27_block5.tif', sep = ''))
type_me <- raster(paste(data_path, 'attribution/disturbance_class_img_spatial_p12_r28_block5.tif', sep = ''))
type_sc <- raster(paste(data_path, 'attribution/disturbance_class_img_spatial_p16_r37_block5.tif', sep = ''))

# check that location is on the map (locations of harvest picked out in Google Earth)
plot(type_wa)
points(x = -123.425, y = 47.30154167, col = 'red')
plot(type_me)
points(x = -69.65611111, y = 46.07035278, col = 'red')
plot(type_sc)
points(x = -80.52555556, y = 33.07871944, col = 'red')

# entire detection/attribution variable dataset
data_wa <- fread(paste(data_path, 'attribution/classified_img_data_spatial_p47_r27_block5.csv', sep = ''))
data_me <- fread(paste(data_path, 'attribution/classified_img_data_spatial_p12_r28_block5.csv', sep = ''))
data_sc <- fread(paste(data_path, 'attribution/classified_img_data_spatial_p16_r37_block5.csv', sep = ''))

# satellite map -----------------------------------------------------------

# map pretty much just the harvest area
sat_wa <- ggmap(get_googlemap(center = c(lon = -123.425, lat = 47.30154167),
                  maptype = "satellite",
                  zoom = 16, key = 'AIzaSyCl4iI4N35PgbbHW2lZxt0DgjppbmiPizc'))
sat_me <- ggmap(get_googlemap(center = c(lon = -69.65611111, lat = 46.07035278),
                              maptype = "satellite",
                              zoom = 16, key = 'AIzaSyCl4iI4N35PgbbHW2lZxt0DgjppbmiPizc'))
sat_sc <- ggmap(get_googlemap(center = c(lon = -80.52555556, lat = 33.07871944),
                              maptype = "satellite",
                              zoom = 17, key = 'AIzaSyCl4iI4N35PgbbHW2lZxt0DgjppbmiPizc'))
# write out
ggsave(filename = '~/disturbr/figs/satellite_harvest_p47_r27.png', 
       plot = sat_wa, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)
ggsave(filename = '~/disturbr/figs/satellite_harvest_p12_r28.png', 
       plot = sat_me, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)
ggsave(filename = '~/disturbr/figs/satellite_harvest_p16_r37.png', 
       plot = sat_sc, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)

# predicted map -----------------------------------------------------------

# create extents same area as satellite image
ext_wa <- extent(-123.430, -123.419, 47.297, 47.306)
ext_me <- extent(-69.664, -69.648, 46.0660, 46.0750)
ext_sc <- extent(-80.529, -80.522, 33.076, 33.082)

# crop rasters
rast_wa <- crop(x = type_wa, y = ext_wa)
rast_me <- crop(x = type_me, y = ext_me)
rast_sc <- crop(x = type_sc, y = ext_sc)

# show everything as disturbed/undisturbed
rast_wa[rast_wa >= 1] <- 1
rast_me[rast_me >= 1] <- 1
rast_sc[rast_sc >= 1] <- 1

# create maps
map_wa <- gplot(rast_wa) +  
  geom_tile(aes(fill = factor(value))) +
  scale_fill_manual(values = c('black', 'white')) +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
map_me <- gplot(rast_me) +  
  geom_tile(aes(fill = factor(value))) +
  scale_fill_manual(values = c('black', 'white')) +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
map_sc <- gplot(rast_sc) +  
  geom_tile(aes(fill = factor(value))) +
  scale_fill_manual(values = c('black', 'white')) +
  theme_bw() +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# write out
ggsave(filename = '~/disturbr/figs/predicted_harvest_p47_r27.png', 
       plot = map_wa, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)
ggsave(filename = '~/disturbr/figs/predicted_harvest_p12_r28.png', 
       plot = map_me, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)
ggsave(filename = '~/disturbr/figs/predicted_harvest_p16_r37.png', 
       plot = map_sc, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)

# var maps ----------------------------------------------------------------

# bring in forest cover rasters
forest_wa <- raster(paste(data_path, '/images/p47_r27/fill_image_p47_r27_block5.tif', sep = ''))
forest_me <- raster(paste(data_path, '/images/p12_r28/fill_image_p12_r28_block5.tif', sep = ''))
forest_sc <- raster(paste(data_path, '/images/p16_r37/fill_image_p16_r37_block5.tif', sep = ''))

# create rasters of 4 vars (magnitude, variance, spatial mag, dist mag)
mag_wa <- fill_raster(template = forest_wa, data = data_wa, varname = 'mag', newext = ext_wa)
mag_me <- fill_raster(template = forest_me, data = data_me, varname = 'mag', newext = ext_me)
mag_sc <- fill_raster(template = forest_sc, data = data_sc, varname = 'mag', newext = ext_sc)

var_wa <- fill_raster(template = forest_wa, data = data_wa, varname = 'variance', newext = ext_wa)
var_me <- fill_raster(template = forest_me, data = data_me, varname = 'variance', newext = ext_me)
var_sc <- fill_raster(template = forest_sc, data = data_sc, varname = 'variance', newext = ext_sc)

s_mag_avg_wa <- fill_raster(template = forest_wa, data = data_wa, varname = 's_mag_avg', newext = ext_wa)
s_mag_avg_me <- fill_raster(template = forest_me, data = data_me, varname = 's_mag_avg', newext = ext_me)
s_mag_avg_sc <- fill_raster(template = forest_sc, data = data_sc, varname = 's_mag_avg', newext = ext_sc)

d_mag_avg_wa <- fill_raster(template = forest_wa, data = data_wa, varname = 'd_mag_med', newext = ext_wa)
d_mag_avg_me <- fill_raster(template = forest_me, data = data_me, varname = 'd_mag_med', newext = ext_me)
d_mag_avg_sc <- fill_raster(template = forest_sc, data = data_sc, varname = 'd_mag_med', newext = ext_sc)

# read in previously-created rasters --------------------------------------

# made in code above
mag_wa <- raster('~/disturbr/figs/mag_harvest_p47_r27.tif')
mag_me <- raster('~/disturbr/figs/mag_harvest_p12_r28.tif')
mag_sc <- raster('~/disturbr/figs/mag_harvest_p16_r37.tif')

var_wa <- raster('~/disturbr/figs/var_harvest_p47_r27.tif')
var_me <- raster('~/disturbr/figs/var_harvest_p12_r28.tif')
var_sc <- raster('~/disturbr/figs/var_harvest_p16_r37.tif')

s_mag_avg_wa <- raster('~/disturbr/figs/s_mag_avg_harvest_p47_r27.tif')
s_mag_avg_me <- raster('~/disturbr/figs/s_mag_avg_harvest_p12_r28.tif')
s_mag_avg_sc <- raster('~/disturbr/figs/s_mag_avg_harvest_p16_r37.tif')

d_mag_avg_wa <- raster('~/disturbr/figs/d_mag_avg_harvest_p47_r27.tif')
d_mag_avg_me <- raster('~/disturbr/figs/d_mag_avg_harvest_p12_r28.tif')
d_mag_avg_sc <- raster('~/disturbr/figs/d_mag_avg_harvest_p16_r37.tif')


# create maps
map_mag_wa <- gplot(mag_wa) +  
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = 'A', name = 'Magnitude', limits = c(-0.53, 0.17))
map_mag_me <- gplot(mag_me) +  
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = 'A', name = 'Magnitude', limits = c(-0.53, 0.17))
map_mag_sc <- gplot(mag_sc) +  
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = 'A', name = 'Magnitude', limits = c(-0.53, 0.17))

map_var_wa <- gplot(var_wa) +  
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = 'A', name = 'Variance', limits = c(0, 0.056))
map_var_me <- gplot(var_me) +  
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = 'A', name = 'Variance', limits = c(0, 0.056))
map_var_sc <- gplot(var_sc) +  
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = 'A', name = 'Variance', limits = c(0, 0.056))

map_s_mag_avg_wa <- gplot(s_mag_avg_wa) +  
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = 'A', name = 'Mag. (Spatial Mean)', limits = c(-0.301, 0.025))
map_s_mag_avg_me <- gplot(s_mag_avg_me) +  
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = 'A', name = 'Mag. (Spatial Mean)', limits = c(-0.301, 0.025))
map_s_mag_avg_sc <- gplot(s_mag_avg_sc) +  
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = 'A', name = 'Mag. (Spatial Mean)', limits = c(-0.301, 0.025))

map_d_mag_avg_wa <- gplot(d_mag_avg_wa) +  
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = 'A', name = 'Mag. (Dist. Mean)', limits = c(-0.306, 0))
map_d_mag_avg_me <- gplot(d_mag_avg_me) +  
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = 'A', name = 'Mag. (Dist. Mean)', limits = c(-0.306, 0))
map_d_mag_avg_sc <- gplot(d_mag_avg_sc) +  
  geom_raster(aes(x = x, y = y, fill = value)) +
  scale_fill_viridis_c(option = 'A', name = 'Mag. (Dist. Mean)', limits = c(-0.306, 0))


# write out
ggsave(filename = '~/disturbr/figs/mag_harvest_p47_r27.png', 
       plot = map_mag_wa, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)
ggsave(filename = '~/disturbr/figs/mag_harvest_p12_r28.png', 
       plot = map_mag_me, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)
ggsave(filename = '~/disturbr/figs/mag_harvest_p16_r37.png', 
       plot = map_mag_sc, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)

ggsave(filename = '~/disturbr/figs/var_harvest_p47_r27.png', 
       plot = map_var_wa, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)
ggsave(filename = '~/disturbr/figs/var_harvest_p12_r28.png', 
       plot = map_var_me, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)
ggsave(filename = '~/disturbr/figs/var_harvest_p16_r37.png', 
       plot = map_var_sc, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)

ggsave(filename = '~/disturbr/figs/s_mag_avg_harvest_p47_r27.png', 
       plot = map_s_mag_avg_wa, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)
ggsave(filename = '~/disturbr/figs/s_mag_avg_harvest_p12_r28.png', 
       plot = map_s_mag_avg_me, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)
ggsave(filename = '~/disturbr/figs/s_mag_avg_harvest_p16_r37.png', 
       plot = map_s_mag_avg_sc, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)

ggsave(filename = '~/disturbr/figs/d_mag_avg_harvest_p47_r27.png', 
       plot = map_d_mag_avg_wa, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)
ggsave(filename = '~/disturbr/figs/d_mag_avg_harvest_p12_r28.png', 
       plot = map_d_mag_avg_me, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)
ggsave(filename = '~/disturbr/figs/d_mag_avg_harvest_p16_r37.png', 
       plot = map_d_mag_avg_sc, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 600)