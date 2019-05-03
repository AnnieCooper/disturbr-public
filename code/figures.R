# figures for detection/attribution paper ---------------------------------


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
library(RColorBrewer)

# set paths ---------------------------------------------------------------

data_path <- "~/disturbr/data/"
code_path <- '~/disturbr/code/'
output_path <- '~/disturbr/figs/'

data_path <- "~/Documents/satellite-field-comp/data/"
code_path <- '~/Documents/satellite-field-comp/code/final/'
output_path <- '~/Documents/satellite-field-comp/figs/'

# import data -------------------------------------------------------------

data <- fread(paste(output_path, 'figure_data_2019.csv', sep = ''), header = T, sep = ',')
data$short_name <- rep(c('OR', 'CO', 'MN', 'ME', 'SC', 'WA', 'PA/NJ'), 4)

# change classes
data$block <- as.factor(data$block)

# scatterplot of error rates ----------------------------------------------

errors <- ggplot(data[is.na(data$block),], aes(x = false_neg_raw, y = false_pos_raw)) +
  geom_point(data = data[!is.na(data$block),], mapping = aes(x = false_neg_raw, y = false_pos_raw), size = 6, color = 'darkgrey') +
  geom_point(size = 6, color = 'black') + 
  geom_text(mapping = aes(label = 'Low Severity Insect \nDamage - CO', x = 0.78, y = 0.06), col = 'darkred', size = 5) +
  geom_text(mapping = aes(label = 'Few Forested \nPoints - CO', x = 0.23, y = 0.98), col = 'darkred', size = 5) +
  geom_text(mapping = aes(label = 'Different Management - ME', x = 0.35, y = 0.54), col = 'darkred', size = 5) +
  theme_bw() +
  xlab('Omission Rate') +
  ylab('Commission Rate') +
  xlim(c(0, 1)) +
  ylim(c(0, 1)) +
  coord_equal(ratio = 1) +
  # scale_color_viridis('Region', discrete = TRUE, option = 'C') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        legend.position = 'none')
        # legend.justification = c(1,1),
        # legend.position = c(1,1))

# write out
ggsave(filename = '~/Documents/satellite-field-comp/figs/error_rate_scatter.png', 
       plot = errors, 
       device = 'png',
       height = 5,
       width = 5,
       units = 'in',
       dpi = 400)

# cumulative distribution function of year error rates --------------------

# add year15_acc (always 1)
data$year15_acc <- 1

# set alpha value
alpha <- data.frame(block = seq(1, 9), alpha = seq(0.6, 1, length.out = 9))
data$alpha <- 0
for (i in 1:nrow(data)) {
  data$alpha[i] <- alpha$alpha[alpha$block == data$block[i]]
}

# first, re-order data into observations (each year is a row) 
cd_data <- melt(data, measure.vars = grep('^year', names(data)), variable.name = 'year', value.name = 'accuracy')
cd_data$year <- as.numeric(substr(as.character(cd_data$year), 5, 6))

# define palette
palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# now plot
year_acc <- ggplot(cd_data[is.na(cd_data$block)], aes(x = year, y = accuracy, col = short_name)) +
  geom_line(aes(group = short_name), size = 1) + #interaction(area, block)), size = 1) +
  #geom_point(aes(group = area)) +
  theme_bw() +
  xlab('Years from True Disturbance Year') +
  ylab('Proportion of Disturbed Pixels') +
  xlim(c(0, 15)) +
  ylim(c(0.3, 1)) +
  scale_colour_manual(values = palette, name = 'Region') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.justification = c(1, 0),
        legend.position = c(1, 0),
        legend.background = element_blank())

# write out
ggsave(filename = '~/Documents/satellite-field-comp/figs/year_accuracy_rate_line_extrablock.png', 
       plot = year_acc, 
       device = 'png', 
       height = 4.5,
       width = 6,
       units = 'in',
       dpi = 400)

# mapped disturbances -----------------------------------------------------

# import all landsat scene bounds, convert to same CRS as rasters
landsat_tiles <- st_read('~/Documents/satellite-field-comp/data/other/WRS2_descending.shp')
landsat_tiles <- st_transform(landsat_tiles, crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

# this will be a panel plot showing year (shade) and disturbance type (color)
# function to convert blocks into full image

###### Washington

# for now, just do one block at a time (WA)
date_path <- 'attribution/disturbance_date_img_spatial_p47_r27_block1.tif'
date_path2 <- 'attribution/disturbance_date_img_spatial_p47_r27_block2.tif'
date_path3 <- 'attribution/disturbance_date_img_spatial_p47_r27_block3.tif'
date_path4 <- 'attribution/disturbance_date_img_spatial_p47_r27_block4.tif'
date_path5 <- 'attribution/disturbance_date_img_spatial_p47_r27_block5.tif'
date_path6 <- 'attribution/disturbance_date_img_spatial_p47_r27_block6.tif'
date_path7 <- 'attribution/disturbance_date_img_spatial_p47_r27_block7.tif'
date_path8 <- 'attribution/disturbance_date_img_spatial_p47_r27_block8.tif'
date_path9 <- 'attribution/disturbance_date_img_spatial_p47_r27_block9.tif'

type_path <- 'attribution/disturbance_class_img_spatial_p47_r27_block1.tif'
type_path2 <- 'attribution/disturbance_class_img_spatial_p47_r27_block2.tif'
type_path3 <- 'attribution/disturbance_class_img_spatial_p47_r27_block3.tif'
type_path4 <- 'attribution/disturbance_class_img_spatial_p47_r27_block4.tif'
type_path5 <- 'attribution/disturbance_class_img_spatial_p47_r27_block5.tif'
type_path6 <- 'attribution/disturbance_class_img_spatial_p47_r27_block6.tif'
type_path7 <- 'attribution/disturbance_class_img_spatial_p47_r27_block7.tif'
type_path8 <- 'attribution/disturbance_class_img_spatial_p47_r27_block8.tif'
type_path9 <- 'attribution/disturbance_class_img_spatial_p47_r27_block9.tif'

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
poly_wa <- landsat_tiles$geometry[landsat_tiles$PATH == 47 & landsat_tiles$ROW == 27]
poly_wa <- as(poly_wa, 'Spatial')
bounds_wa <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 47 & landsat_tiles$ROW == 27]))

# clip the little bit outside of the landsat scene
date_img <- mask(date_img, poly_wa)

# convert the raster to points for plotting
date_pts <- rasterToPoints(date_img)
type_pts <- rasterToPoints(type_img)

# Make the points a dataframe for ggplot
date_df <- data.frame(date_pts)
type_df <- data.frame(type_pts)

# Make appropriate column headings
colnames(date_df) <- c('Longitude', 'Latitude', 'Year')
colnames(type_df) <- c('Longitude', 'Latitude', 'Disturbance')

# merge into single dataframe with both year and type
df <- inner_join(date_df, type_df, by = c('Longitude', 'Latitude'))

# get washington map
state <- map_data('state')
state <- state[state$region %in% c('washington'),]

# disturbance type is a factor
df$Disturbance <- as.factor(df$Disturbance)
df$Year <- as.integer(df$Year)

# colors from swatches
download.file("https://www.pantone.com/images/pages/21348/adobe-ase/Pantone-COY18-Palette-ASE-files.zip", "ultra_violet.zip")
unique(dirname((unzip("ultra_violet.zip"))))
attitude <- read_palette("./Pantone COY18 Palette ASE files/PantoneCOY18-Attitude.ase", use_names=FALSE)

gc()

#Now make the map
wa_map <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
  geom_polygon(data = state, aes(x = long, y = lat, group = group), fill = 'white', col = 'black') +
  geom_raster(aes(fill = Disturbance, alpha = Year)) +
  geom_polygon(data = bounds_wa, aes(x = X, y = Y), fill = 'NA', col = 'grey', linetype = 2) +
  coord_fixed(xlim = c(extent(date_img)[1], extent(date_img)[2]), 
              ylim = c(extent(date_img)[3], extent(date_img)[4]), 1.3) +
  theme_bw() +
  scale_fill_manual('Disturbance \nType', breaks = c('3', '4', '5', '7', '8', '9'),
                     labels = c('Fire\n', 'Harvest\n', 'Wind\n', 'Changes in \nRiver Path', 'Anthropogenic \nLCC', 'Landslide or \nAvalanche'),
                     values = c('#FF7913', '#0079A8', 'darkslategray', "#D32E5E", 'darkgoldenrod2', 'bisque4')) +
  # scale_colour_gradient2('Disturbance \nYear', breaks = seq(2000, 2016, 2), limits = c(2000, 2014), low = 'lightgrey',
  #                     mid = 'darkgrey', high = 'black', midpoint = 2008) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_rect(colour = 'black', size = 1.5),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.justification = c(0, 0),
        legend.position = c(0, 0),
        legend.background = element_blank(),
        panel.background = element_rect(fill = 'darkgrey'))

# write out
ggsave(filename = '~/Documents/satellite-field-comp/figs/wa_map.png', 
       plot = wa_map, 
       device = 'png', 
       dpi = 400)

##### Oregon

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

# Now make the map
or_map <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
  geom_polygon(data = state, aes(x = long, y = lat, group = group), fill = 'white', col = 'black') +
  geom_raster(aes(fill = Disturbance, alpha = Year)) +
  geom_polygon(data = bounds_or, aes(x = X, y = Y), fill = 'NA', col = 'grey', linetype = 2) +
  coord_fixed(xlim = c(extent(date_img)[1], extent(date_img)[2]), 
              ylim = c(extent(date_img)[3], extent(date_img)[4]), 1.3) +
  theme_bw() +
  scale_fill_manual('Disturbance \nType', breaks = c('2', '3', '4'),
                    labels = c('Biotic\n', 'Fire\n', 'Harvest\n'),
                    values = c('darkseagreen', '#FF7913', '#0079A8')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_rect(colour = 'black', size = 1.5),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.justification = c(0, 0),
        legend.position = c(0, 0),
        legend.background = element_blank(),
        panel.background = element_rect(fill = 'darkgrey'))

# write out
ggsave(filename = '~/Documents/satellite-field-comp/figs/or_map.png', 
       plot = or_map, 
       device = 'png', 
       dpi = 400)

##### Colorado

# for now, just do one block at a time
date_path <- 'attribution/disturbance_date_img_spatial_p35_r32_block1.tif'
date_path2 <- 'attribution/disturbance_date_img_spatial_p35_r32_block2.tif'
date_path3 <- 'attribution/disturbance_date_img_spatial_p35_r32_block3.tif'
date_path4 <- 'attribution/disturbance_date_img_spatial_p35_r32_block4.tif'
date_path5 <- 'attribution/disturbance_date_img_spatial_p35_r32_block5.tif'
date_path6 <- 'attribution/disturbance_date_img_spatial_p35_r32_block6.tif'
date_path7 <- 'attribution/disturbance_date_img_spatial_p35_r32_block7.tif'
date_path8 <- 'attribution/disturbance_date_img_spatial_p35_r32_block8.tif'
date_path9 <- 'attribution/disturbance_date_img_spatial_p35_r32_block9.tif'

type_path <- 'attribution/disturbance_class_img_spatial_p35_r32_block1.tif'
type_path2 <- 'attribution/disturbance_class_img_spatial_p35_r32_block2.tif'
type_path3 <- 'attribution/disturbance_class_img_spatial_p35_r32_block3.tif'
type_path4 <- 'attribution/disturbance_class_img_spatial_p35_r32_block4.tif'
type_path5 <- 'attribution/disturbance_class_img_spatial_p35_r32_block5.tif'
type_path6 <- 'attribution/disturbance_class_img_spatial_p35_r32_block6.tif'
type_path7 <- 'attribution/disturbance_class_img_spatial_p35_r32_block7.tif'
type_path8 <- 'attribution/disturbance_class_img_spatial_p35_r32_block8.tif'
type_path9 <- 'attribution/disturbance_class_img_spatial_p35_r32_block9.tif'

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
poly_co <- landsat_tiles$geometry[landsat_tiles$PATH == 35 & landsat_tiles$ROW == 32]
poly_co <- as(poly_co, 'Spatial')
bounds_co <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 35 & landsat_tiles$ROW == 32]))

# clip the little bit outside of the landsat scene
date_img <- mask(date_img, poly_co)

# convert the raster to points for plotting
date_pts <- rasterToPoints(date_img)
type_pts <- rasterToPoints(type_img)

# Make the points a dataframe for ggplot
date_df <- data.frame(date_pts)
type_df <- data.frame(type_pts)

# Make appropriate column headings
colnames(date_df) <- c('Longitude', 'Latitude', 'Year')
colnames(type_df) <- c('Longitude', 'Latitude', 'Disturbance')

# merge into single dataframe with both year and type
df <- inner_join(date_df, type_df, by = c('Longitude', 'Latitude'))

# get washington map
state <- map_data('state')
state <- state[state$region %in% c('colorado', 'wyoming'),]

# disturbance type is a factor
df$Disturbance <- as.factor(df$Disturbance)
df$Year <- as.integer(df$Year)

gc()

#Now make the map
co_map <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
  geom_polygon(data = state, aes(x = long, y = lat, group = group), fill = 'white', col = 'black') +
  geom_raster(aes(fill = Disturbance, alpha = Year)) +
  geom_polygon(data = bounds_co, aes(x = X, y = Y), fill = 'NA', col = 'grey', linetype = 2) +
  coord_fixed(xlim = c(extent(date_img)[1], extent(date_img)[2]), 
              ylim = c(extent(date_img)[3], extent(date_img)[4]), 1.3) +
  theme_bw() +
  scale_fill_manual('Disturbance \nType', breaks = c('2', '3', '4', '8'),
                    labels = c('Biotic\n', 'Fire\n', 'Harvest\n', 'Anthropogenic \nLCC'),
                    values = c('darkseagreen', '#FF7913', '#0079A8', 'darkgoldenrod2')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_rect(colour = 'black', size = 1.5),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.justification = c(0, 0),
        legend.position = c(0, 0),
        legend.background = element_blank(),
        panel.background = element_rect(fill = 'darkgrey'))

# write out
ggsave(filename = '~/Documents/satellite-field-comp/figs/co_map.png', 
       plot = co_map, 
       device = 'png', 
       dpi = 400)

##### Pennsylvania

# for now, just do one block at a time
date_path <- 'attribution/disturbance_date_img_spatial_p14_r32_block1.tif'
date_path2 <- 'attribution/disturbance_date_img_spatial_p14_r32_block2.tif'
date_path3 <- 'attribution/disturbance_date_img_spatial_p14_r32_block3.tif'
date_path4 <- 'attribution/disturbance_date_img_spatial_p14_r32_block4.tif'
date_path5 <- 'attribution/disturbance_date_img_spatial_p14_r32_block5.tif'
date_path6 <- 'attribution/disturbance_date_img_spatial_p14_r32_block6.tif'
date_path7 <- 'attribution/disturbance_date_img_spatial_p14_r32_block7.tif'
date_path8 <- 'attribution/disturbance_date_img_spatial_p14_r32_block8.tif'
date_path9 <- 'attribution/disturbance_date_img_spatial_p14_r32_block9.tif'

type_path <- 'attribution/disturbance_class_img_spatial_p14_r32_block1.tif'
type_path2 <- 'attribution/disturbance_class_img_spatial_p14_r32_block2.tif'
type_path3 <- 'attribution/disturbance_class_img_spatial_p14_r32_block3.tif'
type_path4 <- 'attribution/disturbance_class_img_spatial_p14_r32_block4.tif'
type_path5 <- 'attribution/disturbance_class_img_spatial_p14_r32_block5.tif'
type_path6 <- 'attribution/disturbance_class_img_spatial_p14_r32_block6.tif'
type_path7 <- 'attribution/disturbance_class_img_spatial_p14_r32_block7.tif'
type_path8 <- 'attribution/disturbance_class_img_spatial_p14_r32_block8.tif'
type_path9 <- 'attribution/disturbance_class_img_spatial_p14_r32_block9.tif'

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
poly_pa <- landsat_tiles$geometry[landsat_tiles$PATH == 14 & landsat_tiles$ROW == 32]
poly_pa <- as(poly_pa, 'Spatial')
bounds_pa <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 14 & landsat_tiles$ROW == 32]))

# clip the little bit outside of the landsat scene
date_img <- mask(date_img, poly_pa)
type_img <- mask(type_img, poly_pa)

# convert the raster to points for plotting
date_pts <- rasterToPoints(date_img)
type_pts <- rasterToPoints(type_img)

# Make the points a dataframe for ggplot
date_df <- data.frame(date_pts)
type_df <- data.frame(type_pts)

# Make appropriate column headings
colnames(date_df) <- c('Longitude', 'Latitude', 'Year')
colnames(type_df) <- c('Longitude', 'Latitude', 'Disturbance')

# merge into single dataframe with both year and type
df <- inner_join(date_df, type_df, by = c('Longitude', 'Latitude'))

# get washington map
state <- map_data('state')
state <- state[state$region %in% c('pennsylvania', 'new jersey', 'maryland', 'new york', 'delaware'),]

# disturbance type is a factor
df$Disturbance <- as.factor(df$Disturbance)
df$Year <- as.integer(df$Year)

gc()

# Now make the map
pa_map <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
  geom_polygon(data = state, aes(x = long, y = lat, group = group), fill = 'white', col = 'black') +
  geom_raster(aes(fill = Disturbance, alpha = Year)) +
  geom_polygon(data = bounds_pa, aes(x = X, y = Y), fill = 'NA', col = 'grey', linetype = 2) +
  coord_fixed(xlim = c(extent(date_img)[1], extent(date_img)[2]), 
              ylim = c(extent(date_img)[3], extent(date_img)[4]), 1.3) +
  theme_bw() +
  scale_fill_manual('Disturbance \nType', breaks = c('3', '4', '8'),
                    labels = c('Fire\n', 'Harvest\n', 'Anthropogenic \nLCC'),
                    values = c('#FF7913', '#0079A8', 'darkgoldenrod2')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_rect(colour = 'black', size = 1.5),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.background = element_blank(),
        panel.background = element_rect(fill = 'darkgrey'))

# write out
ggsave(filename = '~/Documents/satellite-field-comp/figs/pa_map.png', 
       plot = pa_map, 
       device = 'png', 
       dpi = 400)

##### Minnesota

# for now, just do one block at a time
date_path <- 'attribution/disturbance_date_img_spatial_p27_r27_block1.tif'
date_path2 <- 'attribution/disturbance_date_img_spatial_p27_r27_block2.tif'
date_path3 <- 'attribution/disturbance_date_img_spatial_p27_r27_block3.tif'
date_path4 <- 'attribution/disturbance_date_img_spatial_p27_r27_block4.tif'
date_path5 <- 'attribution/disturbance_date_img_spatial_p27_r27_block5.tif'
date_path6 <- 'attribution/disturbance_date_img_spatial_p27_r27_block6.tif'
date_path7 <- 'attribution/disturbance_date_img_spatial_p27_r27_block7.tif'
date_path8 <- 'attribution/disturbance_date_img_spatial_p27_r27_block8.tif'
date_path9 <- 'attribution/disturbance_date_img_spatial_p27_r27_block9.tif'

type_path <- 'attribution/disturbance_class_img_spatial_p27_r27_block1.tif'
type_path2 <- 'attribution/disturbance_class_img_spatial_p27_r27_block2.tif'
type_path3 <- 'attribution/disturbance_class_img_spatial_p27_r27_block3.tif'
type_path4 <- 'attribution/disturbance_class_img_spatial_p27_r27_block4.tif'
type_path5 <- 'attribution/disturbance_class_img_spatial_p27_r27_block5.tif'
type_path6 <- 'attribution/disturbance_class_img_spatial_p27_r27_block6.tif'
type_path7 <- 'attribution/disturbance_class_img_spatial_p27_r27_block7.tif'
type_path8 <- 'attribution/disturbance_class_img_spatial_p27_r27_block8.tif'
type_path9 <- 'attribution/disturbance_class_img_spatial_p27_r27_block9.tif'

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
poly_mn <- landsat_tiles$geometry[landsat_tiles$PATH == 27 & landsat_tiles$ROW == 27]
poly_mn <- as(poly_mn, 'Spatial')
bounds_mn <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 27 & landsat_tiles$ROW == 27]))

# clip the little bit outside of the landsat scene
date_img <- mask(date_img, poly_mn)
type_img <- mask(type_img, poly_mn)

# convert the raster to points for plotting
date_pts <- rasterToPoints(date_img)
type_pts <- rasterToPoints(type_img)

# Make the points a dataframe for ggplot
date_df <- data.frame(date_pts)
type_df <- data.frame(type_pts)

# Make appropriate column headings
colnames(date_df) <- c('Longitude', 'Latitude', 'Year')
colnames(type_df) <- c('Longitude', 'Latitude', 'Disturbance')

# merge into single dataframe with both year and type
df <- inner_join(date_df, type_df, by = c('Longitude', 'Latitude'))

# get map
state <- map_data('state')
state <- state[state$region %in% c('minnesota', 'wisconsin'),]

# disturbance type is a factor
df$Disturbance <- as.factor(df$Disturbance)
df$Year <- as.integer(df$Year)

gc()

# Now make the map
mn_map <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
  geom_polygon(data = state, aes(x = long, y = lat, group = group), fill = 'white', col = 'black') +
  geom_raster(aes(fill = Disturbance, alpha = Year)) +
  geom_polygon(data = bounds_mn, aes(x = X, y = Y), fill = 'NA', col = 'grey', linetype = 2) +
  coord_fixed(xlim = c(extent(date_img)[1], extent(date_img)[2]), 
              ylim = c(extent(date_img)[3], extent(date_img)[4]), 1.3) +
  theme_bw() +
  scale_fill_manual('Disturbance \nType', breaks = c('4', '7', '8'),
                    labels = c('Harvest\n', 'Changes in \nRiver Path', 'Anthropogenic \nLCC'),
                    values = c('#0079A8', "#D32E5E", 'darkgoldenrod2')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_rect(colour = 'black', size = 1.5),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.background = element_blank(),
        panel.background = element_rect(fill = 'darkgrey'))

# write out
ggsave(filename = '~/Documents/satellite-field-comp/figs/mn_map.png', 
       plot = mn_map, 
       device = 'png', 
       dpi = 400)

##### South Carolina

# for now, just do one block at a time
date_path <- 'attribution/disturbance_date_img_spatial_p16_r37_block1.tif'
date_path2 <- 'attribution/disturbance_date_img_spatial_p16_r37_block2.tif'
date_path3 <- 'attribution/disturbance_date_img_spatial_p16_r37_block3.tif'
date_path4 <- 'attribution/disturbance_date_img_spatial_p16_r37_block4.tif'
date_path5 <- 'attribution/disturbance_date_img_spatial_p16_r37_block5.tif'
date_path6 <- 'attribution/disturbance_date_img_spatial_p16_r37_block6.tif'
date_path7 <- 'attribution/disturbance_date_img_spatial_p16_r37_block7.tif'
date_path8 <- 'attribution/disturbance_date_img_spatial_p16_r37_block8.tif'
date_path9 <- 'attribution/disturbance_date_img_spatial_p16_r37_block9.tif'

type_path <- 'attribution/disturbance_class_img_spatial_p16_r37_block1.tif'
type_path2 <- 'attribution/disturbance_class_img_spatial_p16_r37_block2.tif'
type_path3 <- 'attribution/disturbance_class_img_spatial_p16_r37_block3.tif'
type_path4 <- 'attribution/disturbance_class_img_spatial_p16_r37_block4.tif'
type_path5 <- 'attribution/disturbance_class_img_spatial_p16_r37_block5.tif'
type_path6 <- 'attribution/disturbance_class_img_spatial_p16_r37_block6.tif'
type_path7 <- 'attribution/disturbance_class_img_spatial_p16_r37_block7.tif'
type_path8 <- 'attribution/disturbance_class_img_spatial_p16_r37_block8.tif'
type_path9 <- 'attribution/disturbance_class_img_spatial_p16_r37_block9.tif'

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
poly_sc <- landsat_tiles$geometry[landsat_tiles$PATH == 16 & landsat_tiles$ROW == 37]
poly_sc <- as(poly_sc, 'Spatial')
bounds_sc <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 16 & landsat_tiles$ROW == 37]))

# clip the little bit outside of the landsat scene
date_img <- mask(date_img, poly_sc)
type_img <- mask(type_img, poly_sc)

# convert the raster to points for plotting
date_pts <- rasterToPoints(date_img)
type_pts <- rasterToPoints(type_img)

# Make the points a dataframe for ggplot
date_df <- data.frame(date_pts)
type_df <- data.frame(type_pts)

# Make appropriate column headings
colnames(date_df) <- c('Longitude', 'Latitude', 'Year')
colnames(type_df) <- c('Longitude', 'Latitude', 'Disturbance')

# merge into single dataframe with both year and type
df <- inner_join(date_df, type_df, by = c('Longitude', 'Latitude'))

# get washington map
state <- map_data('state')
state <- state[state$region %in% c('south carolina', 'georgia', 'north carolina'),]

# disturbance type is a factor
df$Disturbance <- as.factor(df$Disturbance)
df$Year <- as.integer(df$Year)

gc()

# Now make the map
sc_map <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
  geom_polygon(data = state, aes(x = long, y = lat, group = group), fill = 'white', col = 'black') +
  geom_raster(aes(fill = Disturbance, alpha = Year)) +
  geom_polygon(data = bounds_sc, aes(x = X, y = Y), fill = 'NA', col = 'grey', linetype = 2) +
  coord_fixed(xlim = c(extent(date_img)[1], extent(date_img)[2]), 
              ylim = c(extent(date_img)[3], extent(date_img)[4]), 1.3) +
  theme_bw() +
  scale_fill_manual('Disturbance \nType', breaks = c('3', '4', '7', '8'),
                    labels = c('Fire\n', 'Harvest\n', 'Changes in \nRiver Path', 'Anthropogenic \nLCC'),
                    values = c('#FF7913', '#0079A8', "#D32E5E", 'darkgoldenrod2')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_rect(colour = 'black', size = 1.5),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.background = element_blank(),
        panel.background = element_rect(fill = 'darkgrey'))

# write out
ggsave(filename = '~/Documents/satellite-field-comp/figs/sc_map.png', 
       plot = sc_map, 
       device = 'png', 
       dpi = 400)

##### Maine

# for now, just do one block at a time
date_path <- 'attribution/disturbance_date_img_spatial_p12_r28_block1.tif'
date_path2 <- 'attribution/disturbance_date_img_spatial_p12_r28_block2.tif'
date_path3 <- 'attribution/disturbance_date_img_spatial_p12_r28_block3.tif'
date_path4 <- 'attribution/disturbance_date_img_spatial_p12_r28_block4.tif'
date_path5 <- 'attribution/disturbance_date_img_spatial_p12_r28_block5.tif'
date_path6 <- 'attribution/disturbance_date_img_spatial_p12_r28_block6.tif'
date_path7 <- 'attribution/disturbance_date_img_spatial_p12_r28_block7.tif'
date_path8 <- 'attribution/disturbance_date_img_spatial_p12_r28_block8.tif'
date_path9 <- 'attribution/disturbance_date_img_spatial_p12_r28_block9.tif'

type_path <- 'attribution/disturbance_class_img_spatial_p12_r28_block1.tif'
type_path2 <- 'attribution/disturbance_class_img_spatial_p12_r28_block2.tif'
type_path3 <- 'attribution/disturbance_class_img_spatial_p12_r28_block3.tif'
type_path4 <- 'attribution/disturbance_class_img_spatial_p12_r28_block4.tif'
type_path5 <- 'attribution/disturbance_class_img_spatial_p12_r28_block5.tif'
type_path6 <- 'attribution/disturbance_class_img_spatial_p12_r28_block6.tif'
type_path7 <- 'attribution/disturbance_class_img_spatial_p12_r28_block7.tif'
type_path8 <- 'attribution/disturbance_class_img_spatial_p12_r28_block8.tif'
type_path9 <- 'attribution/disturbance_class_img_spatial_p12_r28_block9.tif'

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
poly_me <- landsat_tiles$geometry[landsat_tiles$PATH == 12 & landsat_tiles$ROW == 28]
poly_me <- as(poly_me, 'Spatial')
bounds_me <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 12 & landsat_tiles$ROW == 28]))

# clip the little bit outside of the landsat scene
date_img <- mask(date_img, poly_me)
type_img <- mask(type_img, poly_me)

# convert the raster to points for plotting
date_pts <- rasterToPoints(date_img)
type_pts <- rasterToPoints(type_img)

# Make the points a dataframe for ggplot
date_df <- data.frame(date_pts)
type_df <- data.frame(type_pts)

# Make appropriate column headings
colnames(date_df) <- c('Longitude', 'Latitude', 'Year')
colnames(type_df) <- c('Longitude', 'Latitude', 'Disturbance')

# merge into single dataframe with both year and type
df <- inner_join(date_df, type_df, by = c('Longitude', 'Latitude'))

# get washington map
state <- map_data('world')
state <- state[state$region %in% c('USA', 'Canada'),]

# disturbance type is a factor
df$Disturbance <- as.factor(df$Disturbance)
df$Year <- as.integer(df$Year)

gc()

# Now make the map
me_map <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
  geom_polygon(data = state, aes(x = long, y = lat, group = group), fill = 'white', col = 'black') +
  geom_raster(aes(fill = Disturbance, alpha = Year)) +
  geom_polygon(data = bounds_me, aes(x = X, y = Y), fill = 'NA', col = 'grey', linetype = 2) +
  coord_fixed(xlim = c(extent(date_img)[1], extent(date_img)[2]), 
              ylim = c(extent(date_img)[3], extent(date_img)[4]), 1.3) +
  theme_bw() +
  scale_fill_manual('Disturbance \nType', breaks = c('3', '4', '7', '8'),
                    labels = c('Fire\n', 'Harvest\n', 'Changes in \nRiver Path', 'Anthropogenic \nLCC'),
                    values = c('#FF7913', '#0079A8', "#D32E5E", 'darkgoldenrod2')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_rect(colour = 'black', size = 1.5),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.background = element_blank(),
        panel.background = element_rect(fill = 'darkgrey'))

# write out
ggsave(filename = '~/Documents/satellite-field-comp/figs/me_map.png', 
       plot = me_map, 
       device = 'png', 
       dpi = 400)

# zoomed maps of disturbance ---------------------------------------

##### Oregon

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

# Now make the map
or_map_zoom <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
  geom_polygon(data = state, aes(x = long, y = lat, group = group), fill = 'white', col = 'black') +
  geom_raster(aes(fill = Disturbance, alpha = Year)) +
  geom_polygon(data = bounds_or, aes(x = X, y = Y), fill = 'NA', col = 'grey', linetype = 2) +
  # coord_fixed(xlim = c(-122.8, -122.3), 
  #             ylim = c(42.75, 43.25)) +
  coord_fixed(xlim = c(-121.95, -121.8),
              ylim = c(43.45, 43.60)) +
  theme_bw() +
  scale_fill_manual('Disturbance \nType', breaks = c('2', '3', '4'),
                    labels = c('Biotic\n', 'Fire\n', 'Harvest\n'),
                    values = c('darkseagreen', '#FF7913', '#0079A8')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_rect(colour = 'black', size = 1.5),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        panel.background = element_rect(fill = 'darkgrey'))

# write out
ggsave(filename = '~/Documents/satellite-field-comp/figs/or_map_zoom_small.png', 
       plot = or_map_zoom, 
       device = 'png', 
       dpi = 400)


##### Minnesota

# for now, just do one block at a time
date_path <- 'attribution/disturbance_date_img_spatial_p27_r27_block1.tif'
date_path2 <- 'attribution/disturbance_date_img_spatial_p27_r27_block2.tif'
date_path3 <- 'attribution/disturbance_date_img_spatial_p27_r27_block3.tif'
date_path4 <- 'attribution/disturbance_date_img_spatial_p27_r27_block4.tif'
date_path5 <- 'attribution/disturbance_date_img_spatial_p27_r27_block5.tif'
date_path6 <- 'attribution/disturbance_date_img_spatial_p27_r27_block6.tif'
date_path7 <- 'attribution/disturbance_date_img_spatial_p27_r27_block7.tif'
date_path8 <- 'attribution/disturbance_date_img_spatial_p27_r27_block8.tif'
date_path9 <- 'attribution/disturbance_date_img_spatial_p27_r27_block9.tif'

type_path <- 'attribution/disturbance_class_img_spatial_p27_r27_block1.tif'
type_path2 <- 'attribution/disturbance_class_img_spatial_p27_r27_block2.tif'
type_path3 <- 'attribution/disturbance_class_img_spatial_p27_r27_block3.tif'
type_path4 <- 'attribution/disturbance_class_img_spatial_p27_r27_block4.tif'
type_path5 <- 'attribution/disturbance_class_img_spatial_p27_r27_block5.tif'
type_path6 <- 'attribution/disturbance_class_img_spatial_p27_r27_block6.tif'
type_path7 <- 'attribution/disturbance_class_img_spatial_p27_r27_block7.tif'
type_path8 <- 'attribution/disturbance_class_img_spatial_p27_r27_block8.tif'
type_path9 <- 'attribution/disturbance_class_img_spatial_p27_r27_block9.tif'

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
poly_mn <- landsat_tiles$geometry[landsat_tiles$PATH == 27 & landsat_tiles$ROW == 27]
poly_mn <- as(poly_mn, 'Spatial')
bounds_mn <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 27 & landsat_tiles$ROW == 27]))

# clip the little bit outside of the landsat scene
date_img <- mask(date_img, poly_mn)
type_img <- mask(type_img, poly_mn)

# convert the raster to points for plotting
date_pts <- rasterToPoints(date_img)
type_pts <- rasterToPoints(type_img)

# Make the points a dataframe for ggplot
date_df <- data.frame(date_pts)
type_df <- data.frame(type_pts)

# Make appropriate column headings
colnames(date_df) <- c('Longitude', 'Latitude', 'Year')
colnames(type_df) <- c('Longitude', 'Latitude', 'Disturbance')

# merge into single dataframe with both year and type
df <- inner_join(date_df, type_df, by = c('Longitude', 'Latitude'))

# get map
state <- map_data('state')
state <- state[state$region %in% c('minnesota', 'wisconsin'),]

# disturbance type is a factor
df$Disturbance <- as.factor(df$Disturbance)
df$Year <- as.integer(df$Year)

gc()

# Now make the map
mn_map_zoom <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
  geom_polygon(data = state, aes(x = long, y = lat, group = group), fill = 'white', col = 'black') +
  geom_raster(aes(fill = Disturbance, alpha = Year)) +
  geom_polygon(data = bounds_mn, aes(x = X, y = Y), fill = 'NA', col = 'grey', linetype = 2) +
  coord_fixed(xlim = c(-91.8, -91.25), 
              ylim = c(47.75, 48.15), 1.3) +
  theme_bw() +
  scale_fill_manual('Disturbance \nType', breaks = c('4', '7', '8'),
                    labels = c('Harvest\n', 'Changes in \nRiver Path', 'Anthropogenic \nLCC'),
                    values = c('#0079A8', "#D32E5E", '#E72838')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_rect(colour = 'black', size = 1.5),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.background = element_blank(),
        panel.background = element_rect(fill = 'darkgrey'))

# write out
ggsave(filename = '~/Documents/satellite-field-comp/figs/mn_map_zoom.png', 
       plot = mn_map_zoom, 
       device = 'png', 
       dpi = 400)

##### Maine

# for now, just do one block at a time
date_path <- 'attribution/disturbance_date_img_spatial_p12_r28_block1.tif'
date_path2 <- 'attribution/disturbance_date_img_spatial_p12_r28_block2.tif'
date_path3 <- 'attribution/disturbance_date_img_spatial_p12_r28_block3.tif'
date_path4 <- 'attribution/disturbance_date_img_spatial_p12_r28_block4.tif'
date_path5 <- 'attribution/disturbance_date_img_spatial_p12_r28_block5.tif'
date_path6 <- 'attribution/disturbance_date_img_spatial_p12_r28_block6.tif'
date_path7 <- 'attribution/disturbance_date_img_spatial_p12_r28_block7.tif'
date_path8 <- 'attribution/disturbance_date_img_spatial_p12_r28_block8.tif'
date_path9 <- 'attribution/disturbance_date_img_spatial_p12_r28_block9.tif'

type_path <- 'attribution/disturbance_class_img_spatial_p12_r28_block1.tif'
type_path2 <- 'attribution/disturbance_class_img_spatial_p12_r28_block2.tif'
type_path3 <- 'attribution/disturbance_class_img_spatial_p12_r28_block3.tif'
type_path4 <- 'attribution/disturbance_class_img_spatial_p12_r28_block4.tif'
type_path5 <- 'attribution/disturbance_class_img_spatial_p12_r28_block5.tif'
type_path6 <- 'attribution/disturbance_class_img_spatial_p12_r28_block6.tif'
type_path7 <- 'attribution/disturbance_class_img_spatial_p12_r28_block7.tif'
type_path8 <- 'attribution/disturbance_class_img_spatial_p12_r28_block8.tif'
type_path9 <- 'attribution/disturbance_class_img_spatial_p12_r28_block9.tif'

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
poly_me <- landsat_tiles$geometry[landsat_tiles$PATH == 12 & landsat_tiles$ROW == 28]
poly_me <- as(poly_me, 'Spatial')
bounds_me <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 12 & landsat_tiles$ROW == 28]))

# clip the little bit outside of the landsat scene
date_img <- mask(date_img, poly_me)
type_img <- mask(type_img, poly_me)

# convert the raster to points for plotting
date_pts <- rasterToPoints(date_img)
type_pts <- rasterToPoints(type_img)

# Make the points a dataframe for ggplot
date_df <- data.frame(date_pts)
type_df <- data.frame(type_pts)

# Make appropriate column headings
colnames(date_df) <- c('Longitude', 'Latitude', 'Year')
colnames(type_df) <- c('Longitude', 'Latitude', 'Disturbance')

# merge into single dataframe with both year and type
df <- inner_join(date_df, type_df, by = c('Longitude', 'Latitude'))

# get washington map
state <- map_data('world')
state <- state[state$region %in% c('USA', 'Canada'),]

# disturbance type is a factor
df$Disturbance <- as.factor(df$Disturbance)
df$Year <- as.integer(df$Year)

gc()

# Now make the map
me_map <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
  geom_polygon(data = state, aes(x = long, y = lat, group = group), fill = 'white', col = 'black') +
  geom_raster(aes(fill = Disturbance, alpha = Year)) +
  geom_polygon(data = bounds_me, aes(x = X, y = Y), fill = 'NA', col = 'grey', linetype = 2) +
  coord_fixed(xlim = c(-69.8, -69.7), 
              ylim = c(45.4, 45.5), 1) +
  theme_bw() +
  scale_fill_manual('Disturbance \nType', breaks = c('3', '4', '7', '8'),
                    labels = c('Fire\n', 'Harvest\n', 'Changes in \nRiver Path', 'Anthropogenic \nLCC'),
                    values = c('#FF7913', '#0079A8', "#D32E5E", 'darkgoldenrod2')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_rect(colour = 'black', size = 1.5),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.justification = c(0, 1),
        legend.position = c(0, 1),
        legend.background = element_blank(),
        panel.background = element_rect(fill = 'darkgrey'))

# write out
ggsave(filename = '~/Documents/satellite-field-comp/figs/me_map_zoom.png', 
       plot = me_map, 
       device = 'png', 
       dpi = 400)

##### South Carolina

# for now, just do one block at a time
date_path <- 'attribution/disturbance_date_img_spatial_p16_r37_block1.tif'
date_path2 <- 'attribution/disturbance_date_img_spatial_p16_r37_block2.tif'
date_path3 <- 'attribution/disturbance_date_img_spatial_p16_r37_block3.tif'
date_path4 <- 'attribution/disturbance_date_img_spatial_p16_r37_block4.tif'
date_path5 <- 'attribution/disturbance_date_img_spatial_p16_r37_block5.tif'
date_path6 <- 'attribution/disturbance_date_img_spatial_p16_r37_block6.tif'
date_path7 <- 'attribution/disturbance_date_img_spatial_p16_r37_block7.tif'
date_path8 <- 'attribution/disturbance_date_img_spatial_p16_r37_block8.tif'
date_path9 <- 'attribution/disturbance_date_img_spatial_p16_r37_block9.tif'

type_path <- 'attribution/disturbance_class_img_spatial_p16_r37_block1.tif'
type_path2 <- 'attribution/disturbance_class_img_spatial_p16_r37_block2.tif'
type_path3 <- 'attribution/disturbance_class_img_spatial_p16_r37_block3.tif'
type_path4 <- 'attribution/disturbance_class_img_spatial_p16_r37_block4.tif'
type_path5 <- 'attribution/disturbance_class_img_spatial_p16_r37_block5.tif'
type_path6 <- 'attribution/disturbance_class_img_spatial_p16_r37_block6.tif'
type_path7 <- 'attribution/disturbance_class_img_spatial_p16_r37_block7.tif'
type_path8 <- 'attribution/disturbance_class_img_spatial_p16_r37_block8.tif'
type_path9 <- 'attribution/disturbance_class_img_spatial_p16_r37_block9.tif'

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
poly_sc <- landsat_tiles$geometry[landsat_tiles$PATH == 16 & landsat_tiles$ROW == 37]
poly_sc <- as(poly_sc, 'Spatial')
bounds_sc <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 16 & landsat_tiles$ROW == 37]))

# clip the little bit outside of the landsat scene
date_img <- mask(date_img, poly_sc)
type_img <- mask(type_img, poly_sc)

# convert the raster to points for plotting
date_pts <- rasterToPoints(date_img)
type_pts <- rasterToPoints(type_img)

# Make the points a dataframe for ggplot
date_df <- data.frame(date_pts)
type_df <- data.frame(type_pts)

# Make appropriate column headings
colnames(date_df) <- c('Longitude', 'Latitude', 'Year')
colnames(type_df) <- c('Longitude', 'Latitude', 'Disturbance')

# merge into single dataframe with both year and type
df <- inner_join(date_df, type_df, by = c('Longitude', 'Latitude'))

# get washington map
state <- map_data('state')
state <- state[state$region %in% c('South Carolina'),]

# disturbance type is a factor
df$Disturbance <- as.factor(df$Disturbance)
df$Year <- as.integer(df$Year)

gc()

# Now make the map
sc_map <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
  geom_polygon(data = state, aes(x = long, y = lat, group = group), fill = 'white', col = 'black') +
  geom_raster(aes(fill = Disturbance, alpha = Year)) +
  geom_polygon(data = bounds_sc, aes(x = X, y = Y), fill = 'NA', col = 'grey', linetype = 2) +
  coord_fixed(xlim = c(-80.1, -79.95), 
              ylim = c(33.0, 33.15), 1) +
  theme_bw() +
  scale_fill_manual('Disturbance \nType', breaks = c('3', '4', '7', '8'),
                    labels = c('Fire\n', 'Harvest\n', 'Changes in \nRiver Path', 'Anthropogenic \nLCC'),
                    values = c('#FF7913', '#0079A8', "#D32E5E", '#E72838')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_rect(colour = 'black', size = 1.5),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'))

# write out
ggsave(filename = '~/Documents/satellite-field-comp/figs/sc_map_zoom_small.png', 
       plot = sc_map, 
       device = 'png', 
       dpi = 400)

# types of mislabeled disturbances ---------------------------------

# 1: import data and label with region and block
file_list <- list.files(path = paste(data_path, 'validation/', sep = ''), pattern = 'attributed_sample_data_disturbed_spatial_', full.names = TRUE)

# import csv and add block/region
readin <- function(f) {
  data <- read.csv(f, header = TRUE, stringsAsFactors = FALSE)
  data$region <- substr(f, 102, 108)
  data$block <- as.numeric(substr(f, 115, 115))
  return(data)
}

# import all csvs
combine_csvs <- function(file_list) {
  data <- map_dfr(file_list, readin)
  return(data)
}

data <- combine_csvs(file_list)

# create data frame of errors
raw_counts <-  data %>%
  filter(actual_dist > 0 & actual_dist != predicted) %>%
  mutate(predicted = replace(predicted, predicted == '2', 'Biotic'),
         predicted = replace(predicted, predicted == '3', 'Fire'),
         predicted = replace(predicted, predicted == '4', 'Harvest'),
         predicted = replace(predicted, predicted == '7', 'Water'),
         predicted = replace(predicted, predicted == '8', 'Development'),
         actual_dist = replace(actual_dist, actual_dist == '1', 'Undisturbed'),
         actual_dist = replace(actual_dist, actual_dist == '2', 'Biotic'),
         actual_dist = replace(actual_dist, actual_dist == '3', 'Fire'),
         actual_dist = replace(actual_dist, actual_dist == '4', 'Harvest'),
         actual_dist = replace(actual_dist, actual_dist == '7', 'Water'),
         actual_dist = replace(actual_dist, actual_dist == '8', 'Development')) %>%
  mutate(error_type = paste(predicted, actual_dist, sep = '/')) %>%
  mutate(error_type = replace(error_type, actual_dist == 'Undisturbed', 'False Positive')) %>%
  group_by(region, block, error_type) %>%
  summarize(count = n())

# add in rank
block_order <- data %>%
  filter(actual_dist > 0) %>%
  dplyr::select(region, block) %>%
  group_by(region) %>%
  distinct() %>%
  mutate(block_rank = order(block))

# combine everything into raw and percents (percent of all labeled disturbance)
all_errors <- data %>%
  filter(actual_dist > 0) %>%
  dplyr::select(region, block) %>%
  group_by(region, block) %>%
  summarize(total = n()) %>%
  right_join(raw_counts) %>%
  mutate(percent = count / total) %>%
  left_join(block_order) %>%
  ungroup() %>%
  mutate(region = replace(region, region == 'p12_r28', 'ME'),
         region = replace(region, region == 'p14_r32', 'PA/NJ'),
         region = replace(region, region == 'p16_r37', 'SC'),
         region = replace(region, region == 'p27_r27', 'MN'),
         region = replace(region, region == 'p35_r32', 'CO'),
         region = replace(region, region == 'p45_r30', 'OR'),
         region = replace(region, region == 'p47_r27', 'WA'))

# change factor order
all_errors$error_type <- factor(all_errors$error_type, 
                                levels = c('Biotic/Fire', 'Biotic/Harvest',
                                           'Development/Harvest', 'Development/Water',
                                           'Fire/Harvest', 'Harvest/Biotic',
                                           'Harvest/Development', 'Harvest/Fire',
                                           'Harvest/Water', 'False Positive'))
all_errors$region <- factor(all_errors$region,
                            levels = c('WA', 'OR', 'CO', 'MN', 'ME', 'SC', 'PA/NJ'))

misclass <- ggplot() +
  geom_bar(data = all_errors, aes(x = block_rank, y = count, fill = error_type),
           stat = "identity", col = 'black') +
  # geom_text(data = all_errors, aes(x = block_rank, y = percent, label = as.character(total)),
  #           position = position_dodge(0.9), vjust = -0.25) +
  facet_grid(~region, switch = 'both') +
  theme_bw() +
  ylim(0, 50) +
  xlab('Region') +
  ylab('Percent Incorrectly Labeled Pixels') +
  scale_fill_viridis(option = 'A', name = 'Error Type', discrete = TRUE) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.ticks.x = element_blank(),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.background = element_blank())

# write out
ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/miclassified_disturbance_stacked_bar_rawcounts.png', 
       plot = misclass, 
       device = 'png', 
       dpi = 400)

# stacked bar of which disturbances were missed ---------------------------

# input all validated blocks
wa1 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p47_r27_block2.csv', sep = ''), header = T, sep = ',')
or1 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p45_r30_block5.csv', sep = ''), header = T, sep = ',')
co1 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p35_r32_block7.csv', sep = ''), header = T, sep = ',')
mn1 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p27_r27_block1.csv', sep = ''), header = T, sep = ',')
me1 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p12_r28_block3.csv', sep = ''), header = T, sep = ',')
sc1 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p16_r37_block5.csv', sep = ''), header = T, sep = ',')
pa1 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p14_r32_block1.csv', sep = ''), header = T, sep = ',')

wa2 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p47_r27_block6.csv', sep = ''), header = T, sep = ',')
or2 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p45_r30_block1.csv', sep = ''), header = T, sep = ',')
co2 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p35_r32_block9.csv', sep = ''), header = T, sep = ',')
mn2 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p27_r27_block4.csv', sep = ''), header = T, sep = ',')
me2 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p12_r28_block2.csv', sep = ''), header = T, sep = ',')
sc2 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p16_r37_block4.csv', sep = ''), header = T, sep = ',')
pa2 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p14_r32_block7.csv', sep = ''), header = T, sep = ',')

wa3 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p47_r27_block3.csv', sep = ''), header = T, sep = ',')
or3 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p45_r30_block4.csv', sep = ''), header = T, sep = ',')
co3 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p35_r32_block2.csv', sep = ''), header = T, sep = ',')
mn3 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p27_r27_block6.csv', sep = ''), header = T, sep = ',')
me3 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p12_r28_block7.csv', sep = ''), header = T, sep = ',')
sc3 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p16_r37_block1.csv', sep = ''), header = T, sep = ',')
pa3 <- fread(paste(data_path, 'validation/attributed_sample_data_undisturbed_spatial_p14_r32_block8.csv', sep = ''), header = T, sep = ',')

# new dataframe with region, error type, and error for stacked bars
data <- tibble(region = rep(c('WA', 'OR', 'CO', 'MN', 'ME', 'SC', 'PA'), each = 15),
               block_type = rep(c('block1', 'block2', 'block3'), 35),
               error = rep(rep(c('Biotic', 'Fire', 'Harvest', 'Development', 'Water'), each = 3), 7),
               number = 0)

# use this basic code structure to fill the values
table(pa2$actual_dist)

# fill values with numbers from above (first = predicted, second = actual)
data$number[data$region == 'WA' & data$block_type == 'block1'] <- c(0, 0, 5, 0, 0)
data$number[data$region == 'OR' & data$block_type == 'block1'] <- c(0, 0, 2, 0, 0)
data$number[data$region == 'CO' & data$block_type == 'block1'] <- c(0, 0, 0, 0, 0)
data$number[data$region == 'MN' & data$block_type == 'block1'] <- c(0, 0, 3, 0, 1) 
data$number[data$region == 'ME' & data$block_type == 'block1'] <- c(0, 0, 9, 0, 0)
data$number[data$region == 'SC' & data$block_type == 'block1'] <- c(0, 0, 11, 0, 0)
data$number[data$region == 'PA' & data$block_type == 'block1'] <- c(0, 0, 0, 1, 0)

data$number[data$region == 'WA' & data$block_type == 'block2'] <- c(0, 0, 1, 2, 0)
data$number[data$region == 'OR' & data$block_type == 'block2'] <- c(0, 0, 0, 0, 0)
data$number[data$region == 'CO' & data$block_type == 'block2'] <- c(11, 3, 2, 0, 0)
data$number[data$region == 'MN' & data$block_type == 'block2'] <- c(0, 0, 10, 0, 0) 
data$number[data$region == 'ME' & data$block_type == 'block2'] <- c(0, 0, 12, 0, 0)
data$number[data$region == 'SC' & data$block_type == 'block2'] <- c(0, 0, 18, 0, 0)
data$number[data$region == 'PA' & data$block_type == 'block2'] <- c(0, 0, 0, 2, 0)

data$number[data$region == 'WA' & data$block_type == 'block3'] <- c(0, 0, 4, 1, 0)
data$number[data$region == 'OR' & data$block_type == 'block3'] <- c(0, 0, 3, 0, 0)
data$number[data$region == 'CO' & data$block_type == 'block3'] <- c(3, 1, 1, 1, 0)
data$number[data$region == 'MN' & data$block_type == 'block3'] <- c(0, 2, 6, 0, 0) 
data$number[data$region == 'ME' & data$block_type == 'block3'] <- c(0, 0, 0, 0, 0)
data$number[data$region == 'SC' & data$block_type == 'block3'] <- c(0, 0, 14, 0, 0)
data$number[data$region == 'PA' & data$block_type == 'block3'] <- c(0, 0, 0, 1, 0)

# create data for labels
label_data <- tibble(region = rep(c('WA', 'OR', 'CO', 'MN', 'ME', 'SC', 'PA'), each = 3),
                     block_type = rep(c('block1', 'block2', 'block3'), 7),
                    number = c(5, 3, 5, 2, 0, 3, 0, 16, 6, 4, 10, 8, 9, 12, 0, 11, 18, 14, 1, 2, 1), 
                    label = rep(c('a', 'b', 'c'), 7))

# create stacked bar of mislabeled disturbances
palette11 <- rev(brewer.pal(n = 6, name = 'Greys')[2:6])

# fix factor variables
data$region <- factor(data$region, levels = c('WA', 'OR', 'CO', 'MN', 'ME', 'SC', 'PA'))
label_data$region <- factor(label_data$region, levels = c('WA', 'OR', 'CO', 'MN', 'ME', 'SC', 'PA'))
data$error <- factor(data$error, levels = c('Biotic', 'Fire', 'Harvest', 'Development', 'Water'))
data$block_type <- factor(data$block_type, levels = c('block1', 'block2', 'block3'))
label_data$block_type <- factor(label_data$block_type, levels = c('block1', 'block2', 'block3'))

missed <- ggplot() +
  geom_bar(data = data, aes(x = block_type, y = number, fill = error),
           stat = "identity", col = 'black') +
  geom_text(data = label_data, aes(x = block_type, y = number, label = label), 
            position = position_dodge(0.9), vjust = -0.25) +
  facet_grid(~region, switch = 'both') +
  theme_bw() +
  ylim(0, 18) +
  xlab('Region') +
  ylab('Number of Incorrectly Identified Pixels') +
  scale_fill_manual(values = palette11, name = 'Error Type') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.ticks.x = element_blank(),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.background = element_blank())

# write out
ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/false_negatives_stacked_bar.png', 
       plot = missed, 
       device = 'png', 
       dpi = 400)

# study locations ---------------------------------------------------------

### new way (landsat tiles)

# import all landsat scene bounds, convert to same CRS as rasters
landsat_tiles <- st_read('~/Documents/satellite-field-comp/data/other/WRS2_descending.shp')
landsat_tiles <- st_transform(landsat_tiles, crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

# bounds
bounds_wa <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 47 & landsat_tiles$ROW == 27]))
bounds_or <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 45 & landsat_tiles$ROW == 30]))
bounds_co <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 35 & landsat_tiles$ROW == 32]))
bounds_mn <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 27 & landsat_tiles$ROW == 27]))
bounds_me <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 12 & landsat_tiles$ROW == 28]))
bounds_pa <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 14 & landsat_tiles$ROW == 32]))
bounds_sc <- as.data.frame(st_coordinates(landsat_tiles$geometry[landsat_tiles$PATH == 16 & landsat_tiles$ROW == 37]))

# get states
states <- map_data("state")

# get background raster (forest cover)
forest <- raster('/home/annie/Documents/satellite-field-comp/figs/backgroundforest.tif')
states_poly <- map('state', fill = TRUE)
IDs <- sapply(strsplit(states_poly$names, ":"), function(x) x[1])
states_poly <- map2SpatialPolygons(states_poly, IDs = IDs, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
forest <- mask(forest, states_poly)
forest[forest < 20] <- NA

# convert raster to df
forest_spdf <- as(forest, "SpatialPixelsDataFrame")
forest_df <- as.data.frame(forest_spdf)
colnames(forest_df) <- c("value", "x", "y")

# create map
site_map <- ggplot(data = states, aes(x = long, y = lat)) + 
  geom_tile(data = forest_df, aes(x = x, y = y, fill = 1), col = 'darkolivegreen3', alpha = 0.8) +
  geom_polygon(aes(group = group), color = "black", fill = 'transparent') +
  geom_polygon(data = bounds_wa, aes(x = X, y = Y), fill = "transparent", color = "black", size = 1.5) +
  geom_polygon(data = bounds_or, aes(x = X, y = Y), fill = "transparent", color = "black", size = 1.5) +
  geom_polygon(data = bounds_co, aes(x = X, y = Y), fill = "transparent", color = "black", size = 1.5) +
  geom_polygon(data = bounds_mn, aes(x = X, y = Y), fill = "transparent", color = "black", size = 1.5) +
  geom_polygon(data = bounds_me, aes(x = X, y = Y), fill = "transparent", color = "black", size = 1.5) +
  geom_polygon(data = bounds_pa, aes(x = X, y = Y), fill = "transparent", color = "black", size = 1.5) +
  geom_polygon(data = bounds_sc, aes(x = X, y = Y), fill = "transparent", color = "black", size = 1.5) +
  theme_bw() +
  coord_fixed(1.3) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none',
        axis.ticks = element_blank())

### prettier, but less accurate

# import forested images
img_wa <- raster(paste(data_path, 'images/p47_r27/tree_cover_p47_r27_2000_30m.tif', sep = ''))
img_or <- raster(paste(data_path, 'images/p45_r30/tree_cover_p45_r30_2000_30m.tif', sep = ''))
img_co <- raster(paste(data_path, 'images/p35_r32/tree_cover_p35_r32_2000_30m.tif', sep = ''))
img_mn <- raster(paste(data_path, 'images/p27_r27/tree_cover_p27_r27_2000_30m.tif', sep = ''))
img_me <- raster(paste(data_path, 'images/p12_r28/tree_cover_p12_r28_2000_30m.tif', sep = ''))
img_sc <- raster(paste(data_path, 'images/p16_r37/tree_cover_p16_r37_2000_30m.tif', sep = ''))
img_pa <- raster(paste(data_path, 'images/p14_r32/tree_cover_p14_r32_2000_30m.tif', sep = ''))

wa_ext <- tibble(x = c(-121.861, -121.861, -124.9125, -124.9125), 
                 y = c(46.45907, 48.42997, 46.45907, 48.42997),
                 group = c(1, 2, 1, 2))

# create map
states <- map_data("state")
site_map <- ggplot(data = states, aes(x = long, y = lat, group = group)) + 
  geom_polygon(color = "black", fill = 'white') +
  geom_rect(aes(xmin = xmin(img_wa), xmax = xmax(img_wa), ymin = ymin(img_wa), ymax = ymax(img_wa)),
            fill = "transparent", color = "black", size = 1.5) +
  geom_rect(aes(xmin = xmin(img_or), xmax = xmax(img_or), ymin = ymin(img_or), ymax = ymax(img_or)),
            fill = "transparent", color = "black", size = 1.5) +
  geom_rect(aes(xmin = xmin(img_co), xmax = xmax(img_co), ymin = ymin(img_co), ymax = ymax(img_co)),
            fill = "transparent", color = "black", size = 1.5) +
  geom_rect(aes(xmin = xmin(img_mn), xmax = xmax(img_mn), ymin = ymin(img_mn), ymax = ymax(img_mn)),
            fill = "transparent", color = "black", size = 1.5) +
  geom_rect(aes(xmin = xmin(img_me), xmax = xmax(img_me), ymin = ymin(img_me), ymax = ymax(img_me)),
            fill = "transparent", color = "black", size = 1.5) +
  geom_rect(aes(xmin = xmin(img_sc), xmax = xmax(img_sc), ymin = ymin(img_sc), ymax = ymax(img_sc)),
            fill = "transparent", color = "black", size = 1.5) +
  geom_rect(aes(xmin = xmin(img_pa), xmax = xmax(img_pa), ymin = ymin(img_pa), ymax = ymax(img_pa)),
            fill = "transparent", color = "black", size = 1.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        axis.ticks = element_blank())

# write out
ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/site_map_tiles_forestcover.png', 
       plot = site_map, 
       device = 'png', 
       dpi = 400)

# distribution of variables within a region -------------------------------

# let's use washington because it has large amounts of fires, harvest, and flooding
data <- fread(paste(data_path, 'attribution/classified_img_data_p47_r27_block1.csv', sep = ''), header = T, sep = ',')

# add undisturbed pixels to data
data$predicted[data$preds == 1] <- 1

# frequency histogram for magnitude by disturbance type
data$predicted <- as.factor(data$predicted)

palette4 <- brewer_pal(palette = 'Set1')(4)
palette4 <- c('#FF7913', '#0079A8', "#D32E5E", '#C68F65')

max_index_plot <- ggplot(data[!is.na(data$predicted),], aes(x = max_index, fill = predicted)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Maximum NDVI') +
  ylab('Density') +
  scale_fill_manual(values = palette4) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_text(face = 'bold', size = 11),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none')
min_raw_slope_plot <- ggplot(data[!is.na(data$predicted),], aes(x = min_raw_slope, fill = predicted)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('\nMinimum Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette4, name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_text(face = 'bold', size = 11),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none') 
s_min_slope_avg_plot <- ggplot(data[!is.na(data$predicted),], aes(x = s_min_slope_avg, fill = predicted)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Spatial Mean of Minimum \nLoess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette4, breaks = c(1, 4, 7, 8), labels = c('Undisturbed', 
                                                                          'Harvest', 'Flooding', 'LCC'),
                    name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_text(face = 'bold', size = 11),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        legend.justification = c(0, 1),
        legend.position = c(0, 1)) 
d_avg_ind_range_plot <- ggplot(data[!is.na(data$predicted),], aes(x = d_avg_ind_range, fill = predicted)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Range in Disturbed Pixel \nAvg. NDVI w/i Area') +
  ylab('Density') +
  scale_fill_manual(values = palette4, breaks = c(1, 4, 7, 8), labels = c('Undisturbed', 
                                                                          'Harvest', 'Flooding', 'LCC'),
                    name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_text(face = 'bold', size = 11),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none') 
d_avg_ind_med_plot <- ggplot(data[!is.na(data$predicted),], aes(x = d_avg_ind_med, fill = predicted)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Range in Disturbed Pixel \nMedian NDVI w/i Area') +
  ylab('Density') +
  scale_fill_manual(values = palette4, breaks = c(1, 4, 7, 8), labels = c('Undisturbed', 
                                                                          'Harvest', 'Flooding', 'LCC'),
                    name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_text(face = 'bold', size = 11),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none') 
density_plots <- arrangeGrob(max_index_plot, min_raw_slope_plot, s_min_slope_avg_plot, d_avg_ind_range_plot, d_avg_ind_med_plot, nrow = 1, ncol = 5)

ggsave(filename = '~/disturbr/figs/density_plots_p47_r27_block1.png', 
       plot = density_plots, 
       device = 'png',
       width = 10.5,
       height = 4,
       units = c('in'),
       dpi = 400)

##### Colorado

# let's use washington because it has large amounts of fires, harvest, and flooding
data <- fread(paste(output_path, 'attributed_img_data_p35_r32_block3.csv', sep = ''), header = T, sep = ',')

# frequency histogram for magnitude by disturbance type
data$predicted <- as.factor(data$predicted)

palette4 <- brewer_pal(palette = 'Set1')(4)
palette4 <- c('#B76CA4', '#FF7913')

plot3 <- ggplot(data[!is.na(data$predicted),], aes(x = s_min_slope_range, fill = predicted)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Range in Minimum \nSlope w/i Area') +
  ylab('Density') +
  scale_fill_manual(values = palette4) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_text(face = 'bold', size = 11),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none')
plot2 <- ggplot(data[!is.na(data$predicted),], aes(x = d_min_slope_range, fill = predicted)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Spatial Range in \nMinimum Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette4, name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_text(face = 'bold', size = 11),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none') 
plot1 <- ggplot(data[!is.na(data$predicted),], aes(x = min_tree_slope, fill = predicted)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Minimum Regression \nTree Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette4, breaks = c(1, 2), labels = c('Biotic', 'Fire'),
                    name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_text(face = 'bold', size = 11),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        legend.justification = c(0, 1),
        legend.position = c(0, 1)) 
plot4 <- ggplot(data[!is.na(data$predicted),], aes(x = min_ndvi, fill = predicted)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('\nMinimum NDVI') +
  ylab('Density') +
  scale_fill_manual(values = palette4, breaks = c(1, 2), labels = c('Biotic', 'Fire'),
                    name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_text(face = 'bold', size = 11),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none') 
plot5 <- ggplot(data[!is.na(data$predicted),], aes(x = d_min_slope_med, fill = predicted)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Spatial Avg. of \nMinimum Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette4, breaks = c(1, 2), labels = c('Biotic', 'Fire'),
                    name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_text(face = 'bold', size = 11),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none') 
density_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, ncol = 5)

ggsave(filename = '/home/annie/Documents/annie_projects/satellite-field-comp/figures/density_plots_co.png', 
       plot = density_plots, 
       device = 'png',
       width = 10.5,
       height = 4,
       units = c('in'),
       dpi = 400)

# PCA of variable importance ----------------------------------------------
# pca with variables combined into components, color by undist, fire, bb, etc.
# could also do with variable importance?

readin <- function(f) {
  temp <- fread(f)
  # temp <- temp[temp$disturbance != 0, ]
  # names(temp)[4] <- 'predicted'
  temp$predicted[temp$preds == 1] <- 1 # add undisturbed pixels
  setattr(temp$predicted, "levels", c('1', '2', '3', '4', '5', '6', '7', '8', '9'))
  temp <- temp[!is.na(temp$predicted),] # remove unforested
  gc()
  
  # change all vars with NA to non-NA (could definitely change to data table format for mem)
  temp$variance[is.na(temp$variance)] <- 0
  temp$mad[is.na(temp$mad)] <- 0
  temp$range[is.na(temp$range)] <- 0
  temp$min_index[is.na(temp$min_index)] <- 0
  temp$max_index[is.na(temp$max_index)] <- 0
  temp$slope[is.na(temp$slope)] <- 0
  temp$min_tree_slope[is.na(temp$min_tree_slope)] <- 0
  temp$max_tree_slope[is.na(temp$max_tree_slope)] <- 0
  temp$date_min_slope[is.na(temp$date_min_slope)] <- 2000
  temp$avg_raw_slope[is.na(temp$avg_raw_slope)] <- 0
  temp$var_raw_slope[is.na(temp$var_raw_slope)] <- 0
  temp$min_raw_slope[is.na(temp$min_raw_slope)] <- 0
  temp$min_slope[is.na(temp$min_slope)] <- 0
  temp$max_slope[is.na(temp$max_slope)] <- 0
  temp$avg_slope[is.na(temp$avg_slope)] <- 0
  temp$var_slope[is.na(temp$var_slope)] <- 0
  temp$date_min[is.na(temp$date_min)] <- 2000
  temp$smooth_range[is.na(temp$smooth_range)] <- 0
  temp$s_break_year_range[is.na(temp$s_break_year_range)] <- 0
  temp$s_break_year_range[temp$s_break_year_range != Inf] <- 0
  temp$s_mag_avg[is.na(temp$s_mag_avg)] <- 0
  temp$s_mag_range[is.na(temp$s_mag_range)] <- 0
  temp$s_dur_avg[is.na(temp$s_dur_avg)] <- 0
  temp$s_dur_range[is.na(temp$s_dur_range)] <- 0
  temp$pre_var[is.na(temp$pre_var)] <- 0
  temp$post_var[is.na(temp$post_var)] <- 0
  temp$var_ratio[is.na(temp$var_ratio)] <- 1
  temp$mag[is.na(temp$mag)] <- 0
  temp$dur[is.na(temp$dur)] <- 0
  temp$s_mag_ratio[is.na(temp$s_mag_ratio)] <- 0
  temp$s_dur_ratio[is.na(temp$s_dur_ratio)] <- 0
  temp$dist_date[is.na(temp$dist_date)] <- 0
  temp$s_min_year_ranges[is.na(temp$s_min_year_ranges)] <- 0
  temp$s_var_ratio_ratio[is.na(temp$s_var_ratio_ratio)] <- 1
  temp$s_var_ratio_range[is.na(temp$s_var_ratio_range)] <- 0
  temp$mad_mean_ratio[is.na(temp$mad_mean_ratio)] <- 1
  temp$s_avg_slope_range[is.na(temp$s_avg_slope_range)] <- 0
  temp$s_max_slope_range[is.na(temp$s_max_slope_range)] <- 0
  temp$s_min_slope_range[is.na(temp$s_min_slope_range)] <- 0
  temp$s_var_ratio_avg[is.na(temp$s_var_ratio_avg)] <- 1
  temp$s_avg_slope_ratio[is.na(temp$s_avg_slope_ratio)] <- 1
  temp$s_avg_slope_avg[is.na(temp$s_avg_slope_avg)] <- 0
  temp$s_max_slope_ratio[is.na(temp$s_max_slope_ratio)] <- 1
  temp$s_max_slope_avg[is.na(temp$s_max_slope_avg)] <- 0
  temp$s_min_slope_ratio[is.na(temp$s_min_slope_ratio)] <- 1
  temp$s_min_slope_avg[is.na(temp$s_min_slope_avg)] <- 0
  temp$d_dist_date_range[is.na(temp$d_dist_date_range)] <- 0
  temp$d_dist_date_med[is.na(temp$d_dist_date_med)] <- 2000
  temp$d_num_pix[is.na(temp$d_num_pix)] <- 0
  temp$d_dur_range[is.na(temp$d_dur_range)] <- 0
  temp$d_dur_med[is.na(temp$d_dur_med)] <- 0
  temp$d_mag_range[is.na(temp$d_mag_range)] <- 0
  temp$d_mag_med[is.na(temp$d_mag_med)] <- 0
  temp$d_avg_ind_range[is.na(temp$d_avg_ind_range)] <- 0
  temp$d_avg_ind_med[is.na(temp$d_avg_ind_med)] <- 0
  temp$d_slope_range[is.na(temp$d_slope_range)] <- 0
  temp$d_slope_med[is.na(temp$d_slope_med)] <- 0
  temp$d_min_slope_range[is.na(temp$d_min_slope_range)] <- 0
  temp$d_min_slope_med[is.na(temp$d_min_slope_med)] <- 0
  temp$d_max_slope_range[is.na(temp$d_max_slope_range)] <- 0
  temp$d_max_slope_med[is.na(temp$d_max_slope_med)] <- 0
  temp$d_date_min_range[is.na(temp$d_date_min_range)] <- 0
  temp$d_date_min_med[is.na(temp$d_date_min_med)] <- 0
  temp$d_dist_date_range[temp$d_dist_date_range == -Inf] <- 0
  temp$s_break_year_range[temp$s_break_year_range == Inf] <- 0
  
  return(temp)
}

combine_csvs <- function(file_list) {
  data <- map_dfr(file_list, readin)
  
  return(data)
}

# let's use washington because it has large amounts of fires, harvest, and flooding
# 50-55 G mem to ingest
filelist <- list.files(path = paste(data_path, 'attribution/', sep = ''), pattern = 'classified_img_data_spatial_p47_r27_block*', full.names = TRUE)
attdata <- combine_csvs(filelist)

# dist/no-dist pca
rows <- sample(seq(1, nrow(attdata)), size = 10000)
temp <- attdata[rows,]
temp <- temp[, -c(29, 30)]
temp$predicted <- as.factor(temp$predicted)
# pull out variables (random sample to keep somewhat feasible - 3000?)
comps <- prcomp(temp[, c(seq(3, 12), seq(14, 20), seq(22, 25), seq(27, 46), seq(49, 65))], scale = TRUE)
comps_df <- data.frame(comps$x, dist = temp$predicted)
# tells apart dist/undist
# pc1 = variance and dec slope
# pc2 = magnitude increase over ts

### FIX TO HAVE CORRECT COLORS
wa_plot <- ggplot(comps_df, aes(x = PC1, y = PC2, col = dist)) +
  geom_point(size = 1) +
  theme_bw() +
  scale_color_manual('Disturbance \nType', breaks = c('1', '4', '7', '8'),
                    labels = c('\nUndisturbed\n', '\nHarvest\n', '\nChanges in \nRiver Path', '\nAnthropogenic \nLCC'),
                    values = c('black', '#0079A8', "#D32E5E", 'darkgoldenrod2')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = 'bold')) 

ggsave(filename = '~/disturbr/figs/pca_dist_nodist_p47_r27.png', 
       plot = wa_plot, 
       device = 'png',
       units = c('in'),
       dpi = 400)

# dist types pca
attdata_dist <- attdata[attdata$predicted != 1, ]
rows <- sample(seq(1, nrow(attdata_dist)), size = 2000)
temp <- attdata_dist[rows,]
temp <- temp[, -c(29, 30)]
temp$predicted <- as.factor(temp$predicted)
# pull out variables (random sample to keep somewhat feasible - 3000?)
comps <- prcomp(temp[, c(seq(3, 12), seq(14, 20), seq(22, 25), seq(27, 46), seq(49, 65))], scale = TRUE)
comps_df <- data.frame(comps$x, dist = temp$predicted)
# tells apart dist/undist
# pc1 = variance and dec slope
# pc2 = magnitude increase over ts
# will need a couple of these because they're more complicated (could also just show comps line plots)
wa_plot <- ggplot(comps_df, aes(x = PC3, y = PC4, col = dist)) +
  geom_point(size = 1) +
  theme_bw() +
  scale_color_manual('Disturbance \nType', breaks = c('4', '7', '8'),
                     labels = c('\nHarvest\n', '\nChanges in \nRiver Path', '\nAnthropogenic \nLCC'),
                     values = c('#0079A8', "#D32E5E", 'darkgoldenrod2')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = 'bold')) 

ggsave(filename = '~/disturbr/figs/pca_dist_nodist_p47_r27_pc3pc4.png', 
       plot = wa_plot, 
       device = 'png',
       units = c('in'),
       dpi = 400)

### South Carolina

# let's use washington because it has large amounts of fires, harvest, and flooding
# 50-55 G mem to ingest
filelist <- list.files(path = paste(data_path, 'attribution/', sep = ''), pattern = 'classified_img_data_spatial_p16_r37_block*', full.names = TRUE)
attdata <- combine_csvs(filelist)

# dist/no-dist pca
rows <- sample(seq(1, nrow(attdata)), size = 10000)
temp <- attdata[rows,]
temp <- temp[, -c(29, 30)]
temp$predicted <- as.factor(temp$predicted)
# pull out variables (random sample to keep somewhat feasible - 3000?)
comps <- prcomp(temp[, c(seq(3, 12), seq(14, 20), seq(22, 25), seq(27, 46), seq(49, 65))], scale = TRUE)
comps_df <- data.frame(comps$x, dist = temp$predicted)
# tells apart dist/undist
# pc1 = variance and dec slope
# pc2 = magnitude increase over ts

### FIX TO HAVE CORRECT COLORS
sc_plot <- ggplot(comps_df, aes(x = PC1, y = PC2, col = dist)) +
  geom_point(size = 1) +
  theme_bw() +
  scale_color_manual('Disturbance \nType', breaks = c('1', '4', '8'),
                     labels = c('\nUndisturbed\n', '\nHarvest\n', '\nAnthropogenic \nLCC'),
                     values = c('black', '#0079A8', 'darkgoldenrod2')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = 'bold')) 

ggsave(filename = '~/disturbr/figs/pca_dist_nodist_p16_r37.png', 
       plot = sc_plot, 
       device = 'png',
       units = c('in'),
       dpi = 400)

# dist types pca
attdata_dist <- attdata[attdata$predicted != 1, ]
rows <- sample(seq(1, nrow(attdata_dist)), size = 2000)
temp <- attdata_dist[rows,]
temp <- temp[, -c(29, 30)]
temp$predicted <- as.factor(temp$predicted)
# pull out variables (random sample to keep somewhat feasible - 3000?)
comps <- prcomp(temp[, c(seq(3, 12), seq(14, 20), seq(22, 25), seq(27, 46), seq(49, 65))], scale = TRUE)
comps_df <- data.frame(comps$x, dist = temp$predicted)
# tells apart dist/undist
# pc1 = variance and dec slope
# pc2 = magnitude increase over ts
# will need a couple of these because they're more complicated (could also just show comps line plots)
sc_plot <- ggplot(comps_df, aes(x = PC3, y = PC4, col = dist)) +
  geom_point(size = 1) +
  theme_bw() +
  scale_color_manual('Disturbance \nType', breaks = c('4', '8'),
                     labels = c('\nHarvest\n', '\nAnthropogenic \nLCC'),
                     values = c('#0079A8', 'darkgoldenrod2')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = 'bold')) 

ggsave(filename = '~/disturbr/figs/pca_dist_nodist_p16_r37_pc3pc4.png', 
       plot = sc_plot, 
       device = 'png',
       units = c('in'),
       dpi = 400)

### Oregon

# let's use washington because it has large amounts of fires, harvest, and flooding
# 50-55 G mem to ingest
filelist <- list.files(path = paste(data_path, 'attribution/', sep = ''), pattern = 'classified_img_data_spatial_p45_r30_block*', full.names = TRUE)
attdata <- combine_csvs(filelist)

# dist/no-dist pca
rows <- sample(seq(1, nrow(attdata)), size = 10000)
temp <- attdata[rows,]
temp <- temp[, -c(29, 30)]
temp$predicted <- as.factor(temp$predicted)
# pull out variables (random sample to keep somewhat feasible - 3000?)
comps <- prcomp(temp[, c(seq(3, 12), seq(14, 20), seq(22, 25), seq(27, 46), seq(49, 65))], scale = TRUE)
comps_df <- data.frame(comps$x, dist = temp$predicted)
# tells apart dist/undist
# pc1 = variance and dec slope
# pc2 = magnitude increase over ts

### FIX TO HAVE CORRECT COLORS
or_plot <- ggplot(comps_df, aes(x = PC1, y = PC2, col = dist)) +
  geom_point(size = 1) +
  theme_bw() +
  scale_color_manual('Disturbance \nType', breaks = c('1', '2', '3', '4'),
                     labels = c('\nUndisturbed\n', '\nBiotic\n', '\nFire\n', '\nHarvest\n'),
                     values = c('black', 'darkseagreen', '#FF7913', '#0079A8')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = 'bold')) 

ggsave(filename = '~/disturbr/figs/pca_dist_nodist_p45_r30.png', 
       plot = or_plot, 
       device = 'png',
       units = c('in'),
       dpi = 400)

# dist types pca
attdata_dist <- attdata[attdata$predicted != 1, ]
rows <- sample(seq(1, nrow(attdata_dist)), size = 2000)
temp <- attdata_dist[rows,]
temp <- temp[, -c(29, 30)]
temp$predicted <- as.factor(temp$predicted)
# pull out variables (random sample to keep somewhat feasible - 3000?)
comps <- prcomp(temp[, c(seq(3, 12), seq(14, 20), seq(22, 25), seq(27, 46), seq(49, 65))], scale = TRUE)
comps_df <- data.frame(comps$x, dist = temp$predicted)
# tells apart dist/undist
# pc1 = variance and dec slope
# pc2 = magnitude increase over ts
# will need a couple of these because they're more complicated (could also just show comps line plots)
or_plot <- ggplot(comps_df, aes(x = PC3, y = PC4, col = dist)) +
  geom_point(size = 1) +
  theme_bw() +
  scale_color_manual('Disturbance \nType', breaks = c('2', '3', '4'),
                     labels = c('\nBiotic\n', '\nFire\n', '\nHarvest\n'),
                     values = c('darkseagreen', '#FF7913', '#0079A8')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = 'bold')) 

ggsave(filename = '~/disturbr/figs/pca_dist_nodist_p45_r30_pc3pc4.png', 
       plot = or_plot, 
       device = 'png',
       units = c('in'),
       dpi = 400)

# PCA variable importance - training data ---------------------------------

### edit training data to combine with classified data

combine_csvs <- function(file_list, training = TRUE) {
  if (training == TRUE) {
    data <- map_dfr(file_list, read.csv, header = TRUE, stringsAsFactors = FALSE)
  } else {
    data <- map_dfr(file_list, readin)
  }
  
  return(data)
}

# read in training data
dettraining <- list.files(path = paste(data_path, 'training_points/', sep = ''), pattern = 'img_training_p47_r27_block*', full.names = TRUE)
atttraining <- list.files(path = paste(data_path, 'training_points/', sep = ''), pattern = 'img_training_attribution_p47_r27_block*', full.names = TRUE)
detdata <- combine_csvs(dettraining)
attdata <- combine_csvs(atttraining)
detdata$longitude <- signif(detdata$longitude, digits = 7)
detdata$latitude <- signif(detdata$latitude, digits = 7)
attdata$longitude <- signif(attdata$longitude, digits = 7)
attdata$latitude <- signif(attdata$latitude, digits = 7)

# join det/att by points
trdata <- detdata %>%
  full_join(attdata, by = c('longitude', 'latitude', 'avg_index', 'disturbance'))

# read in classified data
classtraining <- list.files(path = paste(data_path, 'attribution/', sep = ''), pattern = 'classified_img_data_spatial_p47_r27_block*', full.names = TRUE)
classtraining <- classtraining[c(grep('block3', classtraining), grep('block5', classtraining), grep('block7', classtraining))]
classdata <- combine_csvs(classtraining, training = FALSE)
classdata$longitude <- signif(classdata$longitude, digits = 7)
classdata$latitude <- signif(classdata$latitude, digits = 7)

# join by points
data <- trdata %>%
  dplyr::select(longitude, latitude, avg_index, disturbance) %>%
  left_join(classdata, by = c('longitude', 'latitude', 'avg_index')) %>%
  filter(!is.na(variance)) # a few extra points due to close lat/lon

# write out
write.csv(data, '~/disturbr/figs/data/training_data_with_vars_p47_r27.csv', row.names = FALSE)

# PCA
data <- read.csv('/home/annie/Documents/satellite-field-comp/figs/data/training_data_with_vars_p47_r27.csv', stringsAsFactors = FALSE)

# remove unknowns
data <- data[data$disturbance != 0, ]

comps <- prcomp(data[, c(3, seq(5, 13), seq(15, 21), seq(23, 26), seq(28, 29), seq(32, 49), seq(52, 68))], scale = TRUE)
comps_df <- data.frame(comps$x, dist = data$disturbance)

# explore
plot(comps, type = 'l')

wa_plot <- ggplot(comps_df, aes(x = PC1, y = PC2, col = as.factor(dist))) +
  geom_point(size = 1) +
  theme_bw() +
  scale_color_manual('Disturbance \nType', breaks = c('1', '3', '4', '5', '7', '8', '9'),
                     labels = c('\nUndisturbed\n', '\nFire\n', '\nHarvest\n', '\nWind\n', 'Changes in \nRiver Path', '\nAnthropogenic \nLCC', '\nLandslide or \nAvalanche'),
                     values = c('black', '#FF7913', '#0079A8', 'darkslategray', "#D32E5E", 'darkgoldenrod2', 'bisque4')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = 'bold')) 



# time series plots -------------------------------------------------------

##### Washington
ndvi_img_data <- 'image_data/p47_r27/ndvi_data_all_pix_p47_r27_block1.csv'
img_file <- 'images/p47_r27/summer_ndvi_p47_r27.tif'

# first band of the ndvi img is the time series average
img <- brick(paste(data_path, img_file, sep = ''))

# write out ndvi data
all_data <- fread(paste(data_path, ndvi_img_data, sep = ''), sep = ',', header = TRUE)

# figure out .01% of data
lon_lat_data <- all_data[ , .(avg_ndvi = mean(ndvi)), by = c('longitude', 'latitude')]
s_size <- round(nrow(lon_lat_data) / 2000)
if (s_size < 350) {
  s_size <- 350
}

# get coordinates of sample points
set.seed(275)
samp_pts <- sample(seq(1, nrow(lon_lat_data)), s_size, replace = F)
samp_pts <- lon_lat_data[samp_pts, ]

# get year and ndvi data for those points
samp_data <- data.table(longitude = rep(samp_pts$longitude, each = 17), 
                        latitude = rep(samp_pts$latitude, each = 17),
                        year = rep(seq(2000, 2016), times = s_size),
                        ndvi = rep(as.numeric(NA), s_size * 17))

# join data tables by long, lat, and year
samp_data <- all_data[samp_data, .(longitude, latitude, year, ndvi), on = c('longitude', 'latitude', 'year')]

api_key <- #insert as character

# iteratively go through, plot, and decide disturbance status
i <- 4
while (i <= nrow(samp_data[samp_data$year == 2000, ])) {
  # get only data from full dataset where coordinates match those of the sample point
  long <- samp_pts$longitude[i]
  lat <- samp_pts$latitude[i]
  df <- samp_data[which(near(samp_data$longitude, long)), ]
  df <- df[which(near(df$latitude, lat)), ]
  # convert to simple dataframe
  temp <- data.table(x = df$year, y = df$ndvi)
  if (sum(is.na(temp$y)) >= 1) {
    d <- 0
    samp_pts$disturbance[which((samp_pts$longitude == long) & (samp_pts$latitude == lat))] <- d
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
    
    # plot the time series within the 700x700m window
    new_ext <- extent(min(nearest$longitude), max(nearest$longitude),
                      min(nearest$latitude), max(nearest$latitude))
    cropped <- crop(img, new_ext)
    plot(cropped[[seq(2, 18)]], zlim = c(min(minValue(cropped)), max(maxValue(cropped))))
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
    readline(prompt = 'Press [enter] to continue...')
    
    i <-  i + 1
  }
}

# after grabbing undisturbed i from above
# get only data from full dataset where coordinates match those of the sample point
long <- samp_pts$longitude[3]
lat <- samp_pts$latitude[3]
df <- samp_data[which(near(samp_data$longitude, long)), ]
df <- df[which(near(df$latitude, lat)), ]
# convert to simple dataframe
temp <- data.table(x = df$year, y = df$ndvi)
fit <- loess(temp$y ~ temp$x)
fitted <- data.frame(x = df$year, y = fit$fitted)

# plot
wa_undist <- ggplot(data = temp, aes(x = x, y = y)) +
  geom_point(col = 'black', size = 3) +
  geom_line(data = fitted, aes(x = x, y = y), color = 'darkgrey') +
  xlab('Year') +
  ylab('Summertime NDVI') +
  ylim(0.38, 0.95) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

# after getting disturbed i from above
# get only data from full dataset where coordinates match those of the sample point
long <- samp_pts$longitude[22]
lat <- samp_pts$latitude[22]
df <- samp_data[which(near(samp_data$longitude, long)), ]
df <- df[which(near(df$latitude, lat)), ]
# convert to simple dataframe
temp <- data.table(x = df$year, y = df$ndvi)
fit <- loess(temp$y ~ temp$x)
fitted <- data.frame(x = df$year, y = fit$fitted)

# plot
wa_dist <- ggplot(data = temp, aes(x = x, y = y)) +
  geom_point(col = 'black', size = 3) +
  geom_line(data = fitted, aes(x = x, y = y), color = 'darkgrey') +
  xlab('Year') +
  ylab('Summertime NDVI') +
  ylim(0.38, 0.95) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

##### South Carolina
ndvi_img_data <- 'image_data/p35_r32/ndvi_data_all_pix_p35_r32_block3.csv'
img_file <- 'images/p35_r32/summer_ndvi_p35_r32.tif'

# first band of the ndvi img is the time series average
img <- brick(paste(data_path, img_file, sep = ''))

# write out ndvi data
all_data <- fread(paste(data_path, ndvi_img_data, sep = ''), sep = ',', header = TRUE)

# figure out .01% of data
lon_lat_data <- all_data[ , .(avg_ndvi = mean(ndvi)), by = c('longitude', 'latitude')]
s_size <- round(nrow(lon_lat_data) / 2000)
if (s_size < 350) {
  s_size <- 350
}

# get coordinates of sample points
set.seed(275)
samp_pts <- sample(seq(1, nrow(lon_lat_data)), s_size, replace = F)
samp_pts <- lon_lat_data[samp_pts, ]

# get year and ndvi data for those points
samp_data <- data.table(longitude = rep(samp_pts$longitude, each = 17), 
                        latitude = rep(samp_pts$latitude, each = 17),
                        year = rep(seq(2000, 2016), times = s_size),
                        ndvi = rep(as.numeric(NA), s_size * 17))

# join data tables by long, lat, and year
samp_data <- all_data[samp_data, .(longitude, latitude, year, ndvi), on = c('longitude', 'latitude', 'year')]

api_key <- 'AIzaSyAZX1Y9eMfoWkv8LL3b72Rwa3-jTF1VzeU'

# iteratively go through, plot, and decide disturbance status
i <- 1
while (i <= nrow(samp_data[samp_data$year == 2000, ])) {
  # get only data from full dataset where coordinates match those of the sample point
  long <- samp_pts$longitude[i]
  lat <- samp_pts$latitude[i]
  df <- samp_data[which(near(samp_data$longitude, long)), ]
  df <- df[which(near(df$latitude, lat)), ]
  # convert to simple dataframe
  temp <- data.table(x = df$year, y = df$ndvi)
  if (sum(is.na(temp$y)) >= 1) {
    d <- 0
    samp_pts$disturbance[which((samp_pts$longitude == long) & (samp_pts$latitude == lat))] <- d
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
    
    # plot the time series within the 700x700m window
    new_ext <- extent(min(nearest$longitude), max(nearest$longitude),
                      min(nearest$latitude), max(nearest$latitude))
    cropped <- crop(img, new_ext)
    plot(cropped[[seq(2, 18)]], zlim = c(min(minValue(cropped)), max(maxValue(cropped))))
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
    readline(prompt = 'Press [enter] to continue...')
    
    i <-  i + 1
  }
}

# after grabbing undisturbed i from above
# get only data from full dataset where coordinates match those of the sample point
long <- samp_pts$longitude[14]
lat <- samp_pts$latitude[14]
df <- samp_data[which(near(samp_data$longitude, long)), ]
df <- df[which(near(df$latitude, lat)), ]
# convert to simple dataframe
temp <- data.table(x = df$year, y = df$ndvi)
fit <- loess(temp$y ~ temp$x)
fitted <- data.frame(x = df$year, y = fit$fitted)

# plot
co_undist <- ggplot(data = temp, aes(x = x, y = y)) +
  geom_point(col = 'black', size = 3) +
  geom_line(data = fitted, aes(x = x, y = y), color = 'darkgrey') +
  xlab('Year') +
  ylab('Summertime NDVI') +
  ylim(0.38, 0.95) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

# after getting disturbed i from above
# get only data from full dataset where coordinates match those of the sample point
long <- samp_pts$longitude[5]
lat <- samp_pts$latitude[5]
df <- samp_data[which(near(samp_data$longitude, long)), ]
df <- df[which(near(df$latitude, lat)), ]
# convert to simple dataframe
temp <- data.table(x = df$year, y = df$ndvi)
fit <- loess(temp$y ~ temp$x)
fitted <- data.frame(x = df$year, y = fit$fitted)

# plot
co_dist <- ggplot(data = temp, aes(x = x, y = y)) +
  geom_point(col = 'black', size = 3) +
  geom_line(data = fitted, aes(x = x, y = y), color = 'darkgrey') +
  xlab('Year') +
  ylab('Summertime NDVI') +
  ylim(0.38, 0.95) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

# write out
ggsave(filename = '/home/annie/Documents/annie_projects/satellite-field-comp/figures/wa_dist_example.png', 
       plot = wa_dist, 
       device = 'png', 
       dpi = 400)
ggsave(filename = '/home/annie/Documents/annie_projects/satellite-field-comp/figures/wa_undist_example.png', 
       plot = wa_undist, 
       device = 'png', 
       dpi = 400)
ggsave(filename = '/home/annie/Documents/annie_projects/satellite-field-comp/figures/co_dist_example.png', 
       plot = co_dist, 
       device = 'png', 
       dpi = 400)
ggsave(filename = '/home/annie/Documents/annie_projects/satellite-field-comp/figures/co_undist_example.png', 
       plot = co_undist, 
       device = 'png', 
       dpi = 400)


# line plot of variable importance ----------------------------------------

data <- read.csv(paste(output_path, '/importance/rank_imp.csv', sep = ''), header = T)

# sort out factors (turn into factor, order)
data$region <- as.factor(data$region)
data$region <- factor(data$region, levels = c('wa', 'or', 'co', 'mn', 'me', 'sc', 'pa'))

# plot
det_plot <- ggplot(data, aes(x = rank, y = det_imp, col = region)) +
  geom_line(aes(group = region), size = 1) +
  geom_point(aes(group = region), size = 2) +
  theme_bw() +
  xlab('Variable Rank in Detection Model') +
  ylab('Relative Importance') +
  xlim(c(1, 5)) +
  ylim(c(0, 100)) +
  scale_colour_manual(values = palette, name = 'Region', breaks = c('wa', 'or', 'co', 'mn', 'me', 'sc', 'pa'),
                      labels = c('Washington', 'Oregon', 'Colorado', 'Minnesota', 'Maine', 'South Carolina', 
                                 'Pennsylvania')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.justification = c(0, 0),
        legend.position = c(0, 0),
        legend.background = element_blank())

att_plot <- ggplot(data, aes(x = rank, y = att_imp, col = region)) +
  geom_line(aes(group = region), size = 1) +
  geom_point(aes(group = region), size = 2) +
  theme_bw() +
  xlab('Variable Rank in Attribution Model') +
  ylab('Relative Importance') +
  xlim(c(1, 5)) +
  ylim(c(0, 100)) +
  scale_colour_manual(values = palette, name = 'Region', breaks = c('wa', 'or', 'co', 'mn', 'me', 'sc', 'pa'),
                      labels = c('Washington', 'Oregon', 'Colorado', 'Minnesota', 'Maine', 'South Carolina', 
                                 'Pennsylvania')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 16),
        axis.title.y = element_text(face = 'bold', size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(face = 'bold', size = 14),
        legend.text = element_text(size = 12),
        legend.justification = c(0, 0),
        legend.position = c(0, 0),
        legend.background = element_blank())

# write out
ggsave(filename = '/home/annie/Documents/annie_projects/satellite-field-comp/figures/var_rank_detection.png', 
       plot = det_plot, 
       device = 'png', 
       dpi = 400)
ggsave(filename = '/home/annie/Documents/annie_projects/satellite-field-comp/figures/var_rank_attribution.png', 
       plot = att_plot, 
       device = 'png', 
       dpi = 400)
