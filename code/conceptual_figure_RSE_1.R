# conceptual figure #1 ----------------------------------------------------
# time series/variable conceptual figure

# written by LAC Smith 12 Mar 2019
# last edited by LAC Smith 12 Mar 2019

# harvest example locations
# WA = 47.30154167, -123.425

# load packages -----------------------------------------------------------

library(dplyr)
library(data.table)
library(ggplot2)
library(rpart)

# paths -------------------------------------------------------------------

data_path <- '~/disturbr/data/'

# read in time series data for wa
data <- fread(paste(data_path, '/image_data/p47_r27/data_all_pix_p47_r27_block5.csv', sep = ''))

# create time series plot -------------------------------------------------

point <- data.table(longitude = -123.425, latitude = 47.30154167)
data$lonpt <- point$longitude
data$latpt <- point$latitude

# shrink full lat/long dataset so it takes less time
minlat <- point$latitude - 0.001
maxlat <- point$latitude + 0.001
minlong <- point$longitude - 0.001
maxlong <- point$longitude + 0.001
# split subsetting up to conserve memory
test <- data[data$latitude > minlat & data$latitude < maxlat, ]
test <- test[test$longitude > minlong & test$longitude < maxlong, ]

data <- test

# calculate distance between point and all pixels
data$dist <- apply(data, 1, function(row) 
  sqrt(((row[1] - row[5]) ^ 2) + ((row[2] - row[6]) ^ 2)))

# get nearest point to harvest point
nearest <- data.table(arrange(data, dist)[1:17, ])
nearest <- nearest[, 1:4]

# rpart tree
tree <- rpart::rpart(ind ~ year, data = nearest, control = rpart::rpart.control(minsplit = 4, cp = 0.3))
# get sequence of years on which to predict (in case not all are in df)
s <- seq(min(nearest$year), max(nearest$year), by = 1)
nearest$tree_pts <- predict(tree, newdata = data.frame(year = s))

# loess curve
loess_mod <- loess(ind ~ year, data = nearest, span = 0.75)
# pull out predicted values (from loess) for each  year
nearest$fit <- fitted(loess_mod)

# create plot
ts <- ggplot(nearest, aes(x = year, y = ind)) +
  geom_point(size = 3, col = 'darkgrey') +
  geom_line(lty = 3, lwd = 0.75) +
  geom_line(aes(y = tree_pts), lty = 2, lwd = 0.75) +
  geom_line(aes(y = fit), lty = 1, lwd = 0.75) +
  ylab('Summertime NDVI') +
  xlab('Year') +
  scale_linetype_manual(name = 'Slope Type', values = c("Raw" = 3, "Loess" = 1, "RPART" = 2)) +
  theme_bw() +
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
        legend.position = 'right')

# write out
ggsave(filename = '~/disturbr/figs/ts_plot.png', 
  plot = ts, 
  device = 'png',
  height = 3.5,
  width = 5.5,
  units = 'in',
  dpi = 600)