# density figures ---------------------------------------------------------
# Written by LAC Smith 22 Feb 2019
# Last edited by LAC Smith 22 Feb 2019

# import packages ---------------------------------------------------------

library(data.table)
library(tidyverse)
library(raster)
library(ggplot2)
library(gridExtra)
library(maps)
library(scales)
library(ggmap)
library(swatches)
library(ggsci)
library(viridis)
library(sp)
library(sf)
library(ggfortify)

# functions ---------------------------------------------------------------

combine_csvs <- function(file_list, training = TRUE) {
  if (training == TRUE) {
    data <- map_dfr(file_list, read.csv, header = TRUE, stringsAsFactors = FALSE)
  } else {
    data <- map_dfr(file_list, readin)
  }
  
  return(data)
}

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

# set paths ---------------------------------------------------------------

data_path <- "~/disturbr/data/"
code_path <- '~/disturbr/code/'
output_path <- '~/disturbr/figs/'

data_path <- "~/Documents/satellite-field-comp/data/"
code_path <- '~/Documents/satellite-field-comp/code/final/'
output_path <- '~/Documents/satellite-field-comp/figs/'

# create full training dataset --------------------------------------------

# read in

# read in training data
dettraining <- list.files(path = paste(data_path, 'training_points/', sep = ''), pattern = 'img_training_p16_r37_block*', full.names = TRUE)
atttraining <- list.files(path = paste(data_path, 'training_points/', sep = ''), pattern = 'img_training_attribution_p16_r37_block*', full.names = TRUE)
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
classtraining <- list.files(path = paste(data_path, 'attribution/', sep = ''), pattern = 'classified_img_data_spatial_p16_r37_block*', full.names = TRUE)
classtraining <- classtraining[c(grep('block3', classtraining), grep('block5', classtraining), grep('block7', classtraining))]
classdata <- combine_csvs(classtraining, training = FALSE)
classdata$longitude <- signif(classdata$longitude, digits = 7)
classdata$latitude <- signif(classdata$latitude, digits = 7)

# join by points
data <- trdata %>%
  dplyr::select(longitude, latitude, avg_index, disturbance) %>%
  left_join(classdata, by = c('longitude', 'latitude', 'avg_index')) %>%
  filter(!is.na(variance)) # a few extra points due to close lat/lon (ok)

# write out
write.csv(data, '~/disturbr/figs/data/img_training_p16_r37_complete.csv', row.names = FALSE)

# done: WA, OR, CO, MN, ME, PA, SC

# DETECTION VAR PLOTS -----------------------------------------------------

# these show how disturbance was differentiated from undisturbed
# attribution show how disturbances were differentiated from one another

# washington --------------------------------------------------------------
# read in data
data <- read.csv('/home/annie/Documents/satellite-field-comp/figs/data/img_training_p47_r27_complete.csv', stringsAsFactors = FALSE)

# remove unknown values and convert to factor
data <- data[data$disturbance != 0, ]
data$disturbance <- as.factor(data$disturbance)

palette <- brewer_pal(palette = 'Blues')(9)
palette <- c('black', palette[c(3, 4, 5, 7, 8, 9)])

### for everything, detection vars
plot1 <- ggplot(data, aes(x = avg_index, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Mean NDVI\n') +
  ylab('Density') +
  scale_x_continuous(breaks = c(round(min(data$avg_index, na.rm = TRUE), 2), 
                                round(max(data$avg_index, na.rm = TRUE), 2))) +
  scale_fill_manual(values = palette) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_text(face = 'bold', size = 11),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none')
plot2 <- ggplot(data, aes(x = min_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Min. Loess \nSlope') +
  ylab('Density') +
  scale_x_continuous(breaks = c(round(min(data$min_slope, na.rm = TRUE), 2),
                                round(max(data$min_slope, na.rm = TRUE), 2))) +
  scale_fill_manual(values = palette, name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none') 
plot3 <- ggplot(data, aes(x = var_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Var. Loess \nSlope') +
  ylab('Density') +
  scale_x_continuous(breaks = c(round(min(data$var_slope, na.rm = TRUE), 2), 
                                round(max(data$var_slope, na.rm = TRUE), 2)),
                     limits = c(round(min(data$var_slope, na.rm = TRUE), 2), 
                                round(max(data$var_slope, na.rm = TRUE), 2))) +
  scale_fill_manual(values = palette, breaks = c(1, 3, 4, 5, 7, 8, 9), labels = c('Undisturbed', 
                                                                                  'Fire', 'Harvest', 'Wind', 'Water', 
                                                                                  'Development', 'Landslide/Avalanche'),
                    name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        # legend.title = element_blank(),
        # legend.text = element_blank(),
        legend.title = element_text(face = 'bold', size = 11),
        legend.text = element_text(size = 11),
        legend.background = element_blank(),
        legend.position = 'none')#,
        # legend.position = 'bottom')
plot4 <- ggplot(data, aes(x = smooth_range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Range Loess \nCurve') +
  ylab('Density') +
  scale_x_continuous(breaks = c(round(min(data$smooth_range, na.rm = TRUE), 2), 
                                round(max(data$smooth_range, na.rm = TRUE), 2))) +
  scale_fill_manual(values = palette, breaks = c(1, 3, 4, 5, 7, 8, 9), labels = c('Undisturbed', 
                                                                                  'Fire', 'Harvest', 'Wind', 'Water', 
                                                                                  'Development', 'Landslide/Avalanche'),
                    name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_text(face = 'bold', size = 11),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none') 
plot5 <- ggplot(data, aes(x = s_min_slope_avg, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Min. Loess Slope \n(Spat. Mean)') +
  ylab('Density') +
  scale_x_continuous(breaks = c(round(min(data$s_min_slope_avg, na.rm = TRUE), 2), 
                                round(max(data$s_min_slope_avg, na.rm = TRUE), 2))) +
  scale_fill_manual(values = palette, breaks = c(1, 3, 4, 5, 7, 8, 9), labels = c('Undisturbed', 
                                                                                  'Fire', 'Harvest', 'Wind', 'Water', 
                                                                                  'Development', 'Landslide/Avalanche'),
                    name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none')
density_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 2, ncol = 3)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/detection_density_plots_p47_r27_training_nolegend.png', 
       plot = density_plots, 
       device = 'png',
       scale = 0.75,
       units = c('in'),
       dpi = 400)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/detection_density_plots_p47_r27_training_legend.png', 
       plot = plot3, 
       device = 'png',
       scale = 0.75,
       units = c('in'),
       dpi = 400)

# oregon ------------------------------------------------------------------

# read in data
data <- read.csv('/home/annie/Documents/satellite-field-comp/figs/data/img_training_p45_r30_complete.csv', stringsAsFactors = FALSE)

# remove unknown values and convert to factor
data <- data[data$disturbance != 0, ]
data$disturbance <- as.factor(data$disturbance)

palette <- brewer_pal(palette = 'Set1')(9)
palette <- palette[c(1, 2, 3, 4, 8)]

### for everything, detection vars
plot1 <- ggplot(data, aes(x = slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Average Raw \nSlope') +
  ylab('Density') +
  scale_fill_manual(values = palette) +
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
plot2 <- ggplot(data, aes(x = min_tree_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Minimum Regression \nTree Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, name = 'Disturbance') +
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
plot3 <- ggplot(data, aes(x = min_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Minimum \nLoess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(1, 2, 3, 4, 8), labels = c('Undisturbed', 'Insects',
                                                                            'Fire', 'Harvest', 
                                                                            'LCC'),
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
plot4 <- ggplot(data, aes(x = var_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Variance of \nLoess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(1, 2, 3, 4, 8), labels = c('Undisturbed', 'Insects',
                                                                            'Fire', 'Harvest', 
                                                                            'LCC'),
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
plot5 <- ggplot(data, aes(x = post_var, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Variance in NDVI \nPost-Disturbance') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(1, 2, 3, 4, 8), labels = c('Undisturbed', 'Insects',
                                                                                  'Fire', 'Harvest', 
                                                                                  'LCC'),
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
density_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 1, ncol = 5)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/detection_density_plots_p45_r30_training.png', 
       plot = density_plots, 
       device = 'png',
       width = 10.5,
       height = 4,
       units = c('in'),
       dpi = 400)

# colorado ----------------------------------------------------------------

# read in data
data <- read.csv('/home/annie/Documents/satellite-field-comp/figs/data/img_training_p35_r32_complete.csv', stringsAsFactors = FALSE)

# remove unknown values and convert to factor
data <- data[data$disturbance != 0, ]
data$disturbance <- as.factor(data$disturbance)

palette <- brewer_pal(palette = 'Set1')(9)
palette <- palette[c(1, 2, 3, 4, 8)]

### for everything, detection vars
plot1 <- ggplot(data, aes(x = slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Average Raw \nSlope') +
  ylab('Density') +
  scale_fill_manual(values = palette) +
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
plot2 <- ggplot(data, aes(x = var_raw_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Variance in \nRaw Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, name = 'Disturbance') +
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
plot3 <- ggplot(data, aes(x = min_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Minimum \nLoess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(1, 2, 3, 4, 8), labels = c('Undisturbed', 'Insects',
                                                                            'Fire', 'Harvest', 
                                                                            'LCC'),
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
plot4 <- ggplot(data, aes(x = var_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Variance of \nLoess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(1, 2, 3, 4, 8), labels = c('Undisturbed', 'Insects',
                                                                            'Fire', 'Harvest', 
                                                                            'LCC'),
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
plot5 <- ggplot(data, aes(x = s_var_ratio_avg, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Spatial Avg. of Pre- \nvs. Post-Disturbance Ratio') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(1, 2, 3, 4, 8), labels = c('Undisturbed', 'Insects',
                                                                            'Fire', 'Harvest', 
                                                                            'LCC'),
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
density_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 1, ncol = 5)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/detection_density_plots_p35_r32_training.png', 
       plot = density_plots, 
       device = 'png',
       width = 10.5,
       height = 4,
       units = c('in'),
       dpi = 400)

# minnesota ---------------------------------------------------------------

# read in data
data <- read.csv('/home/annie/Documents/satellite-field-comp/figs/data/img_training_p27_r27_complete.csv', stringsAsFactors = FALSE)

# remove unknown values and convert to factor
data <- data[data$disturbance != 0, ]
data$disturbance <- as.factor(data$disturbance)

palette <- brewer_pal(palette = 'Set1')(9)
palette <- palette[c(1, 4, 7, 8)]

### for everything, detection vars
plot1 <- ggplot(data, aes(x = variance, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('NDVI Variance') +
  ylab('Density') +
  scale_fill_manual(values = palette) +
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
plot2 <- ggplot(data, aes(x = range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('NDVI Range') +
  ylab('Density') +
  scale_fill_manual(values = palette, name = 'Disturbance') +
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
plot3 <- ggplot(data, aes(x = var_raw_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Variance in \nRaw Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(1, 4, 7, 8), labels = c('Undisturbed', 'Harvest', 
                                                                            'Flooding', 'LCC'),
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
plot4 <- ggplot(data, aes(x = var_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Variance of \nLoess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(1, 4, 7, 8), labels = c('Undisturbed', 'Harvest', 
                                                                         'Flooding', 'LCC'),
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
plot5 <- ggplot(data, aes(x = s_min_slope_avg, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Spatial Avg. of Min. \nLoess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(1, 4, 7, 8), labels = c('Undisturbed', 'Harvest', 
                                                                         'Flooding', 'LCC'),
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
density_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 1, ncol = 5)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/detection_density_plots_p27_r27_training.png', 
       plot = density_plots, 
       device = 'png',
       width = 10.5,
       height = 4,
       units = c('in'),
       dpi = 400)

# maine -------------------------------------------------------------------

# read in data
data <- read.csv('/home/annie/Documents/satellite-field-comp/figs/data/img_training_p12_r28_complete.csv', stringsAsFactors = FALSE)

# remove unknown values and convert to factor
data <- data[data$disturbance != 0, ]
data$disturbance <- as.factor(data$disturbance)

palette <- brewer_pal(palette = 'Set1')(9)
palette <- palette[c(1, 3, 4, 7, 8)]

### for everything, detection vars
plot1 <- ggplot(data, aes(x = variance, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('NDVI Variance') +
  ylab('Density') +
  scale_fill_manual(values = palette) +
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
plot2 <- ggplot(data, aes(x = mad, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('NDVI Mean Average Deviation') +
  ylab('Density') +
  scale_fill_manual(values = palette, name = 'Disturbance') +
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
plot3 <- ggplot(data, aes(x = max_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Maximum \nLoess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(1, 3, 4, 7, 8), labels = c('Undisturbed', 'Fire', 
                                                                            'Harvest', 
                                                                         'Flooding', 'LCC'),
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
plot4 <- ggplot(data, aes(x = var_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Variance of \nLoess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(1, 3, 4, 7, 8), labels = c('Undisturbed', 'Fire', 
                                                                            'Harvest', 
                                                                            'Flooding', 'LCC'),
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
plot5 <- ggplot(data, aes(x = smooth_range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Range of \nLoess NDVI') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(1, 3, 4, 7, 8), labels = c('Undisturbed', 'Fire', 
                                                                            'Harvest', 
                                                                            'Flooding', 'LCC'),
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
density_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 1, ncol = 5)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/detection_density_plots_p12_r28_training.png', 
       plot = density_plots, 
       device = 'png',
       width = 10.5,
       height = 4,
       units = c('in'),
       dpi = 400)

# pennsylvania ------------------------------------------------------------

# read in data
data <- read.csv('/home/annie/Documents/satellite-field-comp/figs/data/img_training_p14_r32_complete.csv', stringsAsFactors = FALSE)

# remove unknown values and convert to factor
data <- data[data$disturbance != 0, ]
data$disturbance <- as.factor(data$disturbance)

palette <- brewer_pal(palette = 'Blues')(9)
palette <- c('black', palette[c(3, 4, 7, 8)])

### for everything, detection vars
plot1 <- ggplot(data, aes(x = min_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Min. Loess \nSlope') +
  ylab('Density') +
  scale_x_continuous(breaks = c(round(min(data$min_slope, na.rm = TRUE), 2), 
                                round(max(data$min_slope, na.rm = TRUE), 2))) +
  scale_fill_manual(values = palette) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_text(face = 'bold', size = 11),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none')
plot2 <- ggplot(data, aes(x = var_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Var. Loess \nSlope') +
  ylab('Density') +
  scale_x_continuous(breaks = c(round(min(data$var_slope, na.rm = TRUE), 2),
                                round(max(data$var_slope, na.rm = TRUE), 2)),
                     limits = c(round(min(data$var_slope, na.rm = TRUE), 2), 
                                round(max(data$var_slope, na.rm = TRUE), 2))) +
  scale_fill_manual(values = palette, name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none') 
plot3 <- ggplot(data, aes(x = smooth_range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Range Loess \nCurve') +
  ylab('Density') +
  scale_x_continuous(breaks = c(round(min(data$smooth_range, na.rm = TRUE), 2), 
                                round(max(data$smooth_range, na.rm = TRUE), 2)),
                     limits = c(round(min(data$smooth_range, na.rm = TRUE), 2), 
                                round(max(data$smooth_range, na.rm = TRUE), 2))) +
  scale_fill_manual(values = palette, breaks = c(1, 3, 4, 7, 8), labels = c('Undisturbed', 
                                                                                  'Fire', 'Harvest', 'Water', 
                                                                                  'Development'),
                    name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        # legend.title = element_text(face = 'bold', size = 11),
        # legend.text = element_text(size = 11),
        legend.background = element_blank(),
        legend.position = 'none')#,
        # legend.position = 'bottom')
plot4 <- ggplot(data, aes(x = var_ratio, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Pre-/Post-Dist. \nVar. Ratio') +
  ylab('Density') +
  scale_x_continuous(breaks = c(round(min(data$var_ratio, na.rm = TRUE), 2), 
                                round(max(data$var_ratio, na.rm = TRUE), 2))) +
  scale_fill_manual(values = palette, breaks = c(1, 3, 4, 7, 8), labels = c('Undisturbed', 
                                                                                  'Fire', 'Harvest', 'Water', 
                                                                                  'Development'),
                    name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_text(face = 'bold', size = 11),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none') 
plot5 <- ggplot(data, aes(x = mag, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('\nMagnitude') +
  ylab('Density') +
  scale_x_continuous(breaks = c(round(min(data$mag, na.rm = TRUE), 2), 
                                round(max(data$mag, na.rm = TRUE), 2))) +
  scale_fill_manual(values = palette, breaks = c(1, 3, 4, 7, 8), labels = c('Undisturbed', 
                                                                                  'Fire', 'Harvest', 'Water', 
                                                                                  'Development'),
                    name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none')
density_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 2, ncol = 3)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/detection_density_plots_p14_r32_training_nolegend.png', 
       plot = density_plots, 
       device = 'png',
       scale = 0.75,
       units = c('in'),
       dpi = 400)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/detection_density_plots_p14_r32_training_legend.png', 
       plot = plot3, 
       device = 'png',
       scale = 0.75,
       units = c('in'),
       dpi = 400)

# south carolina ----------------------------------------------------------

# read in data
data <- read.csv('/home/annie/Documents/satellite-field-comp/figs/data/img_training_p16_r37_complete.csv', stringsAsFactors = FALSE)

# remove unknown values and convert to factor
data <- data[data$disturbance != 0, ]
data$disturbance <- as.factor(data$disturbance)

palette <- brewer_pal(palette = 'Set1')(9)
palette <- palette[c(1, 3, 4, 7, 8)]

### for everything, detection vars
plot1 <- ggplot(data, aes(x = min_index, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Minimum NDVI') +
  ylab('Density') +
  scale_fill_manual(values = palette) +
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
plot2 <- ggplot(data, aes(x = min_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Minimum \nLoess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, name = 'Disturbance') +
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
plot3 <- ggplot(data, aes(x = var_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Variance in \nLoess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(1, 3, 4, 7, 8), labels = c('Undisturbed', 'Fire', 
                                                                            'Harvest', 
                                                                            'Flooding', 'LCC'),
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
plot4 <- ggplot(data, aes(x = post_var, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Variance in NDVI \nPost-Disturbance') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(1, 3, 4, 7, 8), labels = c('Undisturbed', 'Fire', 
                                                                            'Harvest', 
                                                                            'Flooding', 'LCC'),
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
plot5 <- ggplot(data, aes(x = s_min_slope_range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Spatial Range in \nMinimum Loess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(1, 3, 4, 7, 8), labels = c('Undisturbed', 'Fire', 
                                                                            'Harvest', 
                                                                            'Flooding', 'LCC'),
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
density_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 1, ncol = 5)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/detection_density_plots_p16_r37_training.png', 
       plot = density_plots, 
       device = 'png',
       width = 10.5,
       height = 4,
       units = c('in'),
       dpi = 400)

# ATTRIBUTION VAR PLOTS ---------------------------------------------------

# these should separate out disturbance types

# washington --------------------------------------------------------------

# read in data
data <- read.csv('/home/annie/Documents/satellite-field-comp/figs/data/img_training_p47_r27_complete.csv', stringsAsFactors = FALSE)

# remove unknown values and convert to factor
data <- data[data$disturbance > 1, ]
data$disturbance <- as.factor(data$disturbance)

palette <- brewer_pal(palette = 'BrBG')(9)
palette <- palette[c(3, 4, 5, 7, 8, 9)]

### for everything, detection vars
plot1 <- ggplot(data, aes(x = max_index, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Max. NDVI\n') +
  ylab('Density') +
  scale_x_continuous(breaks = c(round(min(data$max_index, na.rm = TRUE), 2), 
                                round(max(data$max_index, na.rm = TRUE), 2))) +
  scale_fill_manual(values = palette) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_text(face = 'bold', size = 11),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none')
plot2 <- ggplot(data, aes(x = min_raw_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Max. Raw \nSlope') +
  ylab('Density') +
  scale_x_continuous(breaks = c(round(min(data$min_raw_slope, na.rm = TRUE), 2),
                                round(max(data$min_raw_slope, na.rm = TRUE), 2))) +
  scale_fill_manual(values = palette, name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none') 
plot3 <- ggplot(data, aes(x = s_min_slope_avg, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Spatial Avg. Min. \nLoess Slope') +
  ylab('Density') +
  scale_x_continuous(breaks = c(round(min(data$s_min_slope_avg, na.rm = TRUE), 2), 
                                round(max(data$s_min_slope_avg, na.rm = TRUE), 2)),
                     limits = c(round(min(data$s_min_slope_avg, na.rm = TRUE), 2), 
                                round(max(data$s_min_slope_avg, na.rm = TRUE), 2))) +
  scale_fill_manual(values = palette, breaks = c(3, 4, 5, 7, 8, 9), labels = c('Fire', 'Harvest', 'Wind', 'Water', 
                                                                               'Development', 'Landslide/Avalanche'),
                    name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        # legend.title = element_text(face = 'bold', size = 11),
        # legend.text = element_text(size = 11),
        legend.background = element_blank(),
        legend.position = 'none')#,
# legend.position = 'bottom')
plot4 <- ggplot(data, aes(x = d_avg_ind_range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Dist. Spatial \nNDVI Range') +
  ylab('Density') +
  scale_x_continuous(breaks = c(round(min(data$d_avg_ind_range, na.rm = TRUE), 2), 
                                round(max(data$d_avg_ind_range, na.rm = TRUE), 2))) +
  scale_fill_manual(values = palette, breaks = c(3, 4, 5, 7, 8, 9), labels = c('Fire', 'Harvest', 'Wind', 'Water', 
                                                                               'Development', 'Landslide/Avalanche'),
                    name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none') 
plot5 <- ggplot(data, aes(x = d_avg_ind_med, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Dist. Spatial \nMed. NDVI Avg.') +
  ylab('Density') +
  scale_x_continuous(breaks = c(round(min(data$d_avg_ind_med, na.rm = TRUE), 2), 
                                round(max(data$d_avg_ind_med, na.rm = TRUE), 2))) +
  scale_fill_manual(values = palette, breaks = c(3, 4, 5, 7, 8, 9), labels = c('Fire', 'Harvest', 'Wind', 'Water', 
                                                                               'Development', 'Landslide/Avalanche'),
                    name = 'Disturbance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = 'black'),
        panel.border = element_blank(),
        axis.title.x = element_text(face = 'bold', size = 11),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.position = 'none')
density_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 2, ncol = 3)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/attribution_density_plots_p47_r27_training_nolegend.png', 
       plot = density_plots, 
       device = 'png',
       scale = 0.75,
       units = c('in'),
       dpi = 400)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/attribution_density_plots_p47_r27_training_legend.png', 
       plot = plot3, 
       device = 'png',
       scale = 0.75,
       units = c('in'),
       dpi = 400)

# oregon --------------------------------------------------------------

# read in data
data <- read.csv('/home/annie/Documents/satellite-field-comp/figs/data/img_training_p45_r30_complete.csv', stringsAsFactors = FALSE)

# remove unknown values and convert to factor
data <- data[data$disturbance > 1, ]
data$disturbance <- as.factor(data$disturbance)

palette <- brewer_pal(palette = 'Set1')(9)
palette <- palette[c(2, 3, 4, 8)]

### for everything, detection vars
plot1 <- ggplot(data, aes(x = min_index, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('\nMinimum NDVI') +
  ylab('Density') +
  scale_fill_manual(values = palette) +
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
plot2 <- ggplot(data, aes(x = var_raw_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Variance in \nRaw Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, name = 'Disturbance') +
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
plot3 <- ggplot(data, aes(x = s_min_slope_avg, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Spatial Avg. in \nMin. Loess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(2, 3, 4, 8), labels = c('Insects',
                                                                            'Fire', 'Harvest', 
                                                                            'LCC'),
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
plot4 <- ggplot(data, aes(x = s_min_slope_range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Range of Loess \nSlope over Area') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(2, 3, 4, 8), labels = c('Insects',
                                                                            'Fire', 'Harvest', 
                                                                            'LCC'),
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
plot5 <- ggplot(data, aes(x = s_max_slope_ratio, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Ratio of Max. Loess \nSlope to Area Avg.') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(2, 3, 4, 8), labels = c('Insects',
                                                                            'Fire', 'Harvest', 
                                                                            'LCC'),
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
density_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 1, ncol = 5)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/attribution_density_plots_p45_r30_training.png', 
       plot = density_plots, 
       device = 'png',
       width = 10.5,
       height = 4,
       units = c('in'),
       dpi = 400)

# colorado ----------------------------------------------------------------

# read in data
data <- read.csv('/home/annie/Documents/satellite-field-comp/figs/data/img_training_p35_r32_complete.csv', stringsAsFactors = FALSE)

# remove unknown values and convert to factor
data <- data[data$disturbance > 1, ]
data$disturbance <- as.factor(data$disturbance)

palette <- brewer_pal(palette = 'Set1')(9)
palette <- palette[c(2, 3, 4, 8)]

### for everything, detection vars
plot1 <- ggplot(data, aes(x = min_index, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('\nMinimum NDVI') +
  ylab('Density') +
  scale_fill_manual(values = palette) +
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
plot2 <- ggplot(data, aes(x = var_raw_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Variance in \nRaw Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, name = 'Disturbance') +
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
plot3 <- ggplot(data, aes(x = s_min_slope_avg, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Avg. Minimum Loess \nSlope over Area') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(2, 3, 4, 8), labels = c('Insects',
                                                                            'Fire', 'Harvest', 
                                                                            'LCC'),
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
plot4 <- ggplot(data, aes(x = s_min_slope_range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Range of Min. \nLoess Slope over Area') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(2, 3, 4, 8), labels = c('Insects',
                                                                            'Fire', 'Harvest', 
                                                                            'LCC'),
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
plot5 <- ggplot(data, aes(x = d_min_slope_range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Range in Disturb. Min. \nLoess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(2, 3, 4, 8), labels = c('Insects',
                                                                            'Fire', 'Harvest', 
                                                                            'LCC'),
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
density_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 1, ncol = 5)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/attribution_density_plots_p35_r32_training.png', 
       plot = density_plots, 
       device = 'png',
       width = 10.5,
       height = 4,
       units = c('in'),
       dpi = 400)

# minnesota ---------------------------------------------------------------

# read in data
data <- read.csv('/home/annie/Documents/satellite-field-comp/figs/data/img_training_p27_r27_complete.csv', stringsAsFactors = FALSE)

# remove unknown values and convert to factor
data <- data[data$disturbance > 1, ]
data$disturbance <- as.factor(data$disturbance)

palette <- brewer_pal(palette = 'Set1')(9)
palette <- palette[c(4, 7, 8)]

### for everything, detection vars
plot1 <- ggplot(data, aes(x = min_index, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('\nMinimum NDVI') +
  ylab('Density') +
  scale_fill_manual(values = palette) +
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
plot2 <- ggplot(data, aes(x = mad_mean_ratio, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Ratio of Mean Avg. \nDeviance to Mean') +
  ylab('Density') +
  scale_fill_manual(values = palette, name = 'Disturbance') +
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
plot3 <- ggplot(data, aes(x = s_min_slope_ratio, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Ratio of Min. Loess \nSlope to Area Avg.') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(4, 7, 8), labels = c('Harvest', 
                                                                         'Flooding', 'LCC'),
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
plot4 <- ggplot(data, aes(x = d_avg_ind_range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Range in Avg. NDVI \nin Dist. Pixels') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(4, 7, 8), labels = c('Harvest', 
                                                                         'Flooding', 'LCC'),
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
plot5 <- ggplot(data, aes(x = d_slope_med, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Median Loess Slope \nin Dist. Pixels') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(4, 7, 8), labels = c('Harvest', 
                                                                         'Flooding', 'LCC'),
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
density_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 1, ncol = 5)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/attribution_density_plots_p27_r27_training.png', 
       plot = density_plots, 
       device = 'png',
       width = 10.5,
       height = 4,
       units = c('in'),
       dpi = 400)

# maine -------------------------------------------------------------------

# read in data
data <- read.csv('/home/annie/Documents/satellite-field-comp/figs/data/img_training_p12_r28_complete.csv', stringsAsFactors = FALSE)

# remove unknown values and convert to factor
data <- data[data$disturbance > 1, ]
data$disturbance <- as.factor(data$disturbance)

palette <- brewer_pal(palette = 'Set1')(9)
palette <- palette[c(3, 4, 7, 8)]

### for everything, detection vars
plot1 <- ggplot(data, aes(x = avg_raw_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('\nMean Raw Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette) +
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
plot2 <- ggplot(data, aes(x = s_min_slope_avg, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Spatial Avg. in \nMin. Loess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, name = 'Disturbance') +
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
plot3 <- ggplot(data, aes(x = s_min_slope_range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Spatial Range in \nMin. Loess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(3, 4, 7, 8), labels = c('Fire', 
                                                                            'Harvest', 
                                                                            'Flooding', 'LCC'),
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
plot4 <- ggplot(data, aes(x = d_dur_med, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Spatial Med. \Disturbance Duration') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(3, 4, 7, 8), labels = c('Fire', 
                                                                            'Harvest', 
                                                                            'Flooding', 'LCC'),
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
plot5 <- ggplot(data, aes(x = d_avg_ind_range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Range in Mean NDVI \nin Dist. Area') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(3, 4, 7, 8), labels = c('Fire', 
                                                                            'Harvest', 
                                                                            'Flooding', 'LCC'),
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
density_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 1, ncol = 5)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/attribution_density_plots_p12_r28_training.png', 
       plot = density_plots, 
       device = 'png',
       width = 10.5,
       height = 4,
       units = c('in'),
       dpi = 400)

# pennsylvania ------------------------------------------------------------

# read in data
data <- read.csv('/home/annie/Documents/satellite-field-comp/figs/data/img_training_p14_r32_complete.csv', stringsAsFactors = FALSE)

# remove unknown values and convert to factor
data <- data[data$disturbance > 1, ]
data$disturbance <- as.factor(data$disturbance)

palette <- brewer_pal(palette = 'Set1')(9)
palette <- palette[c(3, 4, 7, 8)]

### for everything, detection vars
plot1 <- ggplot(data, aes(x = slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Mean Loess \nSlope') +
  ylab('Density') +
  scale_fill_manual(values = palette) +
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
plot2 <- ggplot(data, aes(x = avg_raw_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Mean Raw \nSlope') +
  ylab('Density') +
  scale_fill_manual(values = palette, name = 'Disturbance') +
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
plot3 <- ggplot(data, aes(x = s_min_slope_range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Spatial Range in \nMin. Loess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(3, 4, 7, 8), labels = c('Fire', 
                                                                            'Harvest', 
                                                                            'Flooding', 'LCC'),
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
plot4 <- ggplot(data, aes(x = d_min_slope_range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Range in Min. Loess \nSlope over Dist. Area') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(3, 4, 7, 8), labels = c('Fire', 
                                                                            'Harvest', 
                                                                            'Flooding', 'LCC'),
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
plot5 <- ggplot(data, aes(x = d_min_slope_med, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Median Min. Loess \nSlope over Dist. Area') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(3, 4, 7, 8), labels = c('Fire', 
                                                                            'Harvest', 
                                                                            'Flooding', 'LCC'),
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
density_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 1, ncol = 5)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/attribution_density_plots_p14_r32_training.png', 
       plot = density_plots, 
       device = 'png',
       width = 10.5,
       height = 4,
       units = c('in'),
       dpi = 400)

# south carolina ----------------------------------------------------------

# read in data
data <- read.csv('/home/annie/Documents/satellite-field-comp/figs/data/img_training_p16_r37_complete.csv', stringsAsFactors = FALSE)

# remove unknown values and convert to factor
data <- data[data$disturbance > 1, ]
data$disturbance <- as.factor(data$disturbance)

palette <- brewer_pal(palette = 'Set1')(9)
palette <- palette[c(3, 4, 7, 8)]

### for everything, detection vars
plot1 <- ggplot(data, aes(x = avg_index, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('\nMean NDVI') +
  ylab('Density') +
  scale_fill_manual(values = palette) +
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
plot2 <- ggplot(data, aes(x = slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Overall \nLoess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, name = 'Disturbance') +
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
plot3 <- ggplot(data, aes(x = avg_slope, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Mean \nLoess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(3, 4, 7, 8), labels = c('Fire', 
                                                                            'Harvest', 
                                                                            'Flooding', 'LCC'),
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
plot4 <- ggplot(data, aes(x = s_min_slope_range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Spatial Range in \nMin. Loess Slope') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(3, 4, 7, 8), labels = c('Fire', 
                                                                            'Harvest', 
                                                                            'Flooding', 'LCC'),
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
plot5 <- ggplot(data, aes(x = d_min_slope_range, fill = disturbance)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  xlab('Range in Min. Loess \nSlope over Dist. Area') +
  ylab('Density') +
  scale_fill_manual(values = palette, breaks = c(3, 4, 7, 8), labels = c('Fire', 
                                                                            'Harvest', 
                                                                            'Flooding', 'LCC'),
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
density_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 1, ncol = 5)

ggsave(filename = '/home/annie/Documents/satellite-field-comp/figs/density/attribution_density_plots_p16_r37_training.png', 
       plot = density_plots, 
       device = 'png',
       width = 10.5,
       height = 4,
       units = c('in'),
       dpi = 400)
