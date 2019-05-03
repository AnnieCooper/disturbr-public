# code used to calculate attribution training breakdown -------------------

data = read.csv('~/disturbr/data/training_points/img_training_attribution_p14_r32_block3.csv')
data2 = read.csv('~/disturbr/data/training_points/img_training_attribution_p14_r32_block5.csv')
data3 = read.csv('~/disturbr/data/training_points/img_training_attribution_p14_r32_block7.csv')
data = rbind(data, data2)
data = rbind(data, data3)
table(data$disturbance)
## disturbance pixels used are everything in the table, minus the 0s


# same, but temporal ------------------------------------------------------

data = read.csv('~/disturbr/data/training_points/img_training_temporal_attribution_p14_r32_block3.csv')
data2 = read.csv('~/disturbr/data/training_points/img_training_temporal_attribution_p14_r32_block5.csv')
data3 = read.csv('~/disturbr/data/training_points/img_training_temporal_attribution_p14_r32_block7.csv')
data = rbind(data, data2)
data = rbind(data, data3)
table(data$disturbance)


