# function to add predictions to image ------------------------------------

create_pred_image <- function(data, orig_data, pred_values, data_path, fill_image, plot = TRUE, output_path, out_data, out_raster) {
  # add predictions to image
  data$preds <- pred_values
  
  data <- orig_data[data, , on = c('longitude', 'latitude', 'avg_index')]
  data <- data[, (names(data)[grep('^i', names(data))]) := NULL]
  
  # get dates of pixels predicted to be disturbance
  pred_values[pred_values <= 1] <- NA
  pred_values[!is.na(pred_values)] <- data$date_min_slope[!is.na(pred_values)] # predicted disturbance date
  
  # single band raster to replace, based on forest img
  class_img <- raster(paste(data_path, fill_image, sep = ''))
  true_coords <- data.table(longitude = coordinates(class_img)[, 1],
                            latitude = coordinates(class_img)[, 2],
                            value = as.numeric(NA))
  values <- data.table(longitude = signif(coordinates(class_img)[, 1], digits = 7),
                       latitude = signif(coordinates(class_img)[, 2], digits = 7),
                       value = getValues(class_img))
  
  # join by longitude and latitude with img_data
  data$predicted <- pred_values
  fill_data <- data[values, .(longitude, latitude, predicted), on = c('longitude', 'latitude')]
  fill_data[, c(1, 2)] <- true_coords[, c(1, 2)]
  
  crs_img <- crs(class_img)
  coordinates(fill_data) <- ~ longitude + latitude
  gridded(fill_data) <- TRUE
  classified <- raster(fill_data)
  projection(classified) <- crs_img
  
  # look at classified image
  if (plot == TRUE) {
    plot(classified)
  }

  # write out
  writeRaster(classified, paste(output_path, out_raster, sep = ''), overwrite = T)
  fwrite(data, paste(output_path, out_data, sep = ''))
  
  print('Finished writing out classified data and image.')
  return(classified)
}
