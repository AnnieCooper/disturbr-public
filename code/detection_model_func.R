# function to create disturbance detection model --------------------------

create_detection_model <- function(data, p_train = 0.6, p_test = 0.4, 
                                   length_importance = 20, output_path, imp_filename,
                                   plot_importance = TRUE, spatial = TRUE) {
  
  # convert data disturbance back to factor
  data$disturbance <- as.factor(data$disturbance)
  
  # divide datasets into training and testing subsets
  set.seed(3522)
  sample_ind <- sample(2,  
                       nrow(data),
                       replace = T,
                       prob = c(p_train, p_test))
  data_train <- data[sample_ind == 1,]  
  data_test <- data[sample_ind == 2,]  
  
  # check data percentages (disturbance classes); these should be similar
  # if not similar, change the set.seed() command above to different number, and try again
  print(paste('Percentage disturbed total:', 
              nrow(data[data$disturbance == '2',]) / nrow(data)))
  print(paste('Percentage disturbed in training:', 
              nrow(data_train[data_train$disturbance == '2',]) / nrow(data_train)))
  print(paste('Percentage disturbed in testing:', 
              nrow(data_test[data_test$disturbance == '2',]) / nrow(data_test)))
  
  # remove date variables from training data
  data_train <- data_train[, preds := NULL]
  data_train <- data_train[, predicted := NULL]

  # remove date and location columns from training dataset
  data_train <- data_train[, c('longitude', 'latitude', 'date_min_slope', 'date_min', 'dist_date') := NULL, ]
  
  # remove spatial variables if requested
  if (spatial == FALSE) {
    spat_list <- names(data)[grep('^s', names(data))]
    data_train <- data_train[, (spat_list) := NULL, ]
  }
  
  # fit rf using caret package
  # Random Search
  set.seed(5032)
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "random")
  mtry <- floor(sqrt(ncol(data_train)))
  tunegrid <- expand.grid(.mtry = mtry)
  rf_random <- train(disturbance ~ .,
                     data = data_train,
                     method = "rf",
                     metric = 'Accuracy',
                     tuneLength = 15,
                     trControl = control)
  
  # Variable Importance Plot
  if (plot_importance == TRUE) {
    plot(varImp(rf_random), top = 20)
  }

  # Write out
  test <- varImp(rf_random, top = 65)$importance
  fwrite(test, paste(output_path, imp_filename, sep = ''), row.names = T)
  
  # print top 5 most important variables
  col_index <- varImp(rf_random)$importance %>% 
    mutate(names = row.names(.)) %>%
    arrange(-Overall)
  col_index <- paste(names(data)[names(data) %in% col_index$names[1:5]], collapse = ', ')
  print(paste('Top 5 variables in model:', col_index))
  
  # predict disturbance with training data
  preds <- predict(rf_random)
  
  # create confusion matrix for training data
  cat('Confusion matrix for training data:\n')
  print(confusionMatrix(data = preds,  
                        reference = data_train$disturbance)) 
  
  # predict disturbance with validation data
  preds <- predict(rf_random, data_test)
  
  # create confusion matrix for validation data
  cat('Confusion matrix for testing data:\n')
  print(confusionMatrix(data = preds,  
                        reference = data_test$disturbance))
  
  return(rf_random)
}
