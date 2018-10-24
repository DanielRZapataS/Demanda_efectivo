#'Train multiple Random Forest models chosen from random forest parameters to 
#'forecast one step ahead, and return Best Random forest model with the
#' lowest recursive forecast error on validation transactions dates.

#' @param matrix_train: Matrix with target and variables on training.
#' @param mtry: Vector of possible values for number of variables randomly 
#' sampled as candidates at each split.(numeric)
#' @param nodesize: Vector of possible values for minimum size of terminal 
#' nodes. (numeric)
#' @param sampsize: Vector of possible values of the size of sample to 
#' draw. (numeric)
#' @param target_val: Matrix on  validation. (numeric) 
#' @return List containing best random forest, rmse, and recursive forecast

randomForest_train <- function(matrix_train, mtry, nodesize,
                               matrix_val){
  # Create a data frame containing all combinations 
  hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize)
  
  RFmodel<-list()
  # Write a loop over the rows of hyper_grid to train the grid of models
  for (i in 1:nrow(hyper_grid)) {
    
    # Train a Random Forest model
    RFmodel[[i]] <- randomForest(formula = TXS ~ ., 
                                 data = matrix_train,
                                 mtry = hyper_grid$mtry[i],
                                 nodesize = hyper_grid$nodesize[i],
                                 ntree = 2000, 
                                 importance = T,
                                 keep.forst = T)
    
    
  }
  
  # Identify optimal set of hyperparmeters based on valdation error
  # Evaluate the grid 
  # Number of potential models in the grid
  num_models <- length(RFmodel)
  
  # Create an empty vector to store RMSE values
  rmse_values_RF <- c()
  
  # save recursive forecast 
  predics_val <- list()
  
  for (i in 1:num_models) {
    
    # Retreive the i^th model from the list
    model <- RFmodel[[i]]
    
    # Generate predictions on grade_valid 
    pred <- predict(object = model,
                    newdata = matrix_val)
    predics_val[[i]] <- pred
    
    # Compute validation RMSE and add to the 
    rmse_values_RF[i] <- rmse(actual = matrix_val$TXS, 
                           predicted = pred)
    
    
  }
  
  names(rmse_values_RF) <- c(paste0("RF", rep(1:nrow(hyper_grid))))
  
  # Identify the model with smallest validation set RMSE
  RF_model <- RFmodel[[which.min(rmse_values_RF)]]
  rmse_train <- rmse(actual = matrix_train$TXS,
                     predicted = predict(RF_model) )
  rmse_val <- rmse_values_RF[which.min(rmse_values_RF)]
  fit_train <- predict(RF_model)
  fit_val <-  predics_val[[which.min(rmse_values_RF)]]
  
  return(list(rmse_train = rmse_train, rmse_val = rmse_val,
              fit_train = fit_train, fit_val = fit_val,
              model_name = paste0(RF_model$call)[1], 
              model = RF_model))
  
  
}