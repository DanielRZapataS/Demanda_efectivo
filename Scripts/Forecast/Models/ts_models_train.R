#'Group of time series models (ARIMA, ETS, NN,TBATS) models from forecast packages,
#'adjusted to forecast one step ahead, and return Best time series model with the
#' lowest recursive forecast error on validation transactions dates.


#' @param matrix_train: 
#' @param matrix_val: 
#' @param xreg_vector:
#' @return list containing best time series model, rmse, and fit values

ts_models_train <- function(matrix_train, matrix_val, xreg_vector){
  
  matrix_tv <- rbindlist(list(matrix_train, matrix_val))
  
  pivot <- which(colnames(matrix_train) %in% xreg_vector)
  matrix_trainxreg = matrix_train[,pivot, with=FALSE]
  matrix_valxreg <- matrix_tv[,pivot, with=FALSE]  

  train_ts <- ts(matrix_train$TXS, frequency = 5 )
  ts_models <- list(
    ARIMA = auto.arima(train_ts, xreg = matrix_trainxreg),
    ETS = ets(train_ts),
    NeuralNetwork = nnetar(train_ts, xreg = matrix_trainxreg),
    TBATS = tbats(train_ts, xreg = matrix_trainxreg)
  )
  
  train_fittes <- sapply(ts_models, fitted)
  nn_pivot <- train_fittes[,3] %>% na.omit %>% length()
  nn_pivot <- nrow(matrix_train) - nn_pivot +1
  train_fittes <- train_fittes[nn_pivot:nrow(matrix_train),]
  rmse_train <-  apply(train_fittes,2, rmse, 
                     matrix_train$TXS[nn_pivot:nrow(matrix_train)] )
  
  val_ts <- ts(matrix_tv$TXS, frequency = 5)
  ts_validation <- list(
    ARIMA = Arima(val_ts, model = ts_models$ARIMA, xreg = matrix_valxreg),
    ETS = ets(val_ts, model = ts_models$ETS),
    NeuralNetwork = nnetar(val_ts, model = ts_models$NeuralNetwork,
                           xreg = matrix_valxreg),
    TBATS = tbats(val_ts, model = ts_models$TBATS,
                  xreg = matrix_valxreg)
  )
  
  val_fittes <- sapply(ts_validation , fitted)
  val_fittes <- val_fittes[(nrow(matrix_tv)- nrow(matrix_val) + 1): nrow(matrix_tv),]
  rmse_val <-  apply(val_fittes,2, rmse, matrix_val$TXS )

  
  model <- which.min(rmse_val)
  TS_models <- ts_models
  fit_train <- train_fittes[, model]
  fit_val <- val_fittes[, model]
  rmse_train <- rmse_train[model]
  rmse_val <- rmse_val[model]
  model_name <-  names(TS_models)[model]

  
  return(list(rmse_train = rmse_train, rmse_val = rmse_val,
              fit_train = fit_train, fit_val = fit_val,
              model_name = model_name, model = model,
              models = TS_models))
}

