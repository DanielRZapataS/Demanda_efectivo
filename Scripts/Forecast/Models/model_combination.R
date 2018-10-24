#'Group of time series models (ARIMA, ETS, NN,TBATS) models from forecast packages,
#'adjusted to forecast one step ahead, and return Best time series model with the
#' lowest recursive forecast error on validation transactions dates.


#' @param matrix_train: 
#' @param matrix_val: 
#' @param xreg_vector:
#' @return list containing best time series model, rmse, and fit values



model_combination <- function(fit_train1, fit_train2, 
                              fit_val1, fit_val2, matrix_train, matrix_val ){
  pivot <- length(fit_train1) - length(fit_train2) + 1
  fit_train1  = fit_train1[pivot:length(fit_train1 )]
  
  ts_train <- matrix_train$TXS[pivot:length(matrix_train$TXS)]
  
  fit_train <- 0.5*fit_train1 + 0.5*fit_train2
  fit_val <- 0.5*fit_val1 + 0.5*fit_val2 
  
  rmse_train <- rmse(ts_train, fit_train)
  rmse_val <- rmse(matrix_val$TXS, fit_val)
  return(list(rmse_train = rmse_train, rmse_val = rmse_val,
              fit_train = fit_train, fit_val = fit_val,
              model_name = "model_combination"))
}