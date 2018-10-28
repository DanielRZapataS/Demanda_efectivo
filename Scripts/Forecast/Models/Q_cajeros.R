Q_cajeros <- function(office ,
                      do.test,
                      staging,
                      train_cut,
                      test_cut,
                      last_test_date,
                      last_forecast_date,
                      holidays,
                      paydays,
                      xreg_vector,
                      offices,
                      j) {
 
  
  dataset <- staging[codigoOficina == offices[j],
                     .(TXS = as.numeric(TXS), FECHA)] 
  dataset[, ':='(DAY = day(FECHA),
                 MES = month(FECHA),
                 YEAR = year(FECHA))]
  
  datalist <- master_maker(dataset, train_cut, test_cut,
                           holidays, paydays)
  dataset <- datalist$dataset
  matrix_train <- datalist$matrix_train
  matrix_val <- datalist$matrix_val
  matrix_test <- datalist$matrix_test
  matrix_set <- datalist$matrix_set
  
  ##  Models' Parameters 
  
  # Establish a list of possible values for mtry, nodesize and sampsize
  mtry <- seq((ncol(matrix_train)-1)/3, ncol(matrix_train) * 0.8, 3) %>% round(0)
  nodesize <- c(3, 9)
  sampsize <- nrow(matrix_train) * c(0.8)
  
  ## Randome forest 
  RF_results <- randomForest_train(matrix_train, mtry , nodesize,
                                   matrix_val)
  TS_results <- ts_models_train(matrix_train, matrix_val, xreg_vector)
  
  MCB_results <- model_combination(fit_train1 = RF_results$fit_train,
                                   fit_train2 = TS_results$fit_train,
                                   fit_val1 = RF_results$fit_val,
                                   fit_val2 = TS_results$fit_val,
                                   matrix_train, matrix_val)
  if(do.test == TRUE){
    test_results <- recursive_forecast(matrix_set,
                                       from = test_cut ,
                                       to = last_test_date,
                                       RF_results,
                                       TS_results,
                                       MCB_results,
                                       is.test = TRUE,
                                       holidays,
                                       paydays,
                                       xreg_vector)
    forecast_results <- recursive_forecast(matrix_set,
                                           from = last_test_date ,
                                           to = last_forecast_date,
                                           RF_results,
                                           TS_results,
                                           MCB_results,
                                           is.test = FALSE,
                                           holidays,
                                           paydays,
                                           xreg_vector)
    rmse_train <- test_results$rmse_train
    rmse_val <- test_results$rmse_val
    rmse_test <- test_results$rmse_test
    fit_train <- test_results$fit_train
    fit_val <- test_results$fit_val
    forecast_test <- test_results$forecast_rec
    forecast_rec <- forecast_results$forecast_rec
    model_name <- test_results$model_name
    
    return(list(oficina =  offices[j], 
                rmse_train = rmse_train, rmse_val = rmse_val,
                rmse_test = rmse_test, fit_train = fit_train,
                fit_val = fit_val, forecast_test = forecast_test,
                forecast_rec = forecast_rec, model_name = model_name))
  }
  
  if(do.test == FALSE){
    forecast_results <- recursive_forecast(matrix_set,
                                           from = last_test_date ,
                                           to = last_forecast_date,
                                           RF_results,
                                           TS_results,
                                           MCB_results,
                                           is.test = FALSE,
                                           holidays,
                                           paydays,
                                           xreg_vector)
    rmse_train <- forecast_results$rmse_train
    rmse_val <- forecast_results$rmse_val
    fit_train <- forecast_results$fit_train
    fit_val <- forecast_results$fit_val
    forecast_rec <- forecast_results$forecast_rec
    model_name <- forecast_results$model_name
    
    return(list(oficina =  offices[j],
                rmse_train = rmse_train, rmse_val = rmse_val,
                fit_train = fit_train, fit_val = fit_val,
                forecast_rec = forecast_rec, model_name = model_name))
  }
  
}