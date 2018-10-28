
recursive_forecast <- function(matrix_set,
                               from,
                               to,
                               RF_results,
                               TS_results,
                               MCB_results,
                               is.test = TRUE,
                               holidays,
                               paydays,
                               xreg_vector){
  rmse_competition <- c(RF_results$rmse_val,
                        TS_results$rmse_val, 
                        MCB_results$rmse_val)
  
  model <- which.min(rmse_competition)
  
  dates_forecast <- dates_maker(from = from, to = to )
  
  master_table <- matrix_set[ FECHA <= from,]
  ## xreg matrix
  matrix_xreg <- data.table(dates_forecast)
  matrix_xreg[, DAY := as.numeric(substr(FECHA, 9,10))]
  matrix_xreg <- merge(matrix_xreg, holidays, by = "FECHA")
  matrix_xreg <- merge(matrix_xreg, paydays, by = "FECHA")
  matrix_xreg[, HOLIDAYS_PRE := shift(HOLIDAYS, 1,0, "lead")]
  matrix_xreg[, HOLIDAYS_POST := shift(HOLIDAYS, 1,0, "lag")]
  matrix_xreg[, PAYDAY_PRE := shift(PAYDAY, 1,0, "lead")]
  matrix_xreg[, PAYDAY_POST := shift(PAYDAY, 1,0, "lag")]
  matrix_xreg[, HALF_MONTH := ifelse(DAY <= 15, 1, 0)]
  matrtix_xreg <- matrix_xreg[HOLIDAYS == 0]
  remove_col <-
    colnames(matrix_xreg)[c((colnames(matrix_xreg) %in% xreg_vector))]
  matrix_xreg <- matrix_xreg[, .SD, .SDcols = remove_col ]
  setcolorder(matrix_xreg, xreg_vector)
  
  #xreg matrix fit
  matrix_xreg_fit <- matrix_set[FECHA <= from,] 
  remove_col <- colnames(matrix_xreg_fit)[c((colnames(matrix_xreg_fit) 
                                             %in% xreg_vector))]
  matrix_xreg_fit <- matrix_xreg_fit[, .SD, .SDcols = remove_col ]
  setcolorder(matrix_xreg_fit, xreg_vector)
  
  ## Random forest
  forecast_rf <- c()
  nrow(dates_forecast)
  
  for(i in 1: nrow(dates_forecast)){
    matrix_rec <- data.table(dates_forecast[1:i,"FECHA"], 
                             TXS = c( forecast_rf,NA))
    datarec <- master_table[,.( FECHA,TXS) ]
    l = list(datarec, matrix_rec)
    datarec <- rbindlist(l, use.names=TRUE) 
    matrix_rec <- master_maker_recursive(datarec, holidays,
                                        paydays, matrix_rec,
                                        matrix_set)
    
    forecast_rf <- predict(RF_results$model, matrix_rec) 
  }
  
  ## TS
  data_ts <- ts(master_table$TXS, frequency = 5)
  ts_models <- list(
    ARIMA = Arima(data_ts, model = TS_results$models$ARIMA, 
                  xreg = matrix_xreg_fit),
    ETS = ets(data_ts, model = TS_results$models$ETS),
    NeuralNetwork = nnetar(data_ts, model = TS_results$models$NeuralNetwork,
                           xreg = matrix_xreg_fit),
    TBATS = tbats(data_ts, model = TS_results$models$TBATS,
                  xreg = matrix_xreg_fit)
  )
  ts_model <- ts_models[[TS_results$model]]
  horizon <- nrow(dates_forecast)
  
  forecast_ts  <- forecast(ts_model, h = horizon, xreg = matrix_xreg)
  forecast_ts <- as.vector(forecast_ts$mean)
  
  ## MODEL COMBINATION
  
  forecast_cb <- 0.5*forecast_ts +0.5*forecast_rf 
  
  ## Forecast
  if(model == 1){
    rmse_train <- RF_results$rmse_train
    rmse_val <- RF_results$rmse_val
    fit_train <- RF_results$fit_train
    fit_val <- RF_results$fit_val
    model_name <- RF_results$model_name
    forecast_rec <- forecast_rf 
  }
  if(model == 2){
    rmse_train <- TS_results$rmse_train
    rmse_val <- TS_results$rmse_val
    fit_train <- TS_results$fit_train
    fit_val <- TS_results$fit_val
    model_name <- TS_results$model_name
    forecast_rec <- forecast_ts 
  }
  if(model == 3){
    rmse_train <- MCB_results$rmse_train
    rmse_val <- MCB_results$rmse_val
    fit_train <- MCB_results$fit_train
    fit_val <- MCB_results$fit_val
    model_name <- MCB_results$model_name
    forecast_rec <- forecast_cb 
  }
  
  
  ## 
  if(is.test == TRUE){
    forecast_rec_table <-
      data.table(FECHA = dates_forecast$FECHA, forecast_rec)
    
    TXS_test <- matrix_set[ FECHA %in% dates_forecast$FECHA,
                            .(FECHA, TXS)]
    forecast_rec_table <- merge(forecast_rec_table, TXS_test, by = "FECHA")
    rmse_test <-
      rmse(forecast_rec_table$TXS, forecast_rec_table$forecast_rec)
    return(list(rmse_train = rmse_train, rmse_val = rmse_val,
                rmse_test = rmse_test, fit_train = fit_train,
                fit_val = fit_val, forecast_rec = forecast_rec,
                model_name = model_name))
  }
  
  if(is.test == FALSE){
    return(list(rmse_train = rmse_train, rmse_val = rmse_val,
                fit_train = fit_train,
                fit_val = fit_val, forecast_rec = forecast_rec,
                model_name = model_name))
    
  }
  
}