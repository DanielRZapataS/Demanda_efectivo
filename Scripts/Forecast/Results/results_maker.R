## results forecasting main ## 
forecastFolder <- forecastFolderRetiros
series <- "Retiros"
pegue <- function(var, long){
  m <- nchar(var)
  ind <- long - m
  zeros <- paste(rep("0",ind ),collapse = '')
  var <- paste0(zeros, var)
  return(var)
}
results_maker <- function(){
  ## get staging data
  staging <- get.path(staging_path, current_month) %>% 
    fread(colClasses = "character")
  staging[, codigOficina := pegue(codigoOficina, 3), by = 1:nrow(staging)]
  #Meta
  holidays <- get.path(meta_path, "holiday") %>% fread()
  paydays <- get.path(meta_path, "payday") %>% fread()
  holidays[ , fecha := as.Date(fecha, "%d/%m/%Y")]
  paydays[, fecha := as.Date(fecha, "%d/%m/%Y")] 
  
  offices <- unique(staging$codigoOficina)
  total <- length(offices) 
  
  ### Validation days 
  dates_val <- dates_maker(from = train_cut, to = test_cut)
  dates_val <- dates_val$FECHA
  ## forecast
  dates_forecast <- dates_maker(from = last_test_date, to = last_forecast_date)
  dates_forecast <- dates_forecast$FECHA
  if(do.test == TRUE){
    ## test
    dates_test <- dates_maker(from = test_cut, to = last_test_date)
    dates_test <- dates_test$FECHA
    }
  
  # importing results 
  results <- importForecastResults(forecastFolder)
  lengthResults <- sapply(results, "[[", "rmse_train") %>% length()
  
  # Model counting
  countingModels <-  countingModels(results)
  write.csv(countingModels, 
            os.path.join(auxFolder, paste0("countingModels",series,".csv")))
  
  # rmse values
  matrix_results <- rmseResults(results)
  fwrite(matrix_results, 
         os.path.join(auxFolder, paste0( "rmse",series,".csv")))
  
  # forecast TXS daily and monthly
  if(do.test == TRUE){
    forecast_daily <- forecastDaily_base(results,dates_test,holidays,
                                         offices, do.test = TRUE )
    fwrite(forecast_daily, 
           os.path.join(auxFolder,
                        paste0("txsForecastDailyTest",series,".csv")
                        )
           )
  }
  
  forecast_daily <- forecastDaily_base(results,dates_forecast,holidays,
                                       offices, do.test = FALSE )
  fwrite(forecast_daily, 
         os.path.join(auxFolder,
                      paste0("txsForecastDaily",series,".csv")
         )
  )
  
 
}
