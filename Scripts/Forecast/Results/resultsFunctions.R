#' Import results from forecasts
#' @param pathResults : path of the forecasts results 
#' @return: list with all the results  
importForecastResults <- function(pathResults){
  results <- list()
  files <- list.files(pathResults)
  position <- sapply(strsplit(files, "o"), "[[", 1) %>% as.numeric
  location <- data.table(files = files , position = position ) 
  location <- location[order(position)] 
  iter1 <- c(0, location$position)
  through <- seq(1,length(iter1)-1)
  
  for(i in through){
    along <- seq(iter1[i]+1, iter1[i+1])
    upload <- paste(pathResults, location[i, files], sep = "/")
    load(upload)
    for(j in along){
      results[[j]] <- Results[[j]]
    }
  }
  return(results)
}

#' Counting model type 
#' @param results: list of results 
#' @return: counting of model types  
countingModels <- function(results){
  # is there any offices withouth forecast ?
  models <- sapply(results, "[[", "model_name")
  if(sum(sapply(models, is.null)) > 0){
    print("There are offices without forecasts")
    models <- models[-which(sapply(models, is.null))]
  }
  
  # Counting type models
  models <- unlist(models)
  models <- text_cleaner(models)
  docs <- Corpus(VectorSource(models))
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(freq=v)
  return(d)
}

#' RMSE results 
#' @param results: list of results 
#' @return: dt with rmse values of trainig, validation and test for all offices 
rmseResults <- function(results){
  matrix_results <- data.table(codigoOficina = sapply(results, "[[","oficina"))
#  matrix_results <- data.table(LLAVE = offices)
  matrix_results[, model_name := sapply(results, "[[", "model_name")]
  matrix_results[, rmse_train := sapply(results, "[[", "rmse_train")]
  matrix_results[, rmse_val := sapply(results, "[[", "rmse_val")]
  if(do.test == TRUE){
    matrix_results[, rmse_test := sapply(results, "[[", "rmse_test")]
  }
  return(matrix_results)
}

#' Forecast daily dt 
#' @param results: list of results 
#' @param dates_forecast: vector of forecasts dates
#' @param holidays: holidays dummy dt 
#' @param offices: vector of offices
#' @return: Forecast daily dt
forecastDaily_base <- function(results, dates_forecast, 
                               holidays, offices, do.test){
  tabla_results <- list()
  for(j in c(1:length(results))){
    if(do.test){
      TXS <- results[[j]]$forecast_test
      forecast <- data.table(fecha = dates_test, TXS)
    }else{
      TXS <- results[[j]]$forecast_rec
      forecast <- data.table(fecha = dates_forecast, TXS)
    }

    forecast[, ':='(MES = month(fecha),
                   YEAR = year(fecha))]
    forecast <- merge(forecast, holidays, by = "fecha")
    forecast[holidays == 1, TXS := 0]
    forecast[, codigoOficina := results[[j]]$oficina]
    tabla_results[[j]] <- forecast  
  }
  
  forecast_daily <- rbindlist(tabla_results)
  
  return(forecast_daily)
}

#' Get work days for forecast months
#' @param last_test_date: last test date
#' @param last_forcast_date: last forecast date
#' @param holidays: holidays dummy dt 
#' @return: data.table of work days by month of forecast range
dias_hb <- function(last_test_date, last_forecast_date, holidays){
  dias_hb <- dates_maker(from = last_test_date, to = last_forecast_date) %>% 
    merge(holidays, by = "FECHA") %>% 
    data.table()
  dias_hb[, ':='(MES = month(FECHA))]
  dias_hb <- dias_hb[HOLIDAYS == 0, ]
  dias_hb <- dias_hb[, .(DIAS_HB=.N), by = MES]
  return(dias_hb)
}
