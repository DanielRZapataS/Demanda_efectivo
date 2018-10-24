## Daily folder 

## ploting txs time series function for daily data 
plotsFolder <- plotsRetiros_path
plotsFolder <- os.path.join(plotsFolder,
                                paste0("Plots_", current_month))
dir.create(plotsFolder)

## get staging data
office_names <- staging[, .(name = unique(nombreOficina)),by = codigoOficina]
office_names[, saver := paste0(codigoOficina, text_cleaner(name))]

### Validation days 
dates_val <- dates_maker(from = train_cut, to = test_cut)
dates_val <- dates_val$FECHA
## forecast days
dates_forecast <- dates_maker(from = last_test_date, to = last_forecast_date)
dates_forecast <- dates_forecast$FECHA

if(do.test == TRUE){
  ## test days 
  dates_test <- dates_maker(from = test_cut, to = last_test_date)
  dates_test <- dates_test$FECHA
}
##staging preparation 
staging[, fecha := as.Date(fecha)]
staging[fecha < range(dates_val)[1] ,Type := "Training"]
staging[fecha %in% dates_val, Type := "Validation"]
master <-
  staging[fecha <= last_test_date, .(codigoOficina, fecha,
                                           TXS = if(series == "Depositos") {
                                             depositos
                                           } else{
                                             if(series == "Retiros") {
                                               retiros
                                             }
                                           }, Type)]
##recursive forecast
forecast_daily <-  os.path.join(auxFolder,
                                paste0("txsForecastDaily", series, ".csv")) %>%
  fread(colClasses = "character")
forecast_daily[, fecha := as.Date(fecha)]
forecast_daily <- forecast_daily[, .(codigoOficina, fecha, TXS)]
forecast_daily[, Type := "Recursive forecast"]

# merging
if(do.test == TRUE){
  master[fecha %in% dates_test, Type := "Test"]
  ## test 
  forecast_dailytest <-  os.path.join(auxFolder,
                                      paste0("txsForecastDailyTest", series, ".csv")) %>%
    fread(colClasses = "character")
  forecast_dailytest[, fecha := as.Date(fecha)]
  forecast_dailytest <- forecast_dailytest[, .(codigoOficina, fecha, TXS)]
  forecast_dailytest[, Type := "Test forecast"]
  dailyTxsPlot <- rbindlist(list(master, forecast_dailytest, forecast_daily))
}else{ 
  dailyTxsPlot <- rbindlist(list(master,  forecast_daily))
  }
dailyTxsPlot <- dailyTxsPlot[order(codigoOficina, fecha, Type)]
dailyTxsPlot <- merge(dailyTxsPlot, holidays, by = "fecha")
dailyTxsPlot <- dailyTxsPlot[holidays == 0, ]
dailyTxsPlot[, holidays := NULL]
dailyTxsPlot[, TXS := as.numeric(TXS)]

# making test plot 
ppi <- 300
matrix_list <- list()
j = 1
for(j in 1:total){
  office <- offices[j]
  office_name <- office_names[codigoOficina == office, name]
  saver <- office_names[codigoOficina == office, saver]
  dailyTxsPlotOff <- na.omit(dailyTxsPlot[codigoOficina == office,])
  matrix_result <- matrix_results[codigoOficina == office,.(model_name, 
                                                    rmse_train, rmse_val, 
                                                    rmse_test)]
  
  test <- dailyTxsPlotOff[Type == "Test", TXS]
  forecastTest <- dailyTxsPlotOff[Type == "Test forecast", TXS]
  
  error <- test - forecastTest   
  
  matrix_result <- copy(matrix_result)
  matrix_result[, "MAE test" :=  mae(test, forecastTest) ]
  matrix_result[, "sMAPE test" := smape(test, forecastTest)  ]
  matrix_result[, "TRX Promedio" := mean( test)]
  matrix_result[, "Max sub-pronóstico" := max(error)]
  matrix_result[, "Max sobre-pronóstico" := min(error)]
  matrix_result[, "Sub-pronósticos" := sum(error > 0)]
  matrix_result[, "Sobre-prónosticos" := sum(error < 0)]
  matrix_result[, "MAE Test/TRX Pr." := (`MAE test`/`TRX Promedio`)*100]
  matrix_list[[j]] <- matrix_result
  
  # for plot 
  matrix <- copy(matrix_result)
  matrix[, model_name := NULL]
  matrix <- matrix[,  lapply(.SD, round, digits = 2)]
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  matrix <- tableGrob(matrix, rows=NULL, theme=tt)
  
  
  plot <-
    ggplot(dailyTxsPlotOff, aes(fecha, TXS, color = Type, linetype = Type)) +
    geom_line(size = 0.8, alpha = 0.7) +
    ylab(paste0("Valor de los ", tolower(series))) +
    xlab("Fecha") +
    labs(
      title = paste0("Pronóstico ",
                     office_name, " entrenamiento, validación y testeo"),
      subtitle = matrix_result$model_name
    ) +
    geom_vline(
      xintercept = as.numeric(c(train_cut, test_cut, last_test_date)),
      linetype = 4,
      size = 1.2,
      color = "darkgreen"
    ) +
    facet_zoom(x = fecha %in% c(dates_test, dates_forecast),
               zoom.size = 1.2)  +
    scale_linetype_manual(values = c(1, 1, 2, 1, 1)) +
    theme_ts + theme(legend.position = "top")
  
  png(
    paste0(os.path.join(plotsFolder, saver), ".png"),
    width = 15 * ppi,
    height = 10 * ppi,
    res = ppi
  )
  print(grid.arrange(
    plot,
    matrix,
    nrow = 2,
    as.table = TRUE,
    heights = c(3, 1)
  ))
  dev.off()
  
}
