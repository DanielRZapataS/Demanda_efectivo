master_maker_recursive<- function(datarec, holidays, 
                                  paydays, matrix_rec,
                                  matrix_set){
  datarec[, DAY := as.numeric(substr(FECHA, 9,10))]
  datarec[, MES := as.numeric(substr(FECHA,6,7))]
  datarec[, YEAR := as.numeric(substr(FECHA,1,4))]
  datarec <- merge(datarec, holidays, by = "FECHA") 
  datarec <- merge(datarec, paydays, by = "FECHA")
  datarec[,WEEKDAY := wday(FECHA)] 
  datarec <- datarec[!(WEEKDAY %in% c(7,1)),]
  #filtro de kalman solo para los días que no son festivos
  datarec[HOLIDAYS == 0, TXS := na.interpolation(TXS)] 
  # días festivos iguales a cero en la serie
  datarec[HOLIDAYS == 1,TXS :=0 ]
  # lag variables 
  datarec[, ':='(TXS_LAG1 = shift(TXS, 1, 0, "lag"))]
  datarec[, ':='(TXS_LAG2 = shift(TXS, 2, 0, "lag"))]
  datarec[, ':='(TXS_LAG3 = shift(TXS, 3, 0, "lag"))]
  datarec[, ':='(TXS_LAG4 = shift(TXS, 4, 0, "lag"))]
  datarec[, ':='(TXS_LAG5 = shift(TXS, 5, 0, "lag"))]
  datarec[, ':='(TXS_LAG6 = shift(TXS, 6, 0, "lag"))]
  datarec[, ':='(TXS_LAG7 = shift(TXS, 7, 0, "lag"))]
  datarec[, ':='(TXS_LAG8 = shift(TXS, 8, 0, "lag"))]
  datarec[, ':='(TXS_LAG9 = shift(TXS, 9, 0, "lag"))]
  datarec[, ':='(TXS_LAG10 = shift(TXS, 10, 0, "lag"))]
  datarec[, ':='(TXS_LAG11 = shift(TXS, 11, 0, "lag"))]
  datarec[, ':='(TXS_LAG12 = shift(TXS, 12, 0, "lag"))]
  datarec[, ':='(TXS_LAG13 = shift(TXS, 13, 0, "lag"))]
  datarec[, ':='(TXS_LAG14 = shift(TXS, 14, 0, "lag"))]
  datarec[, ':='(TXS_LAG15 = shift(TXS, 15, 0, "lag"))]
  datarec[, ':='(TXS_LAG16 = shift(TXS, 16, 0, "lag"))]
  datarec[, ':='(TXS_LAG17 = shift(TXS, 17, 0, "lag"))]
  datarec[, ':='(TXS_LAG18 = shift(TXS, 18, 0, "lag"))]
  datarec[, ':='(TXS_LAG19 = shift(TXS, 19, 0, "lag"))]
  datarec[, ':='(TXS_LAG20 = shift(TXS, 20, 0, "lag"))]
  
  
  data_cut <- datarec$TXS_LAG20 != 0
  data_cut <- min(which(data_cut  == TRUE))
  date_cut <- datarec[data_cut,]$FECHA
  
  datarec[, HOLIDAYS_PRE := shift(HOLIDAYS, 1,0, "lead")]
  datarec[, HOLIDAYS_POST := shift(HOLIDAYS, 1,0, "lag")]
  datarec[, PAYDAY_PRE := shift(PAYDAY, 1,0, "lead")]
  datarec[, PAYDAY_POST := shift(PAYDAY, 1,0, "lag")]
  
  datarec[, HALF_MONTH := ifelse(DAY <= 15, 1, 0)]
  
  # last 5 days mean
  datarec[, LAST5_MEAN := rollmean(TXS, 5, align = "right",
                                   fill = 0)]
  datarec[, LAST5_MEAN := shift(LAST5_MEAN, 1, 0, "lag")]
  
  # last 10 days mean
  datarec[, LAST10_MEAN := rollmean(TXS, 10, align = "right",
                                    fill = 0)]
  datarec[, LAST10_MEAN := shift(LAST10_MEAN, 1, 0, "lag")]
  
  #las 20 days mean
  datarec[, LAST20_MEAN := rollmean(TXS, 20, align = "right",
                                    fill = 0)]
  datarec[, LAST20_MEAN := shift(LAST20_MEAN, 1, 0, "lag")]
  
  factors <- c("WEEKDAY", "DAY",  "MES", "YEAR" )
  datarec[ , (factors) := lapply(.SD, factor), .SDcols = factors]
  
  pivot <- which(colnames(datarec) == "TXS")
  neworder <- c("TXS",colnames(datarec)[-pivot])
  setcolorder(datarec, neworder)
  
  datarec[FECHA == range(datarec$FECHA)[2], TXS := NA ]
  matrix_rec <- datarec[FECHA >= matrix_rec$FECHA[1] , -c("FECHA")]
  levels(matrix_rec$YEAR) <- levels(matrix_set$YEAR)
  return(matrix_rec)
}