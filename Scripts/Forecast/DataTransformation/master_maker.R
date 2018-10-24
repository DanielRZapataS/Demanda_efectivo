#' make the marter table from the datasetof an office,
#' with the next variables: offices id, date, number of transactions 
#' month, and year.
#' @param dataset: dataset from an office (data.table)
#' @param train_cut: Last date of training data (Date)
#' @param test_cut: last day of validation data (Date)
#' @param holidays: holidays table
#' @param paydays: paydays table 
#' @references "https://cran.r-project.org/web/packages/tsoutliers/tsoutliers.pdf"
#' @references "https://cran.r-project.org/web/packages/imputeTS/imputeTS.pdf"
#' @return New dataset
#' @return data_train: training data
#' @return data_val: validation data 
#' @return data_test: test data 
#' @return matrix_set : matrix with target and variables 
#' @return matrix_train : matrix with target and variables on training
#' @return matrix_val : matrix with target and variables on validation
#' @return matrix_test : matrix with target and variables on testing

master_maker <- function(dataset, train_cut, test_cut,
                         holidays, paydays){
  dates_range <- range(dataset$FECHA) #get max and min dates in base
  
  calendar <- seq.Date(from = dates_range[1], to = dates_range[2],
                    by = "day") # sequence of calendar dates
  calendar <- data.table(FECHA = calendar)
  calendar[, DAY := day(FECHA)]
  calendar[, MES := month(FECHA)]
  calendar[, YEAR := year(FECHA)]
  
  # include all calendar dates in main base. Then replace NAs
  dataset <- merge(dataset, calendar,
                   by = c( "FECHA", "DAY", "MES", "YEAR"), 
                   all.y = TRUE)
  
  dataset <- merge(dataset, holidays, by = "FECHA") 
  dataset <- merge(dataset, paydays, by = "FECHA")
  
  #dataset[is.na(dataset)] <- 0
  dataset[,WEEKDAY := wday(FECHA)] 
  dataset <- dataset[!(WEEKDAY %in% c(7,1)),]
  # data_ts <- ts(dataset$TXS)
  # outliers <- tso(y = data_ts, types = c("AO"))
  # dataset[outliers$times, TXS := NA]
  
  #filtro de kalman solo para los días que no son festivos
  dataset[HOLIDAYS == 0, TXS := na.interpolation(TXS)] 
  # días festivos iguales a cero en la serie
  dataset[HOLIDAYS == 1,TXS :=0 ]
  
  
  # lag variables 
  dataset[, ':='(TXS_LAG1 = shift(TXS, 1, 0, "lag"))]
  dataset[, ':='(TXS_LAG2 = shift(TXS, 2, 0, "lag"))]
  dataset[, ':='(TXS_LAG3 = shift(TXS, 3, 0, "lag"))]
  dataset[, ':='(TXS_LAG4 = shift(TXS, 4, 0, "lag"))]
  dataset[, ':='(TXS_LAG5 = shift(TXS, 5, 0, "lag"))]
  dataset[, ':='(TXS_LAG6 = shift(TXS, 6, 0, "lag"))]
  dataset[, ':='(TXS_LAG7 = shift(TXS, 7, 0, "lag"))]
  dataset[, ':='(TXS_LAG8 = shift(TXS, 8, 0, "lag"))]
  dataset[, ':='(TXS_LAG9 = shift(TXS, 9, 0, "lag"))]
  dataset[, ':='(TXS_LAG10 = shift(TXS, 10, 0, "lag"))]
  dataset[, ':='(TXS_LAG11 = shift(TXS, 11, 0, "lag"))]
  dataset[, ':='(TXS_LAG12 = shift(TXS, 12, 0, "lag"))]
  dataset[, ':='(TXS_LAG13 = shift(TXS, 13, 0, "lag"))]
  dataset[, ':='(TXS_LAG14 = shift(TXS, 14, 0, "lag"))]
  dataset[, ':='(TXS_LAG15 = shift(TXS, 15, 0, "lag"))]
  dataset[, ':='(TXS_LAG16 = shift(TXS, 16, 0, "lag"))]
  dataset[, ':='(TXS_LAG17 = shift(TXS, 17, 0, "lag"))]
  dataset[, ':='(TXS_LAG18 = shift(TXS, 18, 0, "lag"))]
  dataset[, ':='(TXS_LAG19 = shift(TXS, 19, 0, "lag"))]
  dataset[, ':='(TXS_LAG20 = shift(TXS, 20, 0, "lag"))]

  
  data_cut <- dataset$TXS_LAG20 != 0
  data_cut <- min(which(data_cut  == TRUE))
  date_cut <- dataset[data_cut,]$FECHA
  
  dataset[, HOLIDAYS_PRE := shift(HOLIDAYS, 1,0, "lead")]
  dataset[, HOLIDAYS_POST := shift(HOLIDAYS, 1,0, "lag")]
  dataset[, PAYDAY_PRE := shift(PAYDAY, 1,0, "lead")]
  dataset[, PAYDAY_POST := shift(PAYDAY, 1,0, "lag")]
  
  dataset[, HALF_MONTH := ifelse(DAY <= 15, 1, 0)]
  
  # last 5 days mean
  dataset[, LAST5_MEAN := rollmean(TXS, 5, align = "right",
                                    fill = 0)]
  dataset[, LAST5_MEAN := shift(LAST5_MEAN, 1, 0, "lag")]
  
  # last 10 days mean
  dataset[, LAST10_MEAN := rollmean(TXS, 10, align = "right",
                                    fill = 0)]
  dataset[, LAST10_MEAN := shift(LAST10_MEAN, 1, 0, "lag")]
  
  #las 20 days mean
  dataset[, LAST20_MEAN := rollmean(TXS, 20, align = "right",
                                    fill = 0)]
  dataset[, LAST20_MEAN := shift(LAST20_MEAN, 1, 0, "lag")]
  
  factors <- c("WEEKDAY", "DAY",  "MES", "YEAR" )
  dataset[ , (factors) := lapply(.SD, factor), .SDcols = factors]
  
  pivot <- which(colnames(dataset) == "TXS")
  neworder <- c("TXS",colnames(dataset)[-pivot])
  setcolorder(dataset, neworder)
  
  matrix_set <- dataset[FECHA >= date_cut,]
  matrix_train <- matrix_set[ FECHA <= train_cut,
                              -c("FECHA")]
  matrix_val <- matrix_set[FECHA > train_cut & FECHA <= test_cut,
                           -c("FECHA")]
  matrix_test <- matrix_set[FECHA > test_cut, 
                            -c("FECHA")]
  return(list(dataset = dataset, matrix_set = matrix_set, 
              matrix_train = matrix_train,
              matrix_val = matrix_val, matrix_test = matrix_test))
}