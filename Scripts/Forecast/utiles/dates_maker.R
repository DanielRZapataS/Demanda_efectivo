#' Make Dates calendar for validation and test data
#' @param from : train_cut for validation data or test_cut for test data (Date)
#' @param to : test_cut for validation data (Date)
dates_maker <- function(from , to){
  from <- from + 1
  dates <- data.table(FECHA = seq.Date(from = from, to = to, by = "day"))
  dates[, WEEKDAY := wday(FECHA)]
  dates <- dates[!(WEEKDAY %in% c(7,1)),]
  return(dates)
}