holidays <- fread("Data/Meta/holiday_dummy.csv")
holidays[ , FECHA := as.Date(FECHA)]
dates_forecast <- dates_maker(from = as.Date("2018-03-01") , to = as.Date("2018-12-31") )
dates_forecast <- merge(dates_forecast, holidays, by = "FECHA")
dates_forecast[, DAY := as.numeric(substr(FECHA, 9,10))]
dates_forecast[, MES := as.numeric(substr(FECHA,6,7))]
dates_forecast[, YEAR := as.numeric(substr(FECHA,1,4))]

base <- dates_forecast[HOLIDAYS == 0, ]
base[, .N, by = MES]
