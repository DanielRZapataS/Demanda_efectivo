
## requisito conteo de datos faltates 

setwd("C:/Users/dzapat3/Documents/Proyectos/Q-cajeros/Q_cajeros1.2")
library(data.table)


holidays <- fread("Data/Meta/holiday_dummy.csv")

holidays[ , FECHA := as.Date(FECHA)]

of_dicc <- fread("Data/Dictionaries/offices_dictionary.csv",
                 colClasses = "character")

base <- fread("data/Original/TXS_OFICINAS_03042018.csv", 
              colClasses = "character")

head(base)
base <- merge(base,of_dicc,by = "OFICINA")

base[is.na(EXT) == T, ':='(EXT= 0)]
base[,FECHA := as.Date(FECHA, "%d/%m/%Y")]


base[, FECHA_LAG := shift(FECHA, 1, 0, "lag")]
base[, DIFF := FECHA - FECHA_LAG]
base <- base[FECHA != "2016-09-01"]

base1 <- base[DIFF > 10]
#base <- merge(base, holidays, by = "FECHA") 

base1 <- base1[ order(-DIFF) ]




dias_habiles <- function(x, y){
  calendar<- seq.Date(from = y, to = x,
                    by = "day")
  calendar <- data.table(FECHA = calendar)
  calendar <- merge(calendar, holidays, by = "FECHA")
  calendar[, WEEKDAY := wday(FECHA)]
  calendar <- calendar[!(WEEKDAY %in% c(7,1)) & HOLIDAYS == 0 ]
  return(nrow(calendar))
}

base1[, DIAS_HABILES := dias_habiles(FECHA, FECHA_LAG), by = 1:nrow(base1)]  
fwrite(base1, "Data/Meta/datosfaltantes.csv")
