install.packages("httr")
install.packages("rvest")
install.packages("stringr")

library(rvest)
library(stringr)
library(data.table)
library(stringi)

URL <- "https://es.wikipedia.org/wiki/Anexo:D%C3%ADas_festivos_en_Colombia"
spage <- read_html(URL)
spage

datos <- spage %>%
  html_table(fill = T) %>% 
  .[[3]] %>% 
  data.table()
datos
datos$`Fecha en 2014`
datos <- datos[, .(`Fecha en 2013`, `Fecha en 2014`, `Fecha en 2015`, 
                   `Fecha en 2016`, `Fechas en 2017`, `Fechas en 2018`)]
colnames(datos) <- c(2013, 2014, 2015, 2016,2017,2018) %>% as.character()

sapply(datos, class)

datos <- data.table(YEAR = rep(c(2013:2018), each = nrow(datos)),
                    HOLIDAYS = c(datos$`2013`, datos$`2014`, datos$`2015`,
                                 datos$`2016`, datos$`2017`, datos$`2018`))
datos[, DAY := as.numeric(substr(HOLIDAYS, 1, 2))]

for(i in 1:nrow(datos)){
  
  datos$DAY[i] <- ifelse(nchar(datos$DAY[i]) == 1, 
                         paste0(0,datos$DAY[i]), datos$DAY[i])
}

datos[, MONTH := gsub(" ", "",HOLIDAYS)]
datos$MONTH = datos[ ,ifelse(
  str_count(MONTH,"enero")==1,1, MONTH)]

datos$MONTH = datos[ ,ifelse(
  str_count(MONTH,"febrero")==1,2, MONTH)]

datos$MONTH = datos[ ,ifelse(
  str_count(MONTH,"marzo")==1,3, MONTH)]

datos$MONTH = datos[ ,ifelse(
  str_count(MONTH,"abril")==1,4, MONTH)]

datos$MONTH = datos[ ,ifelse(
  str_count(MONTH,"mayo")==1,5, MONTH)]

datos$MONTH = datos[ ,ifelse(
  str_count(MONTH,"junio")==1,6, MONTH)]

datos$MONTH = datos[ ,ifelse(
  str_count(MONTH,"julio")==1,7, MONTH)]

datos$MONTH = datos[ ,ifelse(
  str_count(MONTH,"agosto")==1,8, MONTH)]


datos$MONTH = datos[ ,ifelse(
  str_count(MONTH,"septiembre")==1,9, MONTH)]

datos$MONTH = datos[ ,ifelse(
  str_count(MONTH,"octubre")==1,10, MONTH)]

datos$MONTH = datos[ ,ifelse(
  str_count(MONTH,"noviembre")==1,11, MONTH)]

datos$MONTH = datos[ ,ifelse(
  str_count(MONTH,"diciembre")==1,12, MONTH)]

for(i in 1:nrow(datos)){
  
  datos$MONTH[i] <- ifelse(nchar(datos$MONTH[i]) == 1, 
                         paste0(0,datos$MONTH[i]), datos$MONTH[i])
}

datos[, FECHA := as.Date(paste(YEAR,  MONTH, DAY, sep = "/") )]
class(datos$FECHA)

fwrite(datos, "Data/Meta/holidays.csv")

holidays <- fread("Data/Meta/holidays.csv")
holidays[, FECHA := as.Date(FECHA)]
holidays[, HOLIDAYS := 1]

holidays <- holidays[, .(FECHA, HOLIDAYS)]
class(holidays$FECHA)
range(holidays$FECHA)
dates <- data.table(FECHA = seq.Date(from = range(holidays$FECHA)[1], 
                                     to = as.Date("2018-12-31"), 
                                     by = "day"))
holidays <- merge(holidays, dates, by = "FECHA", all.y = T)
holidays[is.na(holidays)] <- 0
fwrite(holidays, "Data/Meta/holiday_dummy.csv")

## pay day 
paydays <- fread("Data/Meta/paydays.csv")
paydays[, FECHA := as.Date(FECHA, "%d/%m/%Y")]
paydays[, PAYDAY := 1]
dates <- data.table(FECHA = seq.Date(from = range(paydays$FECHA)[1], 
                                     to = range(paydays$FECHA)[2], 
                                     by = "day"))

paydays <- merge(paydays, dates, by = "FECHA", all.y = T)
paydays[is.na(paydays)] <- 0
fwrite(paydays, "Data/Meta/payday_dummy.csv")













