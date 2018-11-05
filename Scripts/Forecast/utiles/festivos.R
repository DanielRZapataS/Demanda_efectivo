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
datos <- datos[, .(Fiesta, `Fecha en 2013`, `Fecha en 2014`, `Fecha en 2015`, 
                   `Fecha en 2016`, `Fechas en 2017`, `Fechas en 2018`)]
colnames(datos) <- c("FIESTA", 2013, 2014, 2015, 2016,2017,2018) %>% as.character()

sapply(datos, class)
type <- datos$FIESTA
type <- sapply(strsplit(type, "[", fixed = TRUE), `[[`, 1)
type <- sapply(strsplit(type, "(", fixed = TRUE), `[[`, 1)
unwanted_array = list(
  'S' = 'S',
  's' = 's',
  'Z' = 'Z',
  'z' = 'z',
  'À' = 'A',
  'Á' = 'A',
  'Â' = 'A',
  'Ã' = 'A',
  'Ä' = 'A',
  'Å' = 'A',
  'Æ' = 'A',
  'Ç' = 'C',
  'È' = 'E',
  'É' = 'E',
  'Ê' = 'E',
  'Ë' = 'E',
  'Ì' = 'I',
  'Í' = 'I',
  'Î' = 'I',
  'Ï' = 'I',
  'Ñ' = 'N',
  'Ò' = 'O',
  'Ó' = 'O',
  'Ô' = 'O',
  'Õ' = 'O',
  'Ö' = 'O',
  'Ø' = 'O',
  'Ù' = 'U',
  'Ú' = 'U',
  'Û' = 'U',
  'Ü' = 'U',
  'Ý' = 'Y',
  'Þ' = 'B',
  'ß' = 'Ss',
  'à' = 'a',
  'á' = 'a',
  'â' = 'a',
  'ã' = 'a',
  'ä' = 'a',
  'å' = 'a',
  'æ' = 'a',
  'ç' = 'c',
  'è' = 'e',
  'é' = 'e',
  'ê' = 'e',
  'ë' = 'e',
  'ì' = 'i',
  'í' = 'i',
  'î' = 'i',
  'ï' = 'i',
  'ð' = 'o',
  'ñ' = 'n',
  'ò' = 'o',
  'ó' = 'o',
  'ô' = 'o',
  'õ' = 'o',
  'ö' = 'o',
  'ø' = 'o',
  'ù' = 'u',
  'ú' = 'u',
  'û' = 'u',
  'ý' = 'y',
  'ý' = 'y',
  'þ' = 'b',
  'ÿ' = 'y'
)

type <- chartr(paste(names(unwanted_array), collapse=''),
       paste(unwanted_array, collapse=''),
       type)

datos <- data.table(YEAR = rep(c(2013:2018), each = nrow(datos)),
                    HOLIDAYS = c(datos$`2013`, datos$`2014`, datos$`2015`,
                                 datos$`2016`, datos$`2017`, datos$`2018`))
datos[, TYPE := rep(type, times = length(2013:2018))]
                    
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

fwrite(datos, os.path.join(meta_path, "holidays.csv"))

holidays <- fread(os.path.join(meta_path, "holidays.csv"))
holidays[, FECHA := as.Date(FECHA)]
holidays[, HOLIDAYS := 1]

holidays <- holidays[, .(FECHA, HOLIDAYS, TYPE)]
class(holidays$FECHA)
range(holidays$FECHA)
dates <- data.table(FECHA = seq.Date(from = range(holidays$FECHA)[1], 
                                     to = as.Date("2018-12-31"), 
                                     by = "day"))
holidays <- merge(holidays, dates, by = "FECHA", all.y = T)
holidays[is.na(holidays)] <- 0
fwrite(holidays, os.path.join(meta_path, "holiday_dummy.csv"))

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

paydays <- fread(os.path.join(meta_path, "payday_dummy.csv"))
names(paydays) <- toupper(names(paydays))
paydays[, TYPE := ifelse(PAYDAY == 1, 1, 0)]
fwrite(paydays, os.path.join(meta_path, "payday_dummy.csv"))

# pago de impuestos 
taxes <- get.path(meta_path, "FECHAS_NEGOCIO") %>% fread()
taxes[, FECHA := as.Date(paste(YEAR,  MONTH, DAY, sep = "/") )]
class(taxes$FECHA)

taxes[, TAXE_DAY := 1]

taxes <- taxes[, .(FECHA, TAXE_DAY, TYPE)]
class(holidays$FECHA)
range(holidays$FECHA)
dates <- data.table(FECHA = seq.Date(from = as.Date("2014-01-01"), 
                                     to = as.Date("2018-12-31"), 
                                     by = "day"))
taxes <- merge(taxes, dates, by = "FECHA", all.y = T)
taxes[is.na(taxes)] <- 0
fwrite(taxes, os.path.join(meta_path, "taxes_dummy.csv"))











