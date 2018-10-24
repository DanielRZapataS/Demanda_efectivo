
datos <- fread("Data/Meta/FECHAS_NEGOCIO_BOGOTA.csv")
for(i in 1:nrow(datos)){
  
  datos$MONTH[i] <- ifelse(nchar(datos$MONTH[i]) == 1, 
                           paste0(0,datos$MONTH[i]), datos$MONTH[i])
  datos$DAY[i] <- ifelse(nchar(datos$DAY[i]) == 1, 
                           paste0(0,datos$DAY[i]), datos$DAY[i])
}

datos[, FECHA := as.Date(paste(YEAR,  MONTH, DAY, sep = "/") )]
class(datos$FECHA)

datos[, NEGOCIO := 1]

datos <- datos[, .(FECHA, NEGOCIO)]
class(datos$FECHA)
range(datos$FECHA)
dates <- data.table(FECHA = seq.Date(from = as.Date("2014-01-01"), 
                                     to = as.Date("2018-12-01"), 
                                     by = "day"))
datos <- merge(datos, dates, by = "FECHA", all.y = T)
datos[is.na(datos)] <- 0
fwrite(datos, "Data/Meta/negocio_bogota_dummy.csv")
