month <- "201805"

library(data.table)
data <- fread("Data/Original/TRX_HIST_20180531.csv", 
              colClasses = "character")


of_dicc <- fread("Data/Dictionaries/offices_dictionary.csv",
                 colClasses = "character")

data1 <- data[, .(LLAVE = unique(LLAVE))]
data1[, ':='(
  CNL_OFICINA = substr(LLAVE, 3, 5),
  CNL_JORNADA = substr(LLAVE, 1, 2),
  BANDERA = substr(LLAVE, 6, 6)
)]

officesDictionary <-  merge(data1,of_dicc,by = "CNL_OFICINA")

fwrite(officesDictionary, "Data/Dictionaries/officesDictionary.csv")



staging <- data[FECHA < "2018-05-01", .(LLAVE, FECHA, TXS)]

fwrite(staging, "Data/Staging/staging_201804.csv")
