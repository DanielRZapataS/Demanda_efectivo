library(data.table)

of_dicc <- fread("Data/Dictionaries/OFICINAS_CARTERA.csv",
                 colClasses = "character")
of_dicc <- of_dicc[, .(CENT_COSTO,NOMBRE,ZONA)]

# todos los cod de oficina tienen 3 digitos, encontes emparejo los codigos 
# poniendo 0 al inicio de cada uno para hacer el cruce
long <- 3
for(i in min(of_dicc[, .(nchar(CENT_COSTO))]):(long-1)){
  of_dicc[, m := nchar(CENT_COSTO)]
  of_dicc[, CENT_COSTO := ifelse(m < long,
                                 paste(0, CENT_COSTO, sep=""), 
                                 CENT_COSTO)]
}
of_dicc[, m := NULL]

unique(of_dicc$CENT_COSTO)
colnames(of_dicc) <- c("OFICINA", "NOMBRE", "ZONA")
fwrite(of_dicc, "Data/Dictionaries/offices_dictionary.csv")
