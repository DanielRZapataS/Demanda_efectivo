of_dicc <- fread("Data/Dictionaries/offices_dictionary.csv",
                 colClasses = "character")
of_dicc2 <- fread("Data/Dictionaries/dicc_oficinas.csv",
                  colClasses = "character")
of_dicc2 <- of_dicc2[, .(CNL_OFICINA = BOFIC_CODIGO, 
                         DEPARTAMENTO, 
                         MUNICIPIO)]

long <- 3
for (i in min(of_dicc2 [, .(nchar(CNL_OFICINA))]):(long - 1))
{
  of_dicc2 [, l := nchar(CNL_OFICINA)]
  of_dicc2 [, CNL_OFICINA := ifelse(l < long, paste(0, CNL_OFICINA, sep =
                                                     ""),
                                   CNL_OFICINA)]
}
of_dicc2 [, l := NULL] 

of_dicc <- merge(of_dicc, of_dicc2, by = "CNL_OFICINA")

fwrite(of_dicc, "Data/Dictionaries/offices_dictionary.csv" )
