stagingFile_path <- get.path(staging_path,"staging" )
staging <- fread(stagingFile_path, colClasses = "character")
setnames(staging, "fecha", "FECHA")
staging[, FECHA := as.Date(FECHA, "%Y-%m-%d")]
var_type <- names(staging)[grepl(series_type ,names(staging))]
setnames(staging, var_type, "TXS")
#fil
offices <- unique(staging$codigoOficina)
total <- length(offices) 

#Meta
holidays <- get.path(meta_path, "holiday") %>% fread()
paydays <- get.path(meta_path, "payday") %>% fread()
holidays[ , fecha := as.Date(fecha, "%d/%m/%Y")]
paydays[,  fecha := as.Date(fecha, "%d/%m/%Y")] 
names(holidays) <- toupper(names(holidays))
names(paydays) <- toupper(names(paydays))

dataset <- staging[codigoOficina == offices[3],
                   .(TXS = as.numeric(TXS), FECHA)] 
dataset[, ':='(DAY = day(FECHA),
               MES = month(FECHA),
               YEAR = year(FECHA))]
dates_range <- range(dataset$FECHA) #get max and min dates in base
calendar <- seq.Date(from = dates_range[1], to = dates_range[2],
                     by = "day") # sequence of calendar dates
calendar <- data.table(FECHA = calendar)
calendar[, DAY := day(FECHA)]
calendar[, MES := month(FECHA)]
calendar[, YEAR := year(FECHA)]

# include all calendar dates in main base. Then replace NAs
dataset
dataset <- merge(dataset, calendar,
                 by = c( "FECHA", "DAY", "MES", "YEAR"), 
                 all.y = TRUE)

dataset <- merge(dataset, holidays, by = "FECHA") 
dataset <- merge(dataset, paydays, by = "FECHA")
dataset[,WEEKDAY := wday(FECHA)] 


dataset <- dataset[!(WEEKDAY %in% c(7,1)) ,]
# 
# #filtro de kalman solo para los dÃ?as que no son festivos
# dataset[HOLIDAYS == 0, TXS := na.interpolation(TXS)] 
# # dÃ?as festivos iguales a cero en la serie
# dataset[HOLIDAYS == 1,TXS :=0 ]


plot <- dataset[, .(FECHA, TXS, HOLIDAYS, PAYDAY)]
zeroDays <- plot[HOLIDAYS == 1 , FECHA]

ggplot(plot[HOLIDAYS == 0], aes(FECHA, TXS) )+
  geom_line(size = 1, alpha = 1)+
  geom_vline(
    xintercept = as.numeric(zeroDays),
    linetype = 1,
    size = 0.5,
    alpha = 0.5,
    color = "green"
  )

dataset[TXS == 0 & HOLIDAYS == 0]
## diff

diffBase <- dataset[, .(FECHA, TXS, HOLIDAYS)]
txs_ts <- ts(diffBase$TXS, start = c(2016, 1,1), end = , frequency = 365)
