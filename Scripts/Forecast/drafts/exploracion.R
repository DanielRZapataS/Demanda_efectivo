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
holidays <- get.path(meta_path, "holiday_dummy") %>% fread()
paydays <- get.path(meta_path, "payday_dummy") %>% fread()
taxes <- get.path(meta_path, "taxes_dummy") %>% fread()
names(holidays) <- toupper(names(holidays))
names(paydays) <- toupper(names(paydays))
names(taxes) <- toupper(names(taxes))
setnames(holidays, "TYPE", "HOLIDAY_TYPE")
setnames(paydays, "TYPE", "PAYDAY_TYPE")
setnames(taxes, "TYPE", "TAXE_TYPE")

holidays[ , FECHA := as.Date(FECHA, "%Y-%m-%d")]
paydays[,  FECHA := as.Date(FECHA, "%d/%m/%Y")] 
taxes[,  FECHA := as.Date(FECHA, "%Y-%m-%d")] 

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
dataset <- merge(dataset, taxes, by = "FECHA")
dataset[,WEEKDAY := wday(FECHA)] 


dataset <- dataset[!(WEEKDAY %in% c(7,1)) ,]
# 
# #filtro de kalman solo para los dias que no son festivos
# dataset[HOLIDAYS == 0, TXS := na.interpolation(TXS)] 
# # dias festivos iguales a cero en la serie
# dataset[HOLIDAYS == 1,TXS :=0 ]
dataset <- dataset[HOLIDAYS == 0]

ndiffs(dataset$TXS)

dataset[, ':='(TXS_LAG1 = shift(TXS, 1, NA, "lag"))]
dataset[, TXS_DIFF1 := TXS - TXS_LAG1]
dataset[, ':='(TXS_DIFF1_LAG = shift(TXS_DIFF1, 1, NA, "lag"))]
dataset[, TXS_DIFF2 := TXS_DIFF1 - TXS_DIFF1_LAG]

dataset[, .(FECHA,
            TXS,
            TXS_LAG1,
            TXS_DIFF1,
            TXS_DIFF1_LAG,
            TXS_DIFF2)]

mean <- mean(dataset$TXS, na.rm = T)
sd <- sd(dataset$TXS, na.rm = T)




ggplot(dataset, aes(FECHA, TXS) )+
  geom_line(size = 1, alpha = 1)+
  geom_hline(
    yintercept = c(mean, mean + 2*sd, mean - 2*sd,
                   mean + sd, mean - sd),
    linetype = 1,
    size = 0.5,
    alpha = 0.5,
    color = "blue"
  )+
  geom_vline(data = taxes[TAXE_DAY == 1],
             aes(xintercept = as.numeric(FECHA),
                 color = TAXE_TYPE
             )
  )


plot <- dataset[, .(FECHA, TXS_DIFF1)]
holiDays <- holidays[HOLIDAYS == 1, FECHA]
payDays <- paydays[PAYDAY == 1 , FECHA]
taxeDays <- taxes[TAXE_DAY == 1 , FECHA]

mean <- mean(plot$TXS_DIFF1, na.rm = T)
sd <- sd(plot$TXS_DIFF1, na.rm = T)




plot1 <- ggplot(plot, aes(FECHA, TXS_DIFF1) )+
  geom_line(size = 1, alpha = 1)+
  geom_hline(
    yintercept = c(mean, mean + 2*sd, mean - 2*sd,
                   mean + sd, mean - sd),
    linetype = 1,
    size = 0.5,
    alpha = 0.5,
    color = "blue"
  )+
  geom_vline(data = taxes[TAXE_DAY == 1],
             aes(xintercept = as.numeric(FECHA),
                 color = TAXE_TYPE
                 )
             )

  # geom_vline(
  #   xintercept = as.numeric(holidays[HOLIDAYS == 1, FECHA]),
  #   linetype = 1,
  #   size = 0.5,
  #   alpha = 0.5,
  #   color = HOLIDAY_TYPE
  # )
plot1 <- ggplotly(plot1)
plot1

plot2 <- ggplot(plot, aes(FECHA, TXS_DIFF1) )+
  geom_line(size = 1, alpha = 1)+
  geom_hline(
    yintercept = c(mean, mean + 2*sd, mean - 2*sd,
                   mean + sd, mean - sd),
    linetype = 1,
    size = 0.5,
    alpha = 0.5,
    color = "blue"
  )+
  geom_vline(data = holidays[HOLIDAYS == 1],
             aes(xintercept = as.numeric(FECHA),
                 color = HOLIDAY_TYPE
             )
  )


plot2 <- ggplotly(plot2)
plot2


