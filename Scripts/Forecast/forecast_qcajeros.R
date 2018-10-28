
## Forecasting main ##

forecast_qcajeros <- function(){

  
  #Staging
  staging <- staging_maker()
  setnames(staging, "fecha", "FECHA")
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
  Results <- list()
  saver <- c(seq(0, length(offices), 50), length(offices))
  
  set.seed(1) 
  
  # create progress bar
  pb <- tkProgressBar(title = "progress bar", min = 0,
                      max = total, width = 300)
  # Start the clock!
  tic("Q_Cajeros")
  
  #running for each office nature
  for(j in 1:total){
    Results[[j]] <-
      
      Q_cajeros(office = j , do.test, staging,train_cut,
                test_cut,last_test_date, last_forecast_date,
                holidays, paydays, xreg_vector, offices, j, 
                type = "depositos" )
    
    setTkProgressBar(pb, j, label=paste( round(j/total*100, 0),
                                         "% done"))
    gc()
    if(j %in% saver|j ==1 |j == 5 | j == 10){
      save(Results, 
           file = os.path.join(forecastFolder,paste0(j,"offices.RDta")))
      Results <- list() }
  }
  
  toc()
}

