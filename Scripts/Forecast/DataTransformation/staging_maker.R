# update staging table
#' @return : staging table
staging_maker <- function(){
  # Upload staging field

  stagingFile_path <- get.path(staging_path,"data" )
  staging <- fread(stagingFile_path, colClasses = "character")
  
  if(sum(colSums(is.na(staging))) > 0 ){
    stop("Staging or raw has NA observations")
  }
  
  staging[, fecha := gsub("/", "-", fecha ) ]
  staging[, fecha := as.Date(fecha, "%Y-%m-%d")]
  
  # filters 
  files_meta <- list.files(meta_path)
  exclude_offices <- grep("exclude", files_meta, value = TRUE)
  exclude_offices <- os.path.join(meta_path, exclude_offices)
  exclude_offices <- fread(exclude_offices)  
  exclude_offices <- exclude_offices$codigoOficina %>% unique
  
  current_offices <- staging[, .(LAST_DATE = range(fecha)[2]),
                             by = codigoOficina][LAST_DATE > current_offices_date, codigoOficina]
  hist_offices <- staging[, .(LAST_DATE = range(fecha)[1]),
                          by = codigoOficina][LAST_DATE < hist_offices_date, codigoOficina]
  
  staging <- staging[!(codigoOficina %in% exclude_offices) ]
  staging <- staging[codigoOficina %in% current_offices  & 
                       codigoOficina %in% hist_offices]
  staging <- staging[ciudad == "BOGOTA"]
  fwrite(staging,
         os.path.join(staging_path, paste0("staging_" , current_month, ".csv")),
         row.names = F)
  return(staging)
}
