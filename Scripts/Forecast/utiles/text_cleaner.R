text_cleaner <- function(text_vector){
  unwanted_array = list(    'S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 
                            'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 
                            'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E', 'Ê'='E',
                            'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 
                            'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O',
                            'Ö'='O', 'Ø'='O', 'Ù'='U', 'Ú'='U', 'Û'='U',
                            'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss','à'='a',
                            'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 
                            'æ'='a', 'ç'='c', 'è'='e', 'é'='e', 'ê'='e',
                            'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i',
                            'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o',
                            'õ'='o', 'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u',
                            'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
  text_vector <- iconv(text_vector, to='ASCII//TRANSLIT')
  text_vector <- tolower(text_vector)
  text_vector <- removePunctuation(text_vector)
#  text_vector <- gsub("[^A-Za-z]", " ", text_vector)
  text_vector <-  gsub(" ", "", text_vector) 
  return(text_vector)}