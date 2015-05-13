#' Funkcja zlicza ilosc miesiecy pomiedzy konkretnymi datami - funkcja pomocnicza
#'
#' Funkcja \code{ile_miesiecy}  zlicza ilosc miesiecy pomiedzy konkretnymi datami
#'
#' @param begin poczatek analizy, napis w formacie YYYY-MM-DD
#' @param end koniec analizy, napis w formacie YYYY-MM-DD
#'
#' 
#' @examples
#' 
#' ile_miesiecy("2014-01-01", "2015-02-04")
#' 
#' @import stringi
#'
#' @author Emilia Momotko


ile_miesiecy <- function(begin, end){
  
  
  y1 <- as.numeric(stri_extract_first_regex(begin,"[0-9]{4}"))
  y2 <- as.numeric(stri_extract_first_regex(end,"[0-9]{4}"))
  
  m1 <- as.numeric(stri_extract_first_regex(begin,"(?<=-)[0-9]{2}(?=-)"))
  m2 <- as.numeric(stri_extract_first_regex(end,"(?<=-)[0-9]{2}(?=-)"))
  
  if(y1-y2==0){
    
    n <- m2-m1+1
    
  } else{
    
    n<- 12-m1+1+(y2-y1-1)*12+ m2
  }
  n
  
}

