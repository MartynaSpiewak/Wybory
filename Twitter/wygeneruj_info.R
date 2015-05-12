#' Funkcja pomocnicza - wywolanie wielokrotne kandydaci(data)
#'
#' Funkcja \code{wygeneruj_info} wywoluje funkcje kandydaci(data) na wszystkich datach,
#' ktore sa podane w working directory, nie przyjmuje argumentow
#' 
#' @usage wygeneruj_info()
#' 
#' @return
#' invisible(NULL) - wartosci wskaznikow nie sa przechowywane w pamieci a zapisywane do pliku
#'
#'@author Emilia Momotko
#'
#'@import
#' stringi
#' dplyr
#' 
#'@examples
#' wygeneruj_info()
wygeneruj_info <- function(){
  
  f <- list.files(pattern="tweety_kandydatow")
  daty <- stri_extract_first_regex(f,"[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2}")
  
  sapply(daty,kandydaci)
  return(invisible(NULL))
  
}