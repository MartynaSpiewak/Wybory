#' wyznaczenie wartosci analizy sentymentu
#'
#' Funkcja \code{sentyment} dokonuje bezposrednio analizy sentymentu dla danychz z twittera
#' lub facebooka, wyznacza roznice meiszy iloscia slow pozytywnych i negatywnych
#' 
#' @usage sentyment(x)
#' @param x wektor, lista lub ramka danych z trescia twittera lub komentarzem z facebooka
#' 
#' @return wektor wartosci sentymentu dla poszczegolnych tweetow
#'
#'@author Emilia Momotko

#' 
#'@examples
#'x <- c("Ala ma kota", "A kot lubi Ale")
#' sentyment(x)

sentyment <- function(x){
  
  # wczytujemy slowniki z slownami pozytywnymi/negatywnymi
  pozytywne<-readLines("pozytywne.txt")
  negatywne<-readLines("negatywne.txt")
  # dla kazdego 
  sentyment_wartosc <- unlist(lapply(x, function(y){
    ile_pozytywne <- sum(y%in%pozytywne)
    ile_negatywne <- sum(y%in%negatywne)
    ile_pozytywne-ile_negatywne
  }))
  
  return(sentyment_wartosc)
}