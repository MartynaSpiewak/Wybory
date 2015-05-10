#' Chmura slow z postow ze stron kandydatow
#'
#' Funkcja \code{facebook_wordcloud} zwraca wektor nazwany, ktorego warosciami 
#' jest liczba powtorzen danego slowa w wszystkich postach opublikowanych na 
#' stronach kandydatow na facebook.com
#' 
#' @usage facebook_wordcloud(name_can)
#' @param name_can - imie i nazwisko kandydata 
#' @param from - data, od ktorej rozpoczynamy analize
#' @param to - data, do której prowadzimy analize
#'
#' @details
#' Imię i nazwisko \code{name_can} mozemy wybrać ze zbioru:
#' "Bronislaw Komorowski", "Andrzej Duda", "Magdalena Ogorek", "Pawel Kukiz"          
#' "Adam Jarubas", "Janusz Korwin Mikke",  "Janusz Palikot", "Marian Kowalski"      
#' "Jacek Wilk", "Grzegorz Braun", "Pawel Tanajno"
#' 
#' @return
#' wektor nazwany, na bazie, którego możemy narysować mapę słóW
#'
#'@details
#' wc <- facebook_wordcloud("Janusz Korwin Mikke")
# 
# #wordcloud
#' wordcloud(names(wc), wc, scale=c(4,0.5), random.order=FALSE,
#'         min.freq = 1, max.words=500,
#'         colors=brewer.pal(7, "Dark2"))
#'
#'@author Martyna Spiewak
#'
#'@import
#' stringi
#' dplyr
#' wordcloud
#' RColorBrewer
#' 
#'@examples
#' facebook_wordcloud("Bronislaw Komorowski")

facebook_aktualizacja <- function(){
  
  facebook()
  facebook_like_dziennie()
  facebook_like_podsumuj()
  
}