###########################################
### funkcja: sentyment()
### argumenty wejsciowe: 
### x - wektor, lista lub ramka danych z 
### trescia twittera lub komentarzem z facebooka
### opis:
### funkcja wraca dla kazdej wzmianki roznice
### miedzy liczba slow pozytywnych a liczba 
### slow negatywnych w kazdym wpisie z twittera 
### lub facebooka
###########################################

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