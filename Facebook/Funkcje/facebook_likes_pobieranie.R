#' Sciaganie liczby polubien publicznych stron z facebook.com
#'
#' Funkcja \code{facebook_likes_pobieranie} ma na celu zebranie liczby polubien publicznych stron
#' kandydatow na prezydenta, funkcja przygotowana do uzytku codziennego,
#' dopisuje do pliku csv liczbe polubien dla dzien w ktorym zostala uzyta
#' 
#' @usage facebook_likes_pobieranie(id.kandydata.all)
#' @param id.kandydant.all - numery id do publicznych stron na temat wybranego kandydata
#'
#' @return
#' funkcja zapisuje plik w formacie csv, w ktorym dla kazdego kandydata znajduje sie 
#' podsumowanie dzienne liczby lajkow
#'
#'@author Martyna Spiewak
#'
#'@import
#' stringi
#' Rfacebook
#'
#'@examples
#' facebook_likes_pobieranie(list("pawel tanajno" = 
#' c("469702339755768", "785096541556003", "390816184436123")))
#' 


facebook_likes <- function(id.kandydant.all){
  
  likes<-data.frame()
  id.kandydant<-id.kandydant.all[[1]]
  kandydant.name<-names(id.kandydant.all)
  
  likes<-getUsers(users = id.kandydant, token=fb_oauth)
  likes<-likes[, c(1, 2, 9, 10)]
  
  #dodajemy kolumnę z datą
  likes<-cbind(likes, date=Sys.Date())
  
  #zapisywanie do pliku: kandydant.name_likes
  n<-nrow(likes) #liczba stron dla kandydata (wierszy)
  if(n!=0){
    sciezka<-paste0(sciezka,"Likes/")
    dir.create(sciezka,showWarnings=FALSE)
    fname<-paste0(sciezka, kandydant.name, "_likes.csv")
    if(file.exists(fname)){
      f<-file(fname, open="a")
    }
    else{
      f<-file(fname, open="a")
      #header gdy tworzymy plik:
      writeLines('\"id\";\"name\";\"likes\";\"picture\";\"date\"', f)
    }
    
    for(i in 1:n){
      #dopisujemy do pliku kolejny wiersz
      data<-as.character(likes[i,5])
      wiersz<-paste0(paste0('"',likes[i,-5],collapse='";'),'";"',data,'"')
      writeLines(wiersz,f)
    }
    close(f)
  }
  
}