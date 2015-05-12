#' Wygenerowanie wartosci wskaznikow, pogrupowane po dniach
#'
#' Funkcja \code{kandydaci} pobiera pliki o nazwach  z wzorcem tweety_kandydatow z working directory
#' a nastepnie dokonuje wyznaczania wartosci wskaznikow kandydatow i zapisuje ich wartosci w pliku csv. Funkcja
#' nie przyjmuje zadnych argumentow.
#' 
#' @usage kandydaci(data)
#' @param data data, ktora nas interesuje i ktora bedziemy analizowac, w formacie jako napis
#' w formacie YYYY-MM-DD
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
#' kandydaci("2015-04-04")

kandydaci <- function(data){
  
  #tutaj bedziemy zapisywac info
  fname <- ("Podsumowanie_nasluch_kandydatow.csv")
  
  #jesli plik nie istnieje to tworzymy, jesli istnieje to bedziemy dopisywac
  if(!file.exists(fname)){
    f <- file(fname, open="a")
    #tworze pierwszy wiersz w pliku:
    writeLines(stri_paste('\"lastname\"','\"mean_retweet\"', '\"sum_retweet\"',
                          '\"mean_like\"', '\"sum_like\"', '\"how_many_tweets\"','\"date\"', sep = ";"), f)
    
  } else f <- file(fname, open="a")
  
    pliki <- list.files(pattern=stri_paste("tweety_kandydatow_",data))
    
    n <- length(pliki)
  
    if(n==0){
      
      close(f)
      return(invisible(NULL))
      
    }
    
    #usuwanie duplikatow - na wszelki wypadek
    for(j in seq_along(pliki)){
      
      a <- read.csv2(pliki[j])
      a <- tbl_df(a)
      duplikaty <- which(duplicated(a[,1], fromLast=TRUE))
      
      if(length(duplikaty)>0){
        
        a <- a[-which(duplicated(a[,1], fromLast=TRUE)),]
        
      }
      
      a <- unique(a)
      
      #wydobywamy date
      day=unlist(stri_extract_all_regex(a$created[1], "[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2}"))
      
      
      #PODSUMOWANIE
      podsumowanie_dni <- a%>%
        group_by(screenName)%>%
        summarise(srednio_retweetow=mean(retweetCount),
                  suma_retweetow=sum(retweetCount),
                  srednio_like=mean(favoriteCount),
                  suma_like=sum(favoriteCount),
                  liczba_tweetow=n())
      podsumowanie_dni <- as.data.frame(podsumowanie_dni)
      n <- nrow(podsumowanie_dni)
      if(n!=0){
        for(i in 1:n){
          #dopisuje do pliku kolejny wiersz      
          writeLines(stri_paste(podsumowanie_dni[i,1], podsumowanie_dni[i,2], podsumowanie_dni[i,3],
                                podsumowanie_dni[i,4], podsumowanie_dni[i,5],
                                podsumowanie_dni[i,6],day, sep=";"),f)
        }
      }
      
      
    }
  
  close(f)
  return(invisible(NULL))
}


