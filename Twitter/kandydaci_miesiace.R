#' Wygenerowanie wartosci wskaznikow, pogrupowane po miesiacach
#'
#' Funkcja \code{kandydaci_miesiace} pobiera pliki o nazwach  z wzorcem tweety_kandydatow z working directory
#' a nastepnie dokonuje wyznaczania wartosci wskaznikow kandydatow i zapisuje ich wartosci w pliku csv. Funkcja
#' nie przyjmuje zadnych argumentow.
#' 
#' @usage kandydaci_miesiace()
#' 
#' @return
#' invisible(NULL) - wartosci wskaznikow nie za przechowywane w pamieci a zapisywane do pliku
#'
#'@author Emilia Momotko
#'
#'@import
#' stringi
#' dplyr
#' 
#'@examples
#' kandydaci_miesiace()

kandydaci_miesiace <- function(){
  
  #nazwa - tutaj bedziemy zapisywac wskazniki
  fname <- ("Podsumowanie_nasluch_kandydatow_miesiace.csv")
  
  f <- file(fname, open="w")
  
  #wybieramy te pliki ktore nas interesuja
  pliki <- list.files(pattern=stri_paste("tweety_kandydatow_"))
  
  n <- length(pliki)
  
  if(n==0){
    
    close(f)
    return(invisible(NULL))
    
  }
  
  #tworzymy ramke danych do ktorej wrzucimy cale nasze zebrane dane - nie jest ich duzo, wiec nei bedzie
  #peobmlemow z pamiecia
  a<-data.frame();
  for(j in seq_along(pliki)){
    b <- read.csv2(pliki[j],row.names=NULL)
    if(ncol(b)==18) b<-b[,-1]
    a <- rbind(a,b)
  }
  
  #usuwanie duplikatow na wszelki wypadek
  a <- tbl_df(a)
  duplikaty <- which(duplicated(a[,1], fromLast=TRUE))
  
  if(length(duplikaty)>0){
    
    a <- a[-which(duplicated(a[,1], fromLast=TRUE)),]
    
  }
  
  a <- unique(a)
  
  #wydobywame date i miesiac
  month <- stri_extract_first_regex(a$created,"[0-9]{4,4}-[0-9]{2,2}(?=-)")
  
  a <- a%>%
    mutate(month=month)
  
  #PODSUMOWANIE
  podsumowanie_dni <- a%>%
    group_by(screenName,month)%>%
    summarise(srednio_retweetow=mean(retweetCount),
              suma_retweetow=sum(retweetCount),
              srednio_like=mean(favoriteCount),
              suma_like=sum(favoriteCount),
              liczba_tweetow=n())
  podsumowanie_dni <- as.data.frame(podsumowanie_dni)
  n <- nrow(podsumowanie_dni)
  
  #zapisywanie
  if(n!=0){
    write.csv2(podsumowanie_dni,file=f,row.names = FALSE)
  }
  
  close(f)  
  return(invisible(NULL))
}

