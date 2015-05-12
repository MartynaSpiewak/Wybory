#' Podsumowanie dla wszystkich kandydata komentarzy pojawiajacych sie pod postami
#'
#' Funkcja \code{sentyment_podsumowanie} ma na celu analize wyzdzieku komentarzy
#' pojawiajacych sie pod postami kandydatow na prezydenta
#' 
#' @usage facebook_sentyment_podsumowanie(dir)
#' @param dir - sciezka do katalogu w ktorym znajduja sie pliki z pobieranymi 
#' informacjami
#'
#' @return
#' funkcja zapisuje dopisuje do pliku z komentarzami kolumne w ktorej okresla czy dany 
#' komentarz ma wydźwiek pozytywny/negatywny/neutralny, dodatkowo do pliku z informacjami 
#' o postach opublikowanych na stronach dodaje kolumny 4 kolumny: liczba komentarzy pozytywnych/
#' negatywnych/neutralnych napisanych pod postem oraz srednia liczba polubien napisanych 
#' komentarzy
#'
#'@author Martyna Spiewak
#'
#'@import
#' stringi
#' dplyr
#' data.table
#' 
#' @example
#' dir <- "D:\\R\\Wybory\\Facebook\\Komentarze"
#' facebook_sentyment_podsumowanie(dir = dir)

facebook_sentyment_podsumowanie <- function(dir){
  require(dplyr)
  
  #dir <- "D:\\R\\Wybory\\Facebook\\Komentarze"
  
  # nazwa pliku wyjsciowego
  x <- "facebook_comments.csv"
  
  kandydant_comments <- list.files(path = dir, pattern="_comments")
  
  comments <- read.table(paste(dir, x, sep="\\"), sep=";", head = TRUE)
  # na wszelki wypadek sprawdzamy, czy nie ma powtarzajacych sie komentarzy
  comments <- unique(comments, by = comments.id)
  
  comments_msg <- comments$comments.message
  # oczyszcanie
  comments_msg <- oczyszczanie(comments_msg)
  # sprawdzanie sentymentu
  sentiment_msg <- sentyment(comments_msg)
  
  sentiment <- character(length(sentiment_msg))
  # okre?lamy czy komentarz ma wydzwiek:
  # neutralny - 0, pozytywny - P, negatywny - N
  sentiment[sentiment_msg %in% c(-1,0,1)] <- "0"
  sentiment[sentiment_msg > 1] <- "P"
  sentiment[sentiment_msg < -1] <- "N"
  
  comments %>%
    mutate(sentiment = sentiment) -> comments
  # zapisywanie z dodana kolumna 
  write.table(x = comments, file = paste(dir, "Podsumowanie", x, sep="\\"), append = FALSE)
  
  # zliaczenie komentarzy 0/N/P dla kazdego postu
  podsumowanie <- lapply(unique(comments$post.id), function(x){
    comments %>% 
      filter(comments$post.id == x) %>%
      summarise(neutral = sum(sentiment == "0"), 
                possitive = sum(sentiment == "P"),
                negative = sum(sentiment == "N"),
                comments_like_counts = mean(post.likes.count))
  })
  
  # dla kazdej strony dodajemy kolemne z zliczona liczba komentarzy o danym wydzwieku
  table_pods <- unique(comments[!duplicated(comments$post.id),c(1:4,7:9)])
  table_pods <- cbind(table_pods, do.call(rbind.data.frame, podsumowanie))
  table_pods <- arrange(table_pods, post.from_name)
  
  # zapisujemy do pliku 
  write.table(x = table_pods, file = paste(dir, "Podsumowanie", paste0("sentiment_", x), 
                                           sep="\\"), row.names = FALSE)
  
}

oczyszczanie <- function(tresci){
  require(stringi)
  
  #WYDOBYWAMY SLOWA I ZNAKI INTERPUNKCYJNE
  modified <- stri_extract_all_regex(tresci, "((\\w)|(\\s)|[:punct:])+")
  modified <- unlist(lapply(modified, stri_flatten," "))
  
  #teraz pozbywamy sie linkow - NIC NIE WNOSZA
  modified <- stri_replace_all_regex(modified, "http((\\w)|([:punct:]))+","")
  
  #teraz pozbywamy sie adresow e-mail - NIC NIE WNOSZA
  modified <- stri_replace_all_regex(modified, "((\\w)|([:punct:]))+(@|[0-9])((\\w)|([:punct:]))+","")
  
  #BEDZIEMY ZAMIENIAC ZNAKI POLSKIE NA ZWYKLE  - EFEKTYWNIEJSZA ANALIZA
  znaki_polskie <- c("Ä…", "Ä‡", "Ä™", "Ĺ‚", "Ĺ„", "o", "Ĺ›", "ĹĽ", "Ĺş")
  znaki_zwykle <- c("a", "c", "e", "l", "n", "o", "s", "z", "z")
  
  for(i in 1:length(znaki_polskie)){
    modified <- stri_replace_all_fixed(modified, znaki_polskie[i], znaki_zwykle[i]) 
  }
  
  #MALE LITERY I WYDOBYWAMY SLOWA
  modified <- stri_trans_tolower(modified)
  modified <- stri_extract_all_words(modified)
  
  return(modified)
}

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
