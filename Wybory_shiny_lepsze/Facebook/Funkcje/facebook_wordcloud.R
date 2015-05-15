#' Chmura slow z postow ze stron kandydatow
#'
#' Funkcja \code{facebook_wordcloud} zwraca wektor nazwany, ktorego warosciami 
#' jest liczba powtorzen danego slowa w wszystkich postach opublikowanych na 
#' stronach kandydatow na facebook.com
#' 
#' @usage facebook_wordcloud(name_can, from = as.Date('2015-01-01'), to = Sys.Date()){
#' @param name_can - imie i nazwisko kandydata 
#' @param from - data, od ktorej rozpoczynamy analize
#' @param to - data, do której prowadzimy analize
#' @param frame_posts - ramka wejsciowa ze wszystkimi postami ukazanymi sie na publicznych 
#' profilach kandydatow od 2015-01-01
#' @param can - ramka danych z podstawowymi informacjami o kandydatach - id, imie, nazwisko
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
#' facebook_wordcloud("Bronislaw Komorowski", frame_posts = frame_posts, can = can)

facebook_wordcloud <- function(name_can, from=as.Date("2015-01-01"), to=Sys.Date(),
                                frame_posts, can){
  
#   # wczytujemy ramke z wszystkimi postami opublikowanymi od daty '2015-01-01':
#   kandydaci <- read.table("Facebook\\facebook_posts.csv", sep =";", h = T)
#   # tableka inforamacyjna, w ktorej sa imiona i nazwiska wszystkich kandydatow i id
#   df_read <- read.table("Facebook\\kandydaci.csv", h = T)
  
  kan <- stri_trans_totitle(can$names)
  
  # id wybranego/ych kandydata/ow
  id <- can$id[which(kan %in% name_can)]
  # ograniczamy sie do rekordow o danych kandydacie
  one <- frame_posts[frame_posts$id == id,]
  # zmiana formatu daty
  one$created_time <- stri_extract_first_regex(one$created_time, ".{10}")
  # posty z danego odcinka czasowego
  one %>% 
    filter(as.Date(one$created_time) >= from, as.Date(one$created_time) <=to) -> posts
  # posty
  messages <- posts$message
  # oczyszczanie
  extract_messages <- stri_extract_all_words(messages)
  extract_messages <- oczyszczanie(unlist(extract_messages))
  extract_messages <- unlist(extract_messages)
  
  # wczytujemy ramke ze stopwordsami
  stopwords <- read.table("stopwords.txt", encoding = "UTF-8")
  stopwords<- unlist(sapply(stopwords, oczyszczanie))
  # usuwanie stopwordsow
  tmp <- extract_messages %in% stopwords | is.na(extract_messages)
  extract_messages <- extract_messages[!tmp] 
  
  top500<-sort(table(extract_messages), decreasing = TRUE)[1:500]
  return(top500)
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
  znaki_polskie <- c("ą", "ć", "ę", "ł", "ń", "o", "ś", "ż", "ź")
  znaki_zwykle <- c("a", "c", "e", "l", "n", "o", "s", "z", "z")
  
  for(i in 1:length(znaki_polskie)){
    modified <- stri_replace_all_fixed(modified, znaki_polskie[i], znaki_zwykle[i]) 
  }
  
  #MALE LITERY I WYDOBYWAMY SLOWA
  modified <- stri_trans_tolower(modified)
  modified <- stri_extract_all_words(modified)
  
  return(modified)
}

# Przykład dla Emilki: 
# wc <- facebook_wordcloud("Janusz Korwin Mikke", frame_posts = frame_posts, can = can)
# wc <- facebook_wordcloud("Bronislaw Komorowski", frame_posts = frame_posts, can = can)
# wc <- facebook_wordcloud("Pawel Kukiz", frame_posts = frame_posts, can = can)
# wc <- facebook_wordcloud("Magdalena Ogorek", frame_posts = frame_posts, can = can)
# wc <- facebook_wordcloud("Andrzej Duda", frame_posts = frame_posts, can = can)

# 
#   #wordcloud
#   wordcloud(names(top500),top500,c(8,.3),2,,FALSE,TRUE,.15,pal)
#   wordcloud(names(wc), wc, scale=c(4,0.5), random.order=FALSE,
#               min.freq = 1, max.words=500,
#               colors=brewer.pal(7, "Dark2"))

