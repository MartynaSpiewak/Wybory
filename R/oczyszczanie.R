###################################################
### funkcja: oczyszczanie()
### argumenty wejsciowe: 
### tresci - wektor, lista lub ramka danych z 
### trescia twittera lub komentarzem z facebooka
### opis:
### funkcja, ktora m.in. czysci tresci z znakow interpukcyjnych, 
### linkow, e-maili oraz pozbywa sie polskich liter
#####################################################

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