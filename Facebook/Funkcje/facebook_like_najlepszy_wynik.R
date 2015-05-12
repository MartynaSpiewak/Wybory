#'  Funkcja \code{facebook_like_najlepszy_wynik} dla wszystkich z kandydatow zwraca 
#'  najwyzsza odnotowana liczbe polubien ze wszystkich dostepnych stron na facebook.com
#' 
#' @usage facebook_like_najlepszy_wynik (when=Sys.Date())
#' @param when - dzien, dla ktorego porownujemy liczbe lajkow dla kazdego kandydata
#'
#' @return
#' funkcja zwraca ramke danych: pierwsza kolumna to imię i nazwisko kandydata, druga to
#' najwyzsza liczba polubien danego dnia
#' 
#' @details
#' Na podstawie zwracanej ramki danych mozemy narysowac szereg czasowy:
#' res <- facebook_like_najlepszy_wynik(as.Date("2015-05-01"))
#' ggplot(res, aes(x = factor(name), y = likes)) + geom_bar(stat = "identity") 
#'
#'@author Martyna Spiewak
#'
#'@import
#' stringi
#' dplyr

#'@examples
#' facebook_like_najlepszy_wynik("2015-05-10")

facebook_like_najlepszy_wynik <- function(when=Sys.Date()){
  
  dir <- "Facebook\\Likes\\Podsumowanie\\Podsumowanie.csv"
  df <- read.table(dir, sep=";", header = TRUE)
  
  # zmiana formatu daty
  colnames(df)[5:ncol(df)] <- colnames(df)[5:ncol(df)] %>% 
    stri_extract_all_regex("(?<=X).+") %>%
    unlist %>% stri_replace_all_regex("\\.", "-")
  
  # liczba lajkow danego dnia dla kazdego kandydata, 
  # liczba jest pobierana ze strony, ktora na dany dzien ma 
  # najwiecej takich polubien
  when <- as.character(when)
  
  names <- unique(df$names)
  # czasami zdarzaja sie braki danych, w taki przypadkach zwracamy dane na pierwszy 
  # dzien przez podanym w funkcji, dla ktorych dane sa pobrane
  res <- numeric(length(names))
  res <- sapply(1:length(names), function(x){
    df_new <- df[df$names == names[x],]
    tmp <- which(colnames(df) == when)
    if(length(tmp) == 0){
      while(length(tmp) == 0 ){
        when <- as.Date(when)-1
        when <- as.character(when)
        tmp <- which(colnames(df) == when)
      }
      if(when == '2015-01-01') res[x] <- 0
      else{
        if(all(is.na(df_new[, which(colnames(df) == when)]))) res[x] <- 0
        else res[x] <- max(df_new[, which(colnames(df) == when)], na.rm = TRUE)
      }
    }
    else {
      if(all(is.na(df_new[, which(colnames(df) == when)]))) res[x] <- 0
      else res[x] <- max(df_new[, which(colnames(df) == when)], na.rm = TRUE)
    }
  })
  res <- data.frame(name = names, likes = res)
  return(res) 
}