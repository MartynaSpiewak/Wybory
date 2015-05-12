#' Podsumowanie dla oficjalnej strony kandydata roznice liczby lajków
#'
#' Funkcja \code{facebook_like_roznice_w_czasie} podsumowuje roznice liczby lajkow 
#' dla oficjalne/glownej strony kandydata na prezydenta
#' miedzy dwoma kolejnymi dniami
#' 
#' @usage facebook_like_roznice_w_czasie(name.id, from = as.Date("2015-01-01"), to = Sys.Date())
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
#' funkcja zwraca ramke danych: pierwsza kolumna to data, druga to roznica liczby lajkow
#' odnotowana miedzy kolejnymi dniami dla oficjalnej strony kandydata
#' 
#' @details
#' Na podstawie zwracanej ramki danych mozemy narysowac szereg czasowy:
#'  tmp <- facebook_like_roznice_w_czasie("Bronislaw Komorowski")
#'  ggplot(tmp, aes(data, likes)) + geom_line() +
#'  xlab("") + ylab("Counts of Likes") + 
#'  theme(panel.background = element_rect(colour = "white"))
#'
#'@author Martyna Spiewak
#'
#'@import
#' stringi
#' dplyr

#'@examples
#' facebook_like_roznice_w_czasie("Bronislaw Komorowski")

facebook_like_roznice_w_czasie <- function(name.id, from = as.Date("2015-01-01"), to = Sys.Date()){
  
  dir <- "Facebook\\Likes\\Podsumowanie\\Podsumowanie.csv"
  df <- read.table(dir, sep=";", header = TRUE)
  # dane tylko dla wybranego kandydata
  df_new <- df[df$names == name.id,]
  #usuwamy duplikaty
  dup1 <- which(duplicated(df_new$id.page))
  
  if(length(dup1) > 0){
    dup2 <- which(df_new$id.page[dup1] == df_new$id.page)
    df_new[dup2[1], 5:ncol(df_new)] <- apply(df_new[dup2, 5:ncol(df_new)], 2, sum, na.rm = TRUE)
  }
  
  MAX <- max(df_new[, ncol(df)], na.rm = TRUE)
  df_new <- df_new[which(df_new[, ncol(df)] == MAX),]
  name_page <- as.character(df_new$name.page)
  
  colnames(df_new)[5:ncol(df_new)] <- colnames(df)[5:ncol(df_new)] %>% 
    stri_extract_all_regex("(?<=X).+") %>%
    unlist %>% stri_replace_all_regex("\\.", "-")
  
  # roznicujemy
  data <- c(0, as.numeric(df_new[, 5:ncol(df_new)]) %>% diff)
  # ramka wyjsciowa
  df_res <- data.table(data = colnames(df_new)[5:ncol(df_new)] %>% as.Date(), 
                       diff_likes = data)
  # ograniczamy sie do podanego odcinka czasowego
  df_res <- df_res[df_res$data >= from & df_res$data <= to, ]
  return(df_res)
}

