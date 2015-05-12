#' Podsumowanie dla wszystkich kandydata liczby lajków dla każdej ze stron
#'
#' Funkcja \code{facebook_like_podsumuj} ma na celu podsumowanie dzienne liczby 
#' polubien dla kazdego kandydata na prezydenta w jednym pliku .csv
#' 
#' @usage facebook_like_podsumuj(dir)
#' @param dir - sciezka do katalogu w ktorym znajduja sie pliki z pobieranymi 
#' informacjami
#'
#' @return
#' funkcja zapisuje plik w formacie csv, w ktorym dla wszystkich kandydatow znajduje sie 
#' podsumowanie dzienne liczby lajkow
#'
#'@author Martyna Spiewak
#'
#'@import
#' stringi
#' dplyr
#' data.table

#'@examples
#' facebook_like_podsumuj()

facebook_like_podsumuj <- function(dir = "Facebook\\Likes\\Podsumowanie\\"){
  
  # wczytujemy ramki z danymi dla wszystkich kandydatow
  kandydant_likes <- list.files(path = dir, pattern="_likes")
  res <- data.frame()
  for(name in kandydant_likes){
    table <- read.table(paste(dir, name, sep="/"), sep = ";", 
                        head = TRUE, colClasses = "character")
    colnames(table)[5:ncol(table)] <- colnames(table)[5:ncol(table)] %>% 
      stri_extract_all_regex("(?<=X).+") %>%
      unlist %>% stri_replace_all_regex("\\.", "-")
    res <- rbindlist(list(res, table), use.names=TRUE, fill=TRUE)
  }
  write.table(res, 
              file = paste0(dir, "Podsumowanie.csv"), sep = ";",
              row.names = FALSE, append = FALSE, na = "NA")
}