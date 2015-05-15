#' Podsumowanie dla każdego kandydata liczby lajków dla każdej ze stron
#'
#' Funkcja \code{facebook_like_dziennie} ma na celu podsumowanie dzienne liczby 
#' polubien kazdej ze stron dotyczacej danego kandydata na prezydenta
#' 
#' @usage facebook_like_dziennie(dir)
#' @param dir - sciezka do katalogu w ktorym znajduja sie pliki z pobieranymi 
#' informacjami
#'
#' @return
#' funkcja zapisuje plik w formacie csv, w ktorym dla kazdego kandydata znajduje sie 
#' podsumowanie dzienne liczby lajkow
#'
#'@author Martyna Spiewak
#'
#'@import
#' stringi
#' dplyr
#' tidyr

#'@examples
#' facebook_like_dziennie()


facebook_like_dziennie <- function(dir = "Facebook\\Likes\\"){
  
  # tableka informacyjna, w ktorej sa imiona i nazwiska wszystkich kandydatow i id
  df_read <- read.table("Facebook\\kandydaci.csv", h = T)
  
  kandydant_likes <- list.files(path = dir, pattern="_likes")
  
  for(name in kandydant_likes){
    table <- read.table(paste(dir, name, sep="/"), sep=";", head = TRUE) 
    
    kan <- tbl_df(table)
    kan <- kan[,-4]
    colnames(kan)[c(1,2)] <- c("id.page", "name.page")
    name_new <- name %>% stri_extract_all_regex(".+(?=_likes.csv)") %>%
      unlist %>%  stri_replace_all_regex("_", " ") 
    id_kan <- which(df_read$names == name_new)
    name_new <- stri_trans_totitle(name_new)
    kan <- cbind(data.frame(id = id_kan, names = name_new), kan)
    
    likes_per_day <- spread(kan, date, likes)
    
    # sprawdzamy czy istnieje katolog Podsumowanie
    if(!file.exists(file.path(paste0(dir, "Podsumowanie"))))
      # jesli nie, tworzymy takowy
      dir.create(file.path(paste0(dir, "Podsumowanie")))
    
    output <- file.path(paste0(dir, "Podsumowanie\\", name))
    write.table(likes_per_day, 
                file = output, sep = ";",
                row.names = FALSE, append = FALSE, na = "NA")
  }
}

