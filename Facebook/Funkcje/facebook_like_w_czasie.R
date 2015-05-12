#' Podsumowanie dla oficjalnej strony kandydata liczby lajków
#'
#' Funkcja \code{facebook_like_w_czasie} podsumowuje liczbe lajkow dla oficjalne/glownej
#' strony kandydata na prezydenta
#' 
#' @usage facebook_like_w_czasie(name.id, from = as.Date("2015-01-01"), to = Sys.Date(), 
#'                                frame_likes = frame_likes, can = can)
#' @param name_can - imie i nazwisko kandydata 
#' @param from - data, od ktorej rozpoczynamy analize
#' @param to - data, do której prowadzimy analize
#' @param frame_likes - ramka wejsciowa z liczba lajkow danego dnia dla wszystkich stron 
#' na temat kandydatow
#' @param can - ramka danych z podstawowymi informacjami o kandydatach - id, imie, nazwisko
#'
#' @details
#' Imię i nazwisko \code{name_can} mozemy wybrać ze zbioru:
#' "Bronislaw Komorowski", "Andrzej Duda", "Magdalena Ogorek", "Pawel Kukiz"          
#' "Adam Jarubas", "Janusz Korwin Mikke",  "Janusz Palikot", "Marian Kowalski"      
#' "Jacek Wilk", "Grzegorz Braun", "Pawel Tanajno"
#'
#' @return
#' funkcja zwraca ramke danych: pierwsza kolumna to data, druga to liczba lajkow
#' odnotowana w danym dniu dla oficjalnej strony kandydata
#' 
#' @details
#' Na podstawie zwracanej ramki danych mozemy narysowac szereg czasowy:
#' tmp <- facebook_like_w_czasie(c("Bronislaw Komorowski", "Andrzej Duda", "Pawel Kukiz"), 
#'                               frame_likes = frame_likes, can = can)
#' ggplot(tmp, aes(x=data, y=likes, colour=name)) +
#'   geom_line(size = 1) + theme(legend.position="bottom") + 
#'   ggtitle("Count of likes per day")
#'
#'@author Martyna Spiewak
#'
#'@import
#' stringi
#' dplyr

#'@examples
#' facebook_like_w_czasie(c("Bronislaw Komorowski", "Andrzej Duda", "Pawel Kukiz"), 
#'                          frame_likes = frame_likes, can = can)
#' należy wczytać ramke danych z podsumowanie liczby lajkow, np:
#'  dir <- "Facebook\\Likes\\Podsumowanie\\Podsumowanie.csv"
#   frame_likes <- read.table(dir, sep=";", header = TRUE)

facebook_like_w_czasie <- function(names_can, 
                                   from = as.Date("2015-01-01"), to = Sys.Date(),
                                   frame_likes, can){
  
  
  kan <- stri_trans_totitle(can$names)
  id <- can$id[which(kan %in% names_can)]
  
  # ramka wyjsciowa
  res <- data.frame()
  
  for(i in seq_along(id)){
    # dane tylko dla wybranych kandydata
    df_new <- frame_likes[frame_likes$names == names_can[i],]
    
    # usuwamy duplikaty
    dup1 <- which(duplicated(df_new$id.page))
    
    if(length(dup1) > 0){
      dup2 <- which(df_new$id.page[dup1] == df_new$id.page)
      df_new[dup2[1], 5:ncol(df_new)] <- apply(df_new[dup2, 5:ncol(df_new)], 2, sum, na.rm = TRUE)
    }
    
    # niektore strony przez kilka miesiecy zmienialy swoje nazwy, w miare mozliwosci
    # laczymy wyniki dla tych, ktore nie zmienily id strony
    MAX <- max(df_new[, ncol(df)], na.rm = TRUE)
    df_new <- df_new[which(df_new[, ncol(df)] == MAX),]
    name_page <- as.character(df_new$name.page)
    
    #zmieniamy format daty
    colnames(df_new)[5:ncol(df_new)] <- colnames(df)[5:ncol(df_new)] %>% 
      stri_extract_all_regex("(?<=X).+") %>%
      unlist %>% stri_replace_all_regex("\\.", "-")
    # ramka wyjsciowa
    df_res <- data.table( id = i, name = kan[id[i]],
                          data = colnames(df_new)[5:ncol(df_new)] %>% as.Date(), 
                          likes = as.numeric(df_new[, 5:ncol(df_new)]))
    # ograniczamy sie do podanego odcinka czasowego
    df_res <- df_res[df_res$data >= from & df_res$data <= to, ]
    
    res <- rbind(res, df_res)
  }
  return(res)
}
  
