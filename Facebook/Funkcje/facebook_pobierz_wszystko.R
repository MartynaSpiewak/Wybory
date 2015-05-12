#' Sciaganie informacji z facebook.com
#'
#' Funkcja \code{facebook_pobierz_wszystko} ma na celu sciagniecie informacji na temat 
#' postow, komentarzy pod postami (dane z ostatnich 7 dni) 
#' oraz liczby lajkow na temat wszystkich kandydatow na prezydenta - w dniu sciagania
#' 
#' @usage facebook_pobierz_wszystko(fb_oauth)
#' @param fb_oauth - sciezka do pliku w ktorym znajduje sie "klucz dostepu" do facebooka
#'
#' @return
#' funkcja zapisuje pliki z pobranymi danymi do odpowiednich plikow, 
#' dla kazdego kandydata oddzielnie
#'
#'@author Martyna Spiewak
#'
#'@import
#' stringi
#' Rfacebook

#'@examples
#' facebook_pobierz_wszystko()
#' 


facebook_pobierz_wszystko <- function(fb_oauth){
  #load(file.path("C:\\Users\\MARTYNKA\\Documents", "fb_oauth"))
  
  # podstawowe informacje na temat kandydatow,na podstawie ktorych sciagami dane
  df_read <- read.table("Facebook//kandydaci.csv", h = T)
  id_kandydant_all <- stri_extract_all_words(stri_replace_all_regex(df_read$id_pages, ",", " "))
  names(id_kandydant_all) <- df_read$names
  
  # posty i komentarze
  for(i in 1:ll) facebook_posty_komentarze_pobieranie(id.kandydant.all[i])
  # lajki
  for(i in 1:ll) facebook.likes_pobieranie(id.kandydant.all[i])
}
  
  
  
  