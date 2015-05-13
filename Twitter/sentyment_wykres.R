#' Funkcja tworzy wykres przedstawiaj¹cy zmianê sentymentu kandydatów na prezydenta w czasie.
#'
#' Funkcja \code{sentyment_wykres(frame, name,begin="2015-03-16", end=Sys.Date(),thickness=1)} pobiera dane
#' z ramki danych dla wybranych kandydatów i w podanym przedziale czasowym tworzy wykres zmiany
#' sentymentu w czasie
#'
#'
#' @param frame ramka danych, w której zawarty jest wskaŸnik sentymentu dla kandydatów
#' @param name wektor napisów, zawieraj¹cy nazwiska kandydatów
#' @param begin poczatek analizy, format YYYY-MM-DD
#' @param end koniec analizy, format YYYY-MM-DD
#' @param thickness grubisc linii
#'
#'
#'@details
#' Imie i nazwisko \code{name_can} mozemy wybrac ze zbioru:
#' "Bronislaw Komorowski", "Andrzej Duda", "Magdalena Ogorek", "Pawel Kukiz"          
#' "Adam Jarubas", "Janusz Korwin Mikke",  "Janusz Palikot", "Marian Kowalski"      
#' "Jacek Wilk", "Grzegorz Braun", "Pawel Tanajno"
#' 
#' @examples
#' sentyment_wykres(read.csv2("Podsumowanie_tweetow.csv"),c("Bronislaw Komorowski","Andrzej Duda","Magdalena Ogorek"))
#' sentyment_wykres(read.csv2("Podsumowanie_tweetow.csv"),name=c("Bronislaw Komorowski","Andrzej Duda","Magdalena Ogorek"), begin="2015-03-01", end="2015-05-01")
#'
#' @import dplyr
#' @import stringi
#' @import ggplot2
#' @import scales
#'
#' @author Emilia Momotko


sentyment_wykres <- function(frame, name,begin="2015-03-16", end=Sys.Date(),thickness=1){
  
  #konwersja nazw
  nazwy <- c("komorowski", "duda", "korwin", "ogorek", "jarubas", "kukiz", "palikot",
             "wilk","braun", "kowalski", "tanajno")
  names(nazwy) <- c("Bronislaw Komorowski", "Andrzej Duda", "Janusz Korwin Mikke",
                    "Magdalena Ogorek", "Adam Jarubas", "Pawel Kukiz","Janusz Palikot",
                    "Jacek Wilk", "Grzegorz Braun", "Marian Kowalski", "Pawel Tanajno")
  
  
  ktore <- which(names(nazwy)%in%name)
  name <- nazwy[ktore]
  
  n_plots <- length(name)
  
  begin <- as.Date(begin)
  if(class(end)!="Date"){
    end <- as.Date(end)
  }
  
  
  main_title <- "Analiza sentymentu dla wybranych kandydatów"
  n_days <- as.numeric(end-begin)+1
  
  #generujemy CALY przedzial czasowy
  all_dates <- character(n_days)
  
  for(i in seq_along(all_dates)){
    
    all_dates[i] <- as.character(begin+i-1)
  }
  
  #geeruemy ramke danych z wartosciami, potrzebne jak np nei bylo ktoregos dnia info,
  #dane nie sa na tyle duze zebysmy mueisli to zapisywac do pliku a nie generowac teraz
  frame_help <- data.frame()
  
  for(j in 1:n_plots){
    
    y <- numeric(n_days)
    
    exact_data <- frame %>%
      filter(lastname==name[j] & date %in% all_dates )
    
    for(i in seq_along(all_dates)){
      
      k <- which(exact_data$date==all_dates[i])
      if(length(k)>0&&!is.na(k)){
        y[i] <- exact_data[k,3]
        
      } else y[i] <- NA 
    }
    
    dates_as_dates <- as.Date(all_dates)
    frame_help <- rbind(frame_help,data.frame(y, dates_as_dates,kandydat=rep(names(name)[j],length(y))))
    
  }
  
  if(all(is.na(frame_help$y))){
    print("Niestety w tym okresie nie mamy info o nikim")
    return(invisible(NULL))
  }
  
  p <- ggplot(frame_help, aes(x=dates_as_dates, y=y, colour=kandydat)) + xlab("")+
    ylab("sentiment")+xlab("time")+scale_x_date(labels=date_format("%b-%Y"))+
    theme(panel.background = element_rect(colour = "black"))+
    theme(plot.title = element_text(colour = "red"))+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=16,face="bold"))+
    theme(plot.title = element_text(lineheight=.8, face="bold",size=20))+
    geom_line(size=thickness)+ geom_point() +scale_fill_brewer(palette="Spectral")+
    ggtitle(main_title)+
    theme(plot.title = element_text(lineheight=.8, face="bold"))
    
  p
  
}



