#' Funkcja tworzy wykres typu boxplot, porownanie wartosci wskaznikow dla roznych kandydatow
#'
#' Funkcja \code{tweety_generuj_boxplot} pobiera dane
#' z ramki danych dla wybranych kandydatow i tworzy wykresy typu boxplot
#'
#' @param frame ramka danych, w której zawarte sa wysokosci wskaznikow dla kandydatow :"Podsumowanie_nasluch_kandydatow.csv"
#' @param name wektor napisów, zawieraj¹cy nazwisko kandydata
#' @param begin poczatek analizy, format YYYY-MM-DD
#' @param end koniec analizy, format YYYY-MM-DD
#'
#'
#'@details
#' Imie i nazwisko \code{name_can} mozemy wybrac ze zbioru:
#' "Bronislaw Komorowski", "Andrzej Duda", "Magdalena Ogorek", "Pawel Kukiz"          
#' "Adam Jarubas", "Janusz Korwin Mikke",  "Janusz Palikot", "Marian Kowalski"      
#' "Wanda Nowicka"
#' 
#' Typ wykresu mozemy wybrac ze zbioru: "srednio retweetow", "suma retweetow", "srednio likeow",
#' "suma likeow", "liczba tweetow"
#' 
#' @examples
#' frame<-read.csv2("Podsumowanie_nasluch_kandydatow.csv")
#' tweety_generuj_boxplot(frame=frame,name=c("Bronislaw Komorowski","Andrzej Duda"),type="suma retweetow",begin="2015-04-01")
#'
#' @import dplyr
#' @import stringi
#' @import ggplot2
#'
#' @author Emilia Momotko


tweety_generuj_boxplot<- function(frame, name, type,begin="2014-01-01", end=as.character(Sys.Date())){
  
  #konwersja nazw i typow
  nazwy <- c("Komorowski", "AndrzejDuda", "JkmMikke", "ogorekmagda", "JarubasAdam", "PrezydentKukiz", "Palikot_Janusz",
             "M_Kowalski1", "WandaNowicka")
  names(nazwy) <- c("Bronislaw Komorowski", "Andrzej Duda", "Janusz Korwin Mikke",
                    "Magdalena Ogorek", "Adam Jarubas", "Pawel Kukiz","Janusz Palikot",
                    "Marian Kowalski", "Wanda Nowicka")
  
  action <- c("mean_retweet","sum_retweet","mean_like","sum_like","how_many_tweets")
  names(action) <- c("srednio retweetow", "suma retweetow", "srednio like","suma like","liczba tweetow")
  
  
  ktore <- which(names(nazwy)%in%name)
  name <- nazwy[ktore]
  
  ktora_akcja <- which(names(action)%in%type)
  type <- action[ktora_akcja]
  
  if(length(type)==0){
    print("Zly typ")
    return(invisible(NULL))
  }
  
  #liczba kandydatow analizowanych
  n_plots <- length(name)
  
  
  #tworzymy przedzial czasu, na ktorym bedziemy operowac:
  dates <- sort(unique(as.character(frame$date)))
  N <- length(dates)
  Sys.setlocale("LC_TIME", "Polish")
  
  #jesli uzytkownik nie podaje dat, to bierzemy caly przedzial
  
  #chociaz na wykresie bezposrednio nie widac dat, to biore pod uwage caly przedzial
  # i generuje ponizsze daty zeby brac pod uwage sytuacje kiedy nie bylo danych lub
  #kiedy nie bylo tweetow
  
  begin <- as.Date(begin)
  
  end <- as.Date(end)
  
  n_days <- as.numeric(end-begin)+1
  
  #generujemy CALY przedzial czasowy
  all_dates <- character(n_days)
  
  for(i in seq_along(all_dates)){
    
    all_dates[i] <- as.character(begin+i-1)
  }
  
  #tutaj bedziemy przechowywac wartosci, ktore przedstawimy na wykresie
  y <- numeric(n_days)
  
  #ale najpierw wybierzmy te z danych, ktore chce uzytkownik
  which_col <- which(colnames(frame)==type)
  
  frame[,which_col]<- as.numeric(as.character(frame[,which_col]))
  
  #tytul wykresu
  
  
  main_title <- stri_paste(names(type)," - dla wybranych kandydatow")
  
  frame_help <- data.frame()
  
  #generujemy ramke danych z z wartosciami
  for(j in 1:n_plots){
    
    exact_data <- frame %>%
      filter(lastname==name[j] & date %in% all_dates )
    
    
    
    #przypisujemy wartosci do wektora y
    for(i in seq_along(all_dates)){
      
      k <- which(exact_data$date==all_dates[i])
      if(length(k)>0&&!is.na(k)){
        y[i] <- exact_data[k,which_col]
      } else if(type=="how_many_tweets"){
        y[i] <- 0
      } else y[i] <- NA 
    }
    
    #tworzymy pomocnicza ramke danych,tutaj nie beda juz potrzebne daty, one byly potrzebne
    #do dokladnego zebrania i uporzadkowania danych
    dates_as_dates <- as.Date(all_dates)
    frame_help <- rbind(frame_help,data.frame(y,kandydat=rep(names(name)[j],length(y))))   
    
  }
 
  
  
  if(all(is.na(frame_help$y))){
    print("Niestety w tym okresie nie mamy info o nikim")
    return(invisible(NULL))
  }
  
  p <- ggplot(frame_help, aes(x=kandydat, y=y)) + xlab("")+
    ylab(type)+
    theme(panel.background = element_rect(colour = "black"))+
    theme(plot.title = element_text(lineheight=.8, face="bold",size=16))+
    labs(title = main_title)

  
  p + geom_boxplot()
     
}


