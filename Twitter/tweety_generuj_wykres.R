#' Funkcja tworzy wykres przedstawiaj¹cy zmianê pewnych wartosci wskaznikow na przestrzeni czasu dla okreslonych 
#' kandydatow
#'
#' Funkcja \code{tweety_generuj_wykres} pobiera dane
#' z ramki danych dla wybranego kandydata i w podanym przedziale czasowym tworzy wykres zmiany
#' wybranego wskaznika w czasie
#'
#' @param frame ramka danych, w której zawarte sa wysokosci wskaznikow dla kandydatow :"Podsumowanie_nasluch_kandydatow.csv"
#' @param name wektor napisów, zawieraj¹cy nazwisko kandydata
#' @param begin poczatek analizy, format YYYY-MM-DD
#' @param end koniec analizy, format YYYY-MM-DD
#' @param size rozmiar czcionki
#'
#'
#'@details
#' Imie i nazwisko \code{name_can} mozemy wybrac ze zbioru:
#' "Bronislaw Komorowski", "Andrzej Duda", "Magdalena Ogorek", "Pawel Kukiz"          
#' "Adam Jarubas", "Janusz Korwin Mikke",  "Janusz Palikot", "Marian Kowalski"      
#' "Wanda Nowicka'
#' 
#' Typ wykresu mozemy wybrac ze zbioru: "srednio retweetow", "suma retweetow", "srednio like",
#' "suma like","liczba tweetow"
#' 
#' @examples
#' frame<-read.csv2("Podsumowanie_nasluch_kandydatow.csv")
#' tweety_generuj_wykres(frame=frame,name="Bronislaw Komorowski",type="srednio like",end="2015-05-02")
#' tweety_generuj_wykres(frame=frame,name="Bronislaw Komorowski",type="liczba tweetow",end="2015-05-02")
#'
#' @import dplyr
#' @import stringi
#' @import ggplot2
#' @import scales
#'
#' @author Emilia Momotko

tweety_generuj_wykres <- function(frame, name, type,begin="NA", end="NA",size=12){
  
  #konwersja nazw
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
  
  
  #tworzymy przedzial czasu, na ktorym bedziemy operowac:
  dates <- sort(unique(as.character(frame$date)))
  N <- length(dates)
  Sys.setlocale("LC_TIME", "Polish")
  
  #jesli uzytkownik nie podaje dat, to bierzemy caly przedzial
  if(is.character(begin)&&begin=="NA"){
    begin <- as.Date(dates[1])
  } else begin <- as.Date(begin)
  
  if(is.character(end)&&end=="NA"){
    end <- as.Date(dates[N])
  } else end <- as.Date(end)
  
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
  
  
  main_title <- stri_paste(names(type)," - ",names(name))
  
  exact_data <- frame %>%
    filter(lastname==name & date %in% all_dates )
  
  
  
  #przypisujemy wartosci do wektora y
  for(i in seq_along(all_dates)){
    
    k <- which(exact_data$date==all_dates[i])
    if(length(k)>0&&!is.na(k)){
      y[i] <- exact_data[k,which_col]
    } else if(type=="how_many_tweets"){
      y[i] <- 0
    } else y[i] <- NA 
  }
  
  #tworzymy pomocnicza ramke danych
  dates_as_dates <- as.Date(all_dates)
  frame_help <- data.frame(y, dates_as_dates)
  
  if(all(is.na(frame_help$y))){
    print("Niestety w tym okresie nie mamy info o nikim")
    return(invisible(NULL))
  }
  
  p <- ggplot(frame_help, aes(x=dates_as_dates, y=y, group=3)) + xlab("")+
    ylab(type)+scale_x_date(labels=date_format("%b-%Y"))+
    theme(panel.background = element_rect(colour = "black"))+
    theme(plot.title = element_text(colour = "black"))+
    labs(title = main_title)+
    theme(axis.text=element_text(size=size-2),
          axis.title=element_text(size=size+4,face="bold"))+
    theme(plot.title = element_text(lineheight=.8, face="bold",size=size+6))
  
  if(type=="how_many_tweets"){
    
    p + geom_line()+ geom_point() 
  } else{
    
    p + geom_point()
    
  }
  
}

#przyklad jak zrobic kilka wykresow na jednym - dla Martyny - ale czasami wyglada srenio wiec moze
#lepiej zrobic tak max 2 na jednym
# 
# library(gridExtra)
# 
# names_candidate <- c("Adam Jarubas","Andrzej Duda","Wanda Nowicka")
# 
# n <- floor(sqrt(length(names_candidate)))
# plist <- list()
# for(i in 1:length(names_candidate)){
#   plist[[i]] <- tweety_generuj_wykres(frame=frame,name=names_candidate[i],type="srednio likeow")
# }
# 
# do.call("grid.arrange", c(plist, ncol=n))