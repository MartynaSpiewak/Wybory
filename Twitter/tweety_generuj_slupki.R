#' Funkcja tworzy wykres przedstawiaj?cy zmian? pewnych wartosci wskaznikow na przestrzeni czasu dla okreslonych
#' kandydatow, jednostka czasu to meisiac
#'
#' Funkcja \code{tweety_generuj_slupki} pobiera dane
#' z ramki danych dla wybranego kandydata i w podanym przedziale czasowym tworzy wykres slupkowy zmiany
#' wybranego wskaznika w czasie, tutaj jednostka czasu jest miesiac
#'
#' @param frame ramka danych, w kt?rej zawarte sa wysokosci wskaznikow dla kandydatow :"Podsumowanie_nasluch_kandydatow_miesiace.csv"
#' @param name wektor napis?w, zawieraj?cy nazwiska kandydatow
#' @param begin poczatek analizy
#' @param end koniec analizy
#'
#'
#'@details
#' Imie i nazwisko \code{name} mozemy wybrac ze zbioru:
#' "Bronislaw Komorowski", "Andrzej Duda", "Magdalena Ogorek", "Pawel Kukiz"
#' "Adam Jarubas", "Janusz Korwin Mikke",  "Janusz Palikot", "Marian Kowalski"
#' "Wanda Nowicka"
#'
#' Typ wykresu mozemy wybrac ze zbioru: "srednio retweetow", "suma retweetow", "srednio likeow",
#' "suma likeow", "liczba tweetow"
#'
#' @examples
#' frame<-read.csv2("Podsumowanie_nasluch_kandydatow_miesiace.csv")
#' tweety_generuj_slupki(frame=frame,name=c("Bronislaw Komorowski","Andrzej Duda", "Magdalena Ogorek"),type="srednio likeow",begin="2015-02-01",end="2015-05-02")
#'
#' @import dplyr
#' @import stringi
#' @import ggplot2
#' @import scales
#' @import Hmisc
#'
#' @author Emilia Momotko


tweety_generuj_slupki<- function(frame, name, type, begin="2014-01-01", end=as.character(Sys.Date())){

  #konwersja nazw
  nazwy <- c("Komorowski", "AndrzejDuda", "JkmMikke", "ogorekmagda", "JarubasAdam", "PrezydentKukiz", "Palikot_Janusz",
             "M_Kowalski1", "WandaNowicka")
  names(nazwy) <- c("Bronislaw Komorowski", "Andrzej Duda", "Janusz Korwin Mikke",
                    "Magdalena Ogorek", "Adam Jarubas", "Pawel Kukiz","Janusz Palikot",
                    "Marian Kowalski", "Wanda Nowicka")

  ktore <- which(names(nazwy)%in%name)
  name <- nazwy[ktore]


  action <- c("srednio_retweetow","suma_retweetow","srednio_like","suma_like","liczba_tweetow")
  names(action) <- c("srednio retweetow", "suma retweetow", "srednio like","suma like","liczba tweetow")

  ktora_akcja <- which(names(action)%in%type)
  type <- action[ktora_akcja]

  if(length(type)==0){
    print("Zly typ")
    return(invisible(NULL))
  }

  n_plots <- length(name)

  main_title <- stri_paste(names(type)," - wybrani kandydaci")

  #tworzymy przedzial czasu, na ktorym bedziemy operowac

  n_months <- ile_miesiecy(begin, end)

  #generujemy CALY przedzial czasowy
  all_dates <- character(n_months)

  begin <- as.Date(begin)
  days <- monthDays(begin)
  for(i in seq_along(all_dates)){
    data <- begin+days*(i-1)
    days <- monthDays(data)
    all_dates[i] <- as.character(data)
  }

  all_dates <- stri_extract_first_regex(all_dates,"[0-9]{4}-[0-9]{2}")

  #tutaj bedziemy przechowywac wartosci, ktore przedstawimy na wykresie
  y <- numeric(n_months)

  #ale najpierw wybierzmy te z danych, ktore chce uzytkownik
  which_col <- which(colnames(frame)==type)

  frame[,which_col]<- as.numeric(as.character(frame[,which_col]))

  #generujemy ramke danych z wartosciami
  frame_help <- data.frame()

  for(j in 1:n_plots){
    exact_data <- frame %>%
      filter(screenName==name[j] & month %in% all_dates )

    for(i in seq_along(all_dates)){

      k <- which(exact_data$month==all_dates[i])
      if(length(k)>0&&!is.na(k)){
        y[i] <- exact_data[k,which_col]
      } else if(type=="liczba_tweetow"){
        y[i] <- 0
      } else y[i] <- NA
    }

  frame_help <- rbind(frame_help,data.frame(y, all_dates,kandydat=rep(names(name[j]),length(y))))
  }

  if(all(is.na(frame_help$y))){
    print("Niestety w tym okresie nie mamy info o nikim")
    return(invisible(NULL))
  }

  p <- ggplot(frame_help, aes(x=all_dates,y=y,fill=kandydat))
  p+geom_bar(position="dodge",stat="identity")+ scale_fill_brewer(palette="Dark2")+
    theme(plot.title = element_text(colour = "black"))+
    labs(title = main_title)+ theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))+
    theme(plot.title = element_text(lineheight=.8, face="bold",size=18))+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y= element_blank())

}
