#' Dokonanie analizy sentymantu na tweetach, ktore sa podane w formacie json
#'
#' Funkcja \code{sentyment_tweety} pobiera pliki o nazwach  z wzorcem "R_project1_tweets-" z working directory
#' a nastepnie dokonuje analizy sentymentu na tych tweetach. Funkcja ta dziala dla pliku z konkretnego
#' dnia
#' 
#' @usage sentyment_tweety(data=Sys.Date())
#' @param data data, ktora nas interesuje i ktora bedziemy analizowac, w formacie jako napis
#' w formacie YYYY-MM-DD
#' 
#' @return
#' invisible(NULL) - wartosci analizy sentymentu nie sa przechowywane w pamieci a zapisywane do pliku
#'
#'@author Emilia Momotko
#'
#'@import
#' stringi
#' dplyr
#' streamR
#' 
#'@examples
#' sentyment_tweety("2015-04-04")

sentyment_tweety <- function(data=Sys.Date()){
  
  #tworzymy pusta ramke danych
  Parsed <- data.frame()
  
  fname <- "Podsumowanie_tweetow.csv"
  
  if(!file.exists(fname)){
    f <- file(fname, open="a")
    #tworze pierwszy wiersz w pliku:
    writeLines(stri_paste('\"lastname\"', '\"date\"', '\"sentiment\"', sep = ";"), f)
    
    pliki <- list.files(pattern="R_project1_tweets-")
    N <- length(pliki)
    
    for(i in 1:N){
      
      Parsed <- Parsed %>%
        rbind(parseTweets(pliki[i], simplify = FALSE, verbose = TRUE))
    }
  } else{
    
      f <- file(fname, open="a")
      plik <- stri_paste("R_project1_tweets-",as.character(data),".json")
      if(!file.exists(plik)){
        close(f)
        return(invisible(NULL))
      }
      
      Parsed <- Parsed %>%
      rbind(parseTweets(plik, simplify = FALSE, verbose = TRUE))
      
    }
  
  Parsed

  #BIERZEMY TRESCI TWEETOW - BEDZIEMY JE UPRASZCZAC
  tresci <- Parsed[,1]
  
  modified <- oczyszczanie(tresci)
  
  #BEDZIEMY SPRAWDZAC KTORY TWEET KOGO DOTYCZY
  tagsKomorowski <- c("bronis³aw", "bronis³awa", "bronis³awowi", "bronis³awa", 
                      "bronis³awem", "bronis³awie", "bronis³awie", "bronislaw",
                      "bronislawa", "bronislawowi","bronislawem","komorowski",
                      "komorowskiego", "komorowskiemu", "komorowskiego", 
                      "komorowskim", "komorowskim", "komorowski", "bronkobus",
                      "bronkobusa") 
  tagsDuda <- c("andrzej", "andrzeja", "andrzejowi", "andrzeja", "andrzejem",
                "andrzeju", "duda", "dudy", "dudzie", "dudê", "dud¹",
                "dudzie", "dudo","dude")
  tagsOgorek <- c("magdalena", "magdaleny", "magdalenie", "magdalenê",
                  "magdalen¹", "magdalenie", "magdaleno", "ogórek", "ogorek")
  tagsKorwin <- c("janusz", "janusza", "januszowi", "januszem", "januszu",
                  "korwin-mikke", "korwin", "mikke", "jkm")
  tagsKukiz <- c("kukiz", "kukiza")
  tagsJarubas <- c("jarubas", "jarubasa")
  tagsPalikot <- c("palikot", "palikota")
  tagsWilk <- c("wilk", "wilka")
  tagsBraun <- c("braun", "brauna")
  tagsKowalski <- c("kowalski", "kowalskiego", "kowalskiemu", "kowalskim")
  tagsTanajno <- c("tanajno")
  
  kandydaci <- list(komorowski=tagsKomorowski,duda=tagsDuda,korwin=tagsKorwin,ogorek=tagsOgorek,
                    jarubas=tagsJarubas, kukiz=tagsKukiz, palikot=tagsPalikot,
                    wilk=tagsWilk, braun=tagsBraun, kowalski= tagsKowalski, tanajno=tagsTanajno)
  
  k <- length(kandydaci)
  
  for(i in 1:k){
    
    Parsed<-cbind(Parsed,unlist(lapply(modified,function(y){
      any(y%in%kandydaci[[i]])
    })))
    
  }
  
  names(Parsed)[43:53]<-names(kandydaci)
  
  #dokonujemy analizy
  wartosci <- sentyment(modified)
  
  Parsed<-tbl_df(Parsed)
  
  Parsed <- cbind(Parsed,wartosci)
  
  #ustawiamy na inny jezyk, zeby wychwycilo date
  Sys.setlocale("LC_TIME", "English")
  
  dat<-as.character(strptime(Parsed$created_at,format="%a %b %d %H:%M:%S %z %Y"))
  
  Parsed <- Parsed%>%
    mutate(day=unlist(stri_extract_all_regex(dat, "[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2}")))
  
  #grupowanie, analiza i zapis
  lapply(names(kandydaci), function(y){
    
    dane <- Parsed %>%
      filter(Parsed[,y]==TRUE) %>%
      group_by(day) %>%
      summarise(sentyment=sum(wartosci))
    
    dane<-as.data.frame(dane)
    ile <- nrow(dane)
    if(ile>0){
      for(i in 1:ile){
        
        writeLines(stri_paste(y, dane[i,1], dane[i,2], sep=";"),f)
      }
    }
    
  })
  close(f)
  
  invisible(NULL)
}

