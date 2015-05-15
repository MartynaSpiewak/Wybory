library(prezydentwsieci)
library(stringi)
library(wordcloud)
library(ggplot2)
library(gridExtra)
library(ggvis)

shinyServer(function(input, output) {
  
  
  #twitter  - tabele
  tab_nasluch <- read.csv2("data/Podsumowanie_nasluch_kandydatow.csv")
  
  tab_nasluch_miesiac <- read.csv2("data/Podsumowanie_nasluch_kandydatow_miesiace.csv")

  #nazwiska <- unique(c(input$kandydat, input$wybor))
  
  tab_sentyment <- read.csv2("data/Podsumowanie_tweetow.csv")
  
  frame_posts <- read.table("Facebook/Posty/facebook_posts.csv", sep =";", h = T)
  
  # tableka inforamacyjna, w ktorej sa imiona i nazwiska wszystkich kandydatow i id
  
  can <- read.table("Facebook/kandydaci.csv", h = T)
  # podsumowanie likow
  
  frame_likes <- read.table("Facebook/Likes/Podsumowanie/Podsumowanie.csv", 
                            sep=";", header = TRUE)

  
  #popularnosc na twitterze
  
  output$wykres_popularnosc <- renderPlot({
    
    prezydentwsieci:::tweety_generuj_wykres(frame=tab_nasluch,name=input$kandydat,type=input$typ_wykresu_twitter,begin=input$data[1], end=input$data[2])
    
  })
  
  #aktywnosc na twitterze
  
  output$wykres_aktywnosc <- renderPlot({
    
    prezydentwsieci:::tweety_generuj_wykres(frame=tab_nasluch,name=input$kandydat,type="liczba tweetow",begin=input$data[1], end=input$data[2])
  })
  
  output$wykres_sentyment <- renderPlot({
    
    nazwiska <- unique(c(input$kandydat, input$wybor))
    d1 <- as.character(input$data[1])
    d2 <- as.character(input$data[2])
    prezydentwsieci:::sentyment_wykres(tab_sentyment,name=nazwiska, begin=d1, end=d2)
    
            
  })
  
  
  #podsumowanie miesieczne:
  
  output$wykres_popularnosc_miesiac <- renderPlot({
    
    nazwiska <- unique(c(input$kandydat, input$wybor))
    d1 <- as.character(input$data[1])
    d2 <- as.character(input$data[2])
    prezydentwsieci:::tweety_generuj_slupki(frame=tab_nasluch_miesiac,name=nazwiska,type=input$typ_wykresu_twitter,begin=d1, end=d2)
    
  })
  
  #facebook
  
  output$wykres_like_facebook <- renderPlot({
    
    nazwiska <- unique(c(input$kandydat, input$wybor))
    
    d1 <- as.character(input$data[1])
    d2 <- as.character(input$data[2])
    
    tmp <- prezydentwsieci:::facebook_like_w_czasie(nazwiska, frame_likes = frame_likes,from=d1, to=d2, can = can)
                                   ggplot(tmp, aes(x=data, y=likes, colour=name)) +
                                     geom_line(size = 1) + theme(legend.position="bottom") + 
                                     ggtitle("Count of likes per day")
    
  })
  
  output$wykres_like_roznica_facebook <- renderPlot({
    
    nazwiska <- unique(c(input$kandydat, input$wybor))
    
    d1 <- as.character(input$data[1])
    d2 <- as.character(input$data[2])
    
    resf3 <- prezydentwsieci:::facebook_like_roznice_w_czasie(nazwiska, from=d1, to=d2, frame_likes = frame_likes, can = can)
    
    ggplot(resf3, aes(x=data, y=diff_likes, colour=name)) +
      geom_line(size = 1) + theme(legend.position="bottom") + 
      ggtitle(paste0("Przyrost liczby polubien oficjalnych stron kandydatow w czasie ", "\n")) +
      theme(plot.title = element_text(lineheight=1.2, size = 14, face="bold")) +
      labs(x="", y="Likes")
    
  })
  
  
  output$wykres_post_facebook <- renderPlot({
    
    nazwiska <- unique(c(input$kandydat, input$wybor))
    
    d1 <- input$data[1]
    d2 <- input$data[2]
    
    resf4 <- prezydentwsieci:::facebook_post_ile_dziennie(nazwiska,
                                        frame_posts = frame_posts, can = can, from=d1, to=d2)
    
    if(input$typ_wykresu_facebook=="Liczba postow"){
      
      p <- ggplot(resf4, aes(x=date, y=posts_count, colour=name)) +ggtitle("Liczba postow")
        
    }
    
    if(input$typ_wykresu_facebook=="Srednia liczba polubien postow"){
      
      p <- ggplot(resf4, aes(x=date, y=likes_count, colour=name)) +ggtitle("Srednia liczba polubien postow")
      
    }
    
    if(input$typ_wykresu_facebook=="Srednia liczba komentarzy"){
      
      p <- ggplot(resf4, aes(x=date, y=comments_count, colour=name)) +ggtitle("Srednia liczba komentarzy")
      
    }
    
    if(input$typ_wykresu_facebook=="Srednia liczba udostepnien postow"){
      
      p <- ggplot(resf4, aes(x=date, y=shares_count, colour=name)) +ggtitle("Srednia liczba udostepnien postow")
      
    }
    
    
    p + geom_line(size = 1) + theme(legend.position="bottom") + 
      theme(plot.title = element_text(lineheight=1.2, size = 14, face="bold"))
    
  })
  
  output$wykres_like_oficjalne <- renderPlot({
    
    nazwiska <- unique(c(input$kandydat, input$wybor))
    res <- prezydentwsieci:::facebook_like_najlepszy_wynik(input$data[1], frame_likes = frame_likes, can = can)
    
    k <- which(res$name%in%nazwiska)
    
    res <- res[k,]
    
    ggplot(res, aes(x = factor(name), y = likes)) + geom_bar(stat = "identity") 
    
  })
  dataReactive <- reactive({
    if (is.null(input$wyborStrony)) {
      FF <-frakcja_dla_kandydata(kandydat,czasOd, czasDo, dataTemp, 1,1,1,1,1,1,1,1,1,1)
      return(FF)
    }
    kandydat <- input$wyborKandydata
    czasOd <- input$datyFrakcja[1]
    czasDo <- input$datyFrakcja[2]
    c1 <- ifelse('c1'%in%input$wyborStrony,1,0)
    c2 <- ifelse('c2'%in%input$wyborStrony,1,0)
    c3 <- ifelse('c3'%in%input$wyborStrony,1,0)
    c4 <- ifelse('c4'%in%input$wyborStrony,1,0)
    c5 <-ifelse('c5'%in%input$wyborStrony,1,0)
    c6 <-ifelse('c6'%in%input$wyborStrony,1,0)
    c7 <- ifelse('c7'%in%input$wyborStrony,1,0)
    c8 <- ifelse('c8'%in%input$wyborStrony,1,0)
    c9 <- ifelse('c9'%in%input$wyborStrony,1,0)
    c10 <-ifelse('c10'%in%input$wyborStrony,1,0)
    dataTemp <- read.table("C:/Users/grabarze/Desktop/wyboryApp/data/candidate_fraction_info.csv",
                           header = TRUE, sep = ";", stringsAsFactors = FALSE)
    FF <- frakcja_dla_kandydata(kandydat,czasOd, czasDo, dataTemp, c1,c2,c3,c4,c5,c6,c7,c8,c9,c10)
    FF
  })
  dataReactive  %>% bind_shiny("p", "p_ui")
  
  
  ## 2 wyjkres
  dataReactive2 <- reactive({
    
    kandydat2 <- input$wyborKandydata2
    czasOd2 <- input$datyPozycja[1]
    czasDo2 <- input$datyPozycja[2]
    p1 <- ifelse('p1'%in%input$wyborPozycji,1,0)
    p2 <- ifelse('p2'%in%input$wyborPozycji,1,0)
    p3 <- ifelse('p3'%in%input$wyborPozycji,1,0)
    p4 <- ifelse('p4'%in%input$wyborPozycji,1,0)
    dataTemp <- read.table("C:/Users/grabarze/Desktop/wyboryApp/data/candidate_position.csv",
                           header = TRUE, sep = ";", stringsAsFactors = FALSE)
    FF <- pozycja_dla_kandydata(kandydat2,czasOd2, czasDo2, dataTemp, p1,p2,p3,p4)
    FF
  })
  dataReactive2  %>% bind_shiny("p2", "p_ui2")
  
  
  
  dataReactive3 <- reactive({
    kandydat3 <- input$wyborKandydata3
    czasOd3 <- input$datyStrony[1]
    czasDo3 <- input$datyStrony[2]
    c1 <- ifelse('c1'%in%input$wyborStrony3,1,0)
    c2 <- ifelse('c2'%in%input$wyborStrony3,1,0)
    c3 <- ifelse('c3'%in%input$wyborStrony3,1,0)
    c4 <- ifelse('c4'%in%input$wyborStrony3,1,0)
    c5 <- ifelse('c5'%in%input$wyborStrony3,1,0)
    c6 <- ifelse('c6'%in%input$wyborStrony3,1,0)
    c7 <- ifelse('c7'%in%input$wyborStrony3,1,0)
    c8 <- ifelse('c8'%in%input$wyborStrony3,1,0)
    c9 <- ifelse('c9'%in%input$wyborStrony3,1,0)
    c10 <-ifelse('c10'%in%input$wyborStrony3,1,0)
    dataTemp <- read.table("C:/Users/grabarze/Desktop/wyboryApp/data/candidate_general_info.csv",
                           header = TRUE, sep = ";", stringsAsFactors = FALSE)
    FF <- strony_dla_kandydata(kandydat3,czasOd3, czasDo3, dataTemp, c1,c2,c3,c4,c5,c6,c7,c8,c9,c10)
    FF
  })
  dataReactive3  %>% bind_shiny("p3", "p_ui3")
  
  dataReactive4 <- reactive({
    strona4 <- input$wyborStrony44
    czasOd4 <- input$daty4[1]
    czasDo4 <- input$daty4[2]
    c1 <- ifelse('c1'%in%input$wyborKandydata4,1,0)
    c2 <- ifelse('c2'%in%input$wyborKandydata4,1,0)
    c3 <- ifelse('c3'%in%input$wyborKandydata4,1,0)
    c4 <- ifelse('c4'%in%input$wyborKandydata4,1,0)
    c5 <- ifelse('c5'%in%input$wyborKandydata4,1,0)
    c6 <- ifelse('c6'%in%input$wyborKandydata4,1,0)
    c7 <- ifelse('c7'%in%input$wyborKandydata4,1,0)
    c8 <- ifelse('c8'%in%input$wyborKandydata4,1,0)
    c9 <- ifelse('c9'%in%input$wyborKandydata4,1,0)
    c10 <-ifelse('c10'%in%input$wyborKandydata4,1,0)
    dataTemp <- read.table("C:/Users/grabarze/Desktop/wyboryApp/data/candidate_general_info.csv",
                           header = TRUE, sep = ";", stringsAsFactors = FALSE)
    FF <- kandydaci_dla_strony(strona4,czasOd4, czasDo4, dataTemp, c1,c2,c3,c4,c5,c6,c7,c8,c9,c10)
    FF
  })
  dataReactive4  %>% bind_shiny("p4", "p_ui4")
  
})