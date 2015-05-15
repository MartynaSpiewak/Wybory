#' Podsumowanie podstawowych charakterystyk - facebook.com
#'
#' Funkcja \code{facebook_post_ile_dziennie} podsumowuje podstawowe
#' charakterystyki związane z postami publikowanymi na stronach
#' kandydatow na prezydenta, m.in. liczba postow, liczba polubien, 
#' liczba komentarzy, liczba udostepnien postow opublikowanych na stronie
#' 
#' @usage facebook_post_ile_dziennie(name_can, from=as.Date("2015-01-01"), to=Sys.Date(),
#'                                                                      frame_posts, can)
#' @param name_can - wektor, zawierajacy imie i nazwisko kandydatow
#' @param from - data, od ktorej rozpoczynamy analize
#' @param to - data, do której prowadzimy analize
#' @param frame_posts - ramka wejsciowa ze wszystkimi postami ukazanymi sie na publicznych 
#' profilach kandydatow od 2015-01-01
#' @param can - ramka danych z podstawowymi informacjami o kandydatach - id, imie, nazwisko
#'
#' @details
#' Imię i nazwisko \code{name_can} mozemy wybrać ze zbioru:
#' "Bronislaw Komorowski", "Andrzej Duda", "Magdalena Ogorek", "Pawel Kukiz"          
#' "Adam Jarubas", "Janusz Korwin Mikke",  "Janusz Palikot", "Marian Kowalski"      
#' "Jacek Wilk", "Grzegorz Braun", "Pawel Tanajno"
#' 
#' @return
#' ramka danych, której kolumny to kolejno data, liczba postow, 
#' liczba polubien postow opublikowanego danego dnia, 
#' liczba komentarzy pod postami opublikowanych danego dnia,
#' liczba udostepnien danych postow
#' 
#' @details
#' Ramka danych moze posluzyc do narysowania nastepujacych wykresow
#' 
#' tmp_posts <- facebook_post_ile_dziennie("Pawel Kukiz")
#' 
#' # wykresy 
#' p1 <- ggplot(tmp_posts, aes(x=date, y=posts_count)) +
#'   geom_line() +
#'   ggtitle("Count of posts per day")
#' p2 <- ggplot(tmp_posts, aes(x=date, y=likes_count)) +
#'   geom_line() +
#'   ggtitle("Count of likes per day")
#' p3 <- ggplot(tmp_posts, aes(x=date, y=comments_count)) +
#'   geom_line() +
#'   ggtitle("Count of comments per day")
#' p4 <- ggplot(tmp_posts, aes(x=date, y=shares_count)) +
#'   geom_line() +
#'   ggtitle("Count of shares per day") 
#'  
#'  multiplot(p1, p2, p3, p4, cols=2)
#'
#'@author Martyna Spiewak
#'
#'@import
#' stringi
#' dplyr
#' 
#'@examples
# facebook_post_ile_dziennie(c("Bronislaw Komorowski", "Andrzej Duda", "Pawel Kukiz"), 
#                              frame_posts = frame_posts, can = can)
#' potrzebujemy dwoch ramek danych, np:
#' frame_posts <- read.table("Facebook\\Posty\\facebook_posts.csv", sep =";", h = T)
#  # tableka inforamacyjna, w ktorej sa imiona i nazwiska wszystkich kandydatow i id
#  can <- read.table("Facebook\\kandydaci.csv", h = T)

########################
# # skasować
# 
# require(dplyr)
# require(stringi)
# require(ggplot2)
# require(data.table)

#   wczytujemy ramke z wszystkimi postami opublikowanymi od daty '2015-01-01':
#   frame_posts <- read.table("Facebook\\Posty\\facebook_posts.csv", sep =";", h = T)
#   # tableka inforamacyjna, w ktorej sa imiona i nazwiska wszystkich kandydatow i id
#   can <- read.table("Facebook\\kandydaci.csv", h = T)
############################

facebook_post_ile_dziennie <- function(name_can, 
                                       from=as.Date("2015-01-01"), to=Sys.Date(),
                                       frame_posts, can){
  
    # imiona i nazwiska kandydatow w konwecji shiny
    kan <- stri_trans_totitle(can$names)

    # id wybranego/ych kandydata/ow
    id <- can$id[which(kan %in% name_can)]
    # ograniczamy sie do rekordow o danych kandydacie/ach
    
    # wyjsciowa ramka danych
    res <- data.table()
    
    for( i in seq_along(id)){
    # dane o wybranym kandydacie
    one <- frame_posts[frame_posts$id == id[i],]
    # zmiana formatu daty
    one$created_time <- stri_extract_first_regex(one$created_time, ".{10}")
    # posty z danego odcinka czasowego
    one %>% 
        filter(as.Date(one$created_time) >= from, as.Date(one$created_time) <= to) -> posts
    # dane ze wszystkich stron na temat kandydata
    # ogółem na wszystkich profilach    
    tmp_posts  <- posts %>% 
          group_by(date = as.Date(posts$created_time)) %>%
          summarise(id = id[i],
                    name = kan[id],
                    posts_count = n(),
                    likes_count = round(mean(likes_count), 0),
                    comments_count = round(mean(comments_count), 0),
                    shares_count = round(mean(shares_count), 0)) 
        res <- rbind(res, tmp_posts)
       
}
  return(res)
}


# # Przyklad dla Emilki do Shiny:
# res <- facebook_post_ile_dziennie(c("Bronislaw Komorowski", "Andrzej Duda", "Pawel Kukiz"), 
#                                         frame_posts = frame_posts, can = can)
# 
#   # wykresy 
#     p1 <- ggplot(res, aes(x=date, y=posts_count, colour=name)) +
#       geom_line(size = 1) + theme(legend.position="bottom") + 
#       ggtitle("Count of posts per day")
#     p2 <- ggplot(tmp_posts, aes(x=date, y=likes_count, colour=name)) +
#       geom_line(size = 1) + theme(legend.position="bottom") + 
#       ggtitle("Count of likes per day")
#     p3 <- ggplot(tmp_posts, aes(x=date, y=comments_count, colour=name)) +
#       geom_line(size = 1) + theme(legend.position="bottom") + 
#       ggtitle("Count of comments per day")
#     p4 <- ggplot(tmp_posts, aes(x=date, y=shares_count, colour=name)) +
#       geom_line(size = 1) + theme(legend.position="bottom") + 
#       ggtitle("Count of shares per day") 
#     multiplot(p1, p2, p3, p4, cols=2)



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}