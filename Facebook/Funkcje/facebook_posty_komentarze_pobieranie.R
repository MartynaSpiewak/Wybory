#' Sciaganie postow i komentarzy z publicznych stron na temat 
#' kandydatow na prezydenta z facebook.com
#'
#' Funkcja \code{facebook_posty_komentarze_pobieranie} ma na celu sciagniecie wszystkich postow
#' i komentarzy opublikowanych na stronie w przeciagu ostatnich 7 dni, 
#' funkcja zostala przygotowana do codziennego uzytku, wobec tego zapisywane sa do pliku 
#' csv tylko informacje z dnia poprzedniego (robimy tak poniewaz uznajemy, ze zywotnosc
#' danego postu to maksymalnie 7 dni)
#' 
#' @usage facebook_posty_komentarze_pobieranie(id.kandydant.all)
#' @param id.kandydant.all - numery id do publicznych stron na temat wybranego kandydata
#'
#' @return
#' funkcja zapisuje plik w formacie csv, w ktorym dla kazdego kandydata znajduje sie 
#' podsumowanie na temat postow i komentarzy, m.in. data publikacji, tresc, liczba polubien,
#' liczba komentarzy
#'
#'@author Martyna Spiewak
#'
#'@import
#' stringi
#' Rfacebook
#'
#'@examples
#' facebook_posty_komentarze_pobieranie(list("pawel tanajno" = 
#' c("469702339755768", "785096541556003", "390816184436123")))
#' 

facebook_posty_komentarze_pobieranie<-function(id.kandydant.all){
  
  kandydant<-data.frame()
  id.kandydant<-id.kandydant.all[[1]]
  kandydant.name <- names(id.kandydant.all)
  
  #####################
  ### posts
  ####################
  
  # zbieramy iformacje na temat artykulow z poprzedniego tygodnia, 
  # ale zapisujemy jedynie nowe posty i nowe komentarze do tych postow
  for(x in id.kandydant){
    kandydant<-rbind(kandydant, tryCatch(getPage(page=x, token=fb_oauth, 
                                        since = stri_replace_all_regex(as.character(Sys.Date()-7), "-", "/"),
                                        until = stri_replace_all_regex(as.character(Sys.Date()), "-", "/"), 
                                                 feed=FALSE, n=500),
                                         error = function(e) return(invisible(NULL))))
  }
  
  kandydant <- kandydant[!is.na(kandydant$message),]
  kandydant$message <- stri_replace_all_regex(kandydant$message, ';',"")
  kandydant$message <- stri_trim_both(stri_replace_all_regex(kandydant$message,"(\\n)|(\\t)|(\\r)|(\")"," "))
  # posty i komentarze z dnia poprzedniego:
  kandydant_today<-kandydant[unlist(stri_extract_all_regex(kandydant$created_time, 
                                                           "[0-9\\-]{10}"))==(Sys.Date()-1),]
  
  # zapisywanie do pliku: kandydant.name_facebook
  n<-nrow(kandydant_today) #number of rows
  if(n!=0){
    fname<-paste0("Facebook\\", kandydant.name, "_facebook", ".csv")
    if(file.exists(fname)) {f<-file(fname, open="a")}
    if (!file.exists(fname)){
      f<-file(fname, open="a")
      #header:
      writeLines(stri_paste('\"from_id\"','\"from_name\"','\"message\"','\"created_time\"',
                            '\"type\"','\"link\"','\"id\"','\"likes_count\"',
                            '\"comments_count\"','\"shares_count\"', sep=";"), f)
    }
    
    for(i in 1:n){
      #dopisuje do pliku kolejny wiersz      
      wiersz<-paste0(paste0('"',kandydant_today[i,],collapse='";'),'"')
      writeLines(wiersz,f)
    }
    close(f)
  }
  
  ###############
  ### comments
  ###############
  
  kandydant_comments<-data.frame()
  for(x in kandydant$id){
    GetPost<-getPost(x, token=fb_oauth, likes = FALSE)
    
    tmp_post<-as.data.frame(GetPost[1])
    tmp_post$post.message<-stri_replace_all_regex(tmp_post$post.message, ';', "")
    tmp_post$post.message<-stri_trim_both(stri_replace_all_regex(tmp_post$post.message,
                                                                 "(\\n)|(\\t)|(\\r)|(\")"," "))
    
    tmp_comments<-as.data.frame(GetPost[2])
    tmp_comments$comments.message<-stri_replace_all_regex(tmp_comments$comments.message, ';', "")
    tmp_comments$comments.message<-stri_trim_both(stri_replace_all_regex(tmp_comments$comments.message,
                                                                         "(\\n)|(\\t)|(\\r)|(\")"," "))
    # komentarze z dnia poprzedniego
    tmp_comments<-tmp_comments[unlist(stri_extract_all_regex(tmp_comments$comments.created_time, 
                                                             "[0-9\\-]{10}"))==(Sys.Date()-1),]
    m<-merge(tmp_post, tmp_comments)
    kandydant_comments<-rbind(kandydant_comments, m)
  }
  n<-nrow(kandydant_comments) #number of rows
  if(n!=0){
    gname<-paste0("Facebook\\",kandydant.name, "_facebook_comments", ".csv")
    if(file.exists(gname)) {g<-file(gname, open="a")}
    if (!file.exists(gname)){
      g<-file(gname, open="a")
      #header:
      writeLines(stri_paste('\"post.from_id\"','\"post.from_name\"','\"post.message\"','\"post.created_time\"',
                            '\"post.type\"','\"post.link\"','\"post.id\"','\"post.likes_count\"',
                            '\"post.comments_count\"','\"post.shares_count\"', 
                            '\"comments.from_id\"','\"comments.from_name\"','\"comments.message\"',
                            '\"comments.created_time\"',
                            '\"post.likes.count\"','\"comments.id\"',sep=";"), g)
    }
    
    for(i in 1:n){         
      wiersz<-paste0(paste0('"',kandydant_comments[i,],collapse='";'),'"')
      writeLines(wiersz,g)
    }
    close(g)
  }
}
}