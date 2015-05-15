library(prezydentwsieci)
library(stringi)
library(wordcloud)
library(ggplot2)
library(gridExtra)
library(ggvis)
library(shinythemes)

shinyUI(
  shinyUI(navbarPage(theme = shinytheme("flatly"),"Wybory",
                     tabPanel("HomePage",
                              titlePanel("Analiza widoczności kandydatów na prezydenta"),
                              sidebarLayout(sidebarPanel(
                              selectInput("kandydat",label="Kandydat",choices=c("Bronislaw Komorowski","Andrzej Duda","Adam Jarubas",
                                                                                "Janusz Korwin Mikke","Marian Kowalski","Pawel Kukiz",
                                                                                "Magdalena Ogorek","Janusz Palikot"),selected="Bronislaw Komorowski"),
                              checkboxGroupInput(inputId = "wybor",label = "Porownanie:",
                                                 choices = c("Bronislaw Komorowski","Andrzej Duda","Adam Jarubas",
                                                             "Janusz Korwin Mikke","Marian Kowalski","Pawel Kukiz",
                                                             "Magdalena Ogorek","Janusz Palikot")),
                              dateRangeInput("data",label="Analizowany okres czasu", start="2015-03-18",end="2015-05-10")),
                              mainPanel(h3("Aplikacja Wybory stanowi wizualizację danych dotyczących
                                           widoczności kandydatów na prezydenta w wyborach z 2015 roku. W trzech zakładkach
                                           zawarte są analizy danych zebrane z trzech głównych źródeł: Twittera, Facebooka i portali internetowych.")
                                ))
                              
                              ),
                     
                     
                     tabPanel("Twitter",
                              fluidPage(
                                titlePanel("Analiza danych z twittera."),
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput("typ_wykresu_twitter",
                                                "Popularnosc - typ wykresu",
                                                c("srednio retweetow", "suma retweetow", "srednio like","suma like"),
                                                "srednio retweetow"),
                                    helpText("Wybierz typ wykresu, ktory bedzie odpowiadal analizie popularnosci
                                             kandydatow wsrod uczytkownikow Twittera.")
                                    ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Analiza sentymentu", plotOutput("wykres_sentyment")),
                                  tabPanel("Popularnosc wsrod uzytkowników",plotOutput("wykres_popularnosc")),
                                  tabPanel("Aktywność na twitterze",plotOutput("wykres_aktywnosc")),
                                  tabPanel("Popularnosc wsrod uzytkownikow - miesiace",plotOutput("wykres_popularnosc_miesiac"))
                                  
                                ))))),
                     tabPanel("Facebook",
                              fluidPage(
                                titlePanel("Analiza danych z facebooka"),
                                sidebarLayout(
                                  sidebarPanel(
                                    selectInput("typ_wykresu_facebook", "Analiza postow - typ wykresu",
                                                choices=c("Liczba postow","Srednia liczba polubien postow","Srednia liczba komentarzy",
                                                          "Srednia liczba udostepnien postow"),selected="Liczba postow"
                                      ),
                                    helpText("Wybierz typ wykresu, ktory bedzie odpowiadal analizie postow uzytkownikow facebooka.")),
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel("Like", plotOutput("wykres_like_facebook")),
                                      tabPanel("Like - roznica w czasie", plotOutput("wykres_like_roznica_facebook")),
                                      tabPanel("Like - liczba polubien oficjalnych stron",  plotOutput("wykres_like_oficjalne")),
                                      tabPanel("Analiza postow", plotOutput("wykres_post_facebook"))
                                      
                                  ))
                             
                     )
                     
  )),
  tabPanel("Portale Informacyjne",
           fluidPage(
             
             # Application title
             titlePanel("Widocznosc kandydatow na portalach informacyjnych."),
             
             # Sidebar with a slider input for the number of bins
             sidebarLayout(
               sidebarPanel(
                 h4("Frakcja stron:"),
                 selectInput("wyborKandydata",
                             label = h4("Wybierz kandydata"),
                             choices = list("Komorowski" = 'komorowski',
                                            "Duda" = 'duda', "Ogorek" = 'ogorek',
                                            "Korwin" = 'korwin', "Kukiz" = 'kukiz',
                                            "Jarubas" = 'jarubas', "Palikot" = 'palikot',
                                            "Wilk" = 'wilk', "Braun"= 'braun',
                                            "Tanajno" = 'tanajno'),
                             selected = 'komorowski'),
                 dateRangeInput("datyFrakcja", label = h3("Czas"),
                                start = "2015-03-16",
                                end   = "2015-05-12"),
                 checkboxGroupInput("wyborStrony",
                                    label = h4("Wybierz portal"),
                                    choices = list("TvPInfo" = 'c1',
                                                   "WpPl" = 'c2', "WprostPl" = 'c3',
                                                   "OnetPl" = 'c4', "NewsweekPl" = 'c5',
                                                   "Tvn24Pl" = 'c6', "NatematPl" = 'c7',
                                                   "GazetaPl" = 'c8', "DziennikPl"= 'c9',
                                                   "WyborczaPl" = 'c10'),
                                    selected = 'c1'),
                 br(),
                 br(),
                 h4("Pozycje artykulow na stronie:"),
                 selectInput("wyborKandydata2",
                             label = h4("Wybierz kandydata"),
                             choices = list("Komorowski" = 'komorowski',
                                            "Duda" = 'duda', "Ogorek" = 'ogorek',
                                            "Korwin" = 'korwin', "Kukiz" = 'kukiz',
                                            "Jarubas" = 'jarubas', "Palikot" = 'palikot',
                                            "Wilk" = 'wilk', "Braun"= 'braun',
                                            "Tanajno" = 'tanajno'),
                             selected = 'komorowski'),
                 dateRangeInput("datyPozycja", label = h3("Czas"),
                                start = "2015-03-16",
                                end   = "2015-05-12"),
                 checkboxGroupInput("wyborPozycji",
                                    label = h4("Wybierz pozycje(waznosc)"),
                                    choices = list("Pozycja1" = 'p1',
                                                   "Pozycja2" = 'p2', "Pozycja3" = 'p3',
                                                   "Pozycja4" = 'p4'),
                                    selected = 'p1'),
                 br(),
                 br(),
                 h4("Liczba artykulow dla Kandydata:"),
                 selectInput("wyborKandydata3",
                             label = h4("Wybierz kandydata"),
                             choices = list("Komorowski" = 'komorowski',
                                            "Duda" = 'duda', "Ogorek" = 'ogorek',
                                            "Korwin" = 'korwin', "Kukiz" = 'kukiz',
                                            "Jarubas" = 'jarubas', "Palikot" = 'palikot',
                                            "Wilk" = 'wilk', "Braun"= 'braun',
                                            "Tanajno" = 'tanajno'),
                             selected = 'komorowski'),
                 dateRangeInput("datyStrony", label = h3("Czas"),
                                start = "2015-03-16",
                                end   = "2015-05-12"),
                 checkboxGroupInput("wyborStrony3",
                                    label = h4("Wybierz portal"),
                                    choices = list("TvPInfo" = 'c1',
                                                   "WpPl" = 'c2', "WprostPl" = 'c3',
                                                   "OnetPl" = 'c4', "NewsweekPl" = 'c5',
                                                   "Tvn24Pl" = 'c6', "NatematPl" = 'c7',
                                                   "GazetaPl" = 'c8', "DziennikPl"= 'c9',
                                                   "WyborczaPl" = 'c10'),
                                    selected = 'c1'),
                 br(),
                 br(),
                 h4("Liczba artykulow dla Strony:"),
                 selectInput("wyborStrony44",
                             label = h4("Wybierz strone"),
                             choices = list("TvPInfo" = 'TvPInfo',
                                            "WpPl" = 'WpPl', "WprostPl" = 'WprostPl',
                                            "OnetPl" = 'OnetPl', "NewsweekPl" = 'NewsweekPl',
                                            "Tvn24Pl" = 'Tvn24Pl', "NatematPl" = 'NatematPl',
                                            "GazetaPl" = 'GazetaPl', "DziennikPl"= 'DziennikPl',
                                            "WyborczaPl" = 'WyborczaPl'),
                             selected = 'WpPl'),
                 dateRangeInput("daty4", label = h3("Czas"),
                                start = "2015-03-16",
                                end   = "2015-05-12"),
                 checkboxGroupInput("wyborKandydata4",
                                    label = h4("Wybierz kandydata"),
                                    choices = list("Komorowski" = 'c1',
                                                   "Duda" = 'c2', "Ogorek" = 'c3',
                                                   "Korwin" = 'c4', "Kukiz" = 'c5',
                                                   "Jarubas" = 'c6', "Palikot" = 'c7',
                                                   "Wilk" = 'c8', "Braun"= 'c9',
                                                   "Tanajno" = 'c10'),
                                    selected = 'c1')
               ),
               
               
               # Show a plot of the generated distribution
               mainPanel(
                 h3('Proporcja artykulow o danych kandydacie do wszystkich artykulow dotyczacych wyborow prezydenckich:'),
                 ggvisOutput("p"),
                 h3('Liczba artykulow o okreslonych pozycjach(1-artykul poboczny do 4 - artykul glowny):'),
                 ggvisOutput("p2"),
                 h3('Liczba artykulow na stronie dla wybranego kandydata:'),
                 ggvisOutput("p3"),
                 h3('Porownanie liczby artykulow o kandydatach dla wybranej strony:'),
                 ggvisOutput("p4")
                 # plotOutput("wykres11", width = 500),
                 # verbatimTextOutput("podsumowanie")
               )
             )
           ))
)))