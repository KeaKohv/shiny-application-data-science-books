# https://data-sci-books.shinyapps.io/shinyapp/

library(shinyjs)
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)
library(tidyverse)
library(shiny)
library(DT)
library(leaflet)
library(plotly)
library(shinydashboard)
library(ggrepel)


# Shiny UI

ui = dashboardPage(skin = "black",
                   dashboardHeader(title = "DataSci Books"),
                   dashboardSidebar(
                     sidebarMenu(
                       menuItem("Graafik: parimad raamatud", tabName = "graafik"),
                       menuItem("Graafik: avaldamiste arv ajas", tabName = "graafik2"),
                       menuItem("Graafik: suurimad väljaandjad", tabName = "graafik3"),
                       menuItem("Andmestik ja tulemused", tabName = "andmestik"),
                       menuItem("Seletuskiri", tabName = "seletuskiri")
                     )
                   ),
                   dashboardBody(useShinyjs(),
                                 tags$style(
                                            HTML('
                                                .box-header{ display: none}')
                                            ),
                     tabItems(
                       tabItem("graafik",
                               fluidRow(
                                 box(
                                   width = 12, 
                                   h2("Ülevaade andmeteaduse raamatutest Amazonis"),
                                   p("Siit saad teada, millised Amazonis müüdavad andmeteaduse raamatud on kõige kõrgemalt hinnatud ja millised on kõige populaarsemad."),
                                   p("Iga mull on raamat. ", tags$b("Mida suurem mull, seda populaarsem raamat")," vastavalt hinnangute arvule Amazonis. ",tags$b("Mida heledam mull, seda kõrgemalt hinnatud")," raamat."),
                                   p("Mine hiirega mullile, et näha rohkem infot. ",tags$b("Kliki mullil või alloleva tabeli real,")," et avada selle raamatu link Amazonis.")
                                 )
                               ),
                               fluidRow(
                                 box(solidHeader = FALSE,
                                   width = 8,
                                   girafeOutput("plot")
                                 ),
                                 box(
                                   width = 4,
                                   sliderInput(
                                     "kesk_hinnang",
                                     "Keskmise hinnangu vahemik",
                                     min = 0,
                                     max = 5,
                                     value = c(4.5,5),
                                     step = 0.2),
                                   sliderInput(
                                     "hinnangute_arv",
                                     "Hinnangute arvu vahemik",
                                     min = 1,
                                     max = 7563,
                                     value = c(50,7563),
                                     step = 1,
                                     sep = ""),
                                   sliderInput(
                                     "aasta",
                                     "Avaldamisaasta vahemik",
                                     min = 1972,
                                     max = 2022,
                                     value = c(1972,2022),
                                     step = 1,
                                     sep = ""),
                                   textInput("text", "Pealkirjas sisalduv tekst", value = ""),
                                   tags$i(textOutput("raamatuid"))
                                 ),
                                box(
                                 width = 12,
                                 h3("Graafikul kuvatavad raamatud tabelina"),
                                 dataTableOutput("table_main")
                               )
                            ),
                       ),
                       tabItem("graafik2",
                               fluidRow(
                                 box(
                                   width = 12,
                                   h2("Ülevaade andmeteaduse raamatutest Amazonis"),
                                   p("Siit saad teada, ",tags$b("palju Amazonis müüdavaid andmeteaduse raamatuid on iga aasta avaldatud.")),
                                   p("Joonis näitab üldist trendi. Tegelikult arvud on teistsugused, kuna andmestik on kogutud veebikoorimise teel ehk pole Amazoni ametlik info ja lisaks on eemaldatud puuduvate väärtustega read. Seega on tegelikult raamatuid rohkem.")
                                 )
                               ),
                               fluidRow(
                                 box(
                                   width = 12,
                                   tags$br(),
                                   plotlyOutput("plot2")
                                 )
                               )  
                       ),
                       tabItem("graafik3",
                               fluidRow(
                                 box(
                                   width = 12,
                                   h2("Ülevaade andmeteaduse raamatutest Amazonis"),
                                   p("Siit saad teada, ",tags$b("millised on Amazonis müüdavate andmeteaduse raamatute suurimad väljaandjad.")," Graafikul on alates 10 raamatuga väljaanded."),
                                   p("Tegelikult arvud on teistsugused, kuna andmestik on kogutud veebikoorimise teel ehk pole Amazoni ametlik info ja lisaks on eemaldatud puuduvate väärtustega read. Seega on tegelikult raamatuid rohkem.")
                                 )
                               ),
                               fluidRow(
                                 box(
                                   width = 12,
                                   tags$br(),
                                   plotlyOutput("plot3")
                                 )
                               )  
                       ),
                       tabItem("andmestik",
                               fluidRow(
                                 box(
                                   width = 12,
                                   h1("Andmestik"),
                                   h2("Andmestiku kirjeldus"),
                                   "Andmestik on ", tags$b("Amazonis müüdavate andmeteaduse raamatute")," kohta. Andmed pärinevad Kaggle-i", tags$a(href="https://www.kaggle.com/datasets/die9origephit/amazon-data-science-books","\'Amazon Data Science Books Dataset\'")," andmestikust. Andmed on kogutud Amazoni veebilehe koorimise teel. Andmed on kogutud aastal 2022."
                                 ),
                                 box(id = "t66tlemine",
                                   h2("Andmete töötlemise protsess"),
                                   tags$b("Originaalandmestikus")," on ", tags$b(946)," raamatut ja ", tags$b(18)," veergu. Pärast andmete puhastamist ja teisendusi ", tags$b("jäi alles 813")," raamatut ning ",tags$b("8"),
                                   " huvipakkuvat veergu. Puhastamine sisaldas eestkätt puuduolevate väärtustega ridade eemaldamist. Peamine andmete teisendamine sisaldas avaldamisaasta ja väljaande nime eraldamist ühest veerust.",
                                   tags$br(),
                                   tags$br(),
                                   img(src='flow.png', align = "left", style = "max-width: 100%; width: 100%; height: auto"),
                                   tags$br(),
                                   tags$br(),
                                   tags$br()
                                 ),
                                 box(id = "tulemus",
                                   h2("Peamine tulemus"),
                                   "Projekti peamine kasulikkus seisneb ", tags$b("võimaluses avastada enda jaoks seni tundmatuid, kuid kõrgelt hinnatud andmeteaduse raamatuid"),". Lisaks tuli andmeanalüüsist välja, et pärast aastat 2015 on andmeteaduse raamatute avaldamine hüppeliselt kasvanud. Samuti nähtus, et kõige rohkem andmeteaduse raamatuid annab välja Packt Publishing.",
                                   tags$br(),
                                   tags$br(),
                                   img(src='98.png', align = "left", style = "max-width: 100%; width: 100%; height: auto"),
                                   tags$i("Pilt pärineb Pexelsist ja statistika põhineb lõplikul andmestikul."),
                                   tags$br(),
                                   tags$br(),
                                   "Lõplikus andmestikus on 98 väga kõrge keskmise hinnanguga raamatut, seega avastamisrõõmu jätkub."
                                 ),
                                 box("loplik_andmestik",
                                   width = 12,
                                   h2("Lõplik andmestik"),
                                   dataTableOutput("table")
                                 )
                               )
                       ),
                       tabItem(
                         "seletuskiri",
                         fluidRow(
                           box(
                             width = 12,
                             h1("Seletuskiri"),
                             
                             h2("Üldised printsiibid disainivalikutes"),
                             "Disainis on püütud hoida ", tags$b("minimalismi.")," Eemaldatud on üleliigsed elemendid näiteks nagu",tags$i(" box solidHeader."),
                             
                             h2("Fondid"),
                             "Kasutatud on fonte ", tags$b("Source Sans Pro, Arial, Lato ja Arvo.")," Enim on kasutatud Source Sans Prod, seda nii näidikutahvli kehatekstis kui ka pealkirjades. Graafikutel on lisaks kasutatud Ariali ning piltidel Arvot ja Latot.",
                             
                             h2("Värvid"),
                             "Põhilised värvid on: ", tags$b("eri hallid toonid, must, valge ja esiletoomiseks oranž."),"Piltidel on lisaks pruunikaid toone ning mulligraafiku jaoks on kasutusel kollakad-sinakad-rohelised toonid, kuna seal oli vajalik üleminek ühest heledast rõõmsast värvist tumedamaks.",
                             tags$br(),
                             tags$br(),
                             img(src='palett.png', align = "left", style = "max-width: 100%; width: 100%; height: auto")
                           )
                         )
                       )
                     )
                   )  
)
