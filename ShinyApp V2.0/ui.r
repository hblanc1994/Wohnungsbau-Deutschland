library(shiny)
library(shinythemes)
library(rio)
shinyUI ( fluidPage(theme = shinytheme("darkly"), navbarPage
         ("Entwicklung des Wohnungsbaus in Deutschland",
           #Deckblatt mit Bild und Einleitung zu der Auswertung
           navbarMenu("Einleitung",
                      tabPanel("Deckblatt",tags$img(src="Bau.png")),
                      tabPanel("Ziel und Infos zu den Daten", 
                               h2(("Wie steht es um die Bausituation in Deutschland?"),
                                  style="color:white"),
                               
                               h4("- Der Wohnraum wird knapp und die Mieten steigen"),
                               
                               
                               h4("- Daher lohnt sich ein Blick auf die Entwicklung neuer Wohnungsbauten"),
                               
                               
                               h4("- Wie entwickelt sich die Anzahl erteilter Baugenehmigungen?"),
                               tags$hr(style="border-color: black;"),
                              
                               h2(("Infos zu den Daten:"),style="color:white"),
                              
                               h4("- Analysierter Datensatz: Baugenehmigungen in Deutschland von Anfang 2003 bis Ende 2018"),
                               h4("- Jahr 2019 bis Juni als vorlaeufige Daten im Anhang"),
                               h4("- Die Daten werden in csv. Format zur Verfuegung gestellt. Dieses beinhaltet die Anzahl der erteilten
                                        Baugenehmigungen, veranschlagte Baukosten und mehr. 
                                           Ausserdem wird nach Monat und Jahr unterteilt."),
                               h4("- Die Daten sind verlaesslich, da sie vom statistischen 
                                        Bundesamt aufgezeichnet wurden")),
                      tabPanel("Datenquelle", img(src="/Datenquelle.png"),
                               tags$hr(style="border-color: black;"),
                               a(href ="https://www-genesis.destatis.de/genesis/online/data;sid=F3FF4A1D8C70F8375242861A9A2483A8.GO_1_1?operation=abruftabelleAbrufen&selectionname=31111-0002&levelindex=0&levelid=1567934647704&index=2", h2("Link dazu: Klicken Sie hier"),style="color:white")),   
           #Anzeigen der ersten sechs und die letzten sechs Zeilen der Tabelle         
          tabPanel("Beispieldatensatz", h1(("Die ersten 6 Zeilen des Datensatzes werden ausgegeben"),style="color:white"), tableOutput("data3"),
                   tags$hr(style="border-color: black;"),
                   br(),
                   h1(("Die letzten 6 Zeilen des Datensatzes werden ausgegeben"),style="color:white"), tableOutput("data4"))),
           
#------------------------------------------------------------------------------------------------------------------------------- 
                     
           #Eine univariante Analyse wird durchgefuehrt     
          navbarMenu("Erster Blick auf die Daten",tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
          ),
            tabPanel("Univariate Analyse", 
                            
                             
                              h3(("Datenbasis ist die monatliche Entwicklung der erteilten Baugenehmigungen in Deutschland
                                          (siehe Anhang) "),style="color:white"),
                             verbatimTextOutput("sum"),
                   tags$hr(style="border-color: black;"),
                   h3(("Boxplot"),style="color:white"),
                              fluidRow(
                                column(5, plotOutput("boxplot"))),
                   tags$hr(style="border-color: black;"),
                                h3(("Ein deutlicher Unterschied zwischen den einzelnen Monaten von knapp 9500 bis 30600 Baugenehmigungen. Weitere Analysen zur 
                                            detailierten Betrachtung notwendig (siehe weitere Tabs)"),style="color:white"))),
                              
                                                 
#------------------------------------------------------------------------------------------------------------------------------ 
           
           #Es werden die Entwicklung der Baugenehmigungen mit Arima dargestellt
           navbarMenu("Gesamtentwicklung",
                    tabPanel("Zeitreihe",
                    h1(("Entwicklung der erteilten Baugenehmigungen (Wohnungen) von Anfang 2003 bis Ende 2018"),style="color:white"), plotOutput ("zeitreihe"),
                    tags$hr(style="border-color: black;"),
                    h3(("Der blaue Teil der Grafik ist die Arima"),style="color:blue"),
                    helpText(("- blaue Linie: prognostizierte Entwicklung neu erteilter Baugenehmigungen"),style="color:white"),
                    helpText(("- dunkle Schattierung: Vertrauensintervall mit Wahrscheinlichkeit von 80% "),style="color:white"),
                    helpText(("- helle Schattierung:  Vertrauensintervall mit Wahrscheinlichkeit von 95% "),style="color:white"),
                    br(),
                    h4(("- Sinkende Entwicklung der Anzahl Baugenehmigungen bis circa 2009. 
                           Dann konstanter Anstieg bis 2018"),style="color:white"),
          
                    h4(("- Das Minimum um 2009 kann als Auswirkung der weltweilten Finanz- und Immobilienkrise interpretiert werden"),style="color:white")),
                    
           
                #Es wird die Autokorrelation ausgegeben    
                    tabPanel("Autokorrelation",
                    plotOutput("autokorr"),
                    tags$hr(style="border-color: black;"),
                    h3(("Autokorrelation zeigt:"),style="color:white"),
                    h4(("- Es besteht ein gewisser Zusammenhang der Werte zum jeweiligen Vormonat (siehe Lag 1.0)"),
                       style="color:white"),
                    h4(("- Umso naeher der Wert auf der Y-Achse an 1.0 , umso staerker der Zusammenhang"),style="color:white"))),
           
#--------------------------------------------------------------------------------------------------------------------------------   
           
           #Es werden jaehrliche Analysen dargestellt
           navbarMenu("Jahres-Betrachtung",
                      #Es wird ein Histogramm geplotet, bei dem man die Breite manuell einstellen kann
                      tabPanel("Histogramm",h1(("Haeufigkeitsverteilung der Baugenehmigungen: Darstellung Histogramm"),style="color:white"),
                               h4(("Datenbasis ist die jaehrliche Entwicklung der erteilten Baugenehmigungen in Deutschland von 2003 bis 2018"),style="color:white"),
                               br(),
                               sliderInput("bins", "Anzahl der Klassen", min = 0, max =20, value = 6),plotOutput ("distplot"),
                               tags$hr(style="border-color: black;"),
                               h4(("- Der Mittelwert ist als rote Linie eingezeichnet und liegt zwischen 200.000 und 250.000 Baugenehmigungen"), style="color:white")),
                      
                      # Es werden verschiedene statistische Berechnungen durchgefuehrt
                      
                      tabPanel("Werte mit Berechnungen",
                               h3(("Datenbasis ist die jaehrliche Entwicklung der erteilten Baugenehmigungen in Deutschland von 2003 bis 2018"),
                                  style="color:white"),
                               
                               verbatimTextOutput("sum1"),
                               tags$hr(style="border-color: black;"),
                               h3(("Boxplot"),style="color:white"),
                               fluidRow(
                                 column(5,
                                   plotOutput("boxplot2"))),
                               tags$hr(style="border-color: black;"),
                               h3(("Entwicklung der Gesamtkosten der Bauwerke"),style="color:white"),
                               fluidRow(
                                 column(10,
                                        plotOutput("boxplot3"))),
                               tags$hr(style="border-color: black;"),
                               h3(("Darstellung der Daten, aus denen Boxplot und die statistischen Berechnungen durchgefuehrt
                                            wurden in Tabellenform"),style="color:white"),
                               verbatimTextOutput("jahreInTabelle"), verbatimTextOutput("jahreInTabelle2")),
                      # Es werden verschiedene naehere Analysen zu den Jahren gemacht, es kann nach Jahr gefiltert werden dazu gibt es verschiedene 
                      # Berechnungen und ein Boxplot
                      
                       tabPanel("naehere Betrachtung der Werte mit Berechnungen",
                                h1(("Die Daten nach den einzelnen Jahren gefiltert"),style="color:white"),
                                selectInput("Jahreszahl", "Waehle die Jahreszahl", 
                                            choices = c("2003"=2003,
                                                        "2004"=2004,"2005"=2005,"2006"=2006,
                                                        "2007"=2007,"2008"=2008,"2009"=2009,
                                                        "2010"=2010,"2011"=2011,"2012"=2012,
                                                        "2013"=2013,"2014"=2014,"2015"=2015,
                                                        "2016"=2016,"2017"=2017,"2018"=2018)),
                                radioButtons("Spalten", "Waehle eine Spalte", choices = c("alle Spalten" = 1 ,"Baukosten" = 8,
                                                                                          "Anzahl Baugenehmigungen" = 5),
                                             selected = 1),
                                
                                
                                verbatimTextOutput("baugenehmigungJahr"), h3(("Summe der gefilterten Spalten"),style="color:white"),
                                textOutput("summe"),
                                tags$hr(style="border-color: black;"),
                                br(),
                                h3(("Verschiedene statistische Berechnungen"),style="color:white"),
                                verbatimTextOutput("sumfilterText1"),
                                verbatimTextOutput("sumfilter"),
                                tags$hr(style="border-color: black;"),
                                br(),
                                h3(("Boxplot"),style="color:white"),
                                verbatimTextOutput("sumfilterplottext"),
                                fluidRow(
                                  column(5,
                                plotOutput("sumfilterplot")))),
                      
                                # Es koennen Zeitreihen der monatlichen Entwicklung ueber die Jahre miteinander verglichen werden
                                tabPanel("Vergleich verschiedener Jahre", h1(("Vergleich der monatlichen Entwicklung verschiedener Jahre"),
                                                                             style="color:white"),
                                         selectInput("Anfang", "Waehle das Anfangsjahr", 
                                                     choices = c("2003"=2003,
                                                                  "2004"=2004,"2005"=2005,"2006"=2006,
                                                                  "2007"=2007,"2008"=2008,"2009"=2009,
                                                                  "2010"=2010,"2011"=2011,"2012"=2012,
                                                                  "2013"=2013,"2014"=2014,"2015"=2015,
                                                                  "2016"=2016,"2017"=2017,"2018"=2018), selected = 2015),
                                         selectInput("Ende", "Waehle das Endjahr", 
                                                     choices = c("2003"=2003,
                                                                 "2004"=2004,"2005"=2005,"2006"=2006,
                                                                 "2007"=2007,"2008"=2008,"2009"=2009,
                                                                 "2010"=2010,"2011"=2011,"2012"=2012,
                                                                 "2013"=2013,"2014"=2014,"2015"=2015,
                                                                 "2016"=2016,"2017"=2017,"2018"=2018), selected = 2018),   
                                         plotOutput ("zeitreihe5jahre"))),
                                        
                      
#--------------------------------------------------------------------------------------------------------------------------------           
           
           # Es wird verglichen wie sich die einzelnen Monate ueber die Jahre hinweg entwickelt haben
          
          navbarMenu("Monats-Betrachtung",
                    
                     tabPanel("Entwicklung der Monate", h1(("Trend der einzelnen Monate von Anfang 2003 - Ende 2018"),style="color:white"), 
                              plotOutput("monatsvergleich"),
                              h3(("Entwicklung eines Monats ueber die Jahre und Anzeige des Mittelwertes"),style="color:white")),
                     
                     # Es wird ein Histogramm angezeigt und dann die Anzahl der 
                     # Baugenehmigungen im selektierten Monat ueber die Jahre hinweg absteigend sortiert
                     
                     tabPanel("Histogramm der Monate",
                              h1(("Haeufigkeitsverteilung der Baugenehmigungen: Darstellung Histogramm"),style="color:white"),
                              h4(("Datenbasis ist die Anzahl der Baugenehmigungen pro Monat aggregiert von Anfang 2003 bis Ende 2018"),style="color:white"),
                              selectInput("Monate2", "Waehle den Monat", choices = c("Januar","Februar","Maerz","April","Mai","Juni","Juli","August",
                                                                                     "September", "Oktober", "November", "Dezember")),
                              
                              plotOutput("distplot2"),
                              tags$hr(style="border-color: black;"),
                              h1(("Die Anzahl der Baugenehmigungen im selektierten Monat ueber die Jahre hinweg absteigend sortiert"),
                                 style="color:white"),
                              tableOutput("tabelledistplot2")),
                     
           #Die Daten koennen nach Monat und Spalte gefiltert werden. Ausserdem wird dabei die Summe der Baugenehmigungen angezeigt
           #von diesem Filter statistische berechnungen und ein Boxplot
           
           tabPanel("naehere Betrachtung der Werte mit Berechnungen", h1(("Die Daten nach den einzelnen Monaten gefiltert"),
                                                                         style="color:white"),
                    selectInput("Monate", "Waehle den Monat", choices = c("Januar","Februar","Maerz","April","Mai","Juni","Juli","August",
                                                                          "September", "Oktober", "November", "Dezember")),
                    radioButtons("Spalten2", "Waehle eine Spalte", choices = c("alle Spalten" = 4 ,"Anzahl Baugehnehmigungen" = 5,
                                                                              "Gesamtkosten Bauwerke" = 8),
                                                                               selected = 4),
                    verbatimTextOutput("baugehnehmigungenMonat"), h3(("Summe der gefilterten Spalten"),style="color:white"),
                    textOutput("summe2"),
                    tags$hr(style="border-color: black;"),
                    br(),
                    h3(("Verschiedene statistische Berechnungen"),style="color:white"),
                    verbatimTextOutput("sumfilterText3"),
                    verbatimTextOutput("sumfilter3"),
                    verbatimTextOutput("sumfilterText4"),
                    verbatimTextOutput("sumfilter4"),
                    tags$hr(style="border-color: black;"),
                    br(),
                    h3(("Boxplot"),style="color:white"),
                    verbatimTextOutput("sumfilterplottext1"),
                    fluidRow(
                      column(5,
                    
                    plotOutput("sumfilterplot1"))))
            
                    ), 
           
#---------------------------------------------------------------------------------------------------------------------
           
           #In diesem Tab wird die durchschnittliche monatliche Entwicklung ueber die Jahre angezeigt
           navbarMenu("monatliche durchschn. Entwicklung",
            
                      # Die Mittelwerte der einzelnen Monate werden in einer Zeitreihe dargestellt, ausserdem die Standardabweichung
                      tabPanel("Planung mit Mittelwert und Standardabweichung", h1(("Vergleich Mittelwerte und Standardabweichung der Monate"),
                                                                                   style="color:white"),
                               h5(("- Bereich zwischen roter und gruener Linie ist die Anzahl der Baugenehmigungen 
                                   mit der gerechnet werden kann"),style ="color:white"),
                               plotOutput("zeitreiheMittelwerte"),
                               h2(("Legende der Grafik:"),style="color:white"),
                               h4(("- Standardabweichung zu dem Mittelwert addiert"),style="color:red"),
                               h4(("- Mittelwert"),style="color:blue"),
                               h4(("- Standardabweichung von dem Mittelwert subtrahiert"),style="color:green"),
                               
                               br(),
                               tags$hr(style="border-color: black;"),
                               radioButtons("Sortierung", "Sortierung waehlen", 
                                            choices = c("Von Jan. bis Dez sortiert" = 5,
                                                        "nach Groesse absteigend sortiert" = 4),selected = 5),
                               
                               verbatimTextOutput("filtermon"),
                      tableOutput("tabellemittelwerte"),
                      tags$hr(style="border-color: grey;"),
                      br(),
                      verbatimTextOutput("filtermon1"),
                      tableOutput("standardabw"),
                      tags$hr(style="border-color: grey;"),
                      br(),
                      verbatimTextOutput("filtermon2"),
                      tableOutput("anteil")
                      )),
           
#-------------------------------------------------------------------------------------------------------------------------           
           
           
           #Die Ergebnisse der App werden in einem Fazit zusammengefasst
           tabPanel("Fazit",
                    h2(("Fazit:"),style="color:white"),
                    h5("- bis 2009 deutlich sinkende Anzahl der Baugenehmigungen. Das liegt vor allem an der weltweiten Finanz- und Immobilienkrise
                    2008-2009"),
                    h5("- Ab 2010 ist wieder ein starker Anstieg erkennbar"),
                    h5("- Ausblick: weiter steigende Anzahl Baugenehmigungen aufgrund der aktuellen Niedrigzins-Politik"),
                    h5("- Gleichzeitig erhoehen sich auch die Baukosten fuer Neubauten"),
                    h5("- 2018 wurden etwa 300.000 Baugenehmigungen erteilt, das Ziel der Bundesregierung von 375.000 Wohnungen pro Jahr ist immer noch nicht erreicht"),
                    h5("- Der Wohnungsraum in Deutschland wird auch in Zukunft knapp sein, vor allem in Staedten. Laut einer Berechnung des Instituts fuer Wirtschaft hat sich die Anzahl fehlender Wohnungen auf 400.000 erhoeht"),
                    h5("- geringe Schwankungen der Anzahl der Baugenehmigungen beim Vergleich der Monate innerhalb eines Jahres"),
                    h5("- Ein Knick im Monat November ist auffaellig"),
                    tags$hr(style="border-color: black;"),
                    br()
                    ),
           
#-----------------------------------------------------------------------------------------------------------------------------
           
                      
                      # Es werden verschiedene Daten und Datenstrukturen, die den Lesefluss stoeren, in den Anhang verschoben
                      navbarMenu("Anhang",
                                 tabPanel("Rohdaten", h1(("Rohdaten in Tabellenform"),style="color:white"),tableOutput("data1")),
                                 tabPanel("Vorlaeufige Baugenehmigungen 2019", h1(("Vorlaeufige Anzahl Baugenehmigungen 2019"),style="color:white"),
                                          tableOutput("datav5")),
                                  tabPanel("Datenstruktur", h1(("Datenstruktur der Grunddaten"),style="color:white"), 
                                          h3(("Datenbasis ist die jaehrliche Entwicklung der Anzahl Baugenehmigungen von Anfang 2003 bis Ende 2018."),style="color:white"),
                                             verbatimTextOutput("structure"))
                              
                                       ) )))   
                                 
                                 
           
           
           
