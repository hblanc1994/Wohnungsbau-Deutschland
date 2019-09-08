library(shiny)
library(shinythemes)
shinyServer(function(input,output, session){
  
  #Die Daten werden eingelesen und die Variable x(jaehrliche Entwicklung der Anzahl Baugenehmigungen von Anfang 2003 bis Ende 2018) wird
  #erstellt. Ausserdem wird die Library Forecast geladen
  setwd("C:/Arbeitsordner")
  data <- read.csv2 ("Baugenehmigungen_2003-2019_Gesamt.csv")
  data[, 4] <- as.numeric(as.character( data[, 4] ))
  data[, 5] <- as.numeric(as.character( data[, 5] ))
  data[, 6] <- as.numeric(as.character( data[, 6] ))
  data[, 7] <- as.numeric(as.character( data[, 7] ))
  data[, 8] <- as.numeric(as.character( data[, 8] ))
  
  x <- c(sum(subset(data, Jahr == "2003")[,5]),sum(subset(data, Jahr == "2004")[,5]),
         sum(subset(data, Jahr == "2005")[,5]),sum(subset(data, Jahr == "2006")[,5]),sum(subset(data, Jahr == "2007")[,5]),
         sum(subset(data, Jahr == "2008")[,5]),sum(subset(data, Jahr == "2009")[,5]),sum(subset(data, Jahr == "2010")[,5]),
         sum(subset(data, Jahr == "2011")[,5]),sum(subset(data, Jahr == "2012")[,5]),sum(subset(data, Jahr == "2013")[,5]),
         sum(subset(data, Jahr == "2014")[,5]),sum(subset(data, Jahr == "2015")[,5]),sum(subset(data, Jahr == "2016")[,5]),
         sum(subset(data, Jahr == "2017")[,5]),sum(subset(data, Jahr == "2018")[,5]),sum(subset(data, Jahr == "2019")[,5]))
         
         
  library(forecast)
  
#---------------------------------------------------------------------------------------------------  
  #Panel Einleitung
  #Die ersten 5 Zeilen des Datensatzes werden ausgegeben
  output$data3 <- renderTable({
    head(data)
  })
  #Panel Einleitung
  #Die letzten 5 Zeilen des Datensatzes werden ausgegeben
  output$data4 <- renderTable({
    tail(data)
  })
  
#--------------------------------------------------------------------------------------------------------  
  
  #Panel Erster Blick auf die Daten
  #Hier werden verschiedene statistische Werte wie der Median, der groesste und kleinste Wert und die Quantiele berechnet.
  #Datenbasis sind die Rohdaten fuer eine univariante Analyse
  output$sum <- renderPrint({
    setwd("C:/Arbeitsordner")
    data <- read.csv2 ("Baugenehmigungen_2003-2019_Gesamt.csv")
   summary(data$Wohnungen)
   
  })
  #Panel Erster Blick auf die Daten
  # Nun wird ein Boxplot zu den sum Daten erstellt, um das ganze zu visualisieren.
    output$boxplot <- renderPlot({
      boxplot(data$Wohnungen, ylab = "Anzahl Baugenehmigungen", col ="deepskyblue4")
    
  })
    
#----------------------------------------------------------------------------------------------------------------    
    #Panel jaehrliche Betrachtung
  #Es werden verschiedene statistische Berechnungen fuer die Entwicklung ueber die Jahre durchgefuehrt (Min,max,Mittelwert...)
    output$sum1 <- renderPrint ({
      summary(x, ylab = "Anzahl Baugenehmigungen")
    })
  
    #Panel jaehrliche Betrachtung  
  #Zu dieser jaehrlichen Entwicklung wird ein Boxplot erstellt
    output$boxplot2 <- renderPlot({
      boxplot(x,ylab = "Anzahl Baugenehmigungen", col ="deepskyblue4")
      
    })
    
    
    library(ggplot2)
    library(scales)
    output$boxplot3 <- renderPlot({
      ggplot(data, aes(x = Jahr, y = Baukosten)) +
               geom_col(fill = "deepskyblue4") + ylab("Gesamtkosten") +  scale_y_continuous(labels = dollar_format(suffix = "\u20AC", prefix = ""))
      
      
    })
    
    z <- c(sum(subset(data, Jahr == "2003")[,8]),sum(subset(data, Jahr == "2004")[,8]),
           sum(subset(data, Jahr == "2005")[,8]),sum(subset(data, Jahr == "2006")[,8]),sum(subset(data, Jahr == "2007")[,8]),
           sum(subset(data, Jahr == "2008")[,8]),sum(subset(data, Jahr == "2009")[,8]),sum(subset(data, Jahr == "2010")[,8]),
           sum(subset(data, Jahr == "2011")[,8]),sum(subset(data, Jahr == "2012")[,8]),sum(subset(data, Jahr == "2013")[,8]),
           sum(subset(data, Jahr == "2014")[,8]),sum(subset(data, Jahr == "2015")[,8]),sum(subset(data, Jahr == "2016")[,8]),
           sum(subset(data, Jahr == "2017")[,8]),sum(subset(data, Jahr == "2018")[,8]),sum(subset(data, Jahr == "2019")[,8]))
    #Panel jaehrliche Betrachtung   
  #Die jaehrlichen Baugenehmigungen werden aufgezeigt, um den Ausreisser in dem Plot zu erklaeren
    output$jahreInTabelle <- renderPrint ({
      i <- c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)
      d <- matrix(x, nrow = 1, ncol = 16, byrow =FALSE, dimnames = list("Erteilte Baugenehmigungen",i))
      d
    })
    output$jahreInTabelle2 <- renderPrint ({
      i <- c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)
      f <- matrix(z, nrow = 1, ncol = 16, byrow =FALSE, dimnames = list("Gesamtkosten Bauwerke",i))
      f
    })
  
  #----------------------------------------------------------------------------------------------------------
    #Panel jaehrliche Betrachtung
    #Berechnungen fuer die Jahre
    
  #Hier wird berechnet, dass die Daten nach Jahr und Spalte gefiltert werden koennen. Inputs von UI sind Spalten und
  #die Jahreszahl
  output$baugenehmigungJahr <- renderPrint ({
    setwd("C:/Arbeitsordner")
    data <- read.csv2 ("Baugenehmigungen_2003-2019_Gesamt.csv")
    colm <- as.numeric(input$Spalten)
    if (input$Spalten == 1) {
      colm <- as.numeric(input$Spalten)
      subset(data, Jahr == input$Jahreszahl)[,c(1,2,4,5,6,7,8)]
    } else if (input$Spalten == 8) {
      colm <- as.numeric(input$Spalten)
      subset(data, Jahr == input$Jahreszahl)[,c(1,2,8)]
    } else {
      subset(data, Jahr == input$Jahreszahl)[,c(1,2,5)]
    }
    
  })
    
    #Panel jaehrliche Betrachtung
  #Je nachdem wie die Spalten bzw. die Jahreszahlen gefiltert wurden, wird nun eine Summe berechnet und in der App
  #ausgegeben
  output$summe <- renderText ({
    setwd("C:/Arbeitsordner")
    data <- read.csv2 ("Baugenehmigungen_2003-2019_Gesamt.csv") 
    if (input$Spalten == 1) {
      colm <- as.numeric(input$Spalten)
      b <- sum(subset(data, Jahr == input$Jahreszahl)[,5])
      z <- c("Die Summe der erteilten Baugenehmigungen betraegt:", b)
      z
    } else if (input$Spalten == 5) {
      colm <- as.numeric(input$Spalten)
      b <- sum (subset(data, Jahr == input$Jahreszahl)[,5])
      z <- c("Die Summe der erteilten Baugenehmigungen betraegt: ", b)
      z
    } else {
      colm <- as.numeric(input$Spalten)
      b <- sum (subset(data, Jahr == input$Jahreszahl)[,8])
      z <- c("Die Baukosten im ausgewaehlten jahr betrugen: ", b,"Euro")
      z
    } 
    
  })
  #Panel jaehrliche Betrachtung
  #Es werden die Ueberschriften je nachdem was fuer die output$sumfilter funktion eingegeben wird, erstellt
  output$sumfilterText1 <- renderText({
    if (input$Spalten == 1) {
      "Verschiedene statistische Berechnungen fuer die selektierte Jahreszahl und die Anzahl Baugenehmigungen"
    } else if (input$Spalten == 8) {
      "Verschiedene statistische Berechnungen fuer die selektierte Jahreszahl und die Baukosten"
    } else {
      "Verschiedene statistische Berechnungen fuer die selektierte Jahreszahl und die Anzahl Baugenehmigungen"
    }
  })
  
  
  #Panel jaehrliche Betrachtung
  #Je nachdem wie die Spalten bzw. die Jahreszahlen gefiltert wurden, werden das verschiedene statistische Berechnungen (Min, max...
  #erzeugt und in der App
  #ausgegeben
  output$sumfilter <- renderPrint ({
    setwd("C:/Arbeitsordner")
    data <- read.csv2 ("Baugenehmigungen_2003-2019_Gesamt.csv") 
    if (input$Spalten == 1) {
      colm <- as.numeric(input$Spalten)
      e <- subset(data, Jahr == input$Jahreszahl)[,5]
      f <- subset(data, Jahr == input$Jahreszahl)[,8]
      g <- summary(e)
      g
      
    } else if (input$Spalten == 8) {
      colm <- as.numeric(input$Spalten)
      e <- subset(data, Jahr == input$Jahreszahl)[,8]
      g <- summary(e)
      g
    } else {
      colm <- as.numeric(input$Spalten)
      e <- subset(data, Jahr == input$Jahreszahl)[,5]
      g <- summary(e)
      g
    } 
    
  })

  #Panel jaehrliche Betrachtung
    #Der Text zu den einzelnen Boxplots
    output$sumfilterplottext <- renderText({
      if (input$Spalten == 1) {
        "Boxplot Anzahl Baugenehmigungen"
      } else if  (input$Spalten ==8) {
        "Boxplot Gesamtkosten Bauwerke"
      } else {
        "Boxplot Anzahl Baugenehmigungen"
      }
  })
    #Panel jaehrliche Betrachtung  
  # Die gefilterten Daten der Baugenehmigungen/Baukosten werden in einem Boxplot visualisiert
   output$sumfilterplot <- renderPlot ({
     setwd("C:/Arbeitsordner")
     data <- read.csv2 ("Baugenehmigungen_2003-2019_Gesamt.csv") 
     if (input$Spalten == 1) {
       e2 <- subset(data, Jahr == input$Jahreszahl)[,5]
       boxplot(e2)
     } else if  (input$Spalten ==8) {
       e2 <- subset(data, Jahr == input$Jahreszahl)[,8]
       boxplot(e2, col ="deepskyblue4")
     } else
       e2 <- subset(data, Jahr == input$Jahreszahl)[,5]
       boxplot(e2, col ="deepskyblue4")

   })
   
   
  
#-------------------------------------------------------------------------------------------------
   #Panel monatliche Betrachtung
   #Berechnung fuer die Monate
  
  #Hier wird berechnet, dass die Daten nach Monat und Spalte gefiltert werden koennen. Inputs von UI sind Spalten und
  #die Jahreszahl
  #Je nach Filter wird die Tabelle angezeigt 
  output$baugehnehmigungenMonat <- renderPrint ({
    setwd("C:/Arbeitsordner")
    data <- read.csv2 ("Baugenehmigungen_2003-2019_Gesamt.csv")
    colm <- as.numeric(input$Spalten2)
    if (input$Spalten2 == 4) {
      colm <- as.numeric(input$Spalten2)
      subset(data, Monate == input$Monate)[,c(1,2,4,5,6,7,8)]
    } else if (input$Spalten2 == 5) {
      colm <- as.numeric(input$Spalten2)
      subset(data, Monate == input$Monate)[,c(1,2,5)]
    } else {
      subset(data, Monate == input$Monate)[,c(1,2,8)]
    }
  
  })
 
   #Panel monatliche Betrachtung
  #Je nachdem wie die Spalten bzw. der Monat gefiltert wird, wird nun eine Summe berechnet und in der App
  #ausgegeben
  output$summe2 <- renderText ({
    setwd("C:/Arbeitsordner")
    data <- read.csv2 ("Baugenehmigungen_2003-2019_Gesamt.csv") 
    if (input$Spalten2 == 4) {
      colm <- as.numeric(input$Spalten2)
      b <- sum(subset(data, Monate == input$Monate)[,5])
      y <- sum(subset(data, Monate == input$Monate)[,8])
      z <- c("Die Summe der Baugenehmigungen ist: ", b,"; ","Die Summe der Baukosten der Bauwerke ist: ", y, "Euro")
      z
    } else if (input$Spalten2 == 5) {
      colm <- as.numeric(input$Spalten2)
      b <- sum (subset(data, Monate == input$Monate)[,5])
      z <- c("Die Summe der Baugenehmigungen ist: ", b)
      z
    } else {
      colm <- as.numeric(input$Spalten2)
      b <- sum (subset(data, Monate == input$Monate)[,8])
      z <- c("Die Summe  der Baukosten der Bauwerke ist: ", b, "Euro")
      z
    } 
    
  })
  
  #Panel monatliche Betrachtung
  #Es werden die Ueberschriften je nachdem was fuer die output$sumfilter3 funktion eingegeben wird, erstellt
  output$sumfilterText3 <- renderText({
    if (input$Spalten2 == 4) {
      "Verschiedene statistische Berechnungen fuer den selektierten Monat und die Anzahl Baugenehmigungen"
    } else if (input$Spalten2 == 5) {
      "Verschiedene statistische Berechnungen fuer den selektierten Monat und die Anzahl Baugenehmigungen"
    } else {
      "Verschiedene statistische Berechnungen fuer den selektierten Monat und die Baukosten der Bauwerke"
    }
  })
  
  #Panel monatliche Betrachtung
  #Je nachdem wie die Spalten bzw. die Monate gefiltert wurden, werden verschiedene statistische Berechnungen(max, min...
  #berechnet und in der App
  #ausgegeben
  output$sumfilter3 <- renderPrint ({
    
    if (input$Spalten2 == 4) {
      e1 <- subset(data, Monate == input$Monate)[,5]
      m1 <- subset(data, Monate == input$Monate)[,8]
      g1 <- summary(e1)
      g1
    } else if (input$Spalten2 == 5) {
      e1 <- subset(data, Monate == input$Monate)[,5]
      g1 <- summary(e1)
      g1
    } 
    else {
      e1 <- subset(data, Monate == input$Monate)[,8]
      g1 <- summary(e1)
      g1
      
    }
  })
  
  #Panel monatliche Betrachtung
  #Es wird die Ueberschrift je nachdem was fuer die output$sumfilter4 funktion eingegeben wird, erstellt
  output$sumfilterText4 <- renderText({
    if (input$Spalten2 == 4) {
      "Verschiedene statistische Berechnungen fuer den selektierten Monat und die die Baukosten der Bauwerke"
    }
  })
  
  #Panel monatliche Betrachtung
  #Je nachdem wie die Spalten bzw. die Monate gefiltert wurden, werden verschiedene statistische Berechnungen((summary))min,max...
  #berechnet und in der App
  #ausgegeben
  output$sumfilter4 <- renderPrint ({
    
    if (input$Spalten2 == 4) {
      e1 <- subset(data, Monate == input$Monate)[,5]
      m1 <- subset(data, Monate == input$Monate)[,8]
      n1 <- summary(m1)
      n1
      
    }
  })
  
  #Panel monatliche Betrachtung
  #Der Text zu den einzelnen Boxplots
  output$sumfilterplottext1 <- renderText({
    if (input$Spalten2 == 4) {
      "Boxplot Anzahl Baugenehmigungen"
    } else if  (input$Spalten2 == 5) {
      "Boxplot Anzahl Baugenehmigungen"
    } else {
      "Boxplot Gesamkosten Bauwerke"
    }
  })
  
  #Panel monatliche Betrachtung
  # Die gefilterten Daten der Baugenehmigungen werden in einem Boxplot visualisiert
  output$sumfilterplot1 <- renderPlot ({
    setwd("C:/Arbeitsordner")
    data <- read.csv2 ("Baugenehmigungen_2003-2019_Gesamt.csv") 
    if (input$Spalten2 == 4) {
      e2 <- subset(data, Monate == input$Monate)[,5]
      boxplot(e2)
    } else if  (input$Spalten2 == 5) {
      e2 <- subset(data, Monate == input$Monate)[,5]
      boxplot(e2)
    } else
      e2 <- subset(data, Monate == input$Monate)[,8]
    boxplot(e2,col ="deepskyblue4")
  })
  
  #Panel monatliche Betrachtung
  #Histogramm fuer die einzelnen Monate
  output$distplot2 <- renderPlot({
    e2 <- subset(data, Monate == input$Monate2)[,5]
    hist(e2, col = "deepskyblue4", border = "white", ylab ="Haeufigkeiten", xlab ="Anzahl der monatlichen Baugenehmigungen",
         main = "Histogramm der erteilten Baugenehmigungen in Deutschland")
  })
  
  #Panel monatliche Betrachtung
  #Es wird eine Tabelle fuer den gefilterten Monat erstellt und diese absteigend sortiert in einem Dataframe ausgegeben
  output$tabelledistplot2 <- renderTable({
    a2003 <- subset(data, Monate == input$Monate2)[1,5]
    a2004 <- subset(data, Monate == input$Monate2)[2,5]
    a2005 <- subset(data, Monate == input$Monate2)[3,5]
    a2006 <- subset(data, Monate == input$Monate2)[4,5]
    a2007 <- subset(data, Monate == input$Monate2)[5,5]
    a2008 <- subset(data, Monate == input$Monate2)[6,5]
    a2009 <- subset(data, Monate == input$Monate2)[7,5]
    a2010 <- subset(data, Monate == input$Monate2)[8,5]
    a2011 <- subset(data, Monate == input$Monate2)[9,5]
    a2012 <- subset(data, Monate == input$Monate2)[10,5]
    a2013 <- subset(data, Monate == input$Monate2)[11,5]
    a2014 <- subset(data, Monate == input$Monate2)[12,5]
    a2015 <- subset(data, Monate == input$Monate2)[13,5]
    a2016 <- subset(data, Monate == input$Monate2)[14,5]
    a2017 <- subset(data, Monate == input$Monate2)[15,5]
    a2018 <- subset(data, Monate == input$Monate2)[16,5]
    
    e4 <- data.frame(
                     a2003,a2004,a2005,a2006,a2007,a2008,a2009,a2010,a2011,a2012,a2013,a2014,a2015,a2016,a2017,a2018)
    e4
   
    colnames(e4) <- c("2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")
   sort(e4, decreasing = TRUE)
  })
  #-------------------------------------------------------------------------------
  #Panel jaehrliche Betrachtung
  
  #Hier wird das Histogramm berechnet: Datengrundlage ist die Anzahl Baugehnehmigungen der einzelnen Jahre
  output$distplot <- renderPlot({
    Baugenehmigungen <- c(sum(subset(data, Jahr == "2003")[,5]),sum(subset(data, Jahr == "2004")[,5]),
                          sum(subset(data, Jahr == "2005")[,5]),sum(subset(data, Jahr == "2006")[,5]),sum(subset(data, Jahr == "2007")[,5]),
                          sum(subset(data, Jahr == "2008")[,5]),sum(subset(data, Jahr == "2009")[,5]),sum(subset(data, Jahr == "2010")[,5]),
                          sum(subset(data, Jahr == "2011")[,5]),sum(subset(data, Jahr == "2012")[,5]),sum(subset(data, Jahr == "2013")[,5]),
                          sum(subset(data, Jahr == "2014")[,5]),sum(subset(data, Jahr == "2015")[,5]),sum(subset(data, Jahr == "2016")[,5]),
                          sum(subset(data, Jahr == "2017")[,5]),sum(subset(data, Jahr == "2018")[,5]),sum(subset(data, Jahr == "2019")[,5]))
                          
    
    hist(Baugenehmigungen, breaks = input$bins, col = "deepskyblue4", border = "white", ylab = "Haeufigkeit",xlab = "Anzahl der jaehrlichen Baugenehmigungen",
         main = "Histogramm der Baugenehmigungen",
         xlim = c(100000, 400000))
    abline(v =mean(Baugenehmigungen), col = "red")
  })
  
  #-----------------------------------------------------------------------------------------------------------
  #Verschiedene Zeitreihen
  
  
  #Panel Gesamtentwicklung
  #Zuerst wird eine Zeitreihe ueber den gesamten Zeitraum erstellt und zusaetzlich ein Forecast erzeugt.
  #Zusaetzlich wird eine Trendgerade durch das Schaubild gelegt
  output$zeitreihe <- renderPlot({
  dataTs <- ts(data$Wohnungen, frequency=12, start=c(2003,1))
  plot (dataTs, axes = F, xlab = "2003 - 2018", ylab = "Anzahl Baugenehmigungen")
  title (main = "Erteilte Baugenehmigungen in Deutschland")
  
  forecastDataTs <- (auto.arima(dataTs))
  plot (forecast(forecastDataTs,50),ylab = "Anzahl Baugenehmigungen", main = "Zeitreihe mit Arima", 
        xlab = "zeitliche Entwicklung")
  lines(lowess(dataTs), col = "orange")
  })
  
  #Panel Gesamtentwicklung
  #Es wird die Autokorrelation der kompletten Zeitreihe berechnet
  output$autokorr <- renderPlot({
    Baugenehmigungen <- ts(data$Wohnungen, frequency=12, start=c(2003,1))
    acf (Baugenehmigungen, col ="red", main = "Autokorrelation Zeitreihe Anzahl Baugenehmigungen", 
         ylab= "Staerke des Zusammenhangs (Korrelation)")
  })
  
  #Panel jaehrliche Betrachtung
  #Jetzt werden die erteilten Baugenehmigungen zwischen den Jahren verglichen, um zu pruefen
  #ob zwischen den Jahren Differenzen bestehen
  output$zeitreihe5jahre <- renderPlot({
  dataTs <- ts(data$Wohnungen, frequency=12, start=c(2003,1))
  anfangnum <- as.numeric(input$Anfang)
  endenum <- as.numeric(input$Ende)
  
  seasonplot(window(dataTs, start = c(anfangnum,1), end = c(endenum,12)),col = "deepskyblue4", year.labels = FALSE,
             main = "Baugenehmigungen ueber Monate vs Jahr")
  
  })
  

  #Panel monatliche Betrachtung
  #Nun wird die Entwicklung der einzelnen Monate im Verlauf der Jahre angezeigt
  output$monatsvergleich <- renderPlot({
  dataTs <- ts(data$Wohnungen, frequency=12, start=c(2003,1))
  monthplot(dataTs, ylab = "Anzahl Baugenehmigungen", xlab = "Monate", col ="deepskyblue4")
  title = "Durchschnittswerte der Monate"
  })
  
  #Panel monatl. durchschnittliche Entwicklung
  #Die Entwicklung der Mittelwerte der einzelnen Monate wird in einer Zeitreihe von Anfang 2003 bis Ende 2018
  #dargestellt
  
  output$zeitreiheMittelwerte <- renderPlot({
  #Fuer jeden Monat wird eine Variable erstellt, in der man die Entwicklung der 
  #Baugenehmigungen ueber die Jahre pro Monat ablesen kann
  januar <- subset(data, Monate == "Januar" )
  februar <- subset(data, Monate == "Februar" )
  maerz <- subset(data, Monate == "Maerz" )
  april <- subset(data, Monate == "April" )
  mai <- subset(data, Monate == "Mai" )
  juni <- subset(data, Monate == "Juni" )
  juli <- subset(data, Monate == "Juli" )
  august <- subset(data, Monate == "August" )
  september <- subset(data, Monate == "September" )
  oktober <- subset(data, Monate == "Oktober" )
  november <- subset(data, Monate == "November" )
  dezember <- subset(data, Monate == "Dezember" )
  
  # Fuer jeden Monat wird eine Variable erstellt, die den Mittelwert pro Monat beinhaltet
  meanJanuar <- mean (januar$Wohnungen)
  meanFebruar <- mean (februar$Wohnungen)
  meanMaerz <- mean (maerz$Wohnungen)
  meanApril <- mean (april$Wohnungen)
  meanMai <- mean (mai$Wohnungen)
  meanJuni <- mean (juni$Wohnungen)
  meanJuli <- mean (juli$Wohnungen)
  meanAugust <- mean (august$Wohnungen)
  meanSeptember <- mean (september$Wohnungen)
  meanOktober <- mean (oktober$Wohnungen)
  meanNovember <- mean (november$Wohnungen)
  meanDezember <- mean (dezember$Wohnungen)
  
  #Die Standardabweichungen werden pro Monat berechnet
  sdJanuar <- sd (januar$Wohnungen)
  sdFebruar <- sd (februar$Wohnungen)
  sdMaerz <- sd (maerz$Wohnungen)
  sdApril <- sd (april$Wohnungen)
  sdMai <- sd (mai$Wohnungen)
  sdJuni <- sd (juni$Wohnungen)
  sdJuli <- sd (juli$Wohnungen)
  sdAugust <- sd (august$Wohnungen)
  sdSeptember <- sd (september$Wohnungen)
  sdOktober <- sd (oktober$Wohnungen)
  sdNovember <- sd (november$Wohnungen)
  sdDezember <- sd (dezember$Wohnungen)
  
  #Die Standardabweichnung wird pro Monat vom Mittelwert addiert und subtrahiert
  
  minus <- c(meanJanuar-sdJanuar,meanFebruar-sdFebruar,meanMaerz-sdMaerz,meanApril-sdApril,meanMai-sdMai,
            meanJuni-sdJuni,meanJuli-sdJuli,meanAugust-sdAugust,meanSeptember-sdSeptember,
            meanOktober-sdOktober,meanNovember-sdNovember,meanDezember-sdDezember)
  plus <- c(meanJanuar+sdJanuar,meanFebruar+sdFebruar,meanMaerz+sdMaerz,meanApril+sdApril,meanMai+sdMai,
            meanJuni+sdJuni,meanJuli+sdJuli,meanAugust+sdAugust,meanSeptember+sdSeptember,
            meanOktober+sdOktober,meanNovember+sdNovember,meanDezember+sdDezember)
  
  #Es wird eine Tabelle erstellt, die alle Mittelwerte erhaelt
  meanEntwicklung <- data.frame(meanJanuar, meanFebruar, meanMaerz, meanApril, meanMai, meanJuni, 
                                meanJuli, meanAugust, meanSeptember, meanOktober, meanNovember,
                                meanDezember)
  colnames(meanEntwicklung) <- c("Januar", "Februar", "Maerz", "April", "Mai", "Juni", "Juli", 
                                 "August", "September", "Oktober", "November", "Dezember")
  #In dieser Tabelle werden nun die Zeilen und die Spalten vertauscht, 
  #eine Zeitreihe erstellt und diese ausgegeben
  meanEntwicklung2 <- data.frame(matrix(unlist(unclass(meanEntwicklung)),nrow=length(meanEntwicklung),
                                        byrow=T,dimnames=list(names(meanEntwicklung),meanEntwicklung[,0])))
  colnames(meanEntwicklung2) <- c("Anzahl Baugenehmigungen im Durchschnitt")
  tsMeanEntwicklung2 <- ts(meanEntwicklung2$`Anzahl Baugenehmigungen im Durchschnitt`, start=c(1), end=c(12),
                           frequency=1)
  plot(tsMeanEntwicklung2,ylim =c(10000,30000),ylab = "Anzahl Baugenehmigungen", 
       xlab = "Januar - Dezember")
  lines(plus, col="orange")
  lines(minus, col ="green")
  })
 

 
  #Panel monatl. durchschnittliche Entwicklung
  #Die Mittelwerte werden in einer Tabelle ausgegeben
  output$tabellemittelwerte <- renderTable({
    januar <- subset(data, Monate == "Januar" )
    februar <- subset(data, Monate == "Februar" )
    maerz <- subset(data, Monate == "Maerz" )
    april <- subset(data, Monate == "April" )
    mai <- subset(data, Monate == "Mai" )
    juni <- subset(data, Monate == "Juni" )
    juli <- subset(data, Monate == "Juli" )
    august <- subset(data, Monate == "August" )
    september <- subset(data, Monate == "September" )
    oktober <- subset(data, Monate == "Oktober" )
    november <- subset(data, Monate == "November" )
    dezember <- subset(data, Monate == "Dezember" )
    
    # Fuer jeden Monat wird eine Variable erstellt, die den Mittelwert pro Monat beinhaltet
    meanJanuar <- mean (januar$Wohnungen)
    meanFebruar <- mean (februar$Wohnungen)
    meanMaerz <- mean (maerz$Wohnungen)
    meanApril <- mean (april$Wohnungen)
    meanMai <- mean (mai$Wohnungen)
    meanJuni <- mean (juni$Wohnungen)
    meanJuli <- mean (juli$Wohnungen)
    meanAugust <- mean (august$Wohnungen)
    meanSeptember <- mean (september$Wohnungen)
    meanOktober <- mean (oktober$Wohnungen)
    meanNovember <- mean (november$Wohnungen)
    meanDezember <- mean (dezember$Wohnungen)
    
   
    
    
    
    #Es wird eine Tabelle erstellt, die alle Mittelwerte erhaelt
    meanEntwicklung <- data.frame(meanJanuar, meanFebruar, meanMaerz, meanApril, meanMai, meanJuni, 
                                  meanJuli, meanAugust, meanSeptember, meanOktober, meanNovember,
                                  meanDezember)
    colnames(meanEntwicklung) <- c("Januar", "Februar", "Maerz", "April", "Mai", "Juni", "Juli", 
                                   "August", "September", "Oktober", "November", "Dezember")
    
    if (input$Sortierung == 4) {
    sort(meanEntwicklung, decreasing = TRUE)
    } else {
      meanEntwicklung
    }
  })
  
  #Panel monatl. durchschnittliche Entwicklung
  #Die Standardabweichung werden in einer Tabelle ausgegeben
  output$standardabw <- renderTable ({
    januar <- subset(data, Monate == "Januar" )
    februar <- subset(data, Monate == "Februar" )
    maerz <- subset(data, Monate == "Maerz" )
    april <- subset(data, Monate == "April" )
    mai <- subset(data, Monate == "Mai" )
    juni <- subset(data, Monate == "Juni" )
    juli <- subset(data, Monate == "Juli" )
    august <- subset(data, Monate == "August" )
    september <- subset(data, Monate == "September" )
    oktober <- subset(data, Monate == "Oktober" )
    november <- subset(data, Monate == "November" )
    dezember <- subset(data, Monate == "Dezember" )
    
    sdJanuar <- sd (januar$Wohnungen)
    sdFebruar <- sd (februar$Wohnungen)
    sdMaerz <- sd (maerz$Wohnungen)
    sdApril <- sd (april$Wohnungen)
    sdMai <- sd (mai$Wohnungen)
    sdJuni <- sd (juni$Wohnungen)
    sdJuli <- sd (juli$Wohnungen)
    sdAugust <- sd (august$Wohnungen)
    sdSeptember <- sd (september$Wohnungen)
    sdOktober <- sd (oktober$Wohnungen)
    sdNovember <- sd (november$Wohnungen)
    sdDezember <- sd (dezember$Wohnungen)
    
    #Es wird eine Tabelle erstellt, die alle Standardabweichungen erhaelt
    sdEntwicklung <- data.frame(sdJanuar, sdFebruar, sdMaerz, sdApril, sdMai, sdJuni, 
                                  sdJuli, sdAugust, sdSeptember, sdOktober, sdNovember,
                                  sdDezember)
    colnames(sdEntwicklung) <- c("Januar", "Februar", "Maerz", "April", "Mai", "Juni", "Juli", 
                                   "August", "September", "Oktober", "November", "Dezember")
    if (input$Sortierung == 4) {
    sort(sdEntwicklung, decreasing = TRUE)
    } else {
      sdEntwicklung
    }
  })
  
  #Panel monatl. durchschnittliche Entwicklung
  #Der prozentuale Anteil der Standardabweichung am Mittelwert wird in einer Tabelle ausgegeben
  output$anteil <- renderTable ({
    januar <- subset(data, Monate == "Januar" )
    februar <- subset(data, Monate == "Februar" )
    maerz <- subset(data, Monate == "Maerz" )
    april <- subset(data, Monate == "April" )
    mai <- subset(data, Monate == "Mai" )
    juni <- subset(data, Monate == "Juni" )
    juli <- subset(data, Monate == "Juli" )
    august <- subset(data, Monate == "August" )
    september <- subset(data, Monate == "September" )
    oktober <- subset(data, Monate == "Oktober" )
    november <- subset(data, Monate == "November" )
    dezember <- subset(data, Monate == "Dezember" )
    
    # Fuer jeden Monat wird eine Variable erstellt, die den Mittelwert pro Monat beinhaltet
    meanJanuar <- mean (januar$Wohnungen)
    meanFebruar <- mean (februar$Wohnungen)
    meanMaerz <- mean (maerz$Wohnungen)
    meanApril <- mean (april$Wohnungen)
    meanMai <- mean (mai$Wohnungen)
    meanJuni <- mean (juni$Wohnungen)
    meanJuli <- mean (juli$Wohnungen)
    meanAugust <- mean (august$Wohnungen)
    meanSeptember <- mean (september$Wohnungen)
    meanOktober <- mean (oktober$Wohnungen)
    meanNovember <- mean (november$Wohnungen)
    meanDezember <- mean (dezember$Wohnungen)
    
    sdJanuar <- sd (januar$Wohnungen)
    sdFebruar <- sd (februar$Wohnungen)
    sdMaerz <- sd (maerz$Wohnungen)
    sdApril <- sd (april$Wohnungen)
    sdMai <- sd (mai$Wohnungen)
    sdJuni <- sd (juni$Wohnungen)
    sdJuli <- sd (juli$Wohnungen)
    sdAugust <- sd (august$Wohnungen)
    sdSeptember <- sd (september$Wohnungen)
    sdOktober <- sd (oktober$Wohnungen)
    sdNovember <- sd (november$Wohnungen)
    sdDezember <- sd (dezember$Wohnungen)
    
    mean1 <- c(meanJanuar, meanFebruar, meanMaerz, meanApril, meanMai, meanJuni, 
               meanJuli, meanAugust, meanSeptember, meanOktober, meanNovember,
               meanDezember)
    sd1 <- c(sdJanuar, sdFebruar, sdMaerz, sdApril, sdMai, sdJuni, 
             sdJuli, sdAugust, sdSeptember, sdOktober, sdNovember,
             sdDezember)
    
    anteiljan <- c(100/mean1[1]*sd1[1])
    anteilfeb <- c(100/mean1[2]*sd1[2])
    anteilmae <- c(100/mean1[3]*sd1[3])
    anteilapr <- c(100/mean1[4]*sd1[4])
    anteilmai <- c(100/mean1[5]*sd1[5])
    anteiljun <- c(100/mean1[6]*sd1[6])
    anteiljul <- c(100/mean1[7]*sd1[7])
    anteilaug <- c(100/mean1[8]*sd1[8])
    anteilsep <- c(100/mean1[9]*sd1[9])
    anteilokt <- c(100/mean1[10]*sd1[10])
    anteilnov <- c(100/mean1[11]*sd1[11])
    anteildez <- c(100/mean1[12]*sd1[12])
    
    
    anteilframe <- data.frame(anteiljan,anteilfeb,anteilmae,anteilapr,anteilmai,anteiljun,anteiljul,anteilaug,
                               anteilsep,anteilokt,anteilnov,anteildez)
    colnames(anteilframe) <- c("Januar", "Februar", "Maerz", "April", "Mai", "Juni", "Juli", 
                                 "August", "September", "Oktober", "November", "Dezember")
    if (input$Sortierung == 4) {
      sort(anteilframe, decreasing = TRUE)
    } else {
      anteilframe
    }
  })
  
  #Panel monatl. durchschnittliche Entwicklung
  #Der Text zu den Filtern in der monatlichen durchschnittlichen Entwicklung
  output$filtermon <- renderText({
    if (input$Sortierung == 5) {
      "Die Mittelwerte von Januar bis Dezember sortiert"
    } else if  (input$Sortierung == 4) {
      "Die Mittelwerte nach Groesse absteigend sortiert"
    }
  })
  
  #Panel monatl. durchschnittliche Entwicklung
    #Der Text zu den Filtern in der monatlichen Entwicklung
    output$filtermon1 <- renderText({
      if (input$Sortierung == 5) {
        "Die Standardabweichung von Januar bis Dezember sortiert"
      } else if  (input$Sortierung == 4) {
        "Die Standardabweichung nach Groesse absteigend sortiert"
      }
    })
    
    #Panel monatl. durchschnittliche Entwicklung
    #Der Text zu den Filtern in der monatlichen Entwicklung
    output$filtermon2 <- renderText({
      if (input$Sortierung == 5) {
        "Der prozentuale Anteil der Standardabweichung von dem Mittelwert der Monate von Januar bis Dezember sortiert"
      } else if  (input$Sortierung == 4) {
        "Der prozentuale Anteil der Standardabweichung von dem Mittelwert der Monate nach Groesse absteigend sortiert"
      }
    })
 
  
  #Panel Anhang
  #Die Tabelle mit den Rohdaten wird ausgegeben
  output$data1 <- renderTable({
    setwd("C:/Arbeitsordner")
    data <- read.csv2 ("Baugenehmigungen_2003-2019_Gesamt.csv")
    data
  })
  
  #Panel Anhang
  # Die Struktur des Datensatzes wird berechnet
  output$structure <- renderPrint({
    setwd("C:/Arbeitsordner")
    data <- read.csv2 ("Baugenehmigungen_2003-2019_Gesamt.csv")
    str(data)
  })
  
  #Panel Anhang
  #Die vorlaeufigen Baugenehmigungen von 2019
  output$datav5 <- renderTable({
    setwd("C:/Arbeitsordner")
  datav4 <- read.csv2 ("Baugenehmigungen_2019_bis_Juni.csv")
  datav4
  })
})