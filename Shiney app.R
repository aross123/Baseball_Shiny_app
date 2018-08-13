
library(shiny)
dataset<-read.csv("Shiny_dataset.csv")
ui<- fluidPage(titlePanel(title="TAMU Baseball Shiny App"),
           
    tabsetPanel(id="Select Data",
                tabPanel("Simulated Pitch",
                         sidebarPanel(sliderInput(inputId="askvelocity",label="Pitch Veloity",0,100,0),
                                      radioButtons(inputId="pitcherthrows",label="Pitcher Throws", choices=unique(dataset$PitcherThrows)),
                                      radioButtons(inputId="batterside",label="Batter Side", choices=unique(dataset$BatterSide)),
                                      selectInput(inputId="pitchtype", label="Pitch Type", choices=unique(dataset$TaggedPitchType)))),
                
                tabPanel("Batter's Box",
                         sidebarPanel(selectInput(inputId="Pitcher", "Pitcher:", choices=dataset$Pitcher)),
                         mainPanel(plotOutput(outputId="BatterBoxPlot"))),
                tabPanel("Hit visualization"),
                tabPanel("Historical Pitching Data",
                         sidebarPanel(selectInput(inputId="Pitcher_His","Pitcher:",choices=dataset$Pitcher)),
                         mainPanel(dataTableOutput(outputId="FrequencyofPitches"))
     )
))

server<- function(input, output) {
  dataset<-read.csv("Shiny_dataset.csv")
  
  output$FrequencyofPitches<-renderDataTable({
    datatable(dataset)
    
  })
  
  output$BatterBoxPlot<-renderPlot({
    
  if(!is.null(dataset)){
    
    if(input$Pitcher=="Chafin, Kaylor"){
      dataset2=dataset[dataset$Pitcher == "Chafin, Kaylor",]
      #plot(dataset2$PlateLocSide,dataset2$PlateLocHeight,col = c("blue", "yellow", "green","black", "red")[dataset2$Outcome])
      }
    
    if(input$Pitcher=="Doxakis, John"){
      dataset2=dataset[dataset$Pitcher == "Doxakis, John",]
      #plot(dataset2$PlateLocSide,dataset2$PlateLocHeight,col = c("blue", "yellow", "green","black", "red")[dataset2$Outcome])
      }
    
    if(input$Pitcher=="Hoffman, Nolan"){
      dataset2=dataset[dataset$Pitcher == "Hoffman, Nolan",]
      #plot(dataset2$PlateLocSide,dataset2$PlateLocHeight,col = c("blue", "yellow", "green","black", "red")[dataset2$Outcome])
      }
    
    if(input$Pitcher=="Jozwiak, Chandler"){
      dataset2=dataset[dataset$Pitcher == "Jozwiak, Chandler",]
      #plot(dataset2$PlateLocSide,dataset2$PlateLocHeight,col = c("blue", "yellow", "green","black", "red")[dataset2$Outcome])
      }
    
    if(input$Pitcher=="Kilkenny, Mitchell"){
      dataset2=dataset[dataset$Pitcher == "Kilkenny, Mitchell",]
      #plot(dataset2$PlateLocSide,dataset2$PlateLocHeight,col = c("blue", "yellow", "green","black", "red")[dataset2$Outcome])
      }
    
    if(input$Pitcher=="Kolek, Stephen"){
     dataset2=dataset[dataset$Pitcher == "Kolek, Stephen",]
     #plot(dataset2$PlateLocSide,dataset2$PlateLocHeight,col = c("blue", "yellow", "green","black", "red")[dataset2$Outcome])
     }
    
    if(input$Pitcher=="Lacy, Asa"){
      dataset2=dataset[dataset$Pitcher == "Lacy, Asa",]
      #plot(dataset2$PlateLocSide,dataset2$PlateLocHeight,col = c("blue", "yellow", "green","black", "red")[dataset2$Outcome])
      }
    
    if(input$Pitcher=="Roa, Christian"){
      dataset2=dataset[dataset$Pitcher == "Roa, Christian",]
      #plot(dataset2$PlateLocSide,dataset2$PlateLocHeight,col = c("blue", "yellow", "green","black", "red")[dataset2$Outcome])
      }
    
    if(input$Pitcher=="Saenez, Dustin"){
      dataset2=dataset[dataset$Pitcher == "Saenez, Dustin",]
      #plot(dataset2$PlateLocSide,dataset2$PlateLocHeight,col = c("blue", "yellow", "green","black", "red")[dataset2$Outcome])
      }
    
    if(input$Pitcher=="Sherrod, Cason"){
      dataset2=dataset[dataset$Pitcher == "Sherrod, Cason",]
      #plot(dataset2$PlateLocSide,dataset2$PlateLocHeight,col = c("blue", "yellow", "green","black", "red")[dataset2$Outcome])
      }
    
    batterbox=plot(dataset2$PlateLocSide,dataset2$PlateLocHeight,col = c("blue", "yellow", "green","black", "red")[dataset2$Outcome],xlab="PlateLocSide",ylab="PlateLocHeight",xlim=c(-1.5,1.5), ylim=c(1,4))
    legend("topright",legend=levels(dataset2$Outcome),col=c("blue","yellow","green", "black","red"),pch=1)
    rect(-0.83, 1.52, 0.83, 3.42,density=20, border="black", lty="dotted")
    
    
    
    }})
}


shinyApp(ui=ui,server=server)

