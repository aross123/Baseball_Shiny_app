
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
                         sidebarPanel(radioButtons(inputId="selectpitcher", "Filter By Specific Pitcher?",
                                                   choices = c("Yes", "No"),
                                                   inline = TRUE, 
                                                   selected = "No"),
                                      conditionalPanel(
                                        condition = "input.selectpitcher== 'Yes'",
                                        selectizeInput(inputId="Pitcher_His","Pitcher:", choices=dataset$Pitcher,multiple=TRUE)),
                                      
                                      radioButtons(inputId="selectteam", "Filter By Specific Team?",
                                                   choices = c("Yes", "No"),
                                                   inline = TRUE, 
                                                   selected = "No"),
                                      conditionalPanel(
                                        condition = "input.selectteam== 'Yes'",
                                        selectizeInput(inputId="Team_His","Team:", choices=dataset$BatterTeam,multiple=TRUE),
                                        uiOutput("ui")),
                                      radioButtons(inputId="selectpitchtype", "Filter By Pitch Type?",
                                                   choices = c("Yes", "No"),
                                                   inline = TRUE, 
                                                   selected = "No"),
                                      conditionalPanel(
                                        condition="input.selectpitchtype=='Yes'",
                                        selectizeInput(inputId="pitchtype_m", "Select Pitch Type",
                                        choices=unique(dataset$TaggedPitchType), multiple=TRUE)
                                      ),
                                      radioButtons(inputId="count", "Filter By Count?",
                                                                choices = c("Yes", "No"),
                                                                inline = TRUE, 
                                                                selected = "No"),
                                      conditionalPanel(
                                        condition="input.count=='Yes'",
                                        selectizeInput(inputId="balls","Balls",choices=c(0,1,2,3)),
                                        selectizeInput(inputId="strikes","Strikes", choices=unique(dataset$Strikes))
                                        ),
                                      radioButtons(inputId="selectvel", "Filter By Velocity?",
                                                   choices = c("Yes", "No"),
                                                   inline = TRUE, 
                                                   selected = "No"),
                                      conditionalPanel(
                                        condition="input.selectvel=='Yes'",
                                      sliderInput(inputId="minvel",label="Pitch Veloity",0,100,0),
                                      sliderInput(inputId="maxvel",label="Pitch Veloity",0,100,0)
                                      )),
                         
                         mainPanel(dataTableOutput(outputId="FrequencyofPitches"))
     )
))

server<- function(input, output) {
  dataset<-read.csv("Shiny_dataset.csv")
  
  
  
  output$ui <- renderUI({
    if (is.null(input$Team_His))
      return()
    
    batter_sub=dataset[dataset$BatterTeam==input$Team_His,]
    "Team_His" = selectizeInput("batters", "Select Batter/s",
                                        choices = unique(batter_sub$Batter),multiple=TRUE
    )
  })
  
  output$FrequencyofPitches<-renderDataTable({
    dataset
    
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
    
    batterbox=plot(dataset2$PlateLocSide,dataset2$PlateLocHeight,col = c("blue", "yellow", "green","black", "red")[dataset2$Outcome_hit],xlab="PlateLocSide",ylab="PlateLocHeight",xlim=c(-1.5,1.5), ylim=c(1,4))
    legend("topright",legend=levels(dataset2$Outcome_hit),col=c("blue","yellow","green", "black","red"),pch=1)
    rect(-0.83, 1.52, 0.83, 3.42,density=20, border="black", lty="dotted")
    
    
    
    }})
}


shinyApp(ui=ui,server=server)

