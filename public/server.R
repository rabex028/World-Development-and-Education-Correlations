server <- function(input, output) {
  
  serverPredictions <- reactive({ serverPredictions <- makePrediction(input$reading, input$math, input$science)})
  binaryToWord <- reactive({
    if(identical(serverPredictions()[[8]], 1)) {
      binaryToWord = "Yes"
    }
    else {
      binaryToWord = "No"
    }
  })
  
  legendPositioning <- reactive({
    if(input$Distributions %in% c(4,8,10)) {
      legendPositioning <- "topleft"
    }
    else {
      legendPositioning <- "topright"
    }
  })
  
  plotLineOption <- reactive({
      plotLineOption <- isTRUE(input$plotLineOption)
  })
  
  output$gdpPrediction <- renderText({
    paste("GDP (trillions of US dollars):                           ", paste("$",(round(serverPredictions()[1]/1000000000000,3)),sep = ""))
  }) 
  output$homicidePrediction <- renderText({
    paste("Intentional Homicides (Per 100,000 people):              ",round(serverPredictions()[2], 3))
  })
  output$laborPrediction <- renderText({
    paste("Labor Force Participation Rate (Percentage):              ",paste(round(serverPredictions()[3],3)),"%", sep = "")
  })
  output$lifePrediction <- renderText({
    paste("Life Expectancy in Years:                                ",round(serverPredictions()[4],0))
  })
  output$militaryPrediction <- renderText({
    paste("Military Expenditure (% of GDP):                          ",paste(round(serverPredictions()[5],3)), "%", sep = "")
  })
  output$infantPrediction <- renderText({
    paste("Number of Infant Deaths:                                 ",round(serverPredictions()[6],0))
  }) 
  output$airPrediction <- renderText({
    paste("Mean Annual Exposure to PM2.5 air Pollution (ug/m3):     ",round(serverPredictions()[7],3))
  })
  output$nondiscriminationPrediction <- renderText({
    paste("Law mandates nondiscrimination based on gender in hiring:",binaryToWord())
  })
  output$undernourishmentPrediction <- renderText({
    paste("Prevalence of Undernourishment (Percentage):              ",paste(round(serverPredictions()[9],3)), "%", sep="")
  })
  output$femaleLaborPrediction <- renderText({
    paste("Ratio of Females to Males in Labor Force (Percentage):    ",paste(round(serverPredictions()[10],3)), "%", sep="")
  })
  output$suicidePrediction <- renderText({
    paste("Suicide Mortality Rate (Per 100,000 People):             ",round(serverPredictions()[11],3))
  })
  
  output$densityPlot <- renderPlot({
    plot(density(dataList[[as.numeric(input$Distributions)]], na.rm = TRUE), main = paste("Density Plot of ",nameList[[as.numeric(input$Distributions)]]))
    if(plotLineOption()){
    abline(v = mean(dataList[[as.numeric(input$Distributions)]], na.rm = TRUE), col = "red", lwd = 3, lty = "dotted", cex = 1.5)
    abline(v = serverPredictions()[[as.numeric(input$Distributions)]], col = "seagreen3", lwd = 3, cex = 1.5)
    legend(legendPositioning(), legend = c("Mean", "Predicted Value"), lty = 2:1, lwd= c(3,3), col = c("red", "seagreen3"))
  }})
  
  
  
  output$diagnostics1 <- renderPlot({
    plot(modelList[[as.numeric(input$modelChoice)]], which = c(1))
  })
  
  output$diagnostics2 <- renderPlot({
    plot(modelList[[as.numeric(input$modelChoice)]], which = c(2))
  })
  
  output$modelFamily <- renderText({
    modelType[[as.numeric(input$modelChoice)]][[1]]
  })
  
  output$modelLink <- renderText({
    modelType[[as.numeric(input$modelChoice)]][[2]]
  })
  
  output$modelChoice <- renderText({
    class(input$modelChoice)
  })
}

