#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  tabsetPanel(type = "tabs",
   tabPanel("Predictions", fluid = TRUE,
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("reading",
                     "Reading Score:",
                     min = 300,
                     max = 600,
                     value = 500),
      sliderInput("science",
                  "Math Score:",
                  min = 300,
                  max = 600,
                  value = 500),
   sliderInput("math",
               "Science Score:",
               min = 300,
               max = 600,
               value = 500)),
      # Show generated predictions
      mainPanel(
         textOutput("predictions")
      )
   )
),
tabPanel("Diagnostics", fluid = TRUE,
  sidebarLayout(
    sidebarPanel(
      selectInput(1, "control", nameList, selected = NULL, multiple = FALSE)
  ),
  mainPanel(
    renderText("placeholder!")
)
)
)
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$predictions <- renderText({
     makePrediction(input$reading, input$math, input$science)
   })
}

load("~/Intro Data Science/Final Project/WIDEData.RData")

GDP <- unlist(WIDEData[WIDEData$Indicator.Code == "NY.GDP.MKTP.KD", "X2015"])
homicides <- unlist(WIDEData[WIDEData$Indicator.Code == "VC.IHR.PSRC.P5", "X2015"])
labor <- unlist(WIDEData[WIDEData$Indicator.Code == "SL.TLF.CACT.NE.ZS", "X2015"])
lifeExpectancies <- unlist(WIDEData[WIDEData$Indicator.Code == "SP.DYN.LE00.IN", "X2015"])
military <- unlist(WIDEData[WIDEData$Indicator.Code == "MS.MIL.XPND.GD.ZS", "X2015"])
infantMortality <- unlist(WIDEData[WIDEData$Indicator.Code == "SH.DTH.IMRT", "X2015"])
airPollution <- unlist(WIDEData[WIDEData$Indicator.Code == "EN.ATM.PM25.MC.M3", "X2015"])
nondiscrimination <- unlist(WIDEData[WIDEData$Indicator.Code == "SG.LAW.NODC.HR", "X2015"])
undernourishment <- unlist(WIDEData[WIDEData$Indicator.Code == "SN.ITK.DEFC.ZS", "X2015"])
femaleLaborForce <- unlist(WIDEData[WIDEData$Indicator.Code == "SL.TLF.CACT.FM.ZS", "X2015"])
suicides <- unlist(WIDEData[WIDEData$Indicator.Code == "SH.STA.SUIC.P5", "X2015"])
rScores <- unlist(WIDEData[WIDEData$Indicator.Code == "VC.IHR.PSRC.P5", "readingScores"])
mScores <-unlist(WIDEData[WIDEData$Indicator.Code == "VC.IHR.PSRC.P5", "mathScores"])
sScores <- unlist(WIDEData[WIDEData$Indicator.Code == "VC.IHR.PSRC.P5", "scienceScores"])

modelList <- vector(mode="list", length = 11)

modelList[[1]] <- GDPModel <- glm(GDP ~ rScores+mScores+sScores, family=Gamma(link="log"))
modelList[[2]] <- homicideModel <- glm(homicides ~ rScores+mScores+sScores, family=Gamma(link="log"))
modelList[[3]] <- laborModel <- glm(labor ~ rScores+mScores+sScores, family=Gamma(link="log"))
modelList[[4]] <- lifeExpectancyModel <- glm(lifeExpectancies ~ rScores+mScores+sScores, family=Gamma(link="log"))
modelList[[5]] <- militaryModel <- glm(military ~ rScores+mScores+sScores, family=Gamma(link="log"))
modelList[[6]] <- infantMortalityModel <- glm(infantMortality ~ rScores+mScores+sScores, family=Gamma(link="log"))
modelList[[7]] <- airPollutionModel <- glm(airPollution ~ rScores+mScores+sScores, family=Gamma(link="log"))
modelList[[8]] <- nondiscriminationModel <- glm(nondiscrimination ~ rScores+mScores+sScores, family=binomial(link="logit"))
modelList[[9]] <- undernourishmentModel <- glm(undernourishment ~ rScores+mScores+sScores, family=Gamma(link="log"))
modelList[[10]] <- femaleLaborForceModel <- glm(femaleLaborForce ~ rScores+mScores+sScores, family=Gamma(link="log"))
modelList[[11]] <- suicideModel <- glm(suicides ~ rScores+mScores+sScores, family=Gamma(link="log"))

makePrediction <- function(readingScore, mathScore, scienceScore) {
  for (i in 1:11) {
    predictions[[i]] <<- predict(modelList[[i]], list(rScores = readingScore, mScores = mathScore, sScores = scienceScore), type="response")
  }
  predictions[[8]] <<- round(predictions[[8]])
  predictions
}

deparser <- function(vector, nameList) {
  for (i in vector) {
    nameList[[i]] <- deparse(substitute(vector[[i]]))
  }
}

predictions <- vector(mode = "numeric", length = 11)
nameList <- list(mode = "character", length = 11)

deparser(modelList, nameList)

# Run the application 
shinyApp(ui = ui, server = server)

