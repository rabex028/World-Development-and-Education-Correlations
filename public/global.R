load("WIDEData.RData")

dataList <- vector(mode="list", length = 11)

GDP <- unlist(WIDEData[WIDEData$Indicator.Code == "NY.GDP.MKTP.KD", "X2015"])
homicides <- unlist(WIDEData[WIDEData$Indicator.Code == "VC.IHR.PSRC.P5", "X2015"])
labor <- (unlist(WIDEData[WIDEData$Indicator.Code == "SL.TLF.CACT.NE.ZS", "X2015"])/100)
lifeExpectancies <- unlist(WIDEData[WIDEData$Indicator.Code == "SP.DYN.LE00.IN", "X2015"])
military <- (unlist(WIDEData[WIDEData$Indicator.Code == "MS.MIL.XPND.GD.ZS", "X2015"])/100)
infantMortality <- unlist(WIDEData[WIDEData$Indicator.Code == "SH.DTH.IMRT", "X2015"])
airPollution <- unlist(WIDEData[WIDEData$Indicator.Code == "EN.ATM.PM25.MC.M3", "X2015"])
nondiscrimination <- unlist(WIDEData[WIDEData$Indicator.Code == "SG.LAW.NODC.HR", "X2015"])
undernourishment <- (unlist(WIDEData[WIDEData$Indicator.Code == "SN.ITK.DEFC.ZS", "X2015"])/100)
femaleLaborForce <- unlist(WIDEData[WIDEData$Indicator.Code == "SL.TLF.CACT.FM.ZS", "X2015"])
suicides <- unlist(WIDEData[WIDEData$Indicator.Code == "SH.STA.SUIC.P5", "X2015"])
rScores <- unlist(WIDEData[WIDEData$Indicator.Code == "VC.IHR.PSRC.P5", "readingScores"])
mScores <-unlist(WIDEData[WIDEData$Indicator.Code == "VC.IHR.PSRC.P5", "mathScores"])
sScores <- unlist(WIDEData[WIDEData$Indicator.Code == "VC.IHR.PSRC.P5", "scienceScores"])

dataList[[1]] <- GDP
dataList[[2]] <- homicides
dataList[[3]] <- labor
dataList[[4]] <- lifeExpectancies
dataList[[5]] <- military
dataList[[6]] <- infantMortality
dataList[[7]] <- airPollution
dataList[[8]] <- nondiscrimination
dataList[[9]] <- undernourishment
dataList[[10]] <- femaleLaborForce
dataList[[11]] <- suicides

dataList[[3]] <- (dataList[[3]] * 100)
dataList[[5]] <- (dataList[[5]] * 100)
dataList[[9]] <- (dataList[[9]] * 100)


modelList <- vector(mode="list", length = 11)
modelType <- vector(mode="list", length = 11)

modelType[[1]] <- list("Distribution Family: Gamma", "Link: Log")
modelType[[2]] <- list("Distribution Family: Gamma",  "Link: Log")
modelType[[3]] <- list("Distribution Family: Gamma",  "Link: Logit")
modelType[[4]] <- list("Distribution Family: Gamma",  "Link: Log")
modelType[[5]] <- list("Distribution Family: Gamma",  "Link: Logit")
modelType[[6]] <- list("Distribution Family: Gamma",  "Link: Log")
modelType[[7]] <- list("Distribution Family: Gamma",  "Link: Log")
modelType[[8]] <- list("Distribution Family: Binomial",  "Link: Logit")
modelType[[9]] <- list("Distribution Family: Gamma",  "Link: Logit")
modelType[[10]] <- list("Distribution Family: Gamma",  "Link: Log")
modelType[[11]] <- list("Distribution Family: Gamma",  "Link: Log")

modelList[[1]] <- GDPModel <- glm(GDP ~ rScores+mScores+sScores, family=Gamma(link="log"))
modelList[[2]] <- homicideModel <- glm(homicides ~ rScores+mScores+sScores, family=Gamma(link="log"))
modelList[[3]] <- laborModel <- glm(labor ~ rScores+mScores+sScores, family=Gamma(link ="logit"))
modelList[[4]] <- lifeExpectancyModel <- glm(lifeExpectancies ~ rScores+mScores+sScores, family=Gamma(link="log"))
modelList[[5]] <- militaryModel <- glm(military ~ rScores+mScores+sScores, family=Gamma(link ="logit"))
modelList[[6]] <- infantMortalityModel <- glm(infantMortality ~ rScores+mScores+sScores, family=Gamma(link="log"))
modelList[[7]] <- airPollutionModel <- glm(airPollution ~ rScores+mScores+sScores, family=Gamma(link="log"))
modelList[[8]] <- nondiscriminationModel <- glm(nondiscrimination ~ rScores+mScores+sScores, family=binomial(link="logit"))
modelList[[9]] <- undernourishmentModel <- glm(undernourishment ~ rScores+mScores+sScores, family=Gamma(link = "logit"))
modelList[[10]] <- femaleLaborForceModel <- glm(femaleLaborForce ~ rScores+mScores+sScores, family=Gamma(link="log"))
modelList[[11]] <- suicideModel <- glm(suicides ~ rScores+mScores+sScores, family=Gamma(link="log"))

makePrediction <- function(readingScore, mathScore, scienceScore) {
  for (i in 1:11) {
    predictions[[i]] <<- predict(modelList[[i]], list(rScores = readingScore, mScores = mathScore, sScores = scienceScore), type="response")
  }
  predictions[[3]] <<- (predictions[[3]] * 100)
  predictions[[5]] <<- (predictions[[5]] * 100)
  predictions[[8]] <<- round(predictions[[8]])
  predictions[[9]] <<- (predictions[[9]] * 100)
  
  predictions
}


predictions <- vector(mode = "numeric", length = 11)
nameList <- vector(mode = "character", length = 11)

nameList[[1]] <- "GDP"
nameList[[2]] <- "Homicides"
nameList[[3]] <- "Labor Force Participation"
nameList[[4]] <- "Life Expectancy"
nameList[[5]] <- "Military Expenditure"
nameList[[6]] <- "Infant Mortality"
nameList[[7]] <- "Air Pollution"
nameList[[8]] <- "Gender Nondiscrimination"
nameList[[9]] <- "Undernourishment"
nameList[[10]] <- "Female Labor Force proportion"
nameList[[11]] <- "Suicide Rate"
