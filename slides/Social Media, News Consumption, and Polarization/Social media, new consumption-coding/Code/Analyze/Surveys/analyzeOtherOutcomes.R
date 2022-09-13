# Author: Ro'ee Levy, Yale
# Date: Thu May 30 17:17:09 2019
# Purpose: Analyze additional outcomes from the endline survey
# --------------#



require(data.table)
require(dplyr)
library(fst)


rm(list=ls()) #Remove everything
source("Code/Utils/log.R"); beginLogging()

source("Code/Utils/robustReg.R")
source("Code/Utils/indexFunctions.R")
source("Code/Utils/commonVars.R")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")


# *************************************************************************
# PREPARE  ----
# *************************************************************************

# .prepare other outcomes ----
# .........................................................................

# Create baseline and followup distance from party
SURVEY[, slantRepOp := ifelse(slant_Rep==999, NA, slant_Rep)]
SURVEY[, slantDemOp := ifelse(slant_Dem==999, NA, slant_Dem)]
SURVEY[, slantOwn := ifelse(ideoLeaning==1, slantRepOp, ifelse(ideoLeaning==-1, slantDemOp, NA))]
SURVEY[, slantOther := ifelse(ideoLeaning==-1, slantRepOp, ifelse(ideoLeaning==1, slantDemOp, NA))]

SURVEY[, diffSlantOwn := abs(ideologyN - slantOwn)]
SURVEY[, diffSlantOther := abs(ideologyN - slantOther)]
SURVEY[, diffSlantOwnS := scaleControl(diffSlantOwn, control)]
SURVEY[, diffSlantOtherS := scaleControl(diffSlantOther, control)]
createMissing(SURVEY, list("diffSlantOwn", "diffSlantOther"))

SURVEY[, F_slantRepOp := ifelse(F_slant_Rep==999, NA, F_slant_Rep)]
SURVEY[, F_slantDemOp := ifelse(F_slant_Dem==999, NA, F_slant_Dem)]
SURVEY[, F_slantOwn := ifelse(ideoLeaning==1, F_slantRepOp, ifelse(ideoLeaning==-1, F_slantDemOp, NA))]
SURVEY[, F_slantOther := ifelse(ideoLeaning==-1, F_slantRepOp, ifelse(ideoLeaning==1, F_slantDemOp, NA))]

SURVEY[, F_diffSlantOwn := abs(ideologyN - F_slantOwn)]
SURVEY[, F_diffSlantOther := abs(ideologyN - F_slantOther)]
SURVEY[, F_diffSlantOwnS := scaleControl(F_diffSlantOwn, control)]
SURVEY[, F_diffSlantOtherS := scaleControl(F_diffSlantOther, control)]


# Create ideology
SURVEY[, F_ideologyN := ifelse(F_ideology==999, NA, F_ideology)]
SURVEY[, F_ideologyNS := scaleControl(F_ideologyN, control)]

# Create party affiliation
SURVEY[, F_party7S := scaleControl(F_party7, control)]

SURVEY[, F_partyRepS := scaleControl(ifelse(F_party7>0, F_party7, 0), control)]
SURVEY[, F_partyDemS := scaleControl(ifelse(F_party7<0, -F_party7, 0), control)]
SURVEY[, partyRepS := scaleControl(ifelse(party7>0, party7, 0), control)]
SURVEY[, partyDemS := scaleControl(ifelse(party7<0, -party7, 0), control)]

# Is feed diverse
SURVEY[, F_echoChamberS := scaleControl(F_echoChamber, control)]

# Expected winner o election
SURVEY[, F_whoWillWinFull := scaleControl(ifelse(F_whoWillWin==999, 0, F_whoWillWin), control)]

# Vote
SURVEY[, vote_Rep := ifelse(genericBallotF=="The Republican Party candidate", 1, 
                            ifelse(genericBallotF=="The Democratic Party candidate", 0, NA))]
SURVEY[, F_vote_Rep := ifelse(F_vote==1, 1, ifelse(F_vote==-1, 0, NA))]


# *************************************************************************
# REG ----
# *************************************************************************

NO_CG = SURVEY[treatment!="Control"]

otherOutcomesDT = as.data.table(list(
  var = c("F_echoChamberS", "F_modifiedViews", 
          "F_diffSlantOwnS", "F_diffSlantOtherS", 
         "F_vote_Rep", "F_whoWillWinFull", "F_party7S", "F_ideologyNS",
         "F_partyRepS", "F_partyDemS"),
  varName = c("Facebook Echo Chamber", "Modified Views Social Media",
              "Distance Slant Own Party", "Distance Slant Other Party",
              "2018 Vote, Republican", "Predict Majority Congress", "Party Affiliation", "Ideology",
              "Republican Affiliation", "Democratic Affiliation"),
  pers = c(rep(FALSE, 4), rep(TRUE, 6)),
  pol = c(rep(TRUE, 4), rep(FALSE, 6))))

otherOutcomes = otherOutcomesDT$var

# Create one large table for all regression
regData = as.data.table(expand.grid(outcome = otherOutcomes, 
                                    category=c("Pers", "Pol"), 
                                    type = c("NoCG", "")))

regData[, categoryF := factor(category, 
                              labels = c("Conservative Treatment Minus Liberal Treatment", 
                                         "Counter Att. Treatment Minus Pro Att.Treatment"),
                              levels = c("Pers", "Pol"))]

regData[, controls := ifelse(category=="Pol", controlsPolWithBase, 
                             ifelse(category=="Pers", controlsPersWithBase, NA))]
regData[outcome=="F_diffSlantOwnS", controls := paste0(controls, " + diffSlantOwn")]
regData[outcome=="F_diffSlantOtherS", controls := paste0(controls, " + diffSlantOther")]
regData[outcome=="F_vote_Rep", controls := paste0(controls, " + vote_Rep")]
regData[outcome=="F_echoChamberS", controls := paste0(controls, " + echoChamber")]

regData[, myFormula := paste0(outcome, " ~ ", controls)]
                            
regData[, modelName := paste0(category, "_", outcome, "_Reg", type)]
regData[, myData := ifelse(type=="NoCG", "NO_CG", "SURVEY")]



# run the regressions!
for (i in 1:nrow(regData)) {
  regData[i, assign(modelName,lmR(data=get(myData), formula = as.formula(myFormula)), pos=globalenv())]
}


# *************************************************************************
# PLOT ----
# *************************************************************************

modelVars = paste0(c(paste0("Pol_", otherOutcomesDT[pol==TRUE, var]), 
                     paste0("Pers_", otherOutcomesDT[pers==TRUE, var])), "_RegNoCG")
mainModels = rbindlist(lapply(as.list(modelVars), function(x) tidyRob(get(x), x, ci=0.9)))

mainModelsPlot = mainModels[grepl("reatment", term)]
mainModelsPlot = merge(mainModelsPlot, otherOutcomesDT[, list(var, varName)], by.x="dvName", by.y="var")
mainModelsPlot = merge(mainModelsPlot, regData[, list(modelName, categoryF)], by="modelName")
mainModelsPlot[, varNameF := factor(varName, levels = 
                                     rev(c("Seen Potential Pro-Att. Outlets", "Seen Potential Counter-Att. Outlets",
                                       "Facebook Echo Chamber", "Modified Views Social Media",
                                       "Ideology", "Party Affiliation", "Republican Affiliation", "Democratic Affiliation",
                                       "2018 Vote, Republican", "Predict Majority Congress",
                                       "Distance Slant Pro Outlets", "Distance Slant Counter Outlets",
                                       "Distance Slant Own Party", "Distance Slant Other Party")))]

regressionPlot(ggplot(mainModelsPlot, aes(x=varNameF, y=estimate))) + 
  ylab("Intention to Treat Effect") + 
  scale_y_continuous(limits = c(-0.075, 0.075), breaks = seq(-0.2, 0.2, 0.05)) +
  facet_wrap(~categoryF, nrow=2, scales="free_y")

currentTheme = theme(axis.title.y = element_blank(), legend.position = c("none"), panel.border = element_rect(fill=NA),
                     axis.text.y=element_text(hjust=0))
ggMySaveBeamer("Output/Graphs/Combined/additionalOutcomes_Beamer.eps", theme = currentTheme, noLinks = TRUE)
ggMySaveDoc("Output/Graphs/Combined/additionalOutcomes.eps", theme = currentTheme)


