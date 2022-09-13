# Author: Ro'ee Levy, Yale
# Date: Thu May 30 17:18:00 2019
# Purpose: Analyze effects at the outlet by participant level including whether an outlet was seen, is trusted, and its perceived slant
# --------------#
# Process followup


require(data.table)
require(dplyr)
require(fst)

rm(list=ls()) #Remove everything
source("Code/Utils/log.R"); beginLogging()

source("Code/Utils/robustReg.R")
source("Code/Utils/indexFunctions.R")
source("Code/Utils/commonVars.R")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")
USERS_PAGES <- readRDS("Datasets/UsersPages/users_pages.rds")


# *************************************************************************
# PREPARE ----
# *************************************************************************

# Add survey data
USERS_PAGES <- merge(USERS_PAGES, 
                       SURVEY[, list(treatment, control, ResponseId, ideoLeaning, ideoLeaningF, 
                                     matchTreatment, treatIdeo, ideologyN, potentialText,
                                  ideologyF, partyAllF, trumpF, genderF, ageWM, ageSqWM, ageHave)], 
                     by="ResponseId")

USERS_PAGES[, seenGreaterZero := ifelse(seen>=1, 1, 0)]
USERS_PAGES[, F_seenGreaterZero := ifelse(F_seen>=1, 1, 0)]


# .create outcomes and baseline ----
# .........................................................................

# Difference from respondent's ideology and outlet's
USERS_PAGES[, F_diffSlant := abs(ideologyN - F_slant)]
USERS_PAGES[, diffSlant := abs(ideologyN - slant)]

# Scale outcomes
USERS_PAGES[potential==TRUE, F_seenS := scaleControl(F_seen, control)]
USERS_PAGES[potential==TRUE, F_slantKnowS := scaleControl(F_slantKnow, control)]
USERS_PAGES[potential==TRUE, F_diffSlantS := scaleControl(F_diffSlant, control)]
USERS_PAGES[potential==TRUE, F_trustS := scaleControl(F_trust, control)]

# Create missing covariates for regressions
createMissing(USERS_PAGES, c("seen", "slantKnow", "diffSlant"))


# *************************************************************************
# REG ----
# *************************************************************************

# Comparing the potential outlets for each participant
MY_DATA = USERS_PAGES[potential==TRUE & control==FALSE & !is.na(ideoLeaning)]

# In some cases show pro vs counter and in others counter vs pro
MY_DATA[, matchTreatmentRev := relevel(MY_DATA$matchTreatment, "Counter")]

# Create one large table for all regression
# outcomes are trust, seeing outlet, perceived difference with outlet's ideology and knowing the outlet's ideology
regData = as.data.table(expand.grid(outcome = c("F_seen", "F_slantKnow", "F_diffSlant", "F_trust"), 
                                    category=c("Pro", "Counter")))
regData[, modelName := paste(gsub("F_", "reg", outcome), category, sep="_")]

# Define control covariates. Control for basic controls, lagged outcome if exists, outlet and offered FE
regData[outcome %in% c("F_seen"), controls := paste0(controlsMatch, " + seenWM + seenHave")]
regData[outcome %in% c("F_trust"), controls := controlsMatch]
regData[outcome=="F_slantKnow", controls := paste0(controlsMatch, " + slantKnowWM + slantKnowHave")] 
regData[outcome=="F_diffSlant", controls := paste0(controlsMatch, " + diffSlantWM + diffSlantHave")] 
regData[category=="Pro", controls := gsub("matchTreatment", "matchTreatmentRev", controls)]
regData[, myFormula := paste0(outcome, " ~ ", controls, "| outlet + potentialText | 0 | ResponseId")]

# Define data used
regData[, subDataCond := ifelse(category=="Pro", "potPro==1", ifelse(category=="Counter", "potPro==0", NA))]


for (i in 1:nrow(regData)) {
    regData[i, assign(modelName,felmR(data=MY_DATA[eval(parse(text=subDataCond))], formula = as.formula(myFormula)), pos=globalenv())]
}


# *************************************************************************
# PLOT ----
# *************************************************************************

# Load odels
modelVars = regData[, modelName]
mainModels = rbindlist(lapply(as.list(modelVars), function(x) tidyRob(get(x), x, ci=0.9)))

# Save relevant coefficents
mainModelsPlot = mainModels[grepl("reatment", term)]
mainModelsPlot = merge(mainModelsPlot, regData[, list(modelName, outcome, category)], by="modelName")

# Define variable names
mainModelsPlot[, varName := interaction(outcome, category, sep="")]

allVars = c(sapply(rev(c("F_seen", "F_slantKnow", "F_diffSlant", "F_trust")), function(x) paste0(x, c("Pro", "Counter"))))
newNames = c(sapply(rev(c("Seen in Feed", "Know Slant", "Distance Slant", "Trust Outlet")), function(x) paste0(x, "\n", c("Pro", "Counter"))))
mainModelsPlot[, varNameF := factor(varName, allVars, newNames)]

# Define subplot names
mainModelsPlot[, subPlotF := factor(category, levels = c("Pro", "Counter"),
                                     labels = c("Pro-Att. Treatment\nMinus Counter-Att. Treatment",
                                   "Counter-Att. Treatment\nMinus Pro-Att. Treatment"))]
                  

currentTheme = theme(axis.title.y = element_blank(), legend.position = c("none"), panel.border = element_rect(fill=NA),
                     axis.text.y=element_text(hjust=0))


# Create plot
regressionPlot(ggplot(mainModelsPlot, aes(x=varNameF, y=estimate))) + 
  ylab("Intention to Treat Effect") + 
  scale_y_continuous(breaks = seq(-0.2, 0.2, 0.05)) +
  facet_wrap(~subPlotF,scales="free_y")
ggMySaveBeamer("Output/Graphs/AdditionalSurveyOutcomes/outcomesUserOutletShort_Beamer.eps", theme = currentTheme, noLinks = TRUE)
ggMySaveDoc("Output/Graphs/AdditionalSurveyOutcomes/outcomesUserOutletShort.eps", 
            theme = currentTheme, adjustHeight = 0.6)


