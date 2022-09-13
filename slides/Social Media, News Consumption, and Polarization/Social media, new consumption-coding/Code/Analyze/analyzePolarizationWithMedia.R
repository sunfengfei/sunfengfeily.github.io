# Author:Roee Levy, Yale
# Date: Mon Jan 22 18:23:36 2018
# Purpose: Use extension and endline survey data to estimate the effect of exposure to news on affective polarization 

library(data.table)
library(dplyr)
library(fst)
library(ggplot2)
library(car) # For linear hypothesis

rm(list=ls())
source("Code/Utils/log.R"); beginLogging()

source("Code/Utils/indexFunctions.R")
source("Code/Utils/robustReg.R")
source("Code/Utils/commonVars.R")


#FB_DAY <- read_fst("Datasets/Extension/facebook/fb_user_day.fst", as.data.table=TRUE)
FB_DAY <- readRDS("Datasets/Extension/facebook/fb_user_day.rds")
NAV_HIS_DAY <- readRDS("Datasets/Extension/navigate/nav_user_day.rds")
FB_USER <- readRDS("Datasets/Extension/facebook/fb_user.rds")

NAV_HIS_USER <- readRDS("Datasets/Extension/navigate/nav_user.rds")
SURVEY <- readRDS(file="Datasets/Survey/survey.rds")


# *************************************************************************
# PREPARE ----
# *************************************************************************

SURVEY[, lastRelativeDay := floor(difftime(F_startPOSIX, startPOSIX, units="days"))]

ALL_USER = merge(FB_USER, NAV_HIS_USER, by="ResponseId", all=TRUE)
ALL_USER = merge(ALL_USER, SURVEY, by="ResponseId")


# .Between baseline and followup ----
# .........................................................................

# Find all posts between baseline and followup surveys
USER_DAY = merge(FB_DAY, SURVEY[, list(ResponseId, startPOSIX, lastRelativeDay)], by="ResponseId")
USER_DAY = merge(USER_DAY, NAV_HIS_DAY, by = c("ResponseId", "relativeDay"), all.x=TRUE)

allBetween = USER_DAY[relativeDay < lastRelativeDay, list(liberalBetween_FB = sum(liberal_FB), conservativeBetween_FB = sum(conservative_FB), moderateBetween_FB = sum(moderate_FB),
  liberalBetween_Nav = sum(liberal_Nav), conservativeBetween_Nav = sum(conservative_Nav),
  liberalBetween_NavFromFB = sum(liberal_NavFromFB), conservativeBetween_NavFromFB = sum(conservative_NavFromFB),
  liberalBetween_NavNotFromFB = sum(liberal_NavNotFromFB), conservativeBetween_NavNotFromFB = sum(conservative_NavNotFromFB),
  countBetween_FB = sum(count_slant_FB, na.rm=TRUE), slantBetween_FB = weighted.mean(slant_FB, w=count_slant_FB, na.rm=TRUE),
  countBetween_Nav = sum(count_slant_Nav, na.rm=TRUE), slantBetween_Nav = weighted.mean(slant_Nav, w=count_slant_Nav, na.rm=TRUE),
  slantBetween_NavFromFB = weighted.mean(slant_NavFromFB, w=count_slant_NavFromFB, na.rm=TRUE),
  slantBetween_NavNotFromFB = weighted.mean(slant_NavNotFromFB, w=count_slant_NavNotFromFB, na.rm=TRUE)), by="ResponseId"]
ALL_USER = merge(ALL_USER, allBetween, all=TRUE)

# User have extension until they completed followup
ALL_USER[, haveExtFollow := difftime(maxTimeExt, F_startPOSIX)>0]

# Include people who observed at least two pro or counter-att posts
ALL_USER = ALL_USER[!is.na(matchTreatment) & conservativeBetween_FB + liberalBetween_FB > 1]


# .Measures ----
# .........................................................................

loopVars = c("FB", "Nav", "NavFromFB", "NavNotFromFB")

# Calculate summary stat: share of counter-att. posts and congruence scale and slant
for (i in loopVars) {
  
  # Intermediate variable
  nameAllCounterBet = paste0("countAllCounterBet_", i)
  nameAllProBet = paste0("countAllProBet_", i)
  nameAllProCounterBet = paste0("countAllProCounterBet_", i)
  
  # Summary stats
  nameCongrueneBet = paste0("congruenceBet_", i) 
  nameShareBet = paste0("shareCounterBet_", i)
  nameSlantScale = paste0("slantBet_", i, "Scaled")
    
  ALL_USER[, eval(nameAllCounterBet) := ifelse(ideoLeaning==1, get(paste0("liberalBetween_", i)), 
                                            ifelse(ideoLeaning==-1, get(paste0("conservativeBetween_", i)), NA))]
  ALL_USER[, eval(nameAllProBet) := ifelse(ideoLeaning==-1, get(paste0("liberalBetween_", i)), 
                                        ifelse(ideoLeaning==1, get(paste0("conservativeBetween_", i)), NA))]
  ALL_USER[, eval(nameAllProCounterBet) := (get(nameAllProBet)+get(nameAllCounterBet))]
  ALL_USER[, eval(nameShareBet) := get(nameAllCounterBet) / get(nameAllProCounterBet)]
    
  ALL_USER[tookFollowup==TRUE, eval(paste0(nameShareBet, "Scaled")) := scaleControl(get(nameShareBet), control = control)]
  ALL_USER[, eval(nameCongrueneBet) := ideoLeaning * get(paste0("slantBetween_",i))]
  ALL_USER[tookFollowup==TRUE, eval(paste0(nameCongrueneBet, "Scaled")) := scaleControl(get(nameCongrueneBet), control = control)]
    
  # Standardized slant between baseline and followup, used for summary stats
  ALL_USER[tookFollowup==TRUE, eval(nameSlantScale) := scaleControl(get(paste0("slantBetween_",i)), control = control)]
}


# Slant
controlsPolWithBaseNoTreat = gsub("matchTreatment \\+", "", controlsPolWithBase)

# For cross sectional regressions
ALL_USER_CONTROL = ALL_USER[treatment=="Control"]

# *************************************************************************
# REGRESSIONS ----
# *************************************************************************


# .exposure on affective ----
# .........................................................................

# ShareCounter
regShareCounterIVBet_FB_Affective =  felmR(data=ALL_USER, as.formula(paste0("polAffectiveIndex ~ ", controlsPolWithBaseNoTreat, " | 0 |  (shareCounterBet_FB ~ matchTreatment)")))
regShareCounterIVBet_FBS_Affective =  felmR(data=ALL_USER, as.formula(paste0("polAffectiveIndex ~ ", controlsPolWithBaseNoTreat, " | 0 |  (shareCounterBet_FBScaled ~ matchTreatment)")))
regShareCounterIVBet_FB_Thermo =  felmR(data=ALL_USER, as.formula(paste0("F_thermo_Diff ~ ", controlsPolWithBaseNoTreat, " | 0 |  (shareCounterBet_FB ~ matchTreatment)")))
regShareCounterIVBet_FBS_Thermo =  felmR(data=ALL_USER, as.formula(paste0("F_thermo_Diff ~ ", controlsPolWithBaseNoTreat, " | 0 |  (shareCounterBet_FBScaled ~ matchTreatment)")))

# Congruence
regCongruenceIVBet_Affective =  felmR(data=ALL_USER, as.formula(paste0("polAffectiveIndex ~ ", controlsPolWithBaseNoTreat, " | 0 |  (congruenceBet_FB ~ matchTreatment)")))
regCongruenceSIVBet_Affective =  felmR(data=ALL_USER, as.formula(paste0("polAffectiveIndex ~ ", controlsPolWithBaseNoTreat, " | 0 |  (congruenceBet_FBScaled ~ matchTreatment)")))
regCongruenceIVBet_Thermo =  felmR(data=ALL_USER, as.formula(paste0("F_thermo_Diff ~ ", controlsPolWithBaseNoTreat, " | 0 |  (congruenceBet_FB ~ matchTreatment)")))



# *************************************************************************
# MAIN EFFECTS ----
# *************************************************************************

# .exposure ----
# .........................................................................

# Effect of seeing most posts from pages or generally on affective polarization and specificially feeling thermo.
effectShareCounterBetSFBAffective = getCoef(regShareCounterIVBet_FBS_Affective, "`shareCounterBet_FBScaled(fit)`")
effectShareCounterBetFBAffective = coefficients(regShareCounterIVBet_FB_Affective)[["`shareCounterBet_FB(fit)`"]]
effectShareCounterBetSFBThermo = getCoef(regShareCounterIVBet_FBS_Thermo, "`shareCounterBet_FBScaled(fit)`")
effectShareCounterBetFBThermo = coefficients(regShareCounterIVBet_FB_Thermo)[["`shareCounterBet_FB(fit)`"]]
effectShareCounterBetFBThermoOnePct = coefficients(regShareCounterIVBet_FB_Thermo)[["`shareCounterBet_FB(fit)`"]]/100

saveToLyx(abs(effectShareCounterBetSFBAffective), "effectShareCounterBetSFBAffective", digits=2)
saveToLyx(abs(effectShareCounterBetFBThermoOnePct), "effectShareCounterBetFBThermoOnePct", digits=2)


# .congruence ----
# .........................................................................

effectCongruenceBetSFBAffective = coefficients(regCongruenceSIVBet_Affective)[["`congruenceBet_FBScaled(fit)`"]]
effectCongruenceBetFBAffective = coefficients(regCongruenceIVBet_Affective)[["`congruenceBet_FB(fit)`"]]
effectCongruenceBetFBThermo = coefficients(regCongruenceIVBet_Thermo)[["`congruenceBet_FB(fit)`"]]

saveToLyx(effectCongruenceBetSFBAffective, "effectCongruenceBetSFBAffective", digits = 2)
saveToLyx(round(effectCongruenceBetFBThermo, 2), "effectCongruenceBetFBThermo", digits = 2)


# .slant (only for slides) ----
# .........................................................................

controlsPersWithBaseNoTreat = gsub("treatment \\+", "", controlsPersWithBase)
regSlantSIVBet_Opinions =  felmR(data=ALL_USER, as.formula(paste0("persIndex ~ ", controlsPersWithBaseNoTreat, " | 0 |  (slantBet_FBScaled ~ treatment)")))
effectSlantOpinionsStd = getCoef(regSlantSIVBet_Opinions, "`slantBet_FBScaled(fit)`")

diffIdeoSlant = ALL_USER[ideoLeaning==1, mean(slantBet_FBScaled, na.rm=TRUE)] - 
  ALL_USER[ideoLeaning==-1, mean(slantBet_FBScaled, na.rm=TRUE)]
diffIdeoOpinions = ALL_USER[ideoLeaning==1, mean(persIndex, na.rm=TRUE)] - 
  ALL_USER[ideoLeaning==-1, mean(persIndex, na.rm=TRUE)]

effectSwitchFeedOpinions = effectSlantOpinionsStd*diffIdeoSlant
effectSwitchFeedOpinionsShare = effectSwitchFeedOpinions / diffIdeoOpinions
effectSwitchFeedOpinionsUpperShare = effectSwitchFeedOpinions*1.96 / diffIdeoOpinions

saveToLyx(effectSlantOpinionsStd, "effectSlantOpinionsStd", digits = 3)
saveToLyx(abs(effectSwitchFeedOpinionsShare), "effectSwitchFeedOpinionsShare", percent = TRUE)
saveToLyx(abs(effectSwitchFeedOpinionsUpperShare), "effectSwitchFeedOpinionsUpperShare", percent = TRUE)


# *************************************************************************
# COUNTER FACTUAL ----
# *************************************************************************

# .balanced FB ----
# .........................................................................

controlShareCounter_FB = ALL_USER_CONTROL[!is.na(polAffectiveIndex),  mean(shareCounterBet_FB, na.rm=TRUE)]
controlSDShareCounter_FB = ALL_USER_CONTROL[!is.na(polAffectiveIndex), sd(shareCounterBet_FB, na.rm=TRUE)]
controlCongruence_FB = ALL_USER_CONTROL[!is.na(polAffectiveIndex),  mean(congruenceBet_FB, na.rm=TRUE)]

# Increase in standard deviations required for balanced feed
diffTo50Std = (0.5 - controlShareCounter_FB)/controlSDShareCounter_FB

# Effect on affective polarization if feed was balanced
balancedShareCounterAffective = effectShareCounterBetSFBAffective * diffTo50Std

# Effect on themometer
balancedShareCounterThermo      = effectShareCounterBetSFBThermo * diffTo50Std
balancedShareCounterThermoCheck = effectShareCounterBetFBThermo * (0.5 - controlShareCounter_FB)

# Effect on affective pol. index and on thermometer if congruence was zero 
balancedCongruenceAffective = effectCongruenceBetFBAffective * -controlCongruence_FB
balancedCongruenceThermo      = effectCongruenceBetFBThermo * -controlCongruence_FB

saveToLyx(abs(balancedShareCounterThermo),"balancedShareCounterThermo", digits=2)
saveToLyx(abs(balancedCongruenceThermo), "balancedCongruenceThermo", digits=2)


# . FB=other ----
# .........................................................................

# Difference in share of counter-att in feed and when visiting news sites not through Facebook
shareCounterNavNotFromFB = ALL_USER_CONTROL[!is.na(polAffectiveIndex) & !is.na(shareCounterBet_FB), mean(shareCounterBet_NavNotFromFB, na.rm=TRUE)]
shareCounterFeed = ALL_USER_CONTROL[!is.na(polAffectiveIndex) & !is.na(shareCounterBet_NavNotFromFB), mean(shareCounterBet_FB, na.rm=TRUE)]
diffShareCounter = shareCounterNavNotFromFB - shareCounterFeed

# Difference in congruence
diffCongruence = ALL_USER_CONTROL[!is.na(congruenceBet_FB), mean(congruenceBet_NavNotFromFB, na.rm=TRUE)] - 
  ALL_USER_CONTROL[!is.na(congruenceBet_NavNotFromFB), mean(congruenceBet_FB, na.rm=TRUE)]

equateShareCounterAffective      = effectShareCounterBetFBAffective * diffShareCounter
equateShareCounterThermo         = effectShareCounterBetFBThermo * diffShareCounter

equateCongruenceAffective        = effectCongruenceBetFBAffective * diffCongruence
equateCongruenceThermo           = effectCongruenceBetFBThermo * diffCongruence

saveToLyx(shareCounterNavNotFromFB, "shareCounterNavNotFromFB", percent = TRUE)
saveToLyx(abs(equateCongruenceThermo), "equateCongruenceThermo", digits = 2)
saveToLyx(abs(equateShareCounterThermo), "equateShareCounterThermo", digits = 2)


# .share diff control group ----
# .........................................................................

# Control group cross sectional regressions
regShareCounterIVBet_FBS_ThermoStdControl =  felmR(data=ALL_USER_CONTROL, F_thermo_DiffScale ~ shareCounterBet_FBScaled)
regShareCounterIVBet_FBS_AffectiveControl =  felmR(data=ALL_USER_CONTROL, polAffectiveIndex ~ shareCounterBet_FBScaled)

regCongruenceSIVBet_ThermoStdControl =  felmR(data=ALL_USER_CONTROL, F_thermo_DiffScale ~ congruenceBet_FBScaled)
regCongruenceSIVBet_AffectiveControl =  felmR(data=ALL_USER_CONTROL, polAffectiveIndex ~ congruenceBet_FBScaled)

# Control group effects
effectShareCounterBetSFBThermoStdControl = getCoef(regShareCounterIVBet_FBS_ThermoStdControl, "shareCounterBet_FBScaled")
effectShareCounterBetSFBAffectiveControl = getCoef(regShareCounterIVBet_FBS_AffectiveControl, "shareCounterBet_FBScaled")

effectCongruenceBetSFBThermoStdContol = coefficients(regCongruenceSIVBet_ThermoStdControl)[["congruenceBet_FBScaled"]]
effectCongruenceBetSFBAffectiveContol = coefficients(regCongruenceSIVBet_AffectiveControl)[["congruenceBet_FBScaled"]]

# Share of control group effect
effectCounterSShareControlGroupAffective = effectShareCounterBetSFBAffective / effectShareCounterBetSFBAffectiveControl
effectCongruenceSShareControlGroupAffective = effectCongruenceBetSFBAffective / effectCongruenceBetSFBAffectiveContol

saveToLyx(round(-effectShareCounterBetSFBAffectiveControl, 2), "effectShareCounterBetSFBAffectiveControl")
saveToLyx(effectCounterSShareControlGroupAffective, "effectCounterSShareControlGroupAffective", percent = TRUE)
saveToLyx(effectCongruenceSShareControlGroupAffective, "effectCongruenceSShareControlGroupAffective", percent = TRUE)


# *************************************************************************
# PRESENT ----
# *************************************************************************

# . calculate first stage and add to table ----
# .........................................................................

regshareCounterBet_FBScaled = lmR(data=ALL_USER[tookFollowup==TRUE], reformulate(controlsPolWithBase, "shareCounterBet_FBScaled"))
regcongruenceBet_FBScaled =  lmR(data=ALL_USER[tookFollowup==TRUE], reformulate(controlsPolWithBase, "congruenceBet_FBScaled"))

if (ALL_USER[treatment=="Control", .N] > 0) {
  shareCounterFirstStageF = car::linearHypothesis(regshareCounterBet_FBScaled, c("matchTreatmentCounter=0", "matchTreatmentPro=0"), white.adjust="hc1")$F[2]
  congruenceFirstStageF = car::linearHypothesis(regcongruenceBet_FBScaled, c("matchTreatmentCounter=0", "matchTreatmentPro=0"), white.adjust="hc1")$F[2]
} else {
  shareCounterFirstStageF = car::linearHypothesis(regshareCounterBet_FBScaled, c("matchTreatmentCounter=0"), white.adjust="hc1")$F[2]
  congruenceFirstStageF = car::linearHypothesis(regcongruenceBet_FBScaled, c("matchTreatmentCounter=0"), white.adjust="hc1")$F[2]
}


# .elasticities ----
# .........................................................................

# Affective polarization index - only control
stargazerR(regShareCounterIVBet_FBS_AffectiveControl, regCongruenceSIVBet_AffectiveControl,
           dep.var.labels.include = TRUE, dep.var.labels = rep(c("OLS"), 2),
           column.labels = rep("Affective Polarization"), column.separate=c(2), 
           add.lines = list(c("Data", rep(c("Control Group"),2))),
           keep=c("Counter", "congruence")
           , type="latex", replaceCov = replaceWords, file = "Output/Tables/Combined/elasticityAffectiveControlGroup.tex")

# Affective polarization index - only treat
stargazerR(regShareCounterIVBet_FBS_Affective, regCongruenceSIVBet_Affective, 
           dep.var.labels.include = TRUE, dep.var.labels = rep(c("IV"), 2), omit.table.layout="n",
           column.labels = rep("Affective Polarization"), column.separate=c(4),
           add.lines = list(c("Controls", rep("X",2)), 
                            c("First Stage F", round(c(shareCounterFirstStageF, congruenceFirstStageF), 2))),
           keep=c("Counter", "congruence")
           , type="latex", replaceCov = replaceWords, file = "Output/Tables/Combined/elasticityAffective.tex")



# .Counterfactual (only for slides)----
# .........................................................................

counterfactualFile = "Output/Tables/Combined/counterfactual.tex"
counterFactualReg = stargazerR(regShareCounterIVBet_FB_Affective, regCongruenceIVBet_Affective, regShareCounterIVBet_FB_Thermo, regCongruenceIVBet_Thermo,
           add.lines = list(c("Controls", "X", "X", "X", "X"), 
                            c("\\hline Control Group^^^ Counter Share", rep(c(round(controlShareCounter_FB, 2), ""), 2)),
                            c("Effect of Counter Share = 0.5", round(balancedShareCounterAffective, 2), "", round(balancedShareCounterThermo, 2)),
                            c("\\hline Control Group^^^ Congruence", rep(c("", round(controlCongruence_FB, 2)), 2)),
                            c("Effect of Congruence Scale = 0", "", round(balancedCongruenceAffective, 2), "", round(balancedCongruenceThermo, 2)),
                            c("\\hline Control Group^^^ Diff in Counter Share", rep(c(round(diffShareCounter, 2), ""), 2)),
                            c("Effect of Equating Counter Share", round(equateShareCounterAffective, 2), "", round(equateShareCounterThermo, 2)),
                            c("\\hline Control Group^^^ Diff in Congruence", rep(c("", round(diffCongruence, 2)), 2)),
                            c("Effect of Equating Congruence", "", round(equateCongruenceAffective, 2), "", paste0(round(equateCongruenceThermo, 2)))),
           column.labels = c(rep("\\multirow{2}{1.75cm}{Affective Pol., Std. Dev.}", 2),
                             rep( "\\multirow{2}{1.75cm}{Feeling Thermo., Degrees}", 2)), column.separate=c(1,1,1,1),
           dep.var.labels = c("IV", "IV"), dep.var.labels.include = TRUE, column.sep.width = "0pt",
           keep=c("Counter", "congruence")
           , type="latex", replaceCov = replaceWords, file = counterfactualFile)
addLinePrint(counterFactualReg, counterfactualFile, count = 2)


# .swithFeed ----
# .........................................................................

regSlantBetScaled =  lmR(data=ALL_USER[tookFollowup==TRUE], reformulate(gsub("thermo_TrumpHave \\+", "", controlsPersWithBase), 
                                                                        "slantBet_FBScaled"))


if (ALL_USER[treatment=="Control", .N] > 0) {
  slantFirstStageF = car::linearHypothesis(regSlantBetScaled, c("treatmentConservative=0", "treatmentLiberal=0"), white.adjust="hc1")$F[[2]]
} else {
  slantFirstStageF = car::linearHypothesis(regSlantBetScaled, c("treatmentConservative=0"), white.adjust="hc1")$F[[2]]
}


stargazerR(regSlantSIVBet_Opinions, 
           add.lines = list(c("First Stage F-Stat", round(slantFirstStageF, 2)), 
                            c("Control Difference in Slant^^^ Conservative - Liberal", round(diffIdeoSlant, 3)),
                            c("Effect of Switching Feeds", round(effectSwitchFeedOpinions, 3)),
                            c("Control Difference in Pol. Opinoins^^^ Conservative - Liberal", round(diffIdeoOpinions, 3)),
                            c("Effect of Switching Feed, Share of Control Group", Hmisc::latexTranslate(scales::percent(effectSwitchFeedOpinionsShare, 1)))),
           keep = "slant"
   , type="latex", replaceCov = replaceWords, file = "Output/Tables/Combined/swithFeedEffect.tex")

