# Author: Ro'ee Levy, Yale
# Date: Fri Nov 30 10:03:29 2018
# Purpose: Analyze effect on the affective polarization outcomes
# --------------#

rm(list=ls()) 
source("Code/Utils/log.R"); beginLogging()

require(data.table)
require(dplyr)
library(fst)
library(ggpubr) # for theme_transparent

source("Code/Utils/robustReg.R")
source("Code/Utils/indexFunctions.R")
source("Code/Utils/commonVars.R")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")



# *************************************************************************
# PREPARE ----
# *************************************************************************


# .bounds ----
# .........................................................................

# DT = myDataPro; regForm = formulaFull
myLeeBoundsControls <- function(DT, regForm) {
  trimGroup = ifelse(DT[treat==1, mean(selection)] > DT[treat==0, mean(selection)], 1, 0)
  otherGroup = 1 - trimGroup
  
  shareHigher = DT[eval(treat)==trimGroup, mean(selection)] 
  shareLower = DT[eval(treat)==otherGroup, mean(selection)]
  shareTrim = shareHigher - shareLower
  
  # Calculate share that need to be trimmed
  q = (shareHigher - shareLower) / shareHigher 
  removeNum = round(DT[treat==eval(trimGroup) & selection, .N*q])
  removeNum2 = round(DT[treat==eval(trimGroup), .N*(shareTrim)])
  stopifnot(removeNum==removeNum2)
  
  # Mark highest and lowest values
  DT_ONLY_FOLLOW = data.table::copy(DT[selection==1])
  DT_ONLY_FOLLOW[, rankOutcome := rank(outcome, ties.method = "random"), by="treat"]
  DT_ONLY_FOLLOW[, rankOutcomeNeg := rank(-outcome, ties.method = "random"), by="treat"]
  DT_ONLY_FOLLOW[, trimMin := treat==eval(trimGroup) & rankOutcome <= removeNum]
  DT_ONLY_FOLLOW[, trimMax := treat==eval(trimGroup) & rankOutcomeNeg <= removeNum]
  
  regNoMin = lm(data=DT_ONLY_FOLLOW[trimMin==FALSE], formula=regForm)
  bound1 = getCoef(regNoMin, "treat")
  
  regNoMax = lm(data=DT_ONLY_FOLLOW[trimMax==FALSE], formula=regForm)
  bound2 = getCoef(regNoMax, "treat")
  
  return (list(lowerBound = min(bound1, bound2), upperBound = max(bound1, bound2)))
}

# Took followup and have affective polarization outcome
SURVEY[, tookFollowupAffective := ifelse(is.na(ideoLeaningF), NA, !is.na(polAffectiveIndex))]
SURVEY[, tookFollowupAffectiveNoLast := ifelse(is.na(F_withoutLastControl), FALSE, F_withoutLastControl) & 
      tookFollowupAffective]

# Want to control bounds for each treatment compared to control group separately 
controls = c("ideologyF", "partyAllF", "trumpF", "genderF", "ageWM", "ageSqWM", "ageHave", "ideoLeaningF", 
             "thermo_DiffWM", "thermo_DiffHave", "empathyDifficult_DiffHave", "empathyDifficult_DiffWM")

formulaFull = reformulate(gsub("matchTreatment", "treat", controlsPolWithBase), "polAffectiveIndex")
formulaPart = reformulate(gsub("matchTreatment", "treat", controlsPolBasic), "polAffectiveIndex")
formulaNone = reformulate("treat", "polAffectiveIndex")

myData = SURVEY[, append(list(selection = tookFollowupAffective, outcome = polAffectiveIndex,
                           selectNoLast = tookFollowupAffectiveNoLast), .SD),
                       .SDcols=c(controls, "polAffectiveIndex", "matchTreatment")]

myDataPro = myData[matchTreatment %in% c("Control", "Pro")]
myDataPro[, treat := as.numeric(matchTreatment=="Pro")]
myDataProNoLast = data.table::copy(myDataPro)[, selection := selectNoLast]

myDataCounter = myData[matchTreatment %in% c("Control", "Counter")]
myDataCounter[, treat:= as.numeric(matchTreatment=="Counter")]
myDataCounterNoLast = data.table::copy(myDataCounter)[, selection := selectNoLast]

# Create variable to store bounds
polBoundResults = as.data.table(expand.grid(list(controls = c("FullNoLast", "Full", "Part", ""), 
                                                 treat = c("Pro", "Counter"))))
polBoundResults[, name := paste0("myData", treat, controls)]
polBoundResults[, shortName := paste0("myData", treat)]
polBoundResults[controls=="FullNoLast", shortName := paste0("myData", treat, "NoLast")]
polBoundResults[, regForm := ifelse(grepl("Full", controls), "formulaFull", 
                                    ifelse(controls=="Part", "formulaPart", "formulaNone"))]

for (i in 1:nrow(polBoundResults)) {
  polBoundResults[i, lowerBound := myLeeBoundsControls(get(shortName), get(regForm))$lowerBound]
  polBoundResults[i, upperBound := myLeeBoundsControls(get(shortName), get(regForm))$upperBound]
}


# *************************************************************************
# REGRESSIONS  ----
# *************************************************************************

# .Main reg ----
# .........................................................................

# Primary 
regPolAffective = lmR(data=SURVEY, formula = reformulate(controlsPolWithBase, "polAffectiveIndex"))
regPolAffectiveNoCG = lmR(data=SURVEY[treatment!="Control"], formula = reformulate(controlsPolWithBase, "polAffectiveIndex"))


# .attrition ----
# Compare treatments, differential attrition and controls
# .........................................................................

regPolAffectiveNoControls = lmR(data=SURVEY, formula = polAffectiveIndex ~  matchTreatment )
regPolAffectiveBasic = lmR(data=SURVEY, formula = reformulate(controlsPolBasic, "polAffectiveIndex"))
regPolAdjAffectiveAll = lmR(data=SURVEY, formula = reformulate(controlsPolWithBase, "polAffectiveIndex"))

# Diff attrition 
regWithoutLastControl = lmR(data=SURVEY[F_withoutLastControl==TRUE], formula = reformulate(controlsPolWithBase, "polAffectiveIndex"))

# User bounds calculated earler 
for (i  in c("Pro", "Counter")) {
  for (j in c("lower", "upper")) {
    assign(paste0(i,j), round(c(polBoundResults[treat==i & controls=="", get(paste0(j, "Bound"))], 
                           polBoundResults[treat==i & controls=="Part", get(paste0(j, "Bound"))],
                           polBoundResults[treat==i & controls=="Full", get(paste0(j, "Bound"))],
                           polBoundResults[treat==i & controls=="FullNoLast", get(paste0(j, "Bound"))]), 3))
  }
}
  
# Main regression with different controls and Lee bounds
stargazerR(regPolAffectiveNoControls, regPolAffectiveBasic, regPolAdjAffectiveAll, regWithoutLastControl,
           add.lines = append(append(list(c("Pro-Att. Lower Lee Bound", Prolower),
                                          c("Pro-Att. Upper Lee Bound", Proupper),
                                          c("Counter-Att. Lower Lee Bound", Counterlower),
                                          c("Counter-Att. Upper Lee Bound", Counterupper),
                                          c("\\hline \\\\[-4ex]")),
             diffFValueAddRow("Pro-Att. - Counter-Att. Treat", 
                  list(regPolAffectiveNoControls, regPolAffectiveBasic, regPolAdjAffectiveAll, regWithoutLastControl),
                        matchTreatments)),
                            list(c("Common Controls", " ", "X", "X", "X"),
                                 c("Baseline Polarization Controls", "", "", "X", "X"),
                                 c("Ex. Last Control Group Responders", "", "", "", "X"))),
           keep = c("*Treatment*"), omit.table.layout="n"
           , replaceCov = replaceWords ,type = "latex", file = "Output/Tables/Polarization/affectiveControls.tex") 

SURVEY[!is.na(ideoLeaning), mean(tookFollowupAffective), by="treatment"]
SURVEY[!is.na(ideoLeaning), mean(tookFollowupAffectiveNoLast), by="treatment"]

# .components reg ----
# Used in slides
# .........................................................................

for(i in unique(c(polVarsScale)))  {
  controlsPolBasicLag = controlsPolBasic
  if (grepl("F_empathy", 1)) {controlsPolBasicLag = paste(controlsPolBasic, "empathyDifficult_DiffHave + empathyDifficult_DiffWM", sep=" + ")}
  if (grepl("F_thermo_DiffChange", 1)) {controlsPolBasicLag = paste(controlsPolBasic, "thermo_DiffHave + thermo_DiffWM", sep=" + ")}
  
  assign(paste0("regInd", i), lmR(data=SURVEY, formula = reformulate(controlsPolWithBase, i)))
  assign(paste0("regIndNoCG", i), lmR(data=SURVEY[matchTreatment!="Control"], formula = reformulate(controlsPolWithBase, i)))
}

affectiveIndNames = c("Feeling Thermometer", "Difficult Perspective", 
                      "Consider Perspective", "Party Ideas", "Marry Opposing Party")
polarizationMulti = paste0(paste0("\\multirow{2}{2 cm}{", affectiveIndNames), "}")

starIndPol = stargazerR(regPolAffective, regIndF_thermo_DiffScale, regIndF_empathyDifficult_DiffScale, 
                        regIndF_empathyImportant_DiffScale, regIndF_partyIdeas_DiffScale, regIndF_marry_OpposingScale,
                        column.labels = c("Index", polarizationMulti),
                        add.lines = diffFValueAddRow("Counter - Pro", 
                   list(regPolAffective, regIndF_thermo_DiffScale, regIndF_empathyDifficult_DiffScale, 
                        regIndF_empathyImportant_DiffScale, regIndF_partyIdeas_DiffScale, regIndF_marry_OpposingScale), 
                                     rev(matchTreatments)),
                        keep = "match", order = rev(levels(SURVEY$matchTreatment))
                        , replaceCov = replaceWords,  type = "latex")

index = which(grepl("Marry", starIndPol))
starIndPol = gsub("lccc", "lc|cc", starIndPol)
starIndPol = c(starIndPol[1:index], "\\\\", starIndPol[(index+1):length(starIndPol)]) # Add space after separarting columns to two rows
cat(starIndPol, file = "Output/Tables/Polarization/affectiveIndFollow.tex", sep = "\n")


# .exlcuding one component ----
# .........................................................................

# 4/5 polarization
for (i in polVarsAffectiveFollow) {
  name = gsub("F_", "F_ex", i)
  SURVEY[, eval(name) := createStandardIndex(SURVEY %>% dplyr::select(polVarsAffectiveFollow, -i), 
                                             control = control)]
}


# Run affective polarization with 4 out of 5 components 
for(i in polVarsAffectiveFollow)  {
  nameExclude = gsub("F_", "F_ex", i)
  assign(paste0("regEx", i), lmR(data=SURVEY, formula = reformulate(controlsPolWithBase, nameExclude)))
}

starExPolfile = "Output/Tables/Polarization/exMeasureAffective.tex" 
starExPol = stargazerR(regPolAffective, regExF_thermo_Diff, regExF_empathyDifficult_Diff, 
                       regExF_empathyImportant_Diff, regExF_partyIdeas_Diff, regExF_marry_Opposing,
                        add.lines = append(diffFValueAddRow("Pro - Counter", 
                                   list(regPolAffective, regExF_thermo_Diff, regExF_empathyDifficult_Diff,
                                        regExF_empathyImportant_Diff, regExF_partyIdeas_Diff, regExF_marry_Opposing), 
                                                     matchTreatments),
                             list(c("Excluded Measure", "", polarizationMulti))),
                        keep = "match", replaceCov = replaceWords,  type = "latex")
addLinePrint(starExPol, starExPolfile, count = 2)


# .Pre-analysis Plan ----
# .........................................................................

# By family and match
regPol = lmR(data=SURVEY, formula = reformulate(controlsPolWithBase, "polIndexAll"))
regPolAffective = lmR(data=SURVEY, formula = reformulate(controlsPolWithBase, "polAffectiveIndex"))
regPolBehavior = lmR(data=SURVEY, formula = reformulate(controlsPolWithBase, "polIndexBehavior"))

stargazerR(regPol, regPolAffective, regPolBehavior,
           dep.var.labels = rep(c("All", "Affective", "Behavior"),2), dep.var.labels.include = TRUE, model.numbers = FALSE,
           add.lines = append(diffFValueAddRow("Counter-Att. Treatment - Pro-Att. Treat.", 
                                               list(regPol, regPolAffective, regPolBehavior),matchTreatments),
                              list(c("Controls", rep("X", 3)))),
           keep = c("*Treatment*")
           , replaceCov = replaceWords, type = "latex", file = "Output/Tables/Polarization/polarizationFamilies.tex")


# .own other party ----
# .........................................................................

controlsPolWithBaseOwn = gsub("_Diff", "_Own", controlsPolWithBase)
controlsPolWithBaseOpposing = gsub("_Diff", "_Opposing", controlsPolWithBase)

# Effect on own party vs other party
createMissing(SURVEY, list("thermo_Own", "thermo_Opposing", "empathyDifficult_Own", "empathyDifficult_Opposing"))
regPolAffectiveOwn = lmR(data=SURVEY, formula = reformulate(controlsPolWithBaseOwn, "polAffectiveIndexOwn"))
regPolAffectiveOpposing = lmR(data=SURVEY, formula = reformulate(controlsPolWithBaseOpposing, "polAffectiveIndexOpposing"))

# No control group, compare all three for plot
regPolAffectiveNoMarryNoCG = lmR(data=SURVEY[treatment!="Control"], formula = reformulate(controlsPolWithBase, "polAffectiveIndexNoMarry"))
regPolAffectiveOwnNoCG = lmR(data=SURVEY[treatment!="Control"], formula = reformulate(controlsPolWithBase, "polAffectiveIndexOwn"))
regPolAffectiveOpposingNoCG = lmR(data=SURVEY[treatment!="Control"], formula = reformulate(controlsPolWithBase, "polAffectiveIndexOpposing"))

stargazerR(regPolAffectiveOwn, regPolAffectiveOpposing, 
           dep.var.labels = c("Attitude Own Party", "Attitude Opposing Party"), dep.var.labels.include=TRUE,
           add.lines = diffFValueAddRow("Pro - Counter", list(regPolAffectiveOwn, regPolAffectiveOpposing), matchTreatments),
           keep = "match" 
           , replaceCov = replaceWords ,type = "latex", file = "Output/Tables/Polarization/affectiveOwnOther.tex")



# *************************************************************************
# PLOT ----
# *************************************************************************

# .Plot ind reg ----
# .........................................................................

models = c("regPolAffectiveNoCG", paste0("regIndNoCG", polVarsAffectiveFollow, "Scale"))
mainModels = rbindlist(lapply(as.list(models), function(x) tidyRob(get(x), x, ci=0.9)))

modelsForPlot = as.data.table(tidyr::extract(mainModels, modelName, c("family", "dv"), "(.*NoCG)(.*)", remove=FALSE))
modelsForPlot = modelsForPlot[term %in% c("matchTreatmentPro", "matchTreatmentCounter", "treatmentLiberal", "treatmentConservative") |
                                grepl("treatIdeo", term)]
modelsForPlot[, dvF := factor(dv, levels = c("", "F_thermo_DiffScale" ,"F_empathyDifficult_DiffScale" ,"F_empathyImportant_DiffScale", 
                                             "F_partyIdeas_DiffScale", "F_marry_OpposingScale"),
                              labels=c("Affective Polarization\nIndex", affectiveIndNames))]
modelsForPlot[, familyF := ifelse(grepl("Index", dvF), "Index", "Individual Items")]
modelsForPlot[, treatF_NoCG := factor(term, levels = termLevelsNoCG, labels = termLabelsNoCG )]
modelsForPlot[, limits := max(estimate)*2.25 * (0.73-(dvF %in% c("Pro-Att.", "Liberal Outlets"))), by="familyF"]

currentTheme = theme(legend.position = c("none"), axis.title.y = element_blank(), panel.border = element_rect(fill=NA),
                     axis.text.y=element_text(hjust=0), plot.margin = margin(5,2,5,15))

regressionPlot(ggplot(modelsForPlot[familyF=="Individual Items"], aes(x=reorder(revFactor(dvF), -estimate), y=estimate, color=treatF_NoCG))) + 
  scale_color_manual(values = "black") +
  ylab("Intention to Treat, Standard Deviations") + 
  scale_y_continuous(limits = c(-0.075, 0.075), breaks = seq(-0.06, 0.06, 0.02)) +
  facet_wrap(~treatF_NoCG, nrow=2, scales="free_y")

ggMySaveBeamer("Output/Graphs/Polarization/polIndOrder_Beamer.eps", theme=currentTheme)
ggMySaveDoc("Output/Graphs/Polarization/polIndOrder.eps", theme=currentTheme)


# .own other party ----
# .........................................................................

regModels = rbindlist(lapply(list("regPolAffectiveNoMarryNoCG", "regPolAffectiveOwnNoCG", "regPolAffectiveOpposingNoCG"), 
                             function(x) tidyRob(get(x), x)))

regModels[, noCG := grepl("NoCG", modelName)]
regModels = regModels[term %in% c("matchTreatmentPro", "matchTreatmentCounter")]
regModels[, dvNameF := factor(dvName, levels = dvLevels, labels = dvLabels)]
regModels[, familyFNoCG := factor(term, levels = termLevelsNoCG, labels = termLabelsNoCG)]
regModels[, familyF := factor(term, levels = termLevels, labels = termLabels)]

regressionPlot(plot = ggplot(regModels[noCG==TRUE], aes(x=dvNameF, y=estimate))) +
  facet_wrap(~familyFNoCG)

currentTheme = theme(legend.position = c("none"), axis.title.y = element_blank(), panel.border = element_rect(fill=NA),
                     axis.text.y=element_text(hjust=0), plot.margin = margin(5,2,5,15), axis.title.x=element_blank())       
ggMySaveBeamer("Output/Graphs/Polarization/polarizationOwnOtherParty_Beamer.eps", theme=currentTheme)




# *************************************************************************
# DESCRIPTIVE ----
# *************************************************************************

# .correlation with partisanshop ----
# .........................................................................

behaviorAffective = sapply(polVarsAffectiveFollow, function(x) {SURVEY[, cor(abs(party7), get(x), use="complete.obs")]})
behaviorCor = sapply(polVarsBehavior, function(x) {SURVEY[, cor(abs(party7), get(x), use="complete.obs")]})

minBehaviorPartisanCor = min(behaviorCor)
maxBehaviorPartisanCor = max(behaviorCor)

minAffectivePartisanCor = min(behaviorAffective)
maxAffectivePartisanCor = max(behaviorAffective)

lapply(list("minBehaviorPartisanCor", "maxBehaviorPartisanCor", "minAffectivePartisanCor", "maxAffectivePartisanCor"), 
       function(x) saveToLyx(get(x), x))


# *************************************************************************
# MAGNITUDE ----
# *************************************************************************

SURVEY[, complyMatchPro := comply & matchTreatment=="Pro"]
SURVEY[, complyMatchCounter := comply & matchTreatment=="Counter"]

controlsPolWithBaseFEIV = paste0(gsub("matchTreatment \\+ ", "", controlsPolWithBase), " | 0 | ", '(complyMatchPro|complyMatchCounter ~ matchTreatment)')


regPolAffectiveIVWithBase = felmR(data=SURVEY, formula = reformulate(controlsPolWithBaseFEIV, "polAffectiveIndex"))
regPolThermoIVWithBase = felmR(data=SURVEY, formula = reformulate(controlsPolWithBaseFEIV, "F_thermo_Diff"))


# .descriptive ----
# .........................................................................

regPolThermoNoCG = lmR(data=SURVEY[treatment!="Control"], formula = reformulate(controlsPolWithBase, "F_thermo_Diff"))


# Compare to population
polDiffIdeo = SURVEY[treatment=="Control" & absIdeology==3, mean(polAffectiveIndex, na.rm=TRUE)] - 
  SURVEY[treatment=="Control" & absIdeology==0, mean(polAffectiveIndex, na.rm=TRUE)]
polDiffPartisanAffective = SURVEY[treatment=="Control" & abs(party7)==1, mean(polAffectiveIndex, na.rm=TRUE)] - 
  SURVEY[treatment=="Control" & party7==0, mean(polAffectiveIndex, na.rm=TRUE)]
saveToLyx(round(polDiffPartisanAffective,3), "polDiffPartisan")

# Effect of treatments
polDiffEffect = regPolAffective$coefficients[["matchTreatmentPro"]] - 
                regPolAffective$coefficients[["matchTreatmentCounter"]]
thermoEffect = regPolThermoNoCG$coefficients[["matchTreatmentCounter"]] 

# LATE Effects
polDiffEffectLATE = getCoef(regPolAffectiveIVWithBase, '`complyMatchPro(fit)`') - 
  getCoef(regPolAffectiveIVWithBase, '`complyMatchCounter(fit)`')
thermoEffectLATE = getCoef(regPolThermoIVWithBase, '`complyMatchPro(fit)`') - 
  getCoef(regPolThermoIVWithBase, '`complyMatchCounter(fit)`')

saveToLyx(polDiffEffect, "polDiffEffect", digits = 2)
saveToLyx(polDiffEffectLATE, "polDiffEffectLATE", digits = 2)
saveToLyx(round(thermoEffect,2), "thermoEffect")
saveToLyx(abs(thermoEffect), "thermoEffectAbs")
saveToLyx(round(thermoEffectLATE,2), "thermoEffectLATE")

# Share of effect
shareAffectivePartisan = scales::percent(polDiffEffect/polDiffPartisanAffective, accuracy = 0.1)
shareAffectivePartisanLATE = scales::percent(polDiffEffectLATE/polDiffPartisanAffective, accuracy = 0.1)
saveToLyx(shareAffectivePartisan, "sharePolEffectPartisan")
saveToLyx(shareAffectivePartisanLATE, "sharePolEffectPartisanLATE")

