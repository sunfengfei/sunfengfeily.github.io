# Author: Ro'ee Levy, Yale
# Date: Thu May 30 17:18:52 2019
# Purpose: Analyze effect on the political opinions and polarization indices
# --------------#


require(data.table)
library(fst)
library(ggpubr)

rm(list=ls()) #Remove everything
source("Code/Utils/log.R"); beginLogging()

source("Code/Utils/robustReg.R")
source("Code/Utils/indexFunctions.R")
source("Code/Utils/commonVars.R")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")

US_POP <- readRDS("Datasets/ExternalSurveys/usPop.rds")
US_POP_IDEO <- readRDS("Datasets/ExternalSurveys/usPopIdeo.rds")

# .Add weights ----
# .........................................................................

#WEIGHTS = as.data.table(addWeightsOld(SURVEY[tookFollowup==TRUE], US_POP, reweightVars, "ResponseId"))
WEIGHTS = as.data.table(addWeights(SURVEY[tookFollowup==TRUE], US_POP, reweightVars, "ResponseId"))

#WEIGHTS_IDEO = as.data.table(addWeightsOld(SURVEY[tookFollowup==TRUE], US_POP_IDEO, reweightVars, "ResponseId"))
WEIGHTS_IDEO = as.data.table(addWeights(SURVEY[tookFollowup==TRUE], US_POP_IDEO, reweightVars, "ResponseId"))

SURVEY = merge(SURVEY, WEIGHTS[, list(ResponseId, weightPers = SampleWeight)], by="ResponseId")
SURVEY = merge(SURVEY, WEIGHTS_IDEO[, list(ResponseId, weightPol = SampleWeight)], by="ResponseId")



# Check results
#SURVEY[, lapply(.SD, mean, na.rm=TRUE), .SDcols = reweightVars]
#SURVEY[, lapply(.SD, weighted.mean, weightPers, na.rm=TRUE), .SDcols = reweightVars]
#SURVEY[, lapply(.SD, weighted.mean, weightPol, na.rm=TRUE), .SDcols = reweightVars]


# *************************************************************************
# RUN REGRESSIONS  ----
# *************************************************************************

# .Polarization ----
# .........................................................................

# Main regression with and without controls, and without control group
regPolAffective_Primary = lmR(reformulate(controlsPolWithBase, "polAffectiveIndex"), data=SURVEY)
regPolAffective_Un = lmR(reformulate("matchTreatment", "polAffectiveIndex"), data=SURVEY)
regPolAffective_NoCG = lmR(reformulate(controlsPolWithBase, "polAffectiveIndex"), data=SURVEY[treatment!="Control"])

# Only primary outlets with and without control group, control for outlets
regPolAffective_PrimaryOutlets = lmR(reformulate(controlsPolWithBase, "polAffectiveIndex"), data=SURVEY[primaryOutlets==TRUE])
regPolAffective_NoCG_PrimaryOutlets = lmR(reformulate(controlsPolWithBase, "polAffectiveIndex"), data=SURVEY[treatment!="Control" & primaryOutlets==TRUE])
regPolAffective_ControlOutlets = felmR(reformulate(paste0(controlsPolWithBase, "| potentialText"), "polAffectiveIndex"), data=SURVEY)
regPolAffective_NoCG_ControlOutlets = felmR(reformulate(paste0(controlsPolWithBase, "| potentialText"), "polAffectiveIndex"), data=SURVEY[treatment!="Control"])

# Reweight to match population
regPolAffective_Weighted = lmR(reformulate(controlsPolWithBase, "polAffectiveIndex"), data=SURVEY, weights=SURVEY$weightPol)

# Specific subsamples
regPolAffective_Ext = lmR(reformulate(controlsPolWithBase, "polAffectiveIndex"), data=SURVEY[installTwoWeeks==TRUE])
regPolAffective_Posts = lmR(reformulate(controlsPolWithBase, "polAffectiveIndex"), data=SURVEY[have14DaysPosts==TRUE])
regPolAffective_All = lmR(reformulate(controlsPolWithBase, "polAffectiveIndex"), data=SURVEY[installTwoWeeks==TRUE & have14DaysPosts==TRUE])

# Different indexing
regPolAffective_Anderson = lmR(formula = reformulate(controlsPolWithBase, "polAffectiveAnderson"), data=SURVEY)
regPolAffective_AndersonWM = lmR(formula = reformulate(controlsPolWithBase, "polAffectiveAndersonWithMissing"), data=SURVEY)

# Treatment * ideology
controlsWithBase4 = paste0(gsub("treatment", "treatIdeo + ideoLeaningF", controlsPersWithBase), " | potentialText")
regPolAffective_TreatIdeo = felmR(reformulate(controlsPolWithBase4, "polAffectiveIndex"), data=SURVEY)




# .Political Opinions ----
# .........................................................................

# Main regression with and without controls, and without control group
regPers_Primary = lmR(reformulate(controlsPersWithBase, "persIndex"), data=SURVEY)
regPers_Un = lmR(reformulate("treatment", "persIndex"), data=SURVEY)

regPers_NoCG = lmR(reformulate(controlsPersWithBase, "persIndex"), data=SURVEY[treatment!="Control"])

# Only primary outlets with and without control group, control for outlets
regPers_MainOutlets = lmR(reformulate(controlsPersWithBase, "persIndex"), data=SURVEY[primaryOutlets==TRUE])
regPers_NoCG_MainOutlets = lmR(reformulate(controlsPersWithBase, "persIndex"), data=SURVEY[treatment!="Control" & primaryOutlets==TRUE])
regPers_ControlOutlets = felmR(reformulate(paste0(controlsPersWithBase, "| potentialText"), "persIndex"), data=SURVEY)
regPers_NoCG_ControlOutlets = felmR(reformulate(paste0(controlsPersWithBase, "| potentialText"), "persIndex"), data=SURVEY[treatment!="Control"])

# Subsamples
regPers_Ext = lmR(reformulate(gsub("+ thermo_TrumpHave", "", controlsPersWithBase), "persIndex"), data=SURVEY[installTwoWeeks==TRUE])
regPers_Posts = lmR(reformulate(controlsPersWithBase, "persIndex"), data=SURVEY[have14DaysPosts==TRUE])
regPers_All = lmR(reformulate(gsub("+ thermo_TrumpHave", "", controlsPersWithBase), "persIndex"), data=SURVEY[installTwoWeeks==TRUE & have14DaysPosts==TRUE])

# Weight to match US population
regPers_Weighted = lmR(reformulate(controlsPersWithBase, "persIndex"), data=SURVEY, weights=SURVEY$weightPers)

# Different indexing
regPers_Anderson = lmR(reformulate(controlsPersWithBase, "persAnderson"), data=SURVEY)
regPers_AndersonNoNeg = lmR(reformulate(controlsPersWithBase, "persAndersonNoNeg"), data=SURVEY)
regPers_AndersonWM = lmR(reformulate(controlsPersWithBase, "persAndersonWithMissing"), data=SURVEY)
regPers_AndersonNoNegWM = lmR(reformulate(controlsPersWithBase, "persAndersonNoNegWithMissing"), data=SURVEY)

# treat*ideo
regPers_TreatIdeo = felmR(reformulate(controlsWithBase4, "persIndex"), data=SURVEY)


# .hetero reg ----
# .........................................................................

models = c("Pers", "Pol") 
heteroReg = as.data.table(expand.grid(var = heteroVars, model = models))

# Add outcome, controls
heteroReg[, outcome := ifelse(model=="Pers", "persIndex", ifelse(model=="Pol", "polAffectiveIndex", NA))]
heteroReg[, controls := ifelse(model=="Pers", controlsPersWithBase, controlsPolWithBase)]
heteroReg[, treat := ifelse(model=="Pers", "treatment", "matchTreatment")]

# When var=="All" control for all vars simultaniously
heteroReg[var!="All", heteroControls := mapply(function(treat,y,z) gsub(treat, paste0(treat,"*",y), z), treat, var, controls)]
heteroReg[var=="All", heteroControls := paste0(sapply(treat, function(x) paste(paste0(x, "*", heteroVarsForAll), collapse = "+")), " + ",controls)]     

# Control for potential outlets
heteroReg[, myFormula := paste0(outcome, "~", heteroControls, " | potentialText")]
heteroReg[, varF := factor(var, levels = heteroVars, labels = heteroVarLabels)]
heteroReg[, modelName := paste("regHet", gsub(" ", "", varF), model, sep="_")]

HET_DATA = data.table::copy(SURVEY[treatment!="Control"])
HET_DATA[, matchTreatment := factor(matchTreatment, levels = c("Control", "Counter","Pro"))] 

# Run regressions
for (i in 1:nrow(heteroReg)) {
  heteroReg[i, assign(paste0(modelName),felmR(data = HET_DATA, formula = as.formula(myFormula)), pos=globalenv())]
}



# *************************************************************************
# RESULTS ----
# *************************************************************************

#  .Primary----
# .........................................................................

diffProCounter = diffFValueAddRow("Pro - Counter Attitudinal Treatment", list(regPolAffective_Un, regPolAffective_Primary), 
                                  matchTreatments, length=4, col=c(1,2))
diffConsLib = diffFValueAddRow("Conservative - Liberal Treatment", list(regPers_Un, regPers_Primary), 
                               treatments, length=4, col=c(3,4))
diffFValueAddRow( "Cons. - Lib. Treatment", list(regPers_Primary), treatments, length=4, col=c(3,4))

starMainFile = "Output/Tables/Combined/mainFollowupResults.tex"
stargazerR(regPolAffective_Un, regPolAffective_Primary, regPers_Un, regPers_Primary, 
           column.labels = c("Affective Polarization", "Political Opinions"), column.separate=c(2), 
           order =  c("Pro", "Counter", "Conservative", "Liberal"),
           add.lines = append(diffProCounter, append(diffConsLib,  
                                                     list(c("Controls", "", "X", "", "X")))),
           keep=c("[Tt]reat"), omit.table.layout = "n", header=FALSE
           , type="latex",  replaceCov = replaceWords, file = starMainFile)


# .Magnitude ----
# .........................................................................

diffPersuasionEffect = round(regPers_Primary$coefficients[["treatmentConservative"]] - regPers_Primary$coefficients[["treatmentLiberal"]],3)
saveToLyx(diffPersuasionEffect, "diffPersuasionEffect", digits = 3)

# Find max difference
coefName = names(regPers_NoCG$coefficients)[grepl("treatment", names(regPers_NoCG$coefficients))]
maxPersuasionEffect = regPers_NoCG$coefficients[[coefName]] + 1.96*regPers_NoCG$se[[coefName]]
confint(regPers_NoCG, "treatmentConservative", 0.95)
confint(regPers_NoCG, "treatmentConservative", 0.9)

saveToLyx(round(maxPersuasionEffect,2), "maxPersuasionEffect")

diffPersuasionIndexIdeo = SURVEY[control==TRUE & ideoLeaning==1, mean(persIndex, na.rm=TRUE)] - 
  SURVEY[control==TRUE & ideoLeaning==-1, mean(persIndex, na.rm=TRUE)]
saveToLyx(round(diffPersuasionIndexIdeo, 2), "diffPersuasionIndexIdeo")

diffPersuasionShareIdeo = scales::percent(diffPersuasionEffect/diffPersuasionIndexIdeo, accuracy=0.1)
maxPersuasionShareIdeo = scales::percent(maxPersuasionEffect/diffPersuasionIndexIdeo, accuracy=0.1)
saveToLyx(diffPersuasionShareIdeo, "diffPersuasionShareIdeo")
saveToLyx(maxPersuasionShareIdeo, "maxPersuasionShareIdeo")


# .Only primary outlets ----
# .........................................................................

# Both for slides
stargazerR(regPers_NoCG, regPers_NoCG_ControlOutlets, regPers_NoCG_MainOutlets,
           regPolAffective_NoCG, regPolAffective_NoCG_ControlOutlets, regPolAffective_NoCG_PrimaryOutlets,
           model.names = FALSE,
           column.labels = c(rep("Opinions", 3), rep("Polarization",3)),
           add.lines = list(c("Standard Controls", rep("X", 6)),
                            c("Potential Outlets FE", rep(c("", "X", ""), 2)),
                            c("Include Only Primary Outlet", rep(c("", "", "X"), 2))),
           keep=c("[Tt]reat"), header=FALSE,
           replaceCov = replaceWords ,type = "latex", file = "Output/Tables/Combined/RobustToPrimaryOutlets.tex") 

# Political opinions
stargazerR(regPers_Primary, regPers_ControlOutlets, regPers_MainOutlets, 
           add.lines = append(diffFValueAddRow("Cons. Treat - Lib. Treat", 
                                               list(regPers_Primary, regPers_ControlOutlets, regPers_MainOutlets), 
                                               treatments),  
                              list(c("Standard Controls", rep("X", 3)),
                                   c("Potential Outlets FE", "", "X", ""),
                              c("Include Only Primary Outlet", "", "", "X"))),
           keep=c("[Tt]reat"), model.names = FALSE,
           replaceCov = replaceWords ,type = "latex", file = "Output/Tables/PoliticalOpinions/RobustToPrimaryOutletsOpinions.tex") 

# Affective polarization
stargazerR(regPolAffective_Primary, regPolAffective_ControlOutlets, regPolAffective_PrimaryOutlets,
           add.lines = append(diffFValueAddRow("Pro-Att. Treat. - Counter-Att. Treat", 
                                               list(regPolAffective_Primary, regPolAffective_ControlOutlets, regPolAffective_PrimaryOutlets), 
                                               matchTreatments),  
                              list(c("Standard Controls", rep("X", 3)),
                                   c("Potential Outlets FE", "", "X", ""),
                                   c("Include Only Primary Outlet", "", "", "X"))),
           keep=c("[Tt]reat"), model.names = FALSE
           , replaceCov = replaceWords ,type = "latex", file = "Output/Tables/Polarization/RobustToPrimaryOutletsAffective.tex") 

saveToLyx(diffFValue(regPolAffective_Primary, matchTreatments), "mainEffectPolarization")
saveToLyx(diffFValue(regPers_Primary, treatments), "mainEffectPersuasion")


# .primary by weights ----
# .........................................................................

# Political opinions
stargazerR(regPers_Primary, regPers_Weighted, 
           add.lines = append(diffFValueAddRow("Cons. Treat - Lib. Treat", list(regPers_Primary, regPers_Weighted), 
                                               treatments),  
                              list(c("Controls", rep("X", 3)),
                                   c("Reweighted", "", "X"))),
           keep=c("[Tt]reat"), model.names = FALSE
           ,replaceCov = replaceWords ,type = "latex", file = "Output/Tables/PoliticalOpinions/primaryOpinionsWeighted.tex") 

# Affective polarization
stargazerR(regPolAffective_Primary, regPolAffective_Weighted, 
           add.lines = append(diffFValueAddRow("Pro-Att. Treat. - Counter-Att. Treat", 
                                               list(regPolAffective_Primary, regPolAffective_Weighted), 
                                               matchTreatments),  
                              list(c("Controls", rep("X", 3)),
                                   c("Reweighted", "", "X"))),
           keep=c("[Tt]reat"), model.names = FALSE
           ,replaceCov = replaceWords ,type = "latex", file = "Output/Tables/Polarization/affectiveWeighted.tex") 



# .primary by subsample ----
# .........................................................................

# Both outcomes
stargazerR(regPers_Primary, regPers_Posts, regPers_Ext, regPers_All,
           add.lines = append(diffFValueAddRow("Conservative Treat - Lib. Treat", list(regPers_Primary, regPers_Posts, regPers_Ext, regPers_All), 
                                               treatments),  
                              list(c("Controls", rep("X", 4)),
                                   c("Sample", "Endline", "Endline+", "Endline+", "Endline+"),
                                   c("", "", "Posts", "Ext", "Posts+Ext"))),
           keep=c("[Tt]reat"), model.names = FALSE
           ,replaceCov = replaceWords ,type = "latex", file = "Output/Tables/PoliticalOpinions/RobustToPrimarySampleOpinions.tex") 

# Affective (only for slides)
stargazerR(regPolAffective_Primary, regPolAffective_Posts, regPolAffective_Ext, regPolAffective_All,
           add.lines = append(diffFValueAddRow("Pro-Att. Treat. - Counter-Att. Treat", 
                                               list(regPolAffective_Primary, regPolAffective_Posts, regPolAffective_Ext, regPolAffective_All), 
                                               matchTreatments),  
                              list(c("Controls", rep("X", 4)),
                                   c("Sample", "Endline", "Endline+", "Endline+", "Endline+"),
                                   c("", "", "Posts", "Ext", "Posts+Ext"))),
           keep=c("[Tt]reat"), model.names = FALSE
           , replaceCov = replaceWords ,type = "latex", file = "Output/Tables/Polarization/RobustToPrimarySampleAffective.tex") 


# .Pre-analysis Plan ----
# .........................................................................

# Affective polarization
stargazerR(regPolAffective_Primary, regPolAffective_Anderson, regPolAffective_AndersonWM,
           add.lines = append(diffFValueAddRow( "Pro-Att. Treat. - Counter-Att. Treatment", 
                                                list(regPolAffective_Primary, regPolAffective_Anderson, regPolAffective_AndersonWM), matchTreatments),
                              list(c("Controls", rep("X", 3)),
                                   c("Index Method", "Standard", rep("Inv-", 2)),
                                   c("", "", rep("Cov", 2)),
                                   c("Include Missing Outcomes", "-", "No", "Yes"))),
           keep = c("match", "treat")
           , replaceCov = replaceWords, type = "latex", file = "Output/Tables/Polarization/polarizationAnderson.tex")

# Political opinions
stargazerR(regPers_Primary, regPers_Anderson, regPers_AndersonWM,
           regPers_AndersonNoNeg, regPers_AndersonNoNegWM,
           add.lines = append(diffFValueAddRow( "Cons. - Lib. Treatment", 
                                                list(regPers_Primary, regPers_Anderson, regPers_AndersonWM,
                                                     regPers_AndersonNoNeg, regPers_AndersonNoNegWM), treatments),
                              list(c("Controls", rep("X", 5)),
                                   c("Index Method", "Standard", rep("Inv-", 4)),
                                   c("", "", rep("Cov", 4)),
                                   c("Include Missing Outcomes", "-", "No", "Yes", "No", "Yes"),
                                   c("Replace Negative Weights With 0", "-", "No", "No", "Yes", "Yes"))),
           keep = c("match", "treat")
           , replaceCov = replaceWords, type = "latex", file = "Output/Tables/PoliticalOpinions/persuasionAnderson.tex")



# *************************************************************************
# PLOTS ----
# *************************************************************************

# .prepare ----
# .........................................................................

# Collect results
mainModels = rbindlist(lapply(append(list(
  "regPolAffective_Primary", "regPolAffective_Un", "regPolAffective_NoCG", "regPolAffective_TreatIdeo", 
  "regPers_Primary", "regPers_Un", "regPers_NoCG", "regPers_TreatIdeo"),
                                     heteroReg[, modelName]), 
                              function(x) tidyRob(get(x), x)), use.names = TRUE)

modelsForPlot = as.data.table(tidyr::extract(mainModels, modelName, c("family", "version"), 
                                             "([[:alnum:]]+)_([[:alnum:]]+)", remove=FALSE))
#modelsForPlot[, conf.low := as.numeric(conf.low)]
#modelsForPlot[, conf.high := as.numeric(conf.high)]
modelsForPlot = modelsForPlot[grepl("[Tt]reat", term) | grepl(":", term)]

# Outcome labels
modelsForPlot[, familyF := factor(family, levels = c("regPers", "regPolAffective"), labels=c("Political Opinions", "Affective Polarization"))]
modelsForPlot[, familyFDetail := factor(family, levels = c("regPers", "regPolAffective"), 
                                        labels=c("Political Opinions (higher value = more conservative)", 
                                                 "Affective Polarization (higher value = more polarized)"))]
# Treatment labels
modelsForPlot[, treatF := factor(term, levels = termLevels, labels = termLabels)]
modelsForPlot[, treatFNoCG := factor(term, levels = termLevelsNoCG, labels = termLabelsNoCG)]
modelsForPlot[, treatFNoCGRev := factor(term, levels = rev(termLevelsNoCG), labels = rev(termLabelsNoCG))]

currentTheme = theme(axis.title.y = element_blank(), legend.position = c("none"), panel.border = element_rect(fill=NA),
                     axis.text.y=element_text(hjust=0))


# .Main Plot - No control ----
# .........................................................................

currentModelNoCG = modelsForPlot[version=="NoCG"]
regressionPlot(ggplot(currentModelNoCG, aes(x=familyF, y=estimate, color=treatFNoCG))) + 
  scale_y_continuous(limits = c(-0.06, 0.06)) +
  ylab("Intention to Treat Effect, Standard Deviations") + 
  scale_color_manual(values = c("black", "black"))  +
  facet_wrap(~treatFNoCGRev, nrow=2, scales="free_y")

ggMySaveBeamer("Output/Graphs/Combined/PrimaryOutcomes_Beamer.eps", theme=currentTheme, adjustHeight = 0.8)
ggMySaveDoc("Output/Graphs/Combined/Figure9_PrimaryOutcomes.eps", theme=currentTheme)


# .all 4 treatment ----
# .........................................................................

currentModel = modelsForPlot[version=="Primary"]
currentModel[, familyFRev := revFactor(familyFDetail)]

for (i in unique(currentModel$modelName)) {
  savePlot = gsub("reg", "plot", i)
  
  assign(savePlot, regressionPlot(ggplot(currentModel[modelName==i], aes(x=familyFRev, y=estimate, color=treatF, linetype=treatF, shape=treatF))) + 
           scale_color_manual(values = c(rev(matchColors2), rev(treatColors2)),  
                              limits = c("Counter-Att. Treatment", "Pro-Att. Treatment", "Liberal Treatment","Conservative Treatment"),
                              breaks = currentModel[modelName==i, treatF]) +
           scale_linetype_manual(values = c(rep(c(1,2), 2)),
                                 limits = c("Counter-Att. Treatment", "Pro-Att. Treatment", "Liberal Treatment","Conservative Treatment"),
                                 breaks = currentModel[modelName==i, treatF]) +
           scale_shape_manual(values = c(16, 17, 16, 17),  
                              limits = c("Counter-Att. Treatment", "Pro-Att. Treatment", "Liberal Treatment","Conservative Treatment"),
                              breaks = currentModel[modelName==i, treatF]) +  
           scale_y_continuous(limits = c(-0.05, 0.05)) +
           guides(color = guide_legend(ncol=2, direction="vertical", byrow=TRUE), 
                  linetype = guide_legend(ncol=2, direction="vertical", byrow=TRUE),
                  shape = guide_legend(ncol=2, direction="vertical", byrow=TRUE)) +  
           ylab("Intention to Treat Effect, Standard Deviations") + 
           facet_wrap(~familyFRev, nrow=2, scales="free_y"))
}

fourTreatTheme = theme(axis.title.y = element_blank(), axis.title.x = element_blank(), axis.text.y = element_blank(),
                       axis.ticks.y = element_blank(),
                       legend.position = "bottom", panel.border = element_rect(fill = NA, size=0.2),
                       plot.margin = margin(1,4,6,1),
                       legend.spacing.x = unit(0.2, 'cm'), legend.margin = margin(-5,1,1,1), legend.key.width = unit(3, "line"))

ggarrange(plotPers_Primary + myThemeBeamer + fourTreatTheme, 
          plotPolAffective_Primary + myThemeBeamer + fourTreatTheme, nrow=2)

ggMySaveBeamer("Output/Graphs/Combined/MainOutcomesNoCG_Beamer.eps", theme=NULL, adjustHeight = 1.1)



# .Treat*Ideo ----
# .........................................................................

currentModel = modelsForPlot[version == "TreatIdeo"]
regressionPlot(ggplot(currentModel, aes(x=treatF, y=estimate))) +
  scale_y_continuous(limits = c(-0.07, 0.07), breaks = seq(-0.06, 0.06, 0.02)) +
  ylab("Intention to Treat Effect, Standard Deviations") +
  facet_wrap(~familyFDetail, nrow=2)

ggMySaveBeamer("Output/Graphs/PoliticalOpinions/opinons4Treat_Beamer.eps", theme=currentTheme)
ggMySaveDoc("Output/Graphs/PoliticalOpinions/opinons4Treat.eps", theme=currentTheme)


# .Hetero ----
# .........................................................................

heteroModels = modelsForPlot[family=="regHet"]
heteroModels[, interactVar := gsub("^.*:", "", gsub("TRUE", "", term))]
heteroModels = merge(heteroModels, unique(heteroReg[, list(var, varF)]), by.x = "interactVar", by.y="var")
heteroModels[, regType := ifelse(grepl("All", modelName), "Joint Regression", "Separate Regressions")]
heteroModels[, regTypeF := factor(regType, levels = c("Separate Regressions", "Joint Regression"))]
heteroModels[, outcomeShort := ifelse(grepl("_Pol$", modelName), "Polarization", "Opinion")]
heteroModels[, outcomeF := factor(outcomeShort, levels = c("Opinion", "Polarization"), 
                                  labels = c("Political Opinions\nConservative Treat*Var",
                                             "Affective Polarization\nPro Treat*Var"))]
heteroModels[, mainTest := interactVar %in% mainHeteroVars]

# One regression with all vars (only for slides)
regressionPlot(ggplot(heteroModels[regType=="Joint Regression" & mainTest], aes(x = varF, y=estimate))) + 
  ylab("ITT - Interaction Effect") + 
  facet_wrap(~outcomeF, ncol=2)
ggMySaveBeamer("Output/Graphs/Combined/MainOutcomesHetero_Joint_Beamer.eps", theme=currentTheme)

# Separate regression for each var 
regressionPlot(ggplot(heteroModels[regType=="Separate Regressions" & mainTest], aes(x = varF, y=estimate))) + 
  ylab("ITT - Interaction Effect") + 
  scale_y_continuous(breaks = seq(-0.06, 0.06, 0.03)) +
  facet_wrap(~outcomeF, ncol=2)
ggMySaveBeamer("Output/Graphs/Combined/MainOutcomesHetero_Sep_Beamer.eps", theme=currentTheme)
ggMySaveDoc("Output/Graphs/Combined/MainOutcomesHetero_Sep.eps", theme=currentTheme)
