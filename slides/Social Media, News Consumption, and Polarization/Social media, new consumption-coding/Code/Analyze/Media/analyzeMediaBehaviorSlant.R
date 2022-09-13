# Author:Roee Levy, Yale
# Date: Mon Jan 22 18:23:36 2018
# Purpose: Analyze the effects on the slant of all news participants engaged with

rm(list=ls())
source("Code/Utils/log.R"); beginLogging()

library(data.table)
library(fst)
library(ggplot2)

source("Code/Utils/robustReg.R")
source("Code/Utils/indexFunctions.R")
source("Code/Utils/commonVars.R")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")
FB_USER <- readRDS("Datasets/Extension/facebook/fb_user.rds")
NAV_HIS_USER <- readRDS("Datasets/Extension/navigate/nav_user.rds")
SHARED_USER <- readRDS("Datasets/Shared/shared_user.rds")
US_POP <- readRDS("Datasets/ExternalSurveys/usPop.rds")


# Fix names of fromFB to make it a convienient dep variable
#setnames(NAV_HIS_USER, names(NAV_HIS_USER), gsub("(FromFB|NotFromFB)_(Nav|His)", "_\\2\\1", names(NAV_HIS_USER)))

         
# *************************************************************************
# PREPARE  ----
# *************************************************************************

# Merge survey, FB, NAV, SHARED, 
ALL_USER = merge(FB_USER, NAV_HIS_USER, by="ResponseId", all=TRUE)
ALL_USER = merge(ALL_USER, SHARED_USER, by="ResponseId", all=TRUE)

slantTypes = c("slant", "slantExcluding")
families = c("Shared", "SharedEmpty", "FB", "Nav",  "NavFromFB", "NavNotFromFB")
controls = c("His", "SharedBefore", "SharedBeforeEmpty")

# Create all dependent variables and control
slantVars = as.vector(outer(slantTypes, c(families, controls), paste, sep="_"))

# slantExcluding not create for FromFB NotFromFB
slantVars = slantVars[! slantVars %in% c("slantExcluding_NavFromFB", "slantExcluding_NavNotFromFB")]

# Add user details
mergeVars = c(reweightVars, "ResponseId", "treatment", "matchTreatment", "treatIdeo", "control", 
              "comply", "tookFollowup", "potentialText", "ideoLeaning", "ideoLeaningF", "validExtAndShared")
ALL_USER = merge(ALL_USER[, c(slantVars, "ResponseId"), with=FALSE], 
                 SURVEY[, ..mergeVars], by="ResponseId")

# Note: FULL_DATA is sub sample of extension and posts combined
FULL_DATA = ALL_USER[validExtAndShared==TRUE]

# Standardize each variable
names = c(slantVars)
newNames = gsub("lant", "lantScale", c(slantVars))
ALL_USER[, (newNames) := sapply(.SD, function(x) scaleControl(x, control)), .SDcols=names]
FULL_DATA[, (newNames) := sapply(.SD, function(x) scaleControl(x, control)), .SDcols=names]

# Create missing variables for controls, all dependent variables
controlVars = names(ALL_USER)[grepl("_His|SharedBefore", names(ALL_USER)) & ! grepl("Have|WM", names(ALL_USER))]
ALL_USER <- createMissing(ALL_USER, controlVars)
FULL_DATA <- createMissing(FULL_DATA, controlVars)


# .Add weights ----
# .........................................................................

WEIGHTS_FULL = as.data.table(addWeights(FULL_DATA, US_POP, reweightVars, "ResponseId"))
WEIGHTS_EXT = as.data.table(addWeights(ALL_USER[ResponseId %in% NAV_HIS_USER$ResponseId], US_POP, reweightVars, "ResponseId"))
WEIGHTS_POSTS = as.data.table(addWeights(ALL_USER[ResponseId %in% SHARED_USER$ResponseId], US_POP, reweightVars, "ResponseId"))

FULL_DATA = merge(FULL_DATA, WEIGHTS_FULL[, list(ResponseId, weightFD = SampleWeight)], by="ResponseId")
ALL_USER = merge(ALL_USER, WEIGHTS_EXT[, list(ResponseId, weightExt = SampleWeight)], by="ResponseId", all.x=TRUE)
ALL_USER = merge(ALL_USER, WEIGHTS_POSTS[, list(ResponseId, weightPost = SampleWeight)], by="ResponseId", all.x=TRUE)


# .regData ----
# .........................................................................

# Create one large table for all regression
regData = as.data.table(expand.grid(dvShort=c(slantTypes, gsub("slant", "slantScale", slantTypes)), 
                                    family=families, 
                                    version = c("Primary", "FD"), 
                                    reWeight = c(TRUE, FALSE),
                                    regType = c("lm"), controls=""))

# Remove regression that won't be using: Only reweighting primary estimates
# FromFB only for slant and slantScale
regData = regData[! ((dvShort!="slantScale") & reWeight==TRUE)]
regData = regData[! (! family %in% c("FB", "Nav", "Shared") & reWeight==TRUE)]
regData = regData[! grepl("FromFB", family) | dvShort %in% c("slant", "slantScale")]

# Dependent variables
regData[, dv := paste0(dvShort,"_", family)]

# Independent variable
regData[, treat := ifelse(grepl("slant", dvShort), "treatment", "matchTreatment")] 

# Dataset: Overlap of both posts and ext or just one of them 
regData[, useData := ifelse(version=="FD", "FULL_DATA", "ALL_USER")]

# Add baseline controls
regData[grepl("Shared", family), controls := paste0(gsub("Shared", "SharedBefore", dv), "WM" ,
                                  paste0(" + ", gsub("Shared", "SharedBefore", dv), "Have"))]
regData[grepl("Nav", family), controls := paste0(gsub("Nav", "HisWM", dv), " + " , 
                                  paste0(gsub("Nav", "HisHave", dv)))]

# No baseline for FromFB
regData[, controls := gsub("NotFromFB|FromFB", "", controls)]

# Final formula
regData[, indVar := ifelse(controls=="", treat, paste0(treat, " + ", controls))]
regData[, userFormula := ifelse(controls=="", paste0(dv, "~", treat), paste0(dv, "~", treat, " + ", controls))]

# Define new variable to store regression
regData[, modelName := paste0(dv, "_", version, "_Reg", ifelse(reWeight, "_Reweighted", ""))]

# Define weights for analysis when reweighting the data
regData[reWeight==TRUE, myWeight := ifelse(version=="FD", "weightFD", 
                             ifelse(family %in% c("FB", "Nav"), "weightExt", 
                                    ifelse(family=="Shared", "weightPost", NA)))]

# run the regressions
for (i in 1:nrow(regData)) {
  if (regData[i, reWeight]==TRUE) {
      regData[i, assign(modelName,lmR(data=get(useData), formula = as.formula(userFormula),
                                      weights = get(useData)[[myWeight]]), pos=globalenv())]
  } else {
    regData[i, assign(modelName,lmR(data=get(useData), formula = as.formula(userFormula)), pos=globalenv())]
  }
}



# *************************************************************************
# DISPLAY REGRESSIONS  ----
# *************************************************************************

# .Slant (only for slides) ----
# .........................................................................

for (currentDv in c("slantScale", "slantScaleExcluding")) {
  slantData = regData[dvShort==currentDv & 
                      ((family %in% c("FB", "Nav") & (version=="FD")) | (family == c("Shared") & (version %in% c("FD", "Primary"))))]
  slantFile = paste0("Output/Tables/Combined/allResults", stringr::str_to_title(currentDv), '.tex')
  
  slantData[, diffControl := mapply(function(myData, myDv) {
      get(myData)[treatment=="Control" & ideoLeaning==1, mean(get(myDv), na.rm=TRUE)] -
      get(myData)[treatment=="Control" & ideoLeaning==-1, mean(get(myDv), na.rm=TRUE)] }, useData, dv)]
  
  slantData[, diffTOT := mapply(function(myModel, myData, myDv) {
    get(myModel)$coefficients[["treatmentConservative"]] / get(myData)[!is.na(get(myDv)) & treatment=="Conservative", mean(comply, na.rm=TRUE)] - 
    get(myModel)$coefficients[["treatmentLiberal"]] / get(myData)[!is.na(get(myDv)) & treatment=="Liberal", mean(comply, na.rm=TRUE)]
     }, modelName, useData, dv)]
  
  appendToTable = append(diffFValueAddRow("Cons. Treat. - Lib. Treat.", lapply(slantData$modelName, get), 
                                   treatments, digits = 2),
                         list(c("\\hline \\\\[-4ex]"),
                              c("TOT^^^ Cons. - Lib. Treatment", round(slantData$diffTOT, 2)),
                              c("Control^^^ Cons. Ideo, - Lib. Ideo.", round(slantData$diffControl, 2)),
                              c("Data", rep('Extension', 3), "All")))
  
  regs = paste0(currentDv, c("_FB_FD_Reg", "_Nav_FD_Reg", "_Shared_FD_Reg", "_Shared_Primary_Reg"))
  star = do.call("stargazerR", append(lapply(regs, get),
                      list(header=FALSE, digits=2,
                      add.lines = appendToTable,
                      column.labels = rep(c("\\multirow{2}{1 cm}{News Exposure}", "\\multirow{2}{1 cm}{Browsing Behavior}", 
                                            "\\multirow{2}{1 cm}{Shared Posts}", "\\multirow{2}{1 cm}{Shared Posts}")), 
                      keep = "ment", order = rev(levels(SURVEY$treatment))
          , type="latex", replaceCov = replaceWords)))

  # Add line to title row
  addLine = grep("multirow", star)
  star = c(star[1:(addLine)], "\\\\", star[(addLine+1):length(star)])
  cat(star, file= slantFile, sep = "\n") 
}


# .by subsample ----
# .........................................................................

FULL_DATA_END = FULL_DATA[tookFollowup==TRUE]

slantScale_FB_COMPLETE_Reg = lmR(data = FULL_DATA_END, slantScale_FB ~ treatment)
slantScale_Nav_COMPLETE_Reg = lmR(data = FULL_DATA_END,  slantScale_Nav ~ treatment + slantScale_HisWM + slantScale_HisHave)
slantScale_Shared_COMPLETE_Reg = lmR(data = FULL_DATA_END, slantScale_Shared ~ treatment + slantScale_SharedBeforeWM + slantScale_SharedBeforeHave)

slantModels = c("slantScale_FB_Primary_Reg", "slantScale_FB_FD_Reg", "slantScale_FB_COMPLETE_Reg", 
  "slantScale_Nav_Primary_Reg", "slantScale_Nav_FD_Reg", "slantScale_Nav_COMPLETE_Reg", 
  "slantScale_Shared_Primary_Reg", "slantScale_Shared_FD_Reg", "slantScale_Shared_COMPLETE_Reg")

do.call("stargazerR", append(lapply(slantModels, get), 
          list(add.lines = append(diffFValueAddRow("Cons. Treat. - Lib. Treat.", lapply(slantModels, get),
                                               treatments, digits = 2),
                              list(c("Ext. Subsample", "X", "", "", "X", rep("", 5)),
                                   c("Posts Subsample", rep("", 6), "X", "", ""),
                                   c("Ext. + Posts Subsample", rep(c("", "X", ""), 3)),
                                   c("Ext. + Posts + ", rep(c("", "", "X"), 3)), 
                                   c("Endline Subsample"))),
           column.labels = c("News Exposure", "Browsing Behavior", "Shared Posts"), 
           column.separate = c(3,3,3),
           keep = "treatment"
      , type="latex", replaceCov = replaceWords, file = "Output/Tables/Combined/slantBySample.tex"))) 



# .Reweighted ----
# .........................................................................

regs = c("slantScale_FB_Primary_Reg",  "slantScale_FB_Primary_Reg_Reweighted",
"slantScale_Nav_Primary_Reg", "slantScale_Nav_Primary_Reg_Reweighted", 
"slantScale_Shared_Primary_Reg", "slantScale_Shared_Primary_Reg_Reweighted")

do.call("stargazerR", append(lapply(regs, get),
      list(add.lines = append(diffFValueAddRow("Cons. Treat. - Lib. Treat.", lapply(regs, get), treatments, digits = 2),
                              list(c("Reweigted", rep(c("", "X"), 3)))),
           column.labels = c("News Exposure", "Browsing Behavior", "Shared Posts"), 
           keep = "treatment",
           column.separate = c(2,2,2)
           , type="latex", replaceCov = replaceWords, file = "Output/Tables/Combined/effectOnSlantReweighted.tex")))



# *************************************************************************
# DESC-STAT ----
# *************************************************************************

# .prepare ----
# .........................................................................

# Add compliance variables to calculate LATE
FULL_DATA[, complyL := comply & treatment=="Liberal"]
FULL_DATA[, complyC := comply & treatment=="Conservative"]


# .main effects ----
# .........................................................................

saveToLyx(diffFValue(slantScale_FB_Primary_Reg, treatments), "mainEffectFBSlant")
saveToLyx(diffFValue(slantScale_Nav_Primary_Reg, treatments), "mainEffectNavSlant")


# .treat->exposure ----
# .........................................................................

# Effect of treatment on exposure (lib-conservative)
treatEffectOnFBSlant = getCoef(slant_FB_FD_Reg, "treatmentConservative") - getCoef(slant_FB_FD_Reg, "treatmentLiberal")


# LATE effect of treatment on exposure (lib-conservative)
slant_FB_FD_RegIV = felm(data=FULL_DATA, formula = slant_FB ~  1 | 0 | (complyL+complyC~treatment))
treatEffectOnFBSlantLATE = getCoef(slant_FB_FD_RegIV, "`complyC(fit)`") - getCoef(slant_FB_FD_RegIV, "`complyL(fit)`")

# Association with ideology
ideoEffectOnFBSlant = FULL_DATA[treatment=="Control" & ideoLeaning==1, mean(slant_FB, na.rm=TRUE)] - 
  FULL_DATA[treatment=="Control" & ideoLeaning==-1, mean(slant_FB, na.rm=TRUE)]

# Effect of treatment compared to association with ideology
exposureEffectShareIdeology = treatEffectOnFBSlant / ideoEffectOnFBSlant
exposureEffectShareIdeologyLATE = treatEffectOnFBSlantLATE / ideoEffectOnFBSlant
saveToLyx(exposureEffectShareIdeology, "exposureEffectShareIdeology", percent = TRUE)
saveToLyx(exposureEffectShareIdeologyLATE, "exposureEffectShareIdeologyLATE", percent = TRUE)


# .treat->browsing ----
# .........................................................................

# Treatment effect on browsing slant as share of ideology effect
treatEffectOnNavSlantLib = getCoef(slant_Nav_FD_Reg, "treatmentLiberal")
treatEffectOnNavSlantCon = getCoef(slant_Nav_FD_Reg, "treatmentConservative")
treatEffectOnNavSlant = treatEffectOnNavSlantCon - treatEffectOnNavSlantLib

# Save for comparing states later
saveToLyx(treatEffectOnNavSlantLib, "treatEffectOnNavSlantLib", digits = 4)
saveToLyx(treatEffectOnNavSlantCon, "treatEffectOnNavSlantCon", digits = 4)

# LATE treatment effect
slant_Nav_FD_RegIV = felm(data=FULL_DATA, formula = slant_Nav ~  slant_HisWM + slant_HisHave | 0 | 
                            (complyL+complyC ~ treatment))

treatEffectOnNavSlantLATE = getCoef(slant_Nav_FD_RegIV, "`complyC(fit)`")  - getCoef(slant_Nav_FD_RegIV, "`complyL(fit)`")

# Ideology association with slant
ideoEffectOnNavSlant = FULL_DATA[treatment=="Control" & ideoLeaning==1, mean(slant_Nav, na.rm=TRUE)] - 
  FULL_DATA[treatment=="Control" & ideoLeaning==-1, mean(slant_Nav, na.rm=TRUE)]

# Share of treatment divided by ideology
navEffectShareIdeology = scales::percent(treatEffectOnNavSlant / ideoEffectOnNavSlant, accuracy=1)
navEffectShareIdeologyLATE = scales::percent(treatEffectOnNavSlantLATE / ideoEffectOnNavSlant, accuracy=1)

# Save
saveToLyx(navEffectShareIdeology, "navEffectShareIdeology")
saveToLyx(navEffectShareIdeologyLATE, "navEffectShareIdeologyLATE")


# .exposure->browsing ----
# .........................................................................

regSlant_Nav_FB <- felmR(data=ALL_USER, as.formula(paste0("slant_Nav ~ slant_HisHave + slant_HisWM  | 0 |  (slant_FB ~ treatment)")))
regSlant_BrowsingFB_FB <-  felmR(data=ALL_USER, as.formula(paste0("slant_NavFromFB ~ slant_HisHave + slant_HisWM  | 0 |  (slant_FB ~ treatment)")))

effectSlantFBNav <- getCoef(regSlant_Nav_FB, "`slant_FB(fit)`")
effectSlantFBNavFromFB <- getCoef(regSlant_BrowsingFB_FB, "`slant_FB(fit)`")
saveToLyx(effectSlantFBNav, "effectSlantFBNav", digits = 2)
saveToLyx(effectSlantFBNavFromFB, "effectSlantFBNavFromFB", digits = 2)



# *************************************************************************
# PLOT ----
# *************************************************************************

# .prepare ----
# .........................................................................

# Not plotting reweigted models
allModels= rbindlist(lapply(regData[version %in% c("FD", "Primary") & regType=="lm" & reWeight==FALSE, modelName], 
                                  function(x) tidyRob(get(x), x)))
allModels = allModels[grepl("treatment", term)]

modelsForPlot = merge(allModels, regData[, list(modelName, dvShort, family, version, data=useData)], 
                      by="modelName")

# Name of treatment and labels for panels and outcomes
modelsForPlot[, treatRev := factor(term, levels=rev(termLevels), labels = rev(termLabels))]
modelsForPlot[, treat := factor(term, levels=termLevels, labels = termLabels)]
modelsForPlot[, familyF := factor(family, levels = c("FB", "NavFromFB", "NavNotFromFB", "Nav",
                                                     "Shared", "SharedEmpty"), 
                               labels = c("Facebook exposure, posts in feed", 
                                          "Browsing behavior, visits to news sites through Facebook", 
                                          "Browsing behavior, visits to news sites not through Facebook", 
                                          "Browsing behavior, visits to news sites", 
                                          "Sharing behavior, posts shared", 
                                          "Sharing behavior, posts shared no commentary"))]
modelsForPlot[, familyFNoControl := familyF]
levels(modelsForPlot$familyFNoControl) = gsub(" \\(control mean in parentheses\\)", "", 
                                              levels(modelsForPlot$familyFNoControl))

modelsForPlot[, dvF := factor(dvShort, levels = dvLevels, labels = dvLabels)]

# Difference in Control group
modelsForPlot[dvShort==("slantScale"), slantScaleDiffLibCons := mapply(function(x,y) 
                get(x)[treatment=="Control" & ideoLeaning==1, mean(get(paste0("slantScale_", y)), na.rm=TRUE)] - 
                get(x)[treatment=="Control" & ideoLeaning==-1, mean(get(paste0("slantScale_", y)), na.rm=TRUE)],
                data, family)]


# .graph style ----
# .........................................................................

# legend
myGuide <- function(rev=TRUE) (guides(color = guide_legend(reverse=rev, nrow = 1, keyheight=0.1,default.unit="inch"), 
                                      linetype = guide_legend(reverse=rev, nrow = 1, keyheight=0.1,default.unit="inch"), 
                                      shape = guide_legend(reverse=rev, nrow = 1, keyheight=0.1,default.unit="inch"),
                                      alpha = FALSE))

# theme
currentTheme = theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
                     axis.ticks.y = element_blank(), axis.line.y = element_blank(),
                     legend.position ="top", legend.margin = margin(0,0,0,0), 
                     legend.spacing.x = unit(0.2, 'cm'), legend.key.width = unit(3, "line"))


# .Slant ----
# .........................................................................

for (currentSlant in c("slantScale", "slantScaleExcluding")) {
  currentModels = modelsForPlot[dvShort==currentSlant & ((family %in% c("FB", "Nav", "Shared") & 
                                                            (version=="FD")))]
  
  currentModels[ , familyLabels := familyFNoControl]
  if (currentSlant=="slantScale") {
    currentModels[ , familyLabels := paste0(familyFNoControl, " (conservatives-liberals in control group = ", round(slantScaleDiffLibCons, 2), ")")]
    currentModels[, familyLabels := factor(familyLabels, levels = currentModels$familyLabels[c(grep("exposure", currentModels$familyLabels)[1],
                                                                                               grep("Browsing", currentModels$familyLabels)[1], grep("Sharing", currentModels$familyLabels)[1])])]
  }
  
  # Save version of graph without Shared for slides only
  currentModelsNoShared = currentModels[family!="Shared"]
  
  for (currentName in c("", "NoShared")) {
    currentData = get(paste0("currentModels", currentName))
    
    regressionPlot(ggplot(currentData, aes(x=dvF, y=estimate, color=treatRev, linetype = treatRev, shape=treatRev))) + 
      scale_color_manual(values = rev(treatColors2)) +
      scale_linetype_manual(values = c(1, 2)) +
      facet_wrap(~familyLabels, scales="free",nrow = 4) +
      scale_y_continuous(breaks = scales::pretty_breaks(n=10), limits = c(-0.5, 0.5)) +
      ylab("Slant, Std. Dev. (Higher = More Conservative)") +
      myGuide(rev=FALSE)
  
    dir = "Output/Graphs/Combined/"
    if (currentName=="") {
      if(currentSlant=="slantScale") {
        ggMySaveDoc(paste0(dir, "Figure7_slantScaleFullData.eps"), theme=currentTheme)
      } else {
        ggMySaveDoc(paste0(dir, "slantScaleExcludingFullData.eps"), theme=currentTheme)
      }
    } 
    ggMySaveBeamer(paste0(dir, currentSlant, "FullData", currentName, "_Beamer.eps"), theme=currentTheme)
    
  } 
}  


# .Slant shared (only for slides)----
# .........................................................................

currentModels = modelsForPlot[dvShort=="slantScale" & family %in% c("Shared", "SharedEmpty") & (version=="Primary")]

regressionPlot(ggplot(currentModels, aes(x=dvF, y=estimate, color=treatRev, linetype = treatRev, shape=treatRev))) + 
  scale_color_manual(values = rev(treatColors2)) +
  scale_linetype_manual(values = c(1, 2)) +
  facet_wrap(~familyFNoControl, scales="free", nrow = 4) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10), limits = c(-0.15, 0.15)) +
  ylab("Slant, Std. Dev. (Higher = More Conservative)") +
  myGuide(rev=FALSE)

ggMySaveBeamer(paste0("Output/Graphs/Shared/slantScaleAccessPostData_Beamer.eps"), theme=currentTheme)


# .Slant fromFB vs notFromFB (only for slides) ----
# .........................................................................

currentModels = modelsForPlot[dvShort=="slantScale" & family %in% c("FB", "NavFromFB", "NavNotFromFB") & version=="FD"]

regressionPlot(ggplot(currentModels, aes(x=dvF, y=estimate, color=treatRev, linetype=treatRev, shape=treatRev))) + 
  scale_color_manual(values = rev(treatColors2)) +
  facet_wrap(~familyF, scales="free_y", nrow = 3) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
  ylab("Slant, Std. Dev. (Higher = More Conservative)") +
  myGuide(rev=TRUE) 

ggMySaveBeamer("Output/Graphs/Browsing/slantNavFromFB_Beamer.eps", theme = currentTheme)


# .Slant Exposed, FB, All (only for slides) ----
# .........................................................................

currentModels = modelsForPlot[dvShort=="slantScale" & family %in% c("FB", "NavFromFB", "Nav") & version=="FD"]
currentModels[, alpha := ifelse(family=="FB", 1, 0)]

currentModelsFirst = data.table::copy(currentModels)
currentModelsFirst[, familyF := factor(family, levels = c("FB", "NavFromFB", "Nav"), 
                                       labels=c("Facebook exposure, posts in feed"," ","  "))]
ggplot(currentModelsFirst, aes(x=dvF, y=estimate, color=treat, linetype=treat, shape=treat, alpha=alpha)) +
  scale_color_manual(values = treatColors2) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, width=0.1), position = position_dodge(width=0.2)) + 
  geom_point(size=1.5, position = position_dodge(width=0.2)) +
  geom_hline(yintercept = 0, colour = c("grey60", "white", "white"), linetype = 2) +
  coord_flip() +
  facet_wrap(~familyF, scales="free_y", nrow = 3) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
  ylab("Slant, Std. Dev. (Higher = More Conservative)") +
  myGuide() 
ggMySaveBeamer("Output/Graphs/Combined/exposureSlantFromFB1_Beamer.eps", theme = currentTheme)

regressionPlot(ggplot(currentModels, aes(x=dvF, y=estimate, color=treat, linetype=treat, shape=treat))) + 
  scale_color_manual(values = treatColors2) +
  facet_wrap(~familyF, scales="free_y", nrow = 3) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
  ylab("Slant, Std. Dev. (Higher = More Conservative)") +
  myGuide() 
ggMySaveBeamer("Output/Graphs/Combined/exposureSlantFromFB2_Beamer.eps", theme = currentTheme)
