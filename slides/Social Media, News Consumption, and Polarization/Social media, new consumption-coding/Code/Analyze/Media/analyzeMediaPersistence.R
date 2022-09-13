# Author:Roee Levy, Yale
# Date: Mon Jan 22 18:23:36 2018
# Purpose: Anaylze the effects of the intervention on media outcomes over time 

rm(list=ls())
source("Code/Utils/log.R"); beginLogging()

source("Code/Utils/commonVars.R")
source("Code/Utils/robustReg.R")
source("Code/Utils/indexFunctions.R")
library(data.table)
library(dplyr)
library(fst)
library(ggplot2)

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")

# Same data for pre and followup shared, and for nav and his 
expVars = expVars[expVars!="newLike"]
vars = c(expVars, "slant", "count_slant")

# load data
NAV_HIS_DAY <- readRDS("Datasets/Extension/navigate/nav_user_day.rds")
SHARED_DAY <- readRDS("Datasets/Shared/shared_user_day.rds")
FB_DAY <- readRDS("Datasets/Extension/facebook/fb_user_day.rds")


# *************************************************************************
# PREPARE  ----
# *************************************************************************

# Merge daily data on engagement by day from facebook, navigate, shared
ALL_DAY = merge(FB_DAY[relativeDay<91], NAV_HIS_DAY[relativeDay<91], keyby=c("ResponseId", "relativeDay"), all=TRUE)
ALL_DAY = merge(ALL_DAY, SHARED_DAY[relativeDay<91], by = c("ResponseId", "relativeDay"), all = TRUE)
ALL_DAY = merge(ALL_DAY, SURVEY[, list(control, ResponseId, treatment, startPOSIX, matchTreatment,
                                       validHisOneWeek, maxTimeExt, maxDiffPostsDay)], by = "ResponseId")


# .define vars ----
# .........................................................................

# Don't want extra underline
setnames(ALL_DAY, names(ALL_DAY)[grepl("count_slant", names(ALL_DAY))], 
         gsub("count_slant", "countSlant", names(ALL_DAY)[grepl("count_slant", names(ALL_DAY))]))

# Set weeks
ALL_DAY[, relativeWeek := floor(relativeDay/7)]
ALL_DAY[, relativeMonth := floor(relativeDay/28)]

# Time with ext
ALL_DAY[, maxDiffExtDay := floor(difftime(maxTimeExt, startPOSIX, units="days"))]

# .define baseline vars ----
# .........................................................................

families = c("FB", "Nav", "Shared")
finalVars = apply(expand.grid(expVars, families), 1, paste, collapse="_")
slantVars = paste0("slant_", families)
allVars = c(finalVars, slantVars)

# Need to agg nav history separately to only take valid users
SLANT_BASE_NAV = ALL_DAY[relativeDay<0 & validHisOneWeek,  
                         list(slant_Nav = weighted.mean(slant_Nav, countSlant_Nav, na.rm=TRUE)),
                         by = c("ResponseId")]

SLANT_BASE_SHARED = ALL_DAY[relativeDay<0, 
                            list(slant_Shared = weighted.mean(slant_Shared, countSlant_Shared, na.rm=TRUE)),
                            by = c("ResponseId")]
SLANT_BASE = merge(SLANT_BASE_NAV, SLANT_BASE_SHARED, by="ResponseId", all=TRUE)

# Find baseline history for posts/browsing for engagement with potential outcomes (potLib, potCons..)
POT_BASE_NAV = ALL_DAY[relativeDay<0  & validHisOneWeek, lapply(.SD, sum), 
                         .SDcols = finalVars[grepl("Nav", finalVars)], by = c("ResponseId")]
POT_BASE_SHARED = ALL_DAY[relativeDay<0, lapply(.SD, sum), 
                            .SDcols = finalVars[grepl("Shared", finalVars)], by = c("ResponseId")]

ALL_BASE = merge(POT_BASE_NAV, POT_BASE_SHARED, by="ResponseId", all=TRUE)
ALL_BASE = merge(ALL_BASE, SLANT_BASE, by="ResponseId", all=TRUE)

potBaseNames = names(ALL_BASE)[names(ALL_BASE) != "ResponseId"]
setnames(ALL_BASE, potBaseNames, paste0(potBaseNames, "Base"))

rm(POT_BASE_NAV, POT_BASE_SHARED, SLANT_BASE_SHARED, SLANT_BASE_NAV); gc()


# .aggregate ----
# .........................................................................

# This function aggregates variables by week/month
# timePeriod = "relativeWeek";
aggregateTime <- function(timePeriod, DT_DAY, DT_BASE) {
  
  # Agg slant
  SLANT_WEEK = DT_DAY[,  list(slant_FB = weighted.mean(slant_FB, countSlant_FB, na.rm=TRUE),
                               slant_Nav = weighted.mean(slant_Nav, countSlant_Nav, na.rm=TRUE),
                               slant_Shared = weighted.mean(slant_Shared, countSlant_Shared, na.rm=TRUE)),
                       by = c(timePeriod, "ResponseId")]
  
  # Agg exp outlets
  POT_WEEK = DT_DAY[, lapply(.SD, sum), .SDcols = finalVars, by = c(timePeriod, "ResponseId")]
  
  ALL_WEEK = merge(SLANT_WEEK, POT_WEEK, by = c(timePeriod, "ResponseId"))
  ALL_WEEK = merge(ALL_WEEK, DT_BASE, by = c("ResponseId"), all.x=TRUE)
  
  # Merge data 
  ALL_WEEK = merge(ALL_WEEK, SURVEY[, list(ResponseId, startPOSIX, treatment, control, matchTreatment, 
                                           ideoLeaning)], 
                   by = "ResponseId")

  createMissing(ALL_WEEK, list(names(ALL_WEEK)[grepl("Base", names(ALL_WEEK))]))
  
  return(ALL_WEEK)
}

# Keep only valid participants for shared data
# timePeriod = "relativeWeek"; minDay=41
aggregateTimeShare <- function(timePeriod, minDay) {
  
  DT = ALL_DAY[maxDiffPostsDay>minDay]
  ALL_WEEK_SHARED = aggregateTime(timePeriod, DT, ALL_BASE)
  
  ALL_WEEK_SHARED[, slantScale_SharedBase := scaleControl(slant_SharedBase, control)]
  createMissing(ALL_WEEK_SHARED, list("slantScale_SharedBase"))
  
  return(ALL_WEEK_SHARED)
}

# timePeriod = "relativeWeek"; minDay = 41
# Keep only valid participants for extension data
aggregateTimeExt <- function(timePeriod, minDay) {
  
  DT = ALL_DAY[maxDiffExtDay>minDay]
  
  ALL_WEEK_EXT <- aggregateTime(timePeriod, DT, ALL_BASE)
  ALL_WEEK_EXT[, slantScale_NavBase :=scaleControl(slant_NavBase, control)]
  createMissing(ALL_WEEK_EXT, list("slantScale_NavBase"))
  
  return(ALL_WEEK_EXT)
}


# TODO: Takes too long
ALL_WEEK_EXT <- aggregateTimeExt("relativeWeek", minDay=41)
ALL_WEEK_SHARED <- aggregateTimeShare("relativeWeek", 41)

ALL_MONTH_EXT <- aggregateTimeExt("relativeMonth", 28*3+1)
ALL_MONTH_SHARED <- aggregateTimeShare("relativeMonth", 28*3+1)

numParticipantsPersistenceExt = ALL_WEEK_EXT[, uniqueN(ResponseId)]
numParticipantsPersistenceShared = ALL_WEEK_SHARED[, uniqueN(ResponseId)]
saveToLyx(formatC(numParticipantsPersistenceExt, big.mark = ","), "numParticipantsPersistenceExt")
saveToLyx(formatC(numParticipantsPersistenceShared, big.mark = ","), "numParticipantsPersistenceShared")

numParticipantsPersistenceMonthExt = ALL_MONTH_EXT[, uniqueN(ResponseId)]
numParticipantsPersistenceMonthShared = ALL_MONTH_SHARED[, uniqueN(ResponseId)]
saveToLyx(formatC(numParticipantsPersistenceMonthExt, big.mark = ","), "numParticipantsPersistenceMonthExt")
saveToLyx(formatC(numParticipantsPersistenceMonthShared, big.mark = ","), "numParticipantsPersistenceMonthShared")



# *************************************************************************
# REG ----
# *************************************************************************

regData = as.data.table(expand.grid(dvShort=c(expVars, "slant", "slantScale"), 
                                    family=c("Shared", "FB", "Nav"), 
                                    timePeriod = c("WEEK", "MONTH"),
                                    currentTime = seq(0, 7)))

# Remove irrelevant 
regData = regData[timePeriod=="WEEK" | currentTime<3]
regData = regData[timePeriod=="WEEK" | (dvShort %in% c("slant", "slantScale"))]

# Define dependent variable, treatment and controls
regData[, dv := paste0(dvShort,"_", family)]
regData[, treat := ifelse(dvShort %in% c("potCons", "potLib", "slant", "slantScale"), "treatment", 
                          "matchTreatment")] 
regData[, currentFormula := paste0(dv, " ~ ", treat)]
regData[family!="FB", currentFormula := paste0(currentFormula, " + ", dv, "BaseHave", " + ", dv, "BaseWM")]

# Dataset
regData[, currentData := ifelse(family=="Shared", paste0("ALL_", timePeriod, "_SHARED"),
                                paste0("ALL_", timePeriod, "_EXT"))]

regData[, name := paste0("R", dv, "_", timePeriod, currentTime)]
regData[, name := gsub("-", "N", name)]

# This take a while, if too long can remove control group
for (i in 1:nrow(regData)) {
  
  # First find correct week and scale everything (not scaling in advance because each regression has different data)
  if (regData[i, timePeriod]=="WEEK") {
    tempData = regData[i, subset(get(currentData), relativeWeek==eval(parse(text = currentTime)))]
  } else if (regData[i, timePeriod]=="MONTH") {
    tempData = regData[i, subset(get(currentData), relativeMonth==eval(parse(text = currentTime)))]
  }
  if (regData[i, dvShort %in% c("slantScale")]) {
    set(tempData, ,regData[i,dv], scaleControl(tempData[[gsub("Scale", "", regData[i,dv])]], tempData$control))
  }
  
  # Then exclude control if needed
  if (regData[i, dvShort %in% c("slant", "slantScale")]) {
    tempData = subset(tempData, treatment!="Control")
  }
  
  # Finally run the regression
  regData[i, assign(name, lmR(tempData, formula = currentFormula), pos=globalenv())]
}


# *************************************************************************
# PLOTS ----
# *************************************************************************

allModels= rbindlist(lapply(regData[, name],  function(x) tidyRob(get(x), x)))
allModels = merge(allModels[grepl("reatment", term)], 
                  regData[, list(dv, treat, dvShort, family, timePeriod, currentTime, name)],  
                  by.x="modelName", by.y="name")

# Remove effect of liberal treatment on conservative posts and similar
allModels = allModels[ ! (dvShort=="potLib" & term=="treatmentConservative")]
allModels = allModels[ ! (dvShort=="potCons" & term=="treatmentLiberal")]
allModels = allModels[ ! (dvShort=="potCounter" & term=="matchTreatmentPro")]
allModels = allModels[ ! (dvShort=="potPro" & term=="matchTreatmentCounter")] 
         
allModels[, outcomeType := ifelse(dvShort=="slantScale", "slantScale", 
                                  ifelse(grepl("slant", dv), "Slant", "Exp"))]
allModels[, yAxis := ifelse(dvShort=="slant", "Slant", 
                        ifelse(dvShort=="slantScale", "Slant (Std. Dev.)", NA))]
allModels[, familyF := factor(family, levels = familyLevels, labels = familyLabels)]
allModels[, termF := factor(term, levels = c(treatmentLevels, matchTreatmentLevels), 
                            labels = c(treatmentLabels, matchTreatmentLabels))]
allModels[, nextTime := currentTime+1]

# Theme
currentThemeBeamer = theme(axis.title.x = element_blank(), legend.position = "none")
currentThemeDoc = theme(legend.position = "none")


# .effect on outlets (only for slides) ----
# .........................................................................

modelsForExpSepPlotWeek = allModels[0<=currentTime & currentTime<6 & timePeriod=="WEEK" &
                                  dvShort %in% c(expVars)]

# Create graph for slant and combined graph for pro/counter conservative/liberal
for (i in c("Ext", "Shared")) {
  currentFam = "Shared"
  if (i=="Ext") {
    currentFam = c("Nav", "FB")
  } 
  for (j in unique(modelsForExpSepPlotWeek$dvShort)) {
    
    ggplot(modelsForExpSepPlotWeek[dvShort==j & family %in% currentFam], aes(x=nextTime, y=estimate, color=termF, linetype=termF)) +
      scale_color_manual(values = "black") +
      geom_hline(yintercept = 0, color="gray") +
      geom_line() +
      geom_errorbar(aes(ymin=conf.low, ymax=conf.high), linetype=1, width=0.25) +
      scale_x_continuous(breaks=c(1:6)) +
      labs(x= "Relative Week", y= "Number of Posts/Visits") +
      facet_wrap(~ familyF, nrow = 3, scales = "free_y") 
      
    ggMySaveBeamer(paste0("Output/Graphs/Persistence/PersistentEffect", i, j, "_Beamer.eps"), theme=currentThemeBeamer, noLinks = TRUE)
  }
}


# .effect on slant ----
# .........................................................................


for (i in c("Ext", "Shared")) {
  currentFam = "Shared"
  myAdjust = 0.4
  docTheme = currentThemeDoc
  if (i=="Ext") {
    currentFam = c("Nav", "FB")
    myAdjust = 0.8
    docTheme = docTheme + theme(axis.title.x = element_blank())
  } 
  
  ggplot(allModels[0<=currentTime & currentTime<6 & dvShort=="slantScale" & timePeriod=="WEEK" & family %in% currentFam], 
         aes(x=nextTime, y=estimate, color=termF, linetype=termF)) +
    scale_color_manual(values = "black") +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high), linetype=1, width=0.25) +
    scale_x_continuous(breaks=c(-5:7)) +
    labs(x= "Relative Week", y= "Slant (std. dev.)") +
    facet_wrap(~ familyF, nrow = 3, scales = "free_y") 
  
  ggMySaveBeamer(paste0("Output/Graphs/Persistence/PersistentEffectSlantScale", i , "_Beamer.eps"), theme=currentThemeBeamer, adjustHeight = myAdjust)
  if (i=="Ext") {
    ggMySaveDoc(paste0("Output/Graphs/Persistence/Figure8a_PersistentEffectSlantScaleExt.eps"), theme=docTheme, adjustHeight = myAdjust)
  } else {
    ggMySaveDoc(paste0("Output/Graphs/Persistence/Figure8b_PersistentEffectSlantScaleShared.eps"), theme=docTheme, adjustHeight = myAdjust)
  }
  
  ggplot(allModels[0<=currentTime & currentTime<3 & dvShort=="slantScale" & timePeriod=="MONTH" & family %in% currentFam], 
         aes(x=nextTime, y=estimate, color=termF, linetype=termF)) +
    scale_color_manual(values = "black") +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high), linetype=1, width=0.25) +
    scale_x_continuous(breaks=c(0:7)) +
    labs(x= "Relative month (28 days)", y= "Slant (std. dev.)") +
    facet_wrap(~ familyF, nrow = 3, scales = "free_y") 
  
  if (i=="Ext") {
    ggMySaveDoc(paste0("Output/Graphs/Persistence/PersistentEffectMonthSlantScaleExt.eps"), theme=docTheme, adjustHeight = myAdjust)
  } else {
    ggMySaveDoc(paste0("Output/Graphs/Persistence/PersistentEffectMonthSlantScaleShared.eps"), theme=docTheme, adjustHeight = myAdjust)
  }
  ggMySaveBeamer(paste0("Output/Graphs/Persistence/PersistentEffectMonthSlantScale", i , "_Beamer.eps"), theme=currentThemeBeamer, adjustHeight = myAdjust)
}  

