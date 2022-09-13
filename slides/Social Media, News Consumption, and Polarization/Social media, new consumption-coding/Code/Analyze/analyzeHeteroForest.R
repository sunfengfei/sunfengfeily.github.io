# Author: Ro'ee Levy, Yale
# Date: Sat May 30 22:06:16 2020
# Purpose: Analyze heterogenous effects using random forests
# --------------#

require(data.table)
require(dplyr)
library(fst)
library(kableExtra)
library(grf) # R package https://github.com/grf-labs/grf

rm(list=ls()) #Remove everything
source("Code/Utils/log.R"); beginLogging()
source("Code/Utils/commonVars.R")
source("Code/Utils/commonFunctions.R")
source("Code/Utils/causalForestFunctions.R")
source("Code/Utils/indexFunctions.R")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")
FB_USER <- readRDS("Datasets/Extension/facebook/fb_user.rds")
NAV_HIS_USER <- readRDS("Datasets/Extension/navigate/nav_user.rds")
USERS_PAGES <- readRDS("Datasets/UsersPages/users_pages.rds")



LIKES_COV_FOR_PREDICT <- readRDS("Datasets/Likes/baselineLikes.rds")

# This verified that NA are included in the forest analysis
current.na.action <- options('na.action') # na.omit
options('na.action'='na.pass')

currentSciepenOption <- options('scipen')  # 0
options(scipen=999)

importance = FALSE

set.seed(222)


# *************************************************************************
# PREPARE ----
# *************************************************************************

# .merge more cov----
# .........................................................................

# Add likes as controls
SURVEY = merge(SURVEY, LIKES_COV_FOR_PREDICT, all.x=TRUE, by="ResponseId")

# Add potential pages as controls
POTENTIAL_PAGES <- dcast(USERS_PAGES[potential==TRUE], ResponseId ~ outlet, value.var = "potential")
setnames(POTENTIAL_PAGES, unique(USERS_PAGES[potential==TRUE, outlet]), 
         paste0("Potential_", unique(USERS_PAGES[potential==TRUE, outlet])))

potentialVars = names(POTENTIAL_PAGES)[names(POTENTIAL_PAGES) != "ResponseId"]

SURVEY = merge(SURVEY, POTENTIAL_PAGES, all.x=TRUE, by="ResponseId")


# .merge ext----
# .........................................................................

SURVEY = merge(SURVEY, FB_USER[, list(ResponseId, slant_FB)], by="ResponseId", all.x=TRUE)
SURVEY = merge(SURVEY, NAV_HIS_USER[, list(ResponseId, slant_Nav)], by="ResponseId", all.x=TRUE)

SURVEY[, slant_FBScale := scaleControl(slant_FB, control=control)]
SURVEY[, slant_NavScale := scaleControl(slant_Nav, control=control)]

# Remove NAs
SURVEY[slant_Rep==999, slant_Rep := NA]
SURVEY[slant_Dem==999, slant_Dem := NA]


# .treat and outcome vars ----
# .........................................................................

treatVars = c("treatment", "matchTreatment", "control")
outcomesVars = c("persIndex", "polAffectiveIndex", "slant_FBScale", "slant_NavScale", "complyWithZero")


# .control vars ----
# .........................................................................

controlVars = controlVarsForest

# Add pages liked
likeVars = names(LIKES_COV_FOR_PREDICT %>% select(-ResponseId))
likeVars = likeVars[!grepl("_Recent", likeVars)]
controlVars = unique(c(controlVars, likeVars))

# Add potential pages
controlVars = unique(c(controlVars, potentialVars))

# Turn some variables into factors 
SURVEY = setFactor(SURVEY, "mostNews", exclude=NULL, copyAtt = TRUE, removeOrig = TRUE)
SURVEY = setFactor(SURVEY, "accessWebsites", exclude=NULL, copyAtt = TRUE, removeOrig = TRUE)

varFactors = c("genericBallot", "mostNews", "MuellerFair", "rosenstein", "obstruct", "accessWebsites", "rep")
controlVars = controlVars[! controlVars %in% varFactors]
controlVars = c(controlVars, paste0(varFactors, "F"))

# Add echo chamber
controlVars = c(controlVars, "echoChamber")
saveToLyx(length(controlVars), "forestControlVarsNumber", digits = 0)

# *************************************************************************
# ESIMATIONS ----
# *************************************************************************

DATA = SURVEY[treatment!="Control"]
DATA_POL = DATA[!is.na(ideoLeaning)]


# .opinions ----
# .........................................................................

resultsOpinionsAll <- projectForestWrapper(DATA=DATA, depName = "persIndex", covName = controlVars, 
                                           matchTreat=FALSE, importance=importance, countImportance=5)


# .affective ----
# .........................................................................

resultsAffectiveAll <- projectForestWrapper(DATA = DATA_POL, depName = "polAffectiveIndex", covName = controlVars, 
                                                           matchTreat=TRUE, importance=importance, countImportance=5)



# *************************************************************************
# EXT ESTIMATIONS ----
# *************************************************************************

# .facebook ----
# .........................................................................

resultsFBAll <- projectForestWrapper(DATA=DATA, depName = "slant_FBScale", covName = controlVars, 
                                                  matchTreat=FALSE, importance=importance, countImportance=5)

# .browsing ----
# .........................................................................


resultsNavAll <- projectForestWrapper(DATA=DATA, depName = "slant_NavScale", covName = controlVars, 
                                                 matchTreat=FALSE, importance=importance, countImportance=10)



# *************************************************************************
# COMPLETE ----
# *************************************************************************

# Need to return previous NA mode so NA won't be included in all lm regressions
options('na.action'='na.omit')


# *************************************************************************
# CREATE TABLE ----
# *************************************************************************



# All of these values are calculated in other files and displayed in other tables
# Taking the value here manually but it is also calculated in:
# analyzeMediaBehaviorSlant (variables mainEffectFBSlant and mainEffectNavSlant) and 
# analyzePrimaryOutcomes(variables mainEffectPolarization and mainEffectPersuasion)
mainEffectAffective = "0.033$^{***}$" 
mainEffectOpinions = "0.005$^{ }$"
mainEffectFB = "0.592$^{***}$"
mainEffectNav = "0.193$^{***}$"

#myValues = readRDS("Datasets/Lyx/LyxValues.Rds")
#myValues[name=="mainEffectPolarization", value]
#mainEffectOpinions = myValues[name=="mainEffectPersuasion", value]
#mainEffectFB = myValues[name=="mainEffectFBSlant", value]
#mainEffectNav = myValues[name=="mainEffectNavSlant", value]


# Since analyzing these results takes a while, sometime easier to save results and print table separately
#save(controlVars, file = "Datasets/Projections/controlVars.Rda")
#save(resultsOpinionsAll, file = "Datasets/Projections/resultsOpinionsAll.Rda")
#save(resultsAffectiveAll, file = "Datasets/Projections/resultsAffectiveAll.Rda")
#save(resultsFBAll, file = "Datasets/Projections/resultsFBAll.Rda")
#save(resultsNavAll, file = "Datasets/Projections/resultsNavAll.Rda")

#load("Datasets/Projections/resultsOpinionsAll.Rda")
#load("Datasets/Projections/resultsAffectiveAll.Rda")
#load("Datasets/Projections/resultsFBAll.Rda")
#load("Datasets/Projections/resultsNavAll.Rda")



# .display results ----
# .........................................................................


displayResults = as.data.table(list(outcome = c("FB", "Nav", "Opinions", "Affective")), 
                               predicted =c(""), projected = c(""))

resultsAll = rbind(resultsFBAll[[1]][, outcome:="FB"], resultsNavAll[[1]][, outcome:="Nav"], 
                   resultsOpinionsAll[[1]][, outcome:="Opinions"], resultsAffectiveAll[[1]][, outcome:="Affective"], fill=T)
resultsAll = dcast(resultsAll[, list(outcome, name, value)], outcome~name)

resultsAll[outcome=="Affective", mainEffect := mainEffectAffective]
resultsAll[outcome=="Opinions", mainEffect := mainEffectOpinions]
resultsAll[outcome=="FB", mainEffect := mainEffectFB]
resultsAll[outcome=="Nav", mainEffect := mainEffectNav]
resultsAll[, mainEffect := gsub("[^0-9\\.]", "", mainEffect)]

resultsAll[, outcomeF := factor(outcome, levels = c("FB", "Nav", "Opinions", "Affective"),
              labels = c("News exposure, posts slant (std. dev.)", "Browsing behavior, news sites slant (std. dev.)",
                         "Political opinions index", "Affective polarization index"))]

resultsAll[, predicted := formatC(predicted, digits = 3, format="f")]
resultsAll[, projected := formatC(projected, digits = 3, format="f")]

resultsAll[, treatment := "Conservative treatment, compared to liberal treatment"]
resultsAll[outcome=="Affective", treatment := "Pro-Attitudinal treatment, compared to counter-attitudinal treatment"]

printTable = resultsAll[order(outcomeF), list(outcomeF, treatment, mainEffect, predicted, projected)]
printTable = as.data.table(rbind(list("Outcome", "Treatment", "Main Effect Estimated",
                        "Predicted Effect in Subsample", "Predicted Effect in Baseline Sample"), printTable))



resultsKable = kable(printTable, format = "latex", booktabs=TRUE, align = "l", 
                     col.names = c("", "", "(1)", "(2)", "(3)")) %>% 
  row_spec(1, hline_after = T) %>%
  column_spec(column = 1, width = paste0(docWidth*0.25, "cm")) %>% 
  column_spec(column = 2, width = paste0(docWidth*0.3, "cm")) %>% 
  column_spec(column = 3, width = paste0(docWidth*0.15, "cm")) %>%
  column_spec(column = 4, width = paste0(docWidth*0.15, "cm")) %>%
  column_spec(column = 5, width = paste0(docWidth*0.15, "cm")) 

cat(resultsKable, file = "Output/Tables/Combined/causalForestResults.tex")

