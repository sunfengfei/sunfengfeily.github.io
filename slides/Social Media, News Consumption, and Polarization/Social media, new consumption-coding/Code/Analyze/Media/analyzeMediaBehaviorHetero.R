# Author:Roee Levy, Yale
# Date: Mon Jan 22 18:23:36 2018
# Purpose: Analyze heterogeneous effects on engagement with potential outlets: subscriptions to outlets, exposure to posts in the feed, websites visited, and posts shared


rm(list=ls())
source("Code/Utils/log.R"); beginLogging()

library(data.table)
library(dplyr)
library(fst)
library(ggplot2)
library(broom)


source("Code/Utils/robustReg.R")
source("Code/Utils/indexFunctions.R")
source("Code/Utils/commonVars.R")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")
LIKE_USER <- readRDS("Datasets/Likes/likes_user.rds")
FB_USER <- readRDS("Datasets/Extension/facebook/fb_user.rds")
NAV_HIS_USER <- readRDS("Datasets/Extension/navigate/nav_user.rds")


# *************************************************************************
# PREPARE  ----
# *************************************************************************

depNames = c("New Subscriptions", "Facebook Exposure", "Browsing Behavior")

# Merge survey, FB, NAV, SHARED and user details
ALL_USER = merge(FB_USER, NAV_HIS_USER, by="ResponseId", all=TRUE)
ALL_USER = merge(ALL_USER, LIKE_USER, by="ResponseId", all.x=TRUE)
ALL_USER = merge(ALL_USER, SURVEY, by="ResponseId")

# Create missing
createMissingVars = names(ALL_USER)[grepl("^pot(Counter|Pro).*(_His)", names(ALL_USER))] 
createMissingVars = createMissingVars[!grepl("(Have|WM)", createMissingVars)]
ALL_USER = createMissing(ALL_USER, createMissingVars)

# Note: FULL_DATA is sub sample of extention and posts combined
FULL_DATA = ALL_USER[validExtAndShared==TRUE]


# *************************************************************************
# REGRESSIONS ----
# *************************************************************************


vars = as.vector(outer(c("potCounter", "potPro"), c("FB", "Nav", "His"), paste, sep="_"))

# Create list of regression (no mobile variation with extension)
models = c("potCounter_Like", "potPro_Like", vars[1:4])
heteroReg = as.data.table(expand.grid(var = heteroVars[heteroVars!="mobile"], model = models))

# Add outcome, treatment
heteroReg[, outcome := model]
heteroReg[, treat :=  "matchTreatment"]

# Controls: for navigation have history (don't have that for exposure or subscriptions) also control for potential outlets
heteroReg[, controls := paste0(treat, ifelse(grepl("Nav", model), 
                                             paste0(" + ", gsub("Nav", "His", model), "Have +",
                                                    gsub("Nav", "His", model), "WM"), ""))]

heteroReg[var!="All", heteroControls := mapply(function(treat,y,z) gsub(treat, paste0(treat,"*",y), z), treat, var, controls)]
heteroReg[var=="All", heteroControls := paste0(sapply(treat, function(x) paste(paste0(x, "*", heteroVarsForAll), collapse = "+")), " + ",controls)]     
heteroReg[, myFormula := paste0(outcome, "~", heteroControls, " | potentialText")]
heteroReg[, varF := factor(var, levels = heteroVars, labels = heteroVarLabels)]
heteroReg[, modelName := paste("regHet", gsub("[\\. ]", "", var), model, sep="_")]
heteroReg[, mainHetroReg := var %in% c(mainHeteroVars, "All")]

# Run regressions
for (i in 1:nrow(heteroReg)) {
  if (grepl("Counter", heteroReg[i, model])) {
    heteroReg[i, assign(paste0(modelName),felmR(data = FULL_DATA[matchTreatment!="Pro"], 
                                                formula = as.formula(myFormula)), pos=globalenv())]
  } else {
    heteroReg[i, assign(paste0(modelName),felmR(data = FULL_DATA[matchTreatment!="Counter"], 
                                                formula = as.formula(myFormula)), pos=globalenv())]
  }
}



# *************************************************************************
# PLOT ----
# *************************************************************************

# .Collect models ----
# .........................................................................

heteroModels= rbindlist(lapply(heteroReg[, modelName], function(x) tidyRob(get(x), x)))

# Keep only interaction and add name of interaction var
heteroModels = heteroModels[grepl(":", term)]
heteroModels[, interactVar := gsub("^.*:", "", gsub("TRUE", "", term))]
heteroModels = merge(heteroModels, unique(heteroReg[, list(var, varF)]), by.x = "interactVar", by.y="var")

# One regression with all covariates or multiple regressions
heteroModels[, regType := ifelse(grepl("All", modelName), "Joint Regression", "Separate Regressions")]

# Name of outcome
heteroModels[, type := ifelse(grepl("Counter_", modelName), "Counter", "Pro")]
heteroModels[, outcomeShort := paste0(ifelse(grepl("_Like", modelName), "Subscriptions",
                                             ifelse(grepl("_Nav", modelName), "Browsing",
                                                    ifelse(grepl("_FB", modelName), "Exposure", NA))), "_", type)]
heteroModels[, outcomeF := factor(outcomeShort, levels = c("Subscriptions_Pro", "Exposure_Pro", "Browsing_Pro",
                                                           "Subscriptions_Counter", "Exposure_Counter", "Browsing_Counter"), 
                                  labels = c(paste0(depNames, " \nPro-Att Treat*Var"),
                                             paste0(depNames, " \nCounter-Att Treat*Var")))]

# Main hetero vars to be presented
heteroModels[, mainTest := interactVar %in% mainHeteroVars]
#heteroModels[, scaled := grepl("Scale$", modelName)]
heteroModels[, like := grepl("Like$", modelName)]


heteroModels[varF=="Seen Counter Att." & regType=="Separate Regressions"]

# .plots ----
# .........................................................................

currentTheme = theme(axis.title.y = element_blank(), legend.position = c("none"), panel.border = element_rect(fill=NA),
                     axis.text.y=element_text(hjust=0))

# Effect on engagement with counter-attitudinal outlets
regressionPlot(ggplot(heteroModels[regType=="Separate Regressions" & mainTest & type=="Counter"], 
                      aes(x = varF, y=estimate))) + 
  ylab("ITT - Interaction Effect") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n=3)) +
  facet_wrap(~outcomeF, ncol=3, scales = "free_x")
ggMySaveBeamer("Output/Graphs/Combined/counterHetero_Sep_Beamer.eps", theme=currentTheme)
ggMySaveDoc("Output/Graphs/Combined/counterHetero_Sep.eps", theme=currentTheme)

# Effect on engagement with pro-attitudinal outlets
regressionPlot(ggplot(heteroModels[regType=="Separate Regressions" & mainTest & type=="Pro"], 
                      aes(x = varF, y=estimate))) + 
  ylab("ITT - Interaction Effect") + 
  scale_y_continuous(breaks = scales::pretty_breaks(n=3)) +
  facet_wrap(~outcomeF, ncol=3, scales = "free_x")
ggMySaveBeamer("Output/Graphs/Combined/proHetero_Sep_Beamer.eps", theme=currentTheme)
ggMySaveDoc("Output/Graphs/Combined/proHetero_Sep.eps", theme=currentTheme)


# Only for slides: show that results are similar when adding all heterogenous covariates in one regression
regressionPlot(ggplot(heteroModels[regType=="Joint Regression" & mainTest & type=="Counter"], 
                      aes(x = varF, y=estimate))) + 
  ylab("ITT - Interaction Effect") + 
  facet_wrap(~outcomeF, ncol=3, scales = "free_x")
ggMySaveBeamer("Output/Graphs/Combined/counterHetero_Joint_Beamer.eps", theme=currentTheme)




