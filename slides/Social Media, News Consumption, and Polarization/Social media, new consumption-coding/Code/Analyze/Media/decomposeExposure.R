# Author:Roee Levy, Yale
# Date: Mon Jan 22 18:23:36 2018
# Purpose: Decompose effect on exposure to posts from the pro- and counter-attitudinal outlets offered in the experiment

library(data.table)
library(ggplot2)
library(fst)

rm(list=ls())
source("Code/Utils/log.R"); beginLogging()
source("Code/Utils/robustReg.R")
source("Code/Utils/indexFunctions.R")
source("Code/Utils/commonVars.R")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")
US_POP_IDEO <- readRDS("Datasets/ExternalSurveys/usPopIdeo.rds")
USERS_PAGES <- readRDS("Datasets/UsersPages/users_pages.rds")

FB_USER <- readRDS("Datasets/Extension/facebook/fb_user.rds")
FB_USER_7 <- readRDS("Datasets/Extension/facebook/fb_user_7.rds")

LIKE_USER <- readRDS("Datasets/Likes/likes_user.rds")
NAV_HIS_USER_FB <- readRDS("Datasets/Extension/navigate/nav_user_fb_usage.rds")


# *************************************************************************
# PREPARE ----
# *************************************************************************

# Two values for likes - liked immediately, and continued liking after two weeks
USERS_PAGES[, newLikeKept := newLike]
USERS_PAGES[removedLike14==TRUE, newLikeKept := 0]

# .Load data at the user level ----
# .........................................................................

mergeVars = c(reweightVars, "ResponseId", "comply", "newLikesNum", "matchTreatment", 
              "ideoLeaning", "potentialText", "have14DaysLikes")

FB_USER_SUM = merge(FB_USER[, list(ResponseId, potCounter_SharedByOutletDay_FB, potPro_SharedByOutletDay_FB,
                                  potCounter_SharedByOutletDay_FBPage, potPro_SharedByOutletDay_FBPage, countAllDay_FB)], 
                    SURVEY[, ..mergeVars], by="ResponseId")

# Add usage in first seven days
names7 = names(FB_USER_7)[names(FB_USER_7)!="ResponseId"]
setnames(FB_USER_7, names7, paste0(names7, "_7"))
FB_USER_SUM = merge(FB_USER_SUM, FB_USER_7[, list(ResponseId, potCounter_SharedByOutletDay_FB_7, 
                                                 potPro_SharedByOutletDay_FB_7, countAllDay_FB_7)], by="ResponseId")
FB_USER_SUM[, potCounter_SharedByOutletDay_FB_8_14 := (potCounter_SharedByOutletDay_FB*14 - potCounter_SharedByOutletDay_FB_7*7)/7]
FB_USER_SUM[, potPro_SharedByOutletDay_FB_8_14 := (potPro_SharedByOutletDay_FB*14 - potPro_SharedByOutletDay_FB_7*7)/7]
FB_USER_SUM[, countAllDay_FB_8_14 := (countAllDay_FB*14 - countAllDay_FB_7*7)/7]

# Add initial likes
FB_USER_SUM = merge(FB_USER_SUM, LIKE_USER, by="ResponseId", all.x=TRUE)

# Add historic Facebook use
FB_USER_SUM = merge(FB_USER_SUM, 
                    NAV_HIS_USER_FB[, list(ResponseId, countDay_His, countDay_Nav=countDay_14_Nav, countDay_7_Nav)], 
                    all.x=TRUE, by="ResponseId")
createMissing(FB_USER_SUM, c("countDay_His"))

# Add number of likes after 14 days
FB_USER_SUM = merge(FB_USER_SUM, USERS_PAGES[, list(newLikesNumKept = sum(newLikeKept)), by = "ResponseId"], 
                    by = "ResponseId")
stopifnot(sum(duplicated(FB_USER_SUM$ResponseId))==0)

# Change order of treatment variable
FB_USER_SUM[, matchTreatment := relevel(matchTreatment, "Counter")]


# .define sample ----
# .........................................................................

# Ignore cases where didn't see any posts and excluding one user who liked both liberal and conservative pages
includeResponses = FB_USER_SUM[countAllDay_FB>0 & matchTreatment %in% c("Pro", "Counter") & 
                                 ResponseId!="R_1F2FmVPKDTDJFKQ", ResponseId]
includeResponsesHave14Data = includeResponses[includeResponses %in% FB_USER_SUM[have14DaysLikes==TRUE, ResponseId]]

FB_USER_SUM = FB_USER_SUM[ResponseId %in% includeResponses]

sampleSizeDecompose = FB_USER_SUM[matchTreatment!="Control", .N]
sampleSizeDecomposePro = FB_USER_SUM[matchTreatment=="Pro", .N]
sampleSizeDecomposeCounter = FB_USER_SUM[matchTreatment=="Counter", .N]
saveToLyx(sampleSizeDecompose, "sampleSizeDecompose", digits = 0)
saveToLyx(sampleSizeDecomposePro, "sampleSizeDecomposePro", digits = 0)
saveToLyx(sampleSizeDecomposeCounter, "sampleSizeDecomposeCounter", digits = 0)


# .Add weights ----
# .........................................................................

# Adding missing value, only 3 values imputed
FB_USER_SUM[matchTreatment!="Control" & ResponseId %in% includeResponses & is.na(ideologyN), 
            stopifnot(.N<5)]
FB_USER_SUM[, ideologyN := ifelse(is.na(ideologyN), mean(ideologyN, na.rm=TRUE), ideologyN)]

# 1) Weights for us population
WEIGHTS = as.data.table(addWeights(FB_USER_SUM[matchTreatment!="Control"], US_POP_IDEO, reweightVars, "ResponseId"))
FB_USER_SUM = merge(FB_USER_SUM, WEIGHTS[, list(ResponseId, popWeight = SampleWeight)])

# 2) Weights to make compliers and non-compliers similar: ICSW, based on Aronow
reweightForm = paste0("comply ~ ", paste0(reweightVars, collapse = " + "))

# Create new variables: Have and WM when there are missing responses
FB_USER_SUM = createMissing(FB_USER_SUM, reweightVars)
reWeightWithMissing = paste0("comply ~ ", paste0(reweightVars, collapse = " + "))
for (i in reweightVars) {
  varHave = paste0(i, "Have")
  varWM = paste0(i, "WM")
  if (varHave %in% names(FB_USER_SUM) & ! grepl(varHave, reWeightWithMissing)) {
    reWeightWithMissing = gsub(i, paste(varHave, varWM, sep = " + "), reWeightWithMissing)
  }
}

regComplyPro = glmR(FB_USER_SUM[matchTreatment=="Pro"], formula = reWeightWithMissing, family="binomial")
stopifnot(length(predict(regComplyPro))==FB_USER_SUM[matchTreatment=="Pro", .N])
FB_USER_SUM[matchTreatment=="Pro", predictComplyPro := predict(regComplyPro, type="response")]
FB_USER_SUM[matchTreatment=="Pro" , icsw := 1 / predictComplyPro]

regComplyCounter = glmR(FB_USER_SUM[matchTreatment=="Counter"], formula = reWeightWithMissing, family="binomial")
stopifnot(length(predict(regComplyCounter))==FB_USER_SUM[matchTreatment=="Counter", .N])
FB_USER_SUM[matchTreatment=="Counter", predictComplyCounter := predict(regComplyCounter, type="response")]
FB_USER_SUM[matchTreatment=="Counter", icsw := 1 / predictComplyCounter]

# Compliers now have mean values similar to general population 
# (if used only one variable, e.g. gender - it would be identical), e.g. "comply ~ followAlways"
checkCompliers = 0
if (checkCompliers) {
  FB_USER_SUM[!is.na(comply) & !is.na(icsw) & matchTreatment=="Pro", lapply(.SD, mean, na.rm=TRUE), .SDcols = reweightVars]  
  FB_USER_SUM[comply & !is.na(icsw) & matchTreatment=="Pro", lapply(.SD, function(var) mean(var, na.rm=TRUE)), 
      .SDcols = reweightVars]
  FB_USER_SUM[comply & !is.na(icsw) & matchTreatment=="Pro", lapply(.SD, function(var) weighted.mean(var, icsw, na.rm=TRUE)), 
              .SDcols = reweightVars]
}

# .Create weekly data for robustness when comparing week 1 and week2----
# .........................................................................

# Week 1
FB_USER_SUM_WEEK1 <- data.table::copy(FB_USER_SUM)
FB_USER_SUM_WEEK1[, countAllDay_FB := countAllDay_FB_7]
FB_USER_SUM_WEEK1[, potCounter_SharedByOutletDay_FB := potCounter_SharedByOutletDay_FB_7]
FB_USER_SUM_WEEK1[, potPro_SharedByOutletDay_FB := potPro_SharedByOutletDay_FB_7]

# Week 2
FB_USER_SUM_WEEK2 <- data.table::copy(FB_USER_SUM)
FB_USER_SUM_WEEK2[, countAllDay_FB := countAllDay_FB_8_14]
FB_USER_SUM_WEEK2[, potCounter_SharedByOutletDay_FB := potCounter_SharedByOutletDay_FB_8_14]
FB_USER_SUM_WEEK2[, potPro_SharedByOutletDay_FB := potPro_SharedByOutletDay_FB_8_14]


# .load data at the group level. Observation=user*match, two observations per user ----
# .........................................................................

# Pages liked by group, use this fun
LIKE_GROUP = USERS_PAGES[potential==TRUE & ResponseId %in% includeResponses, 
                           list(ResponseId, offered, like = newLike, likeKept = newLikeKept, match=potPro)]
LIKE_GROUP = dcast(LIKE_GROUP, ResponseId + match ~ 0, value.var = c("offered", "like", "likeKept"), fun.aggregate = sum)
LIKE_GROUP[, offered := offered /4]



# *************************************************************************
# CALCULATE ----
# *************************************************************************

# DT_USER=FB_USER_SUM; withUnsub=TRUE; withTime=TRUE; noAds=FALSE; feature="NULL"; saveReg=TRUE
decomposeFunc <- function(DT_USER, withUnsub = TRUE, withTime=TRUE, noAds=FALSE, feature="NULL", saveReg = FALSE) {
  
  FB_USER_CURRENT = data.table::copy(DT_USER)
  
  # Exposure by group: number of posts from pro/counter pages seen by participants
  FB_GROUP = melt(FB_USER_CURRENT, id.vars = "ResponseId", , value.name = "PageDay_FB",
                  measure.vars = c("potCounter_SharedByOutletDay_FB", "potPro_SharedByOutletDay_FB",
                                   "potCounter_SharedByOutletDay_FBPage", "potPro_SharedByOutletDay_FBPage"))
  FB_GROUP[, match := ifelse(grepl("potPro_SharedByOutletDay_FB", variable), 1, 
                             ifelse(grepl("potCounter_SharedByOutletDay_FB", variable), 0, NA))]
  
  # Exclude ads
  FB_GROUP[, page := grepl("FBPage", variable)]
  
  # Add likes, total exposure and weights, and survey variables
  FB_GROUP = merge(LIKE_GROUP, FB_GROUP, by = c("ResponseId", "match"))
  FB_GROUP = merge(FB_GROUP, FB_USER_SUM[, list(ResponseId, icsw, popWeight, countAllDay_FB)], by="ResponseId")
  FB_GROUP[, count_SharedByOutletDay_FBPctAll := PageDay_FB / (countAllDay_FB) * 100]
  
  mergeVars = c("ResponseId", "potentialText", "treatment", "matchTreatment", "ideoLeaning", "party7",
                reweightVars)
  FB_GROUP = merge(FB_GROUP, SURVEY[, ..mergeVars], by="ResponseId")
  
  # Define whether this group was offered
  #FB_GROUP[, type := ifelse(match==1, "Pro", ifelse(match==0, "Counter", NA))]
  FB_GROUP[, offered := (match==1 & matchTreatment=="Pro") | (match==0 & matchTreatment=="Counter")]
  FB_GROUP[, offeredMatch := offered*(match==1)]
  
  FB_GROUP_CURRENT = FB_GROUP
  
  # Robustness: only use pages still liked after two weeks 
  if (withUnsub) {
    FB_USER_CURRENT = FB_USER_CURRENT[ResponseId %in% includeResponsesHave14Data]
    FB_GROUP_CURRENT = FB_GROUP_CURRENT[ResponseId %in% includeResponsesHave14Data]
    FB_USER_CURRENT[, newLikesNumVersion := newLikesNumKept]
    FB_GROUP_CURRENT[, likeVersion := likeKept]
  } else {
    FB_USER_CURRENT[, newLikesNumVersion := newLikesNum]
    FB_GROUP_CURRENT[, likeVersion := like]
  } 
  FB_GROUP_CURRENT[, likeMatch := likeVersion*(match==1)]
  
  # Robustness: ignore ads
  if (noAds) {
    FB_USER_CURRENT[, potCounter_SharedByOutletDay_FB := potCounter_SharedByOutletDay_FBPage]
    FB_USER_CURRENT[, potPro_SharedByOutletDay_FB := potPro_SharedByOutletDay_FBPage]
    FB_GROUP_CURRENT = FB_GROUP_CURRENT[page==TRUE] 
  } else {
    FB_GROUP_CURRENT = FB_GROUP_CURRENT[page==FALSE] 
  }
  
  
  # .Subscriptions ----
  # .........................................................................
  
  # Main regression
  newLikeRegScaled <- felmR(data = FB_USER_CURRENT, newLikesNumVersion ~ matchTreatment)
  
  # S_C - selection counter, S_Delta - difference
  S_C = getCoef(newLikeRegScaled, "(Intercept)")
  S_Delta = getCoef(newLikeRegScaled, "matchTreatmentPro")
  
  # Robustness Fixed effects
  if (feature=="FE") {
    newLikeRegScaledFE <- felmR(data = FB_USER_CURRENT, newLikesNumVersion ~ matchTreatment + ideoLeaning | potentialText)
    
    S_C = FB_USER_CURRENT[matchTreatment=="Counter", mean(newLikesNumVersion)]
    S_Delta = getCoef(newLikeRegScaledFE, "matchTreatmentPro")
  }
  
  # Robustness: match to US population
  if (feature=="pop") {
    newLikeRegScaledPop <- felmR(data = FB_USER_CURRENT, newLikesNumVersion ~ matchTreatment , weights = FB_USER_CURRENT$popWeight)
    
    S_C = getCoef(newLikeRegScaledPop, "(Intercept)") + getCoef(newLikeRegScaledPop, "matchTreatmentPro")
    S_Delta = getCoef(newLikeRegScaledPop, "matchTreatmentPro")
  }
  
  
  # .platform (algorithm): effect of subscriptions on exposure  ----
  # .........................................................................
  
  # Robustness: ignore Facebook usage, count number of posts instead of share
  if (!withTime)  {
    outcome = "PageDay_FB"
  } else {
    outcome = "count_SharedByOutletDay_FBPctAll"
  }
  
  # Main regression. P_C effect of subscriptions to counter-att on exposure in control, P_Delta: additional effect of subscriptions to pro
  exposedReg  = felmR(data = FB_GROUP_CURRENT, get(outcome) ~ match | 0 | (likeVersion | likeMatch ~ offered+offeredMatch) | ResponseId)
  P_C = getCoef(exposedReg, "`likeVersion(fit)`")
  P_Delta = getCoef(exposedReg, "`likeMatch(fit)`")
  stopifnot(P_C!=0)
  
  # Robustness: Fixed effects
  if (feature=="FE") {
    exposedRegFE  = felmR(data = FB_GROUP_CURRENT, get(outcome) ~ match + ideoLeaning | potentialText | (likeVersion | likeMatch ~ offered+offeredMatch) | ResponseId)
    
    P_C = getCoef(exposedRegFE, "`likeVersion(fit)`")
    P_Delta = getCoef(exposedRegFE, "`likeMatch(fit)`")
  } else if (feature=="pop") { # Robustness: Match to US population
    exposedRegPop  = felmR(data = FB_GROUP_CURRENT, get(outcome) ~ match | 0 | (likeVersion | likeMatch ~ offered+offeredMatch) | ResponseId, weights = FB_GROUP_CURRENT[, popWeight])
    P_C = getCoef(exposedRegPop, "`likeVersion(fit)`")
    P_Delta = getCoef(exposedRegPop, "`likeMatch(fit)`")
  } else if(feature=="icsw") { # Robustness: Match compliers to entire sample
    exposedRegW  = felmR(data = FB_GROUP_CURRENT, get(outcome) ~ match | 0 | (likeVersion | likeMatch ~ offered+offeredMatch) | ResponseId, weights = FB_GROUP_CURRENT[,icsw])
    P_C = getCoef(exposedRegW, "`likeVersion(fit)`")
    P_Delta = getCoef(exposedRegW, "`likeMatch(fit)`")
  }
  
  # Move from pct back to decimal 
  if(withTime==TRUE) {
    P_C = P_C/100
    P_Delta = P_Delta/100
  }
  
  
  # .usage ----
  # .........................................................................

  if (withTime==FALSE) {
    U_C = 1
    U_Delta = 0
  }  else {
    
    # Main regression. U_C number of posts seen in counter-att. treatment. U_Delta, effect on pro-att. treatment on number of posts seen
    totalFBRegControlHis = lmR(data = FB_USER_CURRENT, countAllDay_FB ~ matchTreatment + countDay_HisWM + countDay_HisHave)
    U_C = FB_USER_CURRENT[matchTreatment=="Counter", mean(countAllDay_FB)] 
    U_Delta = getCoef(totalFBRegControlHis, "matchTreatmentPro")
    
    # Calculate for two weeks insted of daily
    FB_USER_CURRENT[, countAllDay_FB14 := countAllDay_FB * 14]
    totalFBRegControlHis14 = lmR(data = FB_USER_CURRENT, countAllDay_FB14 ~ matchTreatment + countDay_HisWM + countDay_HisHave)
    U_C14 = FB_USER_CURRENT[matchTreatment=="Counter", mean(countAllDay_FB14)] 
    
    # Robustness: Fixed effects
    if (feature=="FE") {
      totalFBRegControlHisFE = lmR(data = FB_USER_CURRENT, countAllDay_FB ~ matchTreatment + countDay_HisWM + countDay_HisHave + ideoLeaning + potentialText)
      
      U_C = FB_USER_CURRENT[matchTreatment=="Counter", mean(countAllDay_FB)] 
      U_Delta = getCoef(totalFBRegControlHisFE, "matchTreatmentPro")
    } else if (feature=="pop") { # Robustness: match to US population
      totalFBRegControlHisPop = lmR(data = FB_USER_CURRENT, countAllDay_FB ~ matchTreatment + countDay_HisWM + countDay_HisHave + ideoLeaning + potentialText, weights = FB_USER_CURRENT[, popWeight])
      
      U_C = FB_USER_CURRENT[matchTreatment=="Counter", weighted.mean(countAllDay_FB, popWeight)] 
      U_Delta = getCoef(totalFBRegControlHisPop, "matchTreatmentPro")
    }
  }
  
  
  # .save regression ----
  # .........................................................................

  if (saveReg) {
    meanExposure = FB_GROUP_CURRENT[matchTreatment=="Counter", mean(count_SharedByOutletDay_FBPctAll, na.rm=TRUE)]
    regFile = "Output/Tables/Exposure/explainingExpGapWithUsage.tex"
    mainReg = stargazerR(newLikeRegScaled, totalFBRegControlHis14, exposedReg,                
               dep.var.labels.include = TRUE,
               order = c("offered", "newLikeVersion\\(fit", "nenewLikeVersion", "Counter"),
               add.lines = list(c("Unit", "Participant", "Participant", "Participant*"),
                                c("", "", "", "Outlet Group"), 
                                c("Baseline Controls", "", "X", ""),
                                c("Mean in Counter-Att. Treatment", round(c(S_C, U_C14, meanExposure), 3))), 
               keep = "offeredWith|like|Treatment",
               dep.var.labels = c("\\multirow{2}{2.5cm}{Subscriptions}", 
                                 "\\multirow{2}{2.5cm}{FB Usage^^^ Total Posts Observed}", 
                                 "\\multirow{2}{2.5cm}{Platform Algorithm^^^ Share of Posts}"),
               column.labels =c("OLS", "OLS", "IV"), model.names = FALSE
               ,replaceCov = replaceWords, type = "latex", file = regFile)
    
    addLinePrint(mainReg, regFile, count = 2)
  }
  
  return (list(S_C, S_Delta, P_C, P_Delta, U_C, U_Delta))
}


# .print main regression table ----
# .........................................................................

decomposeFunc(FB_USER_SUM, saveReg = TRUE, withTime = TRUE, noAds=FALSE, withUnsub = FALSE, feature="NULL")


# .store all results ----
# .........................................................................

results = as.data.table(expand.grid(withTime = c(TRUE, FALSE), 
                                    withUnsub = c(TRUE, FALSE), 
                                    noAds = c(TRUE, FALSE), 
                                    feature = c("FE", "NULL", "pop", "icsw"),
                                    S_C = as.numeric(NA), S_Delta = as.numeric(NA), 
                                    P_C = as.numeric(NA), P_Delta = as.numeric(NA), 
                                    U_C = as.numeric(NA), U_Delta = as.numeric(NA)))

# Remove results I don't present
results = results[withTime==TRUE | (feature=="NULL")]
results = results[noAds==FALSE | (feature=="NULL")]

# Add weekly results
weeklyResults = as.data.table(list(withTime=TRUE, withUnsub=c(TRUE, TRUE, FALSE, FALSE), noAds=FALSE, 
                                   feature=c("week1", "week2", "week1", "week2")))
results = rbindlist(list(results, weeklyResults), use.names = TRUE, fill=TRUE)
results[, data := ifelse(feature=="week1", "FB_USER_SUM_WEEK1", 
                         ifelse(feature=="week2", "FB_USER_SUM_WEEK2", "FB_USER_SUM"))]

results[, name:=paste(ifelse(withTime, "Time", "NoTime"), ifelse(withUnsub, "Unsub", "Orig Likes"), 
                      paste0(ifelse(noAds, "NoAds", ""), feature))]
# Run all regressions
for (i in 1:nrow(results)) {
  results[i, c("S_C", "S_Delta", "P_C", "P_Delta", "U_C", "U_Delta") :=
          decomposeFunc(get(data), saveReg = FALSE, withTime = withTime, withUnsub = withUnsub, noAds=noAds, feature=feature)]
}


# .Test ----
# .........................................................................

TEST_RESULTS = 0
if (TEST_RESULTS) {
  
  # Debug
  Debug = 0
  if (Debug) {
    testR = c("R_1MMW930ipqnxPQF", "R_3O0SPRZ8929LtMv", "R_1mxe2BrCUThRjnQ")
    testData = FB_POOLED_SUM[potential==TRUE & ResponseId %in% testR]
    testData = FB_POOLED_SUM[potential==TRUE]
    testData[, list(ResponseId, countDay_FB, newLikeVersion, potPro, offeredWithControl, outlet)]
    exposedRegMatchPool  = felmR(data=testData, countDay_FB ~ potPro | 0  |
                                   (newLikeVersion | newLikepotPro ~ offeredWithControl+offeredWithControlpotPro) | ResponseId)
    
    meanNumLikeMatch = newLikeRegScaled$coefficients[["offeredWithControl"]]+newLikeRegScaled$coefficients[["offeredWithControl:potPro"]]
    exposureMatch14 = exposurePerSubMatch14*meanNumLikeMatch
    
    testUser = FB_USER_SUM # FB_USER_CURRENT[ResponseId %in% testR]
    exposedMReg14User       = lmR(data=testUser, potProDay_FB ~ treatPro) 
    exposureMatch14User = exposedMReg14User$coefficients[["treatProTRUE"]]
  }
  
  resultNoTime = results[withTime==FALSE & withUnsub==TRUE & noAds==FALSE & feature=="NULL"]
  
  exposureMatch14 = resultNoTime[, (P_C+P_Delta) * (S_C+S_Delta)]
  exposureOp14 = resultNoTime[, P_C * S_C]
  
  # Regressions: Effect of matchTreatment on total exposure, used later to check results, here using USER regressions instead of pooled
  exposedMReg14User = lmR(data=FB_USER_SUM[have14DaysLikes==TRUE & matchTreatment!="Control"], potPro_SharedByOutletDay_FB ~ matchTreatment) 
  exposedOReg14User  = lmR(data=FB_USER_SUM[have14DaysLikes==TRUE & matchTreatment!="Control"], potCounter_SharedByOutletDay_FB ~ matchTreatment)
  
  # Actual treatment effect on number of posts seen, used only to test that I didn't make a mistake in calculation
  exposureMatch14User = exposedMReg14User$coefficients[["matchTreatmentPro"]]
  exposureOp14User = exposedOReg14User$coefficients[["matchTreatmentPro"]]
  cat("Actual gap:", exposureMatch14User+exposureOp14User)
  
  # There is a VERY small difference, might be related to two observation that have NA in newLikesNum
  #testthat::expect_lt(abs(exposureMatch14 - exposureMatch14User), 0.01) # Gap should be smaller, say up to 0.01
  #testthat::expect_lt(abs(exposureOp14 + exposureOp14User), 0.01)
  stopifnot(abs(exposureMatch14 - exposureMatch14User)<0.01) # Gap should be smaller, say up to 0.01
  stopifnot(abs(exposureOp14 + exposureOp14User)<0.01)
}


# .summary stats ----
# .........................................................................

results[, counter := S_C * P_C * U_C]
results[, pro := (S_C+S_Delta) * (P_C+P_Delta) * (U_C+U_Delta)]

# Decompose: effect of subscriptions, holding platform and usage constant
results[, sub := U_C*S_Delta*P_C]
results[, filter := U_C*S_C*P_Delta]
results[, usage := U_Delta*S_C*P_C]
results[, combinations :=  U_Delta*S_Delta*P_Delta + U_C*S_Delta*P_Delta + U_Delta*S_Delta*P_C +  U_Delta*S_C*P_Delta]

# Share of each component
results[, total := sub + filter + usage + combinations]
results[, subP := sub / total]
results[, filterP := filter/total]
results[, usageP := usage/total]
results[, combinationsP := combinations/total]



# *************************************************************************
# DESC-STAT ----
# *************************************************************************

# Weekly difference in posts
weeklyMainResults = results[withTime==TRUE & withUnsub==FALSE & feature %in% c("week1", "week2") & noAds==FALSE, 
                            list(feature, counter, pro, total, sub, filter, usage, combinations)]
weeklyMainResults[, increase := (pro-counter)/(counter)]
saveToLyx(weeklyMainResults[feature=="week1", increase], "increaseProWeekOne", percent = TRUE)
saveToLyx(weeklyMainResults[feature=="week2", increase], "increaseProWeekTwo", percent = TRUE)



# *************************************************************************
# PLOT ----
# *************************************************************************

# .graph all ----
# .........................................................................

legendNames = c("Combinations", "Usage", "Subscriptions", "Algorithm")

allResultsPct = melt(results, id.vars = "name", measure.vars = c("subP", "filterP", "usageP", "combinationsP"), value.name = "percent")
allResultsPct[, variable := gsub("P", "", variable)]
allResults = melt(results, id.vars = c("name"), measure.vars = c("sub", "filter", "usage", "combinations"))
allResults = merge(allResults, allResultsPct, by=c("name", "variable"))

allResults[, myName := factor(name, levels= rev(c("Time Orig Likes NULL", "Time Orig Likes FE", "Time Unsub NULL", 
                                                  "Time Orig Likes NoAdsNULL",
                                           "Time Orig Likes icsw", "Time Orig Likes pop", "NoTime Orig Likes NULL",
                                           "Time Orig Likes week1", "Time Orig Likes week2")), 
                              labels = rev(c("Primary", "Potential Outlet FE", "Exclude Unsubscriptions", 
                                             "Exclude Suspected Ads",
                                         "Reweight Based on Compliance", "Reweight for Population", "Exclude Facebook Usage",
                                         "First Week", "Second Week")))]

# Percent is the label
allResults[, myLabel := scales::percent(percent, accuracy = 1)]
allResults[percent==0, myLabel := ""]

# Legend name
allResults = allResults[!is.na(myName)]
allResults[, varF := factor(variable, levels = c("combinations", "usage", "sub", "filter"), labels = legendNames)]

# Change scale for entire two weeks
allResults[, valueTW := value * 14]

ggplot(allResults, aes(x=myName, y=valueTW, fill=varF, label=myLabel, alpha=varF)) + 
  geom_col(color="darkgray") + 
  labs(y = "Gap in Exposure") +
  coord_flip() +
  scale_alpha_manual(values = c(0.5, 0.65, 0.75, 0.80))  +
  scale_fill_manual(values = c("lightskyblue1", "steelblue1", "dodgerblue2", "royalblue4"))  +
  geom_text(size = 2.5, position = position_stack(vjust = 0.5), fontface = "bold", alpha=1) +
  guides(fill = guide_legend(reverse=T), alpha=guide_legend(reverse=T)) 
themeCurrent = theme(axis.title.y=element_blank(), legend.position = "right", legend.key.size = unit(0.7, "cm"))

ggMySaveDoc("Output/Graphs/Exposure/filterBubbleRobust.eps", theme = themeCurrent, device = cairo_ps)
ggMySaveBeamer("Output/Graphs/Exposure/filterBubbleRobust_Beamer.eps", theme = themeCurrent, device = cairo_ps, noLinks = TRUE)


# .graph main ----
# .........................................................................

# Main results graph
mainResults = as.data.table(t(results[withTime==TRUE & withUnsub==FALSE & feature=="NULL" & noAds==FALSE, 
                                      list(counter, pro, total, sub, filter, usage, combinations)]), keep.rownames = T)
setnames(mainResults, "V1", "value")

plotExpDiffNew = as.data.table(expand.grid(bar=c("pro", "counter", "diff"), 
                                           variable=c("total", "filter", "sub", "usage", "combinations")))

# Name of each columns
plotExpDiffNew[, barF := factor(bar, labels = c("Pro-Attitudinal\nTreatment", "Counter-Attitudinal\nTreatment", "Difference"))]

# For pro and counter save total exposure, for diff decompose to components
plotExpDiffNew = rbind(merge(plotExpDiffNew[bar=="diff"], mainResults, by.x="variable", by.y="rn"),
                       merge(plotExpDiffNew[bar!="diff"], mainResults, by.x ="bar", by.y="rn"))
plotExpDiffNew[variable!="total" & bar!="diff", value:= 0]

# Start difference plot where counter plot ends
plotExpDiffNew[variable=="total" & bar=="diff", value:= plotExpDiffNew[variable=="total" & bar=="counter", value]]

# Don't show the following in the graph
plotExpDiffNew[, empty := ifelse(variable=="total" & bar=="diff", "white", "black")]

# Legend names
plotExpDiffNew[, group := ifelse(variable!="total" & bar=="diff", as.character(variable), 
                                 ifelse(variable=="total" & bar=="diff", "empty", "total"))]
plotExpDiffNew[, groupF := factor(group, levels = c("empty", "total", "combinations", "usage", "sub", "filter"),
                                  labels = c("", "All", "Combinations", "Usage", "Subscriptions", "Algorithm"))]

# Percent for labels and where to place the labels (height)
plotExpDiffNew[variable!="total" & bar=="diff", percentDiff := scales::percent(value / sum(value))]
plotExpDiffNew[, height := sapply(as.numeric(groupF), 
     function(x) plotExpDiffNew[as.numeric(groupF)<=x & bar=="diff", sum(value, na.rm=TRUE)])]

# Change scale for entire two weeks
plotExpDiffNew[, valueTW := value * 14]
plotExpDiffNew[, heightTW := height * 14]

plotExpDiffNew[, emptyGray := bar=="diff" | empty=="white"]

# Only gray bars for slides
ggplot(plotExpDiffNew, aes(x = barF, y = valueTW, fill = groupF, alpha = groupF, color=emptyGray)) + 
  geom_col(width=0.7, position = position_stack(reverse = TRUE)) + # ifelse(plotExpDiffLong3$empty, "White", "black"), 
  ylab("Facebook Posts from Assigned Outlets") +
  scale_color_manual(values = c("black", "white"), guide = guide_legend(override.aes = list(color = "white"))) +
  scale_fill_manual(values = c("white", "gray", rep("white", 4)))  +
  scale_alpha_manual(values = c(0, 1, rep(0, 4))) +
  scale_y_continuous(breaks=seq(10, 60, 10)) +
  geom_text(aes(x= barF, y=heightTW, label=(percentDiff)), nudge_y=-2, nudge_x=-0.6, size=3, alpha=0, fontface="bold") + 
  guides(fill = FALSE, alpha = guide_legend(override.aes=list(fill=NA)), color=FALSE) 
themeCurrent = theme(axis.title.x=element_blank(), plot.caption = element_text(size = 8, hjust = 0),
                     legend.position = "right", legend.key.size = unit(0.7, "cm"), 
                     legend.text = element_text(color = "white"),
                     legend.key = element_rect(color="white"))
ggMySaveBeamer("Output/Graphs/Exposure/filterBubbleSelectiveExposureWithUsageGray_Beamer.eps", theme = themeCurrent, device = cairo_ps)

# All
ggplot(plotExpDiffNew[order(-empty)], aes(x = barF, y = valueTW, fill = groupF, alpha = groupF, color=empty)) + 
  geom_col(width=0.7, position = position_stack(reverse = TRUE)) +
  ylab("Facebook Posts from Assigned Outlets") +
  scale_color_manual(values = c("black", "white"), breaks = c("black", "white")) +
  scale_fill_manual(breaks = c("Algorithm", "Subscriptions", "Usage", "Combinations"),
                    values = c("white", "gray", "lightskyblue1", "steelblue1", "dodgerblue3", "royalblue4"))  +
  scale_alpha_manual(breaks = c("Algorithm", "Subscriptions", "Usage", "Combinations"),
                     values = c(0, 1, 0.5, 0.65, 0.85, 1)) +
  scale_y_continuous(breaks=seq(10, 60, 10)) +
  guides(color=FALSE) +
  geom_text(aes(x= barF, y=heightTW, label=(percentDiff)), nudge_y=-2, nudge_x=-0.6, size=3, alpha=1, fontface="bold")
themeCurrent = theme(axis.title.x=element_blank(), plot.caption = element_text(size = 8, hjust = 0),
                     legend.position = "right", legend.key.size = unit(0.7, "cm"))

ggMySaveDoc("Output/Graphs/Exposure/Figure10_decomposition.eps", theme = themeCurrent, device = cairo_ps)
ggMySaveBeamer("Output/Graphs/Exposure/decomposition_Beamer.eps", theme = themeCurrent, device = cairo_ps)


