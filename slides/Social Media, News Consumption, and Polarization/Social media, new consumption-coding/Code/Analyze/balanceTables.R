# Author:Roee Levy, Yale
# Date: Tue Jan 16 22:54:05 2018
# Purpose: Create balance tables and additional descriptive statistics of participants in the study

rm(list=ls()) #Remove everything
source("Code/Utils/log.R"); beginLogging()
source("Code/Utils/commonVars.R")

require(data.table)
require(dplyr)
library(fst)
library(kableExtra)

source("Code/Utils/robustReg.R")
source("Code/Utils/indexFunctions.R")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")
US_POP <- readRDS("Datasets/ExternalSurveys/usPop.rds")
US_POP_IDEO <- readRDS("Datasets/ExternalSurveys/usPopIdeo.rds")
US_FB_PEW <- readRDS("Datasets/ExternalSurveys/usFbPew.rds")


# *************************************************************************
# FUNCTIONS ----
# *************************************************************************

# Return number with stars based on pvalue
symnumWrapper <- function (x) {
  myCutPoints = c(0, .01, .05, .1, 1)
  mySymbols = c("***","**","*  ","   ")
  symnum(x, cutpoints = myCutPoints, symbols = mySymbols)
}

# Perform simple t-test and return p-value
pValueFunc <-  function (x, y) {
  if(uniqueN(x)==1) {
    warning("No variance")
  }
  
  returnValue = (t.test(x ~ y)$p.value)
  return(returnValue)
}

# Receive value and keep two digits if number is smaller than 100
fixDigitsValue <- function(x) {
  numX = as.numeric(x)
  myDigits = ifelse(numX[!is.na(numX)]>100,0, 2) 
  numX[!is.na(numX)] = mapply(function(x, y) formatC(x, digits=y, format="f", big.mark=","), 
                              numX[!is.na(numX)], myDigits)
  return(numX)
}

# Fix all digits in table
fixDigits <- function(DT) {
  numericVars = lapply(DT, class)
  numericVars = names(numericVars[numericVars=="numeric"])
  for (numericVar in numericVars) {
    DT[, eval(numericVar) := fixDigitsValue(get(numericVar))]
    DT[is.na(get(numericVar)), eval(numericVar) := ""]
  }
  return (DT)
}

# Fix variables name, change order of variables and columns, fix digits, add row number
prepareTable <- function(DT, addRowNumber=FALSE) {

  # Fix variable names
  DT = merge(DT, allVars[, list(var, varName, order)], by="var", all.x=TRUE)
  
  # Make sure variables are in the first column
  DT[is.na(varName), varName := var][, var:=NULL]
  setcolorder(DT, "varName")
  
  # Order variable s
  DT[is.na(order), order := Inf]
  DT = DT[order(order)][, order:=NULL]
  
  # Set numbers
  DT = fixDigits(DT)
  
  # Fix row with N
  DT[which(DT[, varName]=="N"), names(DT) := as.list(gsub("\\.00", "", DT[which(DT[, varName]=="N")]))]
  
  # Add row number
  if (addRowNumber) {
    DT = cbind(paste0(1:nrow(DT), ")"), DT)
    names(DT)[1] = ""
  }
  
  # Fix var name
  setnames(DT, "varName", "")
  
  return (DT)
}


# ******************************************************************
# CREATE_VARS  -----------------------------------------------------
# ******************************************************************

# Define additonal treatment vars, needed for F test
SURVEY[, treatL := treatment=="Liberal"]
SURVEY[, treatC := treatment=="Conservative"]
SURVEY[, treatPro:= matchTreatment=="Pro"]
SURVEY[, treatCounter:= matchTreatment=="Counter"]

# Vote or support 2016
SURVEY[, voteOrSupportTrump := ifelse(voteSupport2016F==2, 1, 
                                      ifelse(voteSupport2016F %in% c("1", "9"), 0, NA))] 
SURVEY[, voteOrSupportClinton := ifelse(voteSupport2016F==1, 1, 
                                        ifelse(voteSupport2016F %in% c("2", "9"), 0, NA))] 

# Binary variables
SURVEY[, female := ifelse(genderF=="female",1, ifelse(genderF=="male",0,NA))]
SURVEY[, followAboveMed := follow>median(follow, na.rm=TRUE)] # >= 3


# .national average ----
# .........................................................................

# US mean
US_POP_FOR_MERGE = as.data.table(t(US_POP), keep.rownames = TRUE)
US_IDEO_POP_FOR_MERGE = as.data.table(t(US_POP_IDEO), keep.rownames = TRUE)
US_FB_PEW = as.data.table(t(US_FB_PEW), keep.rownames = TRUE)

setnames(US_POP_FOR_MERGE, c("var", "USMean"))
setnames(US_IDEO_POP_FOR_MERGE, c("var", "USMeanIdeo"))
setnames(US_FB_PEW, c("var", "USFBMeanIdeo"))


# .datasets ----
# .........................................................................

FOLLOW = data.table::copy(SURVEY[tookFollowup==TRUE])
EXT_DATA = data.table::copy(SURVEY[installTwoWeeks==TRUE])
POST_DATA = data.table::copy(SURVEY[have14DaysPosts==TRUE])


# .list of vars ----
# .........................................................................

allVars = as.data.table(list(var = unique(c(balanceVars, attritionVars, "comply"))))
allVars[, attrition := var %in% attritionVars]
allVars[, source := ifelse(var %in% balanceBaselineVars, "Baseline Survey", 
                           ifelse(var %in% balanceDeviceVars, "Device", 
                                  ifelse(var %in% balanceFacebookVars, "Facebook",
                                         ifelse(var %in% attritionVars, "Attrition", ""))))]
opennessVars = c("certain", "openessPersonality", "seenCounterAnyShare")

# Set order for tables
allVars[, order := as.numeric(.I), by="source"]
allVars[source=="Device", order := order+100]
allVars[source=="Facebook", order := order+1000]
allVars[attrition==TRUE, order := order+10000]
allVars[var %in% c("follow", "mostNewsSM"), order := allVars[source == "Baseline Survey", max(order)]+order/100]
allVars[var %in% c(opennessVars, "comply"), order := order+100000]
allVars[var=="absthermo_DiffRepDem", order := allVars[var == "thermo_Diff", order]-order/100]

# Add US Means
allVars = merge(allVars, US_POP_FOR_MERGE, all.x=TRUE, by="var")
allVars = merge(allVars, US_IDEO_POP_FOR_MERGE, all.x=TRUE, by="var")
allVars = merge(allVars, US_FB_PEW, all.x=TRUE, by="var")

# Create missing vars
SURVEY = createMissing(SURVEY, allVars$var, warn=FALSE)

# Create sets of vars for printing balance tables
displayBalanceBars = c("democrat", "republican", "independent", "voteOrSupportClinton", "voteOrSupportTrump",
                       "follow", "mostNewsSM", "female", "age", "initialLikesNum", "postsInitialAccess", "echoChamber", "mobile", 
                       attritionVars)
allVars[, treatVars := var %in% c(displayBalanceBars, "ideologyN",
                                  "thermo_Dem", "thermo_Rep", "empathy_Dem", "empathy_Rep",  "meanInitialSlant")]
allVars[, matchVars := var %in% c(displayBalanceBars, "absIdeology", "absthermo_DiffRepDem", 
                                  "thermo_Diff", "empathyDifficult_Diff", "absInitialSlant")]

allVars[, complianceVars := ((matchVars | var %in% c("ideologyN", opennessVars, "countInitialNewsLikes")) &
                               (! var %in% c("follow", "postsInitialAccess", "absInitialSlant", attritionVars)))]
allVars[, sampleVars := var %in% c("ideologyN", "absIdeology", "republican", "democrat", "independent",
                                    "thermo_Diff", "empathyDifficult_Diff", "mostNewsSM", 
                                   "mobile", "female", "age", "initialLikesNum", "countInitialNewsLikes", "comply")]


# Provide new names for each var when printing the tables
allVars[var=="ideologyN", varName := "Ideology (-3, 3)"]
allVars[var=="absIdeology", varName := "Ideology, Abs. Value (0, 3)"]
allVars[var=="voteOrSupportClinton", varName := "Vote Support Clinton"]
allVars[var=="voteOrSupportTrump", varName := "Vote Support Trump"]
allVars[var=="thermo_Rep", varName := "Feeling Therm., Rep."]
allVars[var=="thermo_Dem", varName := "Feeling Therm., Dem."]
allVars[var=="thermo_Diff", varName := "Feeling Therm., Difference"]
allVars[var=="empathy_Rep", varName := "Difficult Pers., Rep. (1, 5)"]
allVars[var=="empathy_Dem", varName := "Difficult Pers., Dem. (1, 5)"]
allVars[var=="empathyDifficult_Diff", varName := "Difficult Pers., Difference"]
allVars[var=="initialLikesNum", varName := "Total Subscriptions"]
allVars[var=="countInitialNewsLikes", varName := "News Outlets Subscriptions"]
allVars[var=="meanInitialSlant", varName := "News Outlets Slant (-1, 1)"]
allVars[var=="absInitialSlant", varName :=       "News Outlets Slant, Abs. Value"]
allVars[var=="postsInitialAccess", varName := "Access Posts, Pre-Treat."]
allVars[var=="tookFollowup", varName := "Took Followup Survey"]
allVars[var=="have14DaysPosts", varName := "Access Posts, 2 Weeks"]
allVars[var=="installTwoWeeks", varName := "Extension Install, 2 Weeks"]
allVars[var=="persIndex", varName := "Political Opinions Index"]
allVars[var=="polAffectiveIndex", varName := "Affective Polarization Index"]
allVars[var=="mobile", varName := "Took Survey Mobile"]
allVars[var=="comply", varName := "Compliance"]
allVars[var=="echoChamber", varName := "Facebook Echo Chamber"]
allVars[var=="mostNewsSM", varName := "Most News Social Media"]
allVars[var=="absthermo_DiffRepDem", varName := "Absolute Rep-Dem Feeling Therm."]
allVars[var=="follow", varName := "Follows News"]
allVars[var=="openessPersonality", varName := "Open Personality (1, 7)"]
allVars[var=="certain", varName := "Certain (0, 4)"]
allVars[var=="seenCounterAnyShare", varName := "Seen Counter-Att. Share"]

allVars[is.na(varName), varName := 
          tools::toTitleCase(gsub("([[:lower:]]+)([[:upper:]])", "\\1 \\2", gsub("_", " ", var)))]


# .F-Tests ----
# .........................................................................


# Run F test and return F-stat and p-value. 
# Example: myData=SURVEY[treatment!="Conservative"]; vars=varsFTreat_WM; dependent="treatL"
getFTest <- function(myData, vars, dependent) {
  reg = lmR(data=myData, formula = reformulate(vars, dependent))
  sm = summary(reg)
  fstat = sm$fstatistic[["value"]]
  pValue = 1 - pf(sm$fstatistic[["value"]], sm$fstatistic[["numdf"]], sm$fstatistic[["dendf"]]) 
  return (list(f=fstat, pValue=pValue))
}

# Run all relevant F-tests in advance. Example myData=SURVEY
getAllFTests <- function(myData) {
  
  varsFTreat_WM = paste(ifelse(paste0(varsFTreat,"Have") %in% names(myData), paste0(varsFTreat, "Have+", varsFTreat,"WM"), varsFTreat), collapse = '+')
  varsFMatch_WM = paste(ifelse(paste0(varsFMatch,"Have") %in% names(myData), paste0(varsFMatch, "Have+", varsFMatch,"WM"), varsFMatch), collapse = '+')
  
  allFtest =  list(LibControl = getFTest(myData[treatment!="Conservative"], varsFTreat_WM, "treatL"),
                   ConsControl = getFTest(myData[treatment!="Liberal"], varsFTreat_WM, "treatC"),
                   LibCons = getFTest(myData[treatment!="Control"], varsFTreat_WM, "treatL"),
                   ProControl = getFTest(myData[matchTreatment!="Counter"], varsFMatch_WM, "treatPro"),
                   CounterControl = getFTest(myData[matchTreatment!="Pro"], varsFMatch_WM, "treatCounter"),
                   ProCounter = getFTest(myData[matchTreatment!="Control"], varsFMatch_WM, "treatPro"))
  return (allFtest)
}


# List of variables used for F tests
varsFTreat = allVars[treatVars & !attrition, var]
varsFMatch = allVars[matchVars & !attrition, var]

# Generate F tests for balance tables
FTest = getAllFTests(SURVEY)
FTestF = getAllFTests(FOLLOW)

# ******************************************************************
# Descriptive  -----------------------------------------------------
# ******************************************************************

completedEndlineControl = SURVEY[treatment=="Control", mean(tookFollowup)]
completedEndlineLiberal = SURVEY[treatment=="Liberal", mean(tookFollowup)]
completedEndlineConservative = SURVEY[treatment=="Conservative", mean(tookFollowup)]

saveToLyx(completedEndlineControl, "completedEndlineControl", percent = TRUE)
saveToLyx(completedEndlineLiberal, "completedEndlineLiberal", percent = TRUE)
saveToLyx(completedEndlineConservative, "completedEndlineConservative", percent = TRUE)

# ******************************************************************
# CREATE BALANCE  -----------------------------------------------------
# ******************************************************************

# Create a balance table for either baseline or followup, with or without F-test, 
# with either treatment defined as liberal/conservative/control or defined as pro/counter/control
# DATA = SURVEY; fileName = "balanceBaseline"; fTest = TRUE; matchTreat = FALSE; includePost = TRUE
createBalanceTable <- function(DATA, fileName, fTest=FALSE, matchTreat=FALSE, includePost = TRUE) {
  fullFileName = paste0("Output/Tables/Balance/", fileName, ".tex")
  print(fullFileName)
  
  # Find relevant Ftests
  if (grepl("Baseline", fileName)) {
    fData = FTest
  } else if (grepl("Follow", fileName)) {
    fData = FTestF
  }  
  
  # Find relevant variables
  if (matchTreat==TRUE) {    
    myVars = allVars[matchVars==TRUE][order(order)] 
  } else {
    myVars = allVars[treatVars==TRUE][order(order)] 
  }
  if (includePost==FALSE) {    
    myVars = myVars[source!="Attrition"][order(order)]  
  }
  currentVars = myVars[, var]
  
  # Start calculating values
  if (matchTreat==TRUE) {  
    sampleMean = DATA[!is.na(matchTreatment), lapply(.SD, mean, na.rm=TRUE), .SDcols = currentVars]
    sampleMean = melt(sampleMean, measure.vars = currentVars, value.name="Sample", variable.name = "var")
    
    groupMeans = DATA[!is.na(matchTreatment), lapply(.SD, mean, na.rm=TRUE), .SDcols = currentVars, by="matchTreatment"]
    groupMeans = dcast(melt(groupMeans, id.vars="matchTreatment", variable.name = "var"), var~matchTreatment, value.var="value")
    
    pvalueControlPro = DATA[treatCounter==0, sapply(.SD, "pValueFunc", y=treatPro), .SDcols=currentVars]
    pvalueControlCounter = DATA[treatPro==0, sapply(.SD, "pValueFunc", y=treatCounter), .SDcols=currentVars]
    pvalueProCounter = DATA[control==0, sapply(.SD, "pValueFunc", y=treatPro), .SDcols=currentVars]
    
    groupMeans[, ControlPro := paste0(fixDigitsValue(Control - Pro), symnumWrapper(pvalueControlPro))]
    groupMeans[, ControlCounter := paste0(fixDigitsValue(Control - Counter), symnumWrapper(pvalueControlCounter))]
    groupMeans[, ProCounter := paste0(fixDigitsValue(Pro - Counter), symnumWrapper(pvalueProCounter))]
  } else {
    sampleMean = DATA[, lapply(.SD, mean, na.rm=TRUE), .SDcols = currentVars]
    sampleMean = melt(sampleMean, measure.vars = currentVars, value.name="Sample", variable.name = "var")
    
    groupMeans = DATA[, lapply(.SD, mean, na.rm=TRUE), .SDcols = currentVars, by="treatment"]
    groupMeans = dcast(melt(groupMeans, id.vars="treatment", variable.name = "var"), var~treatment, value.var="value")
    
    pvalueControlLiberal = DATA[treatC==0, sapply(.SD, "pValueFunc", y=treatL), .SDcols=currentVars]
    pvalueControlCons = DATA[treatL==0, sapply(.SD, "pValueFunc", y=treatC), .SDcols=currentVars]
    pvalueConsLiberal = DATA[control==0, sapply(.SD, "pValueFunc", y=treatL), .SDcols=currentVars]
    
    groupMeans[, ControlLiberal := paste0(fixDigitsValue(Control - Liberal), symnumWrapper(pvalueControlLiberal))]
    groupMeans[, ControlCons := paste0(fixDigitsValue(Control - Conservative), symnumWrapper(pvalueControlCons))]
    groupMeans[, ConsLiberal := paste0(fixDigitsValue(Conservative - Liberal), symnumWrapper(pvalueConsLiberal))]
  }
  
  balance = merge(groupMeans, sampleMean, by="var")
  balance[, US := myVars[, USMean]]
  balance[, USIdeo := myVars[, USMeanIdeo]]
  balance[, USFBIdeo := myVars[, USFBMeanIdeo]]
  
  # Add last columns by combining diff with star and select columns
  if (matchTreat==TRUE) {
    balance = balance[, list(var, Sample, `US` = USIdeo,  
                             `Control - \nPro.` = ControlPro, `Control - \nCounter.`= ControlCounter, `Pro. - \nCounter.`= ProCounter)]
    setnames(balance, "Sample", paste0("Sample\nN=", fixDigitsValue(DATA[!is.na(matchTreatment), .N])))
  } else {
    balance = balance[, list(var, Sample, `US` = US, `FB\nUsers` = USFBIdeo, 
                             `Control - \nLib.` = ControlLiberal, `Control - \nCons.`= ControlCons, `Cons. - \nLib.`= ConsLiberal)]
    setnames(balance, "Sample", paste0("Sample\nN=", fixDigitsValue(DATA[, .N])))
  }
  
  # Fix digits and variable names
  balance = prepareTable(balance, addRowNumber = FALSE)
  
  # Add F-stat
  if (matchTreat==FALSE) {
    fTestLine = list("F-Test", "", "", "",
                   paste0(formatC(fData$LibControl$f, digits=2, format = "f"), symnumWrapper(fData$LibControl$pValue)),
                   paste0(formatC(fData$ConsControl$f, digits=2, format = "f"), symnumWrapper(fData$ConsControl$pValue)),
                   paste0(formatC(fData$LibCons$f, digits=2, format = "f"), symnumWrapper(fData$LibCons$pValue)))
    fTestPLine = list("P-Value", "", "", "",
                    paste0("[", formatC(fData$LibControl$pValue, digits=2, format = "f"), "]"),
                    paste0("[", formatC(fData$ConsControl$pValue, digits=2, format = "f"), "]"),
                    paste0("[", formatC(fData$LibCons$pValue, digits=2, format = "f"), "]"))
  } else {
    fTestLine = list("F-Test", "", "", 
                   paste0(formatC(fData$ProControl$f, digits=2, format = "f"), symnumWrapper(fData$ProControl$pValue)),
                   paste0(formatC(fData$CounterControl$f, digits=2, format = "f"), symnumWrapper(fData$CounterControl$pValue)),
                   paste0(formatC(fData$ProCounter$f, digits=2, format = "f"), symnumWrapper(fData$ProCounter$pValue)))
    fTestPLine = list("P-value", "", "", 
                    paste0("[", formatC(fData$ProControl$pValue, digits=2, format = "f"), "]"),
                    paste0("[", formatC(fData$CounterControl$pValue, digits=2, format = "f"), "]"),
                    paste0("[", formatC(fData$ProCounter$pValue, digits=2, format = "f"), "]"))
  }
  
  balance = rbindlist(list(balance, fTestLine, fTestPLine))
  setnames(balance, "V1","Variable")
  
  myVars[, index:=.I]
  myVars[, diffIndex:= max(index) - min(index) + 1, by="source"]
  vars_groups = unique(myVars[, list(source, diffIndex)])
  indexGroup = vars_groups$diffIndex
  names(indexGroup) = vars_groups$source
  
  kable(balance, digits = 3, booktabs = TRUE, align=NULL, escape = FALSE, 
        format = "latex", col.names = linebreak(colnames(balance), align = "l"))  %>%
    group_rows(index = indexGroup) %>%
    row_spec(c(nrow(myVars)), hline_after=TRUE) %>%
    column_spec(2:ncol(balance)) %>%
    add_header_above(c(" " = 1, "Mean" = ncol(balance)-4, "Difference" = 3)) %>% 
    cat(., file = fullFileName) 
  #myTexPreview(file = FALSE)  
}
  
# Create balance tables
createBalanceTable(SURVEY, "Table2_balanceBaseline", fTest = TRUE, matchTreat = FALSE)
createBalanceTable(SURVEY, "balanceBaselineMatch", fTest = TRUE, matchTreat = TRUE)
createBalanceTable(FOLLOW, "balanceFollow", fTest = TRUE, includePost = FALSE)
createBalanceTable(FOLLOW, "balanceFollowMatch", fTest = TRUE, matchTreat = TRUE, includePost = FALSE)


# *************************************************************************
# ADDITIONAL TABLES ----
# *************************************************************************

# .compliance ----
# .........................................................................

SURVEY[, complyF := factor(comply, levels=c(TRUE, FALSE), labels = c("Comply All", "Non-\nComply All"))]
SURVEY[, myGroup := ifelse(treatment=="Conservative", ifelse(comply, "Comply Conservative", "Non-\nComply Conservative"),
                        ifelse(treatment=="Liberal", ifelse(comply, "Comply Liberal", "Non-\nComply Liberal"), 
                               ifelse(treatment=="Control", "Control", NA)))]
SURVEY[, myGroupF := factor(myGroup, levels = c("Control", "Comply Liberal", "Non-\nComply Liberal",
                                             "Comply Conservative", "Non-\nComply Conservative"))]

SURVEY[, myGroupMatch := ifelse(matchTreatment=="Counter", ifelse(comply, "Comply Counter", "Non-\nComply Counter"),
                             ifelse(matchTreatment=="Pro", ifelse(comply, "Comply Pro", "Non-\nComply Pro"), 
                                    ifelse(treatment=="Control", "Control", NA)))]
SURVEY[, myGroupMatchF := factor(myGroupMatch, levels = c("Control", "Comply Pro", "Non-\nComply Pro",
                                                       "Comply Counter", "Non-\nComply Counter"))]

# Descriptive stats
SURVEY[, openessPersonalityS := scale(openessPersonality)]
SURVEY[, certainS := scale(certain)]
SURVEY[, seenCounterAnyShareS := scale(seenCounterAnyShare)]

diffOpeness = SURVEY[myGroupMatch=="Comply Counter", lapply(.SD, mean, na.rm=TRUE), .SDcols = 
         c("openessPersonalityS", "certainS", "seenCounterAnyShareS")] - 
  SURVEY[myGroupMatch=="Non-\nComply Counter", lapply(.SD, mean, na.rm=TRUE), .SDcols = 
           c("openessPersonalityS", "certainS", "seenCounterAnyShareS")]
minDiffOpeness = min(abs(diffOpeness))
maxDiffOpeness = max(abs(diffOpeness))
rangeDiffOpeness = paste0(round(minDiffOpeness, 2), "-", round(maxDiffOpeness, 2))

saveToLyx(minDiffOpeness, "minDiffOpeness", digits = 2)
saveToLyx(maxDiffOpeness, "maxDiffOpeness", digits = 2)


# create table
complyVars = c(allVars[complianceVars==TRUE, var])

complyTableMatch = SURVEY[!is.na(myGroupMatchF), append(lapply(.SD, mean, na.rm = TRUE), list(N = as.double(.N))), 
                          .SDcols=c(complyVars), by="myGroupMatchF"]
complyTableMatch = dcast(melt(complyTableMatch, id.vars = "myGroupMatchF", variable.name = "var"), var~myGroupMatchF)

complyTableTreat = SURVEY[!is.na(myGroupF), append(lapply(.SD, mean, na.rm = TRUE), list(N = as.double(.N))), 
                          .SDcols=c(complyVars), by="myGroupF"]
complyTableTreat = dcast(melt(complyTableTreat, id.vars = "myGroupF", variable.name = "var"), var~myGroupF)

complyTableAllTreat = SURVEY[control==FALSE, append(lapply(.SD, mean, na.rm = TRUE), list(N = as.double(.N))), 
                          .SDcols=c(complyVars), by="complyF"]
complyTableAllTreat = dcast(melt(complyTableAllTreat, id.vars = "complyF", variable.name = "var"), var~complyF)

# Add Control and comply with all treatments
complyTableCombined = merge(complyTableMatch[ , list(var, Control)], complyTableAllTreat, by="var")

# Add specific treatment
complyTableCombined = merge(complyTableCombined, complyTableMatch[,! "Control"], by="var")
complyTableCombined = merge(complyTableCombined, complyTableTreat[,! "Control"], by="var")

complyTableCombined = prepareTable(complyTableCombined, addRowNumber = TRUE)

colnames(complyTableCombined) = gsub("(All|Pro|Counter|Control|var|Liberal|Conservative)", "", colnames(complyTableCombined))
colnames(complyTableCombined) = gsub("Non-\nComply", "No", colnames(complyTableCombined))
colnames(complyTableCombined) = gsub("^Comply", "Yes", colnames(complyTableCombined))

kable(complyTableCombined, digits = 3, booktabs = TRUE, align=c('l'), format="latex",
      escape = FALSE, col.names = linebreak(colnames(complyTableCombined), align = "l"),
      linesep = c(rep("",6), '\\addlinespace', rep("", 3), '\\addlinespace', rep("", 4), '\\addlinespace', rep("", 2), '\\addlinespace', ""))  %>%
  add_header_above(c(" " = 2, "Control" = 1, "All\nComply:" = 2, 
                     "Pro-Att.\nComply:" = 2, "Counter-Att.\nComply:" = 2, 
                     "Liberal\nComply:" = 2, "Conservative\nComply:" = 2)) %>% 
  cat(., file = paste0("Output/Tables/Balance/complianceDescriptive.tex")) 


# .samples ----
# .........................................................................

SURVEY[, N := .N]; POST_DATA[, N := .N]; FOLLOW[, N := .N]; EXT_DATA[, N := .N]

currentVars = c(allVars[sampleVars==TRUE, var], "N")
baselineSample = melt(SURVEY[, lapply(.SD, mean, na.rm=TRUE), .SDcols = currentVars], 
                      measure.vars=currentVars, value.name = "Baseline\nSample", variable.name = "var")
accessPostsSubsample = melt(POST_DATA[, lapply(.SD, mean, na.rm=TRUE), .SDcols = currentVars],
                            measure.vars=currentVars, value.name = "Access\nPosts\nSubsample", variable.name = "var")
endlineSubsample = melt(FOLLOW[, lapply(.SD, mean, na.rm=TRUE), .SDcols = currentVars],
                            measure.vars=currentVars, value.name = "Endline\nSurvey\nSubsample", variable.name = "var")
extSubsample = melt(EXT_DATA[, lapply(.SD, mean, na.rm=TRUE), .SDcols = currentVars],
                            measure.vars=currentVars, value.name = "Extension\nSubsample", variable.name = "var")

allSamples = merge(baselineSample, accessPostsSubsample, id.vars="var")
allSamples = merge(allSamples, endlineSubsample, id.vars="var")
allSamples = merge(allSamples, extSubsample, id.vars="var")

allSamples = prepareTable(allSamples, addRowNumber = TRUE)

kable(allSamples, digits = 3, booktabs = TRUE, align=c('l'), escape = FALSE, format="latex",
      col.names = linebreak(colnames(allSamples), align = "l")) %>% 
  cat(., file = paste0("Output/Tables/Balance/descriptiveSubsample.tex")) 
  
