# Author:Roee Levy, Yale
# Date: Mon Jan 22 18:23:36 2018
# Purpose: Analyze effects on engagement with potential outlets: subscriptions to outlets, exposure to posts in the feed, websites visited, and posts shared


rm(list=ls())
source("Code/Utils/log.R"); beginLogging()

library(data.table)
library(dplyr)
library(fst)
library(ggplot2)
library(broom)
library(ggpubr) # For gradual plots

source("Code/Utils/robustReg.R")
source("Code/Utils/indexFunctions.R")
source("Code/Utils/commonVars.R")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")
LIKE_USER <- readRDS("Datasets/Likes/likes_user.rds")
FB_USER <- readRDS("Datasets/Extension/facebook/fb_user.rds")
NAV_HIS_USER <- readRDS("Datasets/Extension/navigate/nav_user.rds")
SHARED_USER <- readRDS("Datasets/Shared/shared_user.rds")


# *************************************************************************
# PREPARE  ----
# *************************************************************************

depNames = c("New Subscriptions", "Facebook Exposure", "Browsing Behavior", "Sharing Behavior")
depNamesShared = c("Shared", "Shared No Commentary")

# Merge survey, FB, NAV, SHARED and user details
ALL_USER = merge(FB_USER, NAV_HIS_USER, by="ResponseId", all=TRUE)
ALL_USER = merge(ALL_USER, SHARED_USER, by="ResponseId", all=TRUE)
ALL_USER = merge(ALL_USER, SURVEY, by="ResponseId")
ALL_USER = merge(ALL_USER, LIKE_USER, by="ResponseId", all.x=TRUE)

# Historical news consumption 
ALL_USER[, potLib_HisTwoWeeks := potLibDay_His*14]
ALL_USER[, potCons_HisTwoWeeks := potConsDay_His*14]
ALL_USER[, potPro_HisTwoWeeks := potProDay_His*14]
ALL_USER[, potCounter_HisTwoWeeks := potCounterDay_His*14]
ALL_USER[, liberal_HisTwoWeeks := liberalDay_His*14]
ALL_USER[, conservative_HisTwoWeeks := conservativeDay_His*14]
#ALL_USER = createMissing(ALL_USER, "potPro_SharedBefore")

# Create all pro and all counter variables, don't need to analyze fromFB
conservativeNames = names(ALL_USER)[grepl("conservative", names(ALL_USER))]
conservativeNames = conservativeNames[!grepl("From", conservativeNames)]

# Create missing
ALL_USER = createMissing(ALL_USER, names(ALL_USER)[grepl("_His|SharedBefore", names(ALL_USER)) & 
                                                     ! grepl("Have|WM", names(ALL_USER))])

# Note: FULL_DATA is sub sample of extention and posts combined
FULL_DATA = ALL_USER[validExtAndShared==TRUE]
rm(LIKE_USER, FB_USER); gc()


# .regData ----
# .........................................................................

# Create one large table for all regression
regData = expand.grid(dvShort=c("potCons", "potPro", "potLib", "potCounter"),
                      family=c("Shared", "SharedEmpty", "FB", "FBFriend", "FBAd", "FBPage", 
                               "Nav", "NavFromFB", "NavNotFromFB", "Like"), 
                      version = c("Primary", "FD", "Group4"), 
                      regType = c("lm", "Poisson"),
                      controls="") %>% as.data.table()

# Remove regression that won't be using: Don't have slant DV for likes, 
# only have fromFB for Nav, don't always run poisson, for friend only interested in potential outlets
# group4 (treat*ideo) only for potential outlets
regData = regData[! (family=="Like" & (!grepl("pot", dvShort)))]
regData = regData[!grepl("FromFB", family) | grepl("pot", dvShort)]
regData = regData[regType=="lm" | (version=="FD" & family %in% c("Shared", "FB", "Nav", "Like") & 
                                     dvShort %in% c("potCons", "potPro", "potLib", "potCounter"))]
regData = regData[! grepl("Friend|Ad|Page", family) | (grepl("pot", dvShort))]
regData  = regData[! (version=="Group4" & (!grepl("pot", dvShort) | grepl("(From|Ad|Page|Empty)", family)))]

# Define dependent variable and treatment
regData[, dv := paste0(dvShort,"_", family)]
regData[, treat := ifelse(grepl("potCons", dvShort) | grepl("potLib", dvShort), "treatment", "matchTreatment")] 
regData[version=="Group4", treat := "treatIdeo + ideoLeaningF"]

# Which dataset to use
regData[, useData := ifelse(version=="FD" | version=="Group4", "FULL_DATA", "ALL_USER")]

# Add controls, only add missing when exists, otherwise have NA
regData[grepl("Shared", family), uniqueHave := mapply(function(myData, myDv) 
  get(myData)[!is.na(get(myDv)), uniqueN(get(paste0(gsub("Shared", "SharedBefore", myDv), "Have")))], useData, dv)]
regData[grepl("Shared", family), controls := ifelse(version=="Un", "", paste0(gsub("Shared", "SharedBefore", dv), "WM" ,
                                                                              ifelse(uniqueHave>1, paste0(" + ", gsub("Shared", "SharedBefore", dv), "Have"), "")))]

# With His don't have fromFB 
regData[grepl("Nav", family), controls := paste0(gsub("Nav", "HisTwoWeeksWM", dv), " + " ,
                                                 paste0(gsub("Nav", "HisTwoWeeksHave", dv)))]
regData[, controls := gsub("NotFromFB", "", controls)]
regData[, controls := gsub("FromFB", "", controls)]

# Define formula
regData[, indVar := ifelse(controls=="", treat, paste0(treat, " + ", controls))]
regData[, userFormula := ifelse(controls=="", paste0(dv, "~", treat), paste0(dv, "~", treat, " + ", controls))]

# Model name
regData[, modelName := paste0(dv, "_", version, "_Reg")]
regData[regType!="lm", modelName := paste0(modelName, regType)]

# Get control group mean
regData[treat=="treatment", controlMean := mapply(function(d, dep) 
  get(d)[treatment=="Control", mean(get(dep), na.rm=TRUE)], useData, dv)]
regData[treat=="matchTreatment", controlMean := mapply(function(d, dep) 
  get(d)[matchTreatment=="Control", mean(get(dep), na.rm=TRUE)], useData, dv)]

# Run the regressions
for (i in 1:nrow(regData)) {
  if (regData[i, regType=="lm"]) {
    regData[i, assign(modelName,lmR(data=get(useData), formula = as.formula(userFormula)), pos=globalenv())]
  } else if (regData[i, regType=="Poisson"]) {
    regData[i, assign(modelName,glmR(data=get(useData), formula = as.formula(userFormula), family="poisson"), pos=globalenv())]
  } 
}



# *************************************************************************
# DISPLAY REGRESSIONS  ----
# *************************************************************************

# .Pro vs Counter ----
# .........................................................................

starMatchFile = "Output/Tables/Combined/allResultsMediaMatch.tex"
starMatchFilePoisson = "Output/Tables/Combined/allResultsMediaMatchPoisson.tex"

matchData = c("potPro_Like_FD_Reg",  "potPro_FB_FD_Reg",  "potPro_Nav_FD_Reg", "potPro_Shared_FD_Reg",
              "potCounter_Like_FD_Reg", "potCounter_FB_FD_Reg", "potCounter_Nav_FD_Reg", "potCounter_Shared_FD_Reg")
matchDataPoisson = paste0(matchData[!grepl("Like", matchData)], "Poisson")

# Dependent var names for tables
depNamesPro = paste0("Pro-Att. Outlets ", depNames)
depNamesCounter = paste0("Counter-Att. Outlets ", depNames)
depNamesLiberal = paste0("Liberal Outlets ", depNames)
depNamesCons = paste0("Conservative Outlets ", depNames)

multirowOpen = "\\multirow{2}{2.1cm}{"
depNamesProCounter = c(paste0(paste0(multirowOpen, depNamesPro), "}"), paste0(paste0(multirowOpen, depNamesCounter), "}"))
depNamesProCounterPoisson = c(paste0(paste0(multirowOpen, paste0("Pro-Att. Outlets ", depNames[2:4])), "}"), 
                        paste0(paste0(multirowOpen, paste0("Counter-Att. Outlets ", depNames[2:4])), "}"))
depNamesLibCons = c(paste0(paste0(multirowOpen, depNamesLiberal), "}"), paste0(paste0(multirowOpen, depNamesCons), "}"))
depNamesSharedProCounter = c(paste0(paste0(multirowOpen, paste0("Pro-Att. Outlets ", depNamesShared)), "}"), 
                             paste0(paste0(multirowOpen, paste0("Counter-Att. Outlets ", depNamesShared)), "}")) 

# Effect on pro/counter potential outlets (Note: using a list within stargazer takes much longer)
starMatch = stargazerR(potPro_Like_FD_Reg, potPro_FB_FD_Reg, potPro_Nav_FD_Reg, potPro_Shared_FD_Reg,
                       potCounter_Like_FD_Reg, potCounter_FB_FD_Reg, potCounter_Nav_FD_Reg, potCounter_Shared_FD_Reg,
       add.lines = append(diffFValueAddRow("Pro Treat - Counter Treat", lapply(matchData, get), matchTreatments, digits = 2),
                          list(c("\\hline \\\\[-4ex]"),
                               c("Control Mean", round(sapply(matchData, function(x) regData[modelName==x, controlMean]), 2)))),
       column.labels = c(depNamesProCounter), column.separate= rep(1,6), omit.table.layout = "d",
       keep = "Treatment", order = levels(SURVEY$matchTreatment), digits = 2
       , type="latex", replaceCov = replaceWords, file = starMatchFile)
addLinePrint(starMatch, starMatchFile, count = 3)

# Effect on pro/counter potential outlets, Poisson
starMatchPoisson = stargazerR(potPro_FB_FD_RegPoisson, potPro_Nav_FD_RegPoisson, potPro_Shared_FD_RegPoisson,   
                        potCounter_FB_FD_RegPoisson, potCounter_Nav_FD_RegPoisson, potCounter_Shared_FD_RegPoisson,
           column.labels = c(depNamesProCounterPoisson), column.separate= rep(1,6), omit.table.layout = "d",
           add.lines = list(c("Pro-Att. exponentiated", 
                              round(exp(sapply(matchDataPoisson, function(x) {getCoef(get(x), "matchTreatmentPro")} )), 2)),
                            c("Counter-Att. exponentiated", 
                               round(exp(sapply(matchDataPoisson, function(x) {getCoef(get(x), "matchTreatmentCounter")} )), 2))),
           keep = "Treatment", order = levels(SURVEY$matchTreatment), digits = 2,  column.sep.width = "0pt"
, type="latex", replaceCov = gsub("Treatment", "Treat.", replaceWords), file = starMatchFilePoisson)
addLinePrint(starMatchPoisson, starMatchFilePoisson, count = 3)

# Effect on pro/counter shared outlets (only for slides)
starPostsFile = "Output/Tables/Shared/sharedWithEmpty.tex"
postsData = sort(regData[version=="Primary" & dvShort %in% c("potPro", "potCounter") & 
                           family %in% c("Shared", "SharedEmpty"), modelName])
starPosts = stargazerR(potPro_Shared_Primary_Reg, potPro_SharedEmpty_Primary_Reg,
                       potCounter_Shared_Primary_Reg, potCounter_SharedEmpty_Primary_Reg,
                       add.lines = append(diffFValueAddRow("Pro Treat - Counter Treat", lapply(postsData, get), matchTreatments, digits = 2),
                                          list(c("\\hline \\\\[-4ex]"),
                                               c("Control Mean", round(sapply(postsData, function(x) regData[modelName==x, controlMean]), 3)))),
                       column.labels = depNamesSharedProCounter, column.separate = rep(1,4), digits = 2,
                       keep = "reatment", order = levels(SURVEY$matchTreatment)
           , type="latex", replaceCov = replaceWords, file = starPostsFile) 
addLinePrint(starPosts, starPostsFile, 3)


# .Lib vs Con (only for slides) ----
# .........................................................................

starTreatFile = "Output/Tables/Combined/allResultsMediaTreat.tex"
treatData = c("potLib_Like_FD_Reg",  "potLib_FB_FD_Reg",  "potLib_Nav_FD_Reg", "potCons_Like_FD_Reg", "potCons_FB_FD_Reg", "potCons_Nav_FD_Reg")

starTreat = stargazerR(potLib_Like_FD_Reg,  potLib_FB_FD_Reg, potLib_Nav_FD_Reg, 
                       potCons_Like_FD_Reg, potCons_FB_FD_Reg, potCons_Nav_FD_Reg,
                       add.lines = append(diffFValueAddRow("Conservative Treat - Liberal Treat", lapply(treatData, get), 
                                                           treatments, digits = 2),
                                          list(c("\\hline \\\\[-4ex]"),
                                               c("Control Mean", round(sapply(treatData, function(x) regData[modelName==x, controlMean]), 3)))),
                       column.labels = c(depNamesLibCons), column.separate= rep(1,6), omit.table.layout = "d",
                       keep ="ment", order=levels(SURVEY$treatment), digits=2
                       , type="latex", replaceCov = replaceWords, file = starTreatFile) 

# Add line in title row
addLinePrint(starTreat, starTreatFile, 3)


# .By subgroup (only for slides) ----
# .........................................................................

group4Data = gsub("FD", "Group4", treatData)
starGroupFile = "Output/Tables/Combined/allResultsMediaSubgroup.tex"

starGroup = stargazerR(potLib_Like_Group4_Reg, potLib_FB_Group4_Reg, potLib_Nav_Group4_Reg,
                       potCons_Like_Group4_Reg, potCons_FB_Group4_Reg, potCons_Nav_Group4_Reg,
           column.labels = c(depNamesLibCons[1:3],depNamesLibCons[5:7]), column.separate= rep(1,6), omit.table.layout = "d", 
           keep ="treat", order=c("treatLIdeoL", "treatLIdeoC", "treatCIdeoL", "treatCIdeoC"), digits=2
      , type="latex", replaceCov = replaceWords, file = starGroupFile) 

addLinePrint(starGroup, starGroupFile, 3)



# *************************************************************************
# DESC-STAT ----
# *************************************************************************

# .sample size ----
# .........................................................................

extAndPostsSampleSize = FULL_DATA[, .N]
saveToLyx(extAndPostsSampleSize, "extAndPostsSampleSize", digits = 0)

extAndPostsMatchSampleSize = FULL_DATA[!is.na(matchTreatment), .N]
saveToLyx(extAndPostsMatchSampleSize, "extAndPostsMatchSampleSize", digits = 0)


# .likes ----
# .........................................................................

numLikeProTwoWeeksExt = getCoef(potPro_Like_FD_Reg, "matchTreatmentPro")
numLikeCounterTwoWeeksExt = getCoef(potCounter_Like_FD_Reg, "matchTreatmentCounter")
numLikeCounterExt = SURVEY[ResponseId %in% FULL_DATA$ResponseId & matchTreatment=="Counter", 
                           mean(newLikesNum)]

saveToLyx(numLikeProTwoWeeksExt, "numLikeProTwoWeeksExt", digits = 2)
saveToLyx(numLikeCounterTwoWeeksExt, "numLikeCounterTwoWeeksExt", digits = 2)
saveToLyx(numLikeCounterExt, "numLikeCounterExt", digits = 2)

# always takers
controlsNum <- ALL_USER[treatment=="Control", .N]
controlsAlwaysCompliersLibShare <- ALL_USER[treatment=="Control" & potLib_Like>0, .N] / controlsNum
controlsAlwaysCompliersConsShare <- ALL_USER[treatment=="Control" & potCons_Like>0, .N] / controlsNum

saveToLyx(scales::percent(controlsAlwaysCompliersLibShare,accuracy = 0.1), "controlsAlwaysCompliersLibShare")
saveToLyx(scales::percent(controlsAlwaysCompliersConsShare,accuracy = 0.1), "controlsAlwaysCompliersConsShare")


# .exposure ----
# .........................................................................

effectExposurePro = getCoef(potPro_FB_FD_Reg, "matchTreatmentPro")
effectExposureCounter = getCoef(potCounter_FB_FD_Reg, "matchTreatmentCounter")

saveToLyx(effectExposurePro, "effectExposurePro", digits=0)
saveToLyx(effectExposureCounter, "effectExposureCounter", digits=0)


# .browsing ----
# .........................................................................

consumptionCounterTwoWeeks = getCoef(potCounter_Nav_FD_Reg, "matchTreatmentCounter")
consumptionCounterMean = regData[modelName=="potCounter_Nav_FD_Reg", controlMean]
consumptionCounterEffectPct = consumptionCounterTwoWeeks / consumptionCounterMean

saveToLyx(consumptionCounterTwoWeeks, "consumptionCounterTwoWeeks")
saveToLyx(consumptionCounterMean, "consumptionCounterMean")
saveToLyx(consumptionCounterEffectPct, "consumptionCounterEffectPct", percent = TRUE)

consumptionProTwoWeeks = getCoef(potPro_Nav_FD_Reg, "matchTreatmentPro")
consumptionProMean = regData[modelName=="potPro_Nav_FD_Reg", controlMean]
consumptionProEffectPct = consumptionProTwoWeeks / consumptionProMean

saveToLyx(consumptionProTwoWeeks, "consumptionProTwoWeeks")
saveToLyx(consumptionProMean, "consumptionProMean")
saveToLyx(consumptionProEffectPct, "consumptionProEffectPct", percent = TRUE)


# .shared ----
# .........................................................................

sharedCounterTwoWeeks = getCoef(potCounter_Shared_Primary_Reg, "matchTreatmentCounter")
sharedCounterMean = regData[modelName=="potCounter_Shared_Primary_Reg", controlMean]
sharedCounterEffectPct = sharedCounterTwoWeeks / sharedCounterMean

sharedProTwoWeeks = getCoef(potPro_Shared_Primary_Reg, "matchTreatmentPro")
sharedProMean = regData[modelName=="potPro_Shared_Primary_Reg", controlMean]
sharedProEffectPct = sharedProTwoWeeks / sharedProMean

saveToLyx(sharedCounterEffectPct, "sharedCounterEffectPct", percent = TRUE)
saveToLyx(sharedProEffectPct, "sharedProEffectPct", percent = TRUE)



# *************************************************************************
# PLOT ----
# *************************************************************************

# .prepare ----
# .........................................................................

allModels= rbindlist(lapply(regData[version %in% c("FD", "Primary") & regType=="lm", modelName], 
                                  function(x) tidyRob(get(x), x)))

modelsForPlot = as.data.table(tidyr::extract(allModels, modelName, c("dv", "family", "version"), 
                                             "([[:alnum:]]+)_([[:alnum:]]+)_([[:alnum:]]+)", remove=FALSE))
modelsForPlot = merge(modelsForPlot, regData[, list(modelName, controlMean)], by="modelName")

# Only keep effect of treatment
modelsForPlot = modelsForPlot[grepl("treatment|matchTreatment", term)]

modelsForPlot[, treatF := factor(term, levels=termLevels, labels = termLabels)]
modelsForPlot[, treatRev := revFactor(treatF)]
modelsForPlot[, familyF := factor(family, 
     levels = c("Like", 
                "FB", "FBPage", "FBAd", "FBFriend", 
                "Nav",  "NavFromFB",  "NavNotFromFB",  
                "Shared", "SharedEmpty"), 
     labels = c("Subscriptions, number of outlets", 
                "Facebook exposure, posts in feed (control mean in parentheses)", 
                "Facebook exposure, posts shared by pages excluding suspected ads",
                "Facebook exposure, suspected ads", 
                "Facebook exposure, posts shared by friends", 
                "Browsing behavior, visits to news sites (control mean in parentheses)", 
                "Browsing behavior, visits to news sites through Facebook", 
                "Browsing behavior, visits to news sites not through Facebook", 
                "Sharing behavior, posts shared (control mean in parentheses)", 
                "Sharing behavior, posts shared with no commentary"))]

# Effect of pro on pro, counter on counter, etc.
modelsForPlot[, bold := (grepl("Pro", dv) & grepl("Pro", term)) | (grepl("Counter", dv) & grepl("Counter", term)) | 
                (grepl("Cons", dv) & grepl("Cons", term)) | (grepl("Lib", dv) & grepl("Liberal", term))]

modelsForPlot[, dvF := factor(dv, levels = dvLevels, labels = dvLabels)]
modelsForPlot[, dvFWithMean := 
                ifelse(family=="Like", paste0(as.character(dvF),      " Outlets"), 
                    ifelse(grepl("FB", family), paste0(as.character(dvF),    " Posts  \n(", round(controlMean,2), ")"),
                      ifelse(family=="Nav", paste0(as.character(dvF), " Sites   \n(", round(controlMean,2), ")"),
                         ifelse(grepl("Shared", family), paste0(as.character(dvF), " Posts\n(", round(controlMean,2), ")"), NA))))]
modelsForPlot[, dvFWithMean := gsub("Pro-Att\\. ", "Pro-Att.        ", dvFWithMean)]


boldColors = c(1, 0.4)
myGuide <- function(rev=TRUE) (guides(color = guide_legend(reverse=rev, nrow = 1, keyheight=0.1,default.unit="inch"), 
                                      linetype = guide_legend(reverse=rev, nrow = 1, keyheight=0.1,default.unit="inch"), 
                                      shape = guide_legend(reverse=rev, nrow = 1, keyheight=0.1,default.unit="inch"),
                                      alpha = FALSE))
genericTheme =  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "top",
                      legend.spacing.x = unit(0.2, 'cm'), legend.key.width = unit(3, "line"))


# .feed: ads, pages, and friends  ----
# .........................................................................

currentModels = modelsForPlot[grepl("Pro|Counter", dvF) & bold==TRUE &
                                family %in% c("FB", "FBPage", "FBAd", "FBFriend") & version=="FD"]

# Remove parenthesis and explain this refers to all posts in feed
currentModels[, familyF := factor(family, levels = c("FB", "FBPage", "FBAd", "FBFriend"),
                                  labels = c("Facebook exposure, all posts in feed", 
                                  "Facebook exposure, posts shared by pages excluding suspected ads",
                                  "Facebook exposure, suspected ads", 
                                  "Facebook exposure, posts shared by friends"))]

regressionPlot(ggplot(currentModels, aes(x=dvFWithMean, y=estimate, color=treatRev, linetype=treatRev, shape=treatRev))) + 
  scale_color_manual(values = rev(matchColors2)) +
  facet_wrap(~familyF, scales="free_y", nrow = 4) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=12)) +
  myGuide(rev=TRUE) 

ggMySaveBeamer("Output/Graphs/Exposure/ExposureFriendsAdsPages_Beamer.eps", theme = genericTheme, noLinks = TRUE)
ggMySaveDoc("Output/Graphs/Exposure/ExposureFriendsAdsPages.eps", theme = genericTheme)


# .browsing: fromFB/notFromFB  ----
# .........................................................................

currentModels = modelsForPlot[grepl("Pro|Counter", dvF) & bold==TRUE &
                                family %in% c("Nav", "NavFromFB", "NavNotFromFB") & version=="FD"]

# Remove parenthesis and explain this refers to all posts in feed
# Remove parenthesis and explain this refers to all posts in feed
currentModels[, familyF := factor(family, levels = c("Nav",  "NavFromFB",  "NavNotFromFB"),
                                  labels = c("Browsing behavior, all visits to news sites", 
                                             "Browsing behavior, visits to news sites through Facebook", 
                                             "Browsing behavior, visits to news sites not through Facebook"))]

regressionPlot(ggplot(currentModels, aes(x=dvFWithMean, y=estimate, color=treatRev, linetype=treatRev, shape=treatRev))) + 
  scale_color_manual(values = rev(matchColors2)) +
  facet_wrap(~familyF, scales="free_y", nrow = 4) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=12)) +
  myGuide(rev=TRUE) + guides(alpha = FALSE) 

ggMySaveBeamer("Output/Graphs/Browsing/NavFromFB_Beamer.eps", theme = genericTheme)
ggMySaveDoc("Output/Graphs/Browsing/NavFromFB.eps", theme = genericTheme)


# .Main Ext Graph - all 4  ----
# .........................................................................

currentModels = modelsForPlot[bold==TRUE & family %in% c("FB", "Like", "Nav", "Shared") & 
                                  version=="FD" & grepl("Pro|Counter", dvF)]
currentModels[, dvFWithMean := gsub("  ", "", dvFWithMean)]
currentModels[, limits := max(estimate)*3.3 * (0.75-(dvF %in% c("Pro-Att.", "Liberal"))), by="familyF"]
dummy = data.table::copy(currentModels)[, estimate := limits]
    
regressionPlot(ggplot(currentModels, aes(x=dvFWithMean, y=estimate, color=treatRev, linetype=treatRev, shape=treatRev))) + 
  scale_color_manual(values = rev(matchColors2)) +
  facet_wrap(~familyF, scales="free",nrow = 4) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=12)) +
  myGuide(rev=TRUE) + guides(alpha = FALSE)  +   
  geom_blank(data = dummy) 

ggMySaveDoc("Output/Graphs/Combined/Figure6_ITT_FullDataMatch.eps", theme = genericTheme, adjustHeight=1.1)


# .Main Ext Gradual for slides ----
# .........................................................................

# Run for treat and match
for (currentType in c("Treat", "Match") ) {
  
  if (currentType=="Match") {
    myColors = rev(matchColors2)
    currentModels = modelsForPlot[grepl("Pro|Counter", dvF) & !grepl("Share",family) & version=="FD"]
  } else {
    myColors = rev(treatColors2)
    currentModels = modelsForPlot[grepl("Liberal|Cons", dvF) & !grepl("Share",family) & version=="FD"]
  }
  currentModels[, limits := max(estimate)*2.4 * (0.73-(dvF %in% c("Pro-Att.", "Liberal"))), by="familyF"]
  
  # Show effect of pro on counter
  for(other in c("WithOther", "")) {
    if (other=="") {
      currentModels = currentModels[bold==TRUE]
    }
    
    # Show only pro
    for (outcome in c("", "NoCounter")) {
      if (outcome=="NoCounter" & (currentType=="Treat" | other=="WithOther")) {
        next;
      }
      
      for (currentFamily in c("Like", "FB", "Nav")) {
        currentModel = currentModels[family %in% c(currentFamily)]
        if (outcome=="NoCounter") {
            myColors = c("white", rev(matchColors2)[2])
            currentModel[dvF=="Counter-Att.", dvFWithMean := ""]
        }
        
        dummy = data.table::copy(currentModel)
        dummy[, estimate := limits]
        
        currentTheme = theme(axis.title.y = element_blank(), axis.title.x = element_blank(), plot.margin = margin(0,5.5,0,5.5),
                             legend.position = "top", panel.border = element_rect(fill = NA, size=0.1), #axis.text.y = element_text(family="mono"),
                             legend.spacing.x = unit(0.2, 'cm'), legend.key.width = unit(3, "line"))
         
        assign(paste0("ITT_", currentFamily, other, outcome, currentType), regressionPlot(ggplot(currentModel, aes(x=dvFWithMean, y=estimate, color=revFactor(treatF), alpha=bold, linetype=revFactor(treatF)))) + 
                   scale_color_manual(values = myColors) +
                   scale_alpha_manual(limits = c(TRUE, FALSE), values = boldColors) +
                   scale_y_continuous(breaks = scales::pretty_breaks(n=12)) +
                   myGuide(rev=TRUE) +
                   facet_wrap(~familyF, scales="free") + 
                   geom_blank(data = dummy))
      }
      
      # Print graphs gradually (only for slides)
      myBlank = ggplot(currentModel, aes(x=dvFWithMean, y=estimate, color=revFactor(treatF), linetype=revFactor(treatF), shape=revFactor(treatF))) +
        geom_blank() + theme_transparent()
      ggarrange(get(paste0("ITT_Like", other, outcome, currentType)) + myThemeBeamer + currentTheme, myBlank, myBlank, nrow = 3, heights = c(1.25,1,1))
      ggMySaveBeamer(paste0("Output/Graphs/Combined/ITT_FullData", other, outcome, currentType, "1_Beamer.eps"), theme=NULL, adjustHeight = 1, device=cairo_ps)
    
      ggarrange(get(paste0("ITT_Like", other, outcome, currentType)) + myThemeBeamer + currentTheme,
                get(paste0("ITT_FB", other, outcome, currentType)) + myThemeBeamer + currentTheme + theme(legend.position="none"), myBlank, nrow = 3, heights = c(1.25,1,1))
      ggMySaveBeamer(paste0("Output/Graphs/Combined/ITT_FullData", other, outcome, currentType, "2_Beamer.eps"), theme=NULL, adjustHeight=1, device=cairo_ps)

      ggarrange(get(paste0("ITT_Like", other, outcome, currentType))  + myThemeBeamer + currentTheme, 
                get(paste0("ITT_FB", other, outcome, currentType)) + myThemeBeamer + currentTheme + theme(legend.position="none"), 
                get(paste0("ITT_Nav", other, outcome, currentType)) + myThemeBeamer + currentTheme + theme(legend.position="none"), nrow = 3, heights = c(1.25,1,1))
      ggMySaveBeamer(paste0("Output/Graphs/Combined/ITT_FullData", other, outcome, currentType, "3_Beamer.eps"), theme=NULL, adjustHeight=1, device=cairo_ps)
    }
  }
}


# .Shared posts ----
# .........................................................................

# Create ITTSharedMatch.png and graduate exposure of figure for slides
for (currentType in c("Treat", "Match") ) {
  if (currentType=="Match") {
    currentColors = rev(matchColors2)
    includeDv = c("potCounter", "potPro")
  } else {
    currentColors = rev(treatColors2)
    includeDv = c("potLib", "potCons") 
  }
  
  for(other in c("WithOther", "")) {
    for (currentFamily in c("Shared", "SharedEmpty", "AllShared")) {
      
      if (currentFamily=="AllShared") {
        currentModel = modelsForPlot[family %in% c("Shared", "SharedEmpty") & dv %in% includeDv & version=="Primary"]
      } else {
        currentModel = modelsForPlot[family==currentFamily & dv %in% includeDv & version=="Primary"]
      }
      if (other=="") {
        currentModel = currentModel[bold==TRUE]
      }
      
      # Remove parenthesis and state that this refers to all posts shared
      currentModel[, familyF := gsub("posts shared \\(.*", "all posts shared", familyF)]
      
      assign(paste0("ITT_", currentFamily), 
             regressionPlot(ggplot(currentModel, aes(x=dvFWithMean, y=estimate, color=treatRev, alpha = bold, linetype = treatRev, shape=treatRev))) + 
               scale_color_manual(values = currentColors) +
               scale_alpha_manual(limits = c(TRUE, FALSE), values = boldColors) +
               scale_y_continuous(breaks = scales::pretty_breaks(n=10), limits = c(-0.05, 0.45)) +
               myGuide(rev=TRUE) + guides(alpha = FALSE) + 
               xlab("Treatment (control mean in parentheses)")+
               facet_wrap(~familyF, scales="free", nrow = 2)) 
      
      currentTheme = theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = margin(0,5.5,0,5.5),
                           legend.position = "top", legend.spacing.x = unit(0.2, 'cm'), legend.key.width = unit(3, "line"),
                           panel.border = element_rect(fill = NA, size=0.1))
    }  
    
    # Print graphs gradually (only for slides)
    myBlank = ggplot(currentModel, aes(x=dvFWithMean, y=estimate, color=revFactor(treatF), linetype=revFactor(treatF))) +
      geom_blank() + theme_transparent()
    ggarrange(ITT_Shared + myThemeBeamer + currentTheme, myBlank, nrow = 2, heights = c(1.2, 1)) 
    ggMySaveBeamer(paste0("Output/Graphs/Shared/ITTShared1", other, currentType, "_Beamer.eps"), theme=NULL, device=cairo_ps)
    ggarrange(ITT_Shared + myThemeBeamer + currentTheme, 
              ITT_SharedEmpty + myThemeBeamer + currentTheme + theme(legend.position = "none"), nrow = 2, heights = c(1.2, 1))
    ggMySaveBeamer(paste0("Output/Graphs/Shared/ITTShared2", other, currentType, "_Beamer.eps"), theme=NULL, device=cairo_ps)
    
    # Main graph
    ITT_AllShared + myThemeDoc + currentTheme + theme(legend.position = "top")
    ggMySaveDoc(paste0("Output/Graphs/Shared/ITTShared", other, currentType, ".eps"), theme=NULL, device=cairo_ps) 
  }
}

