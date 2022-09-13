# Author:Roee Levy, Yale
# Date: Tue Jan 16 22:54:05 2018
# Purpose: Analyze compliance with the intervention 



require(data.table)
library(fst)


rm(list=ls()) #Remove everything
source("Code/Utils/log.R"); beginLogging()
source("Code/Utils/robustReg.R")
source("Code/Utils/commonVars.R")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")
MEDIA <- readRDS("Datasets/Media/media.rds")

USERS_PAGES <- readRDS("Datasets/UsersPages/users_pages.rds")


# Add slant 
TOP500 <- readRDS("Datasets/Media/top500Outlets.rds")
MEDIA = merge(MEDIA, TOP500[, list(pageID, slantBakshy = avg_align)], by="pageID", all.x=TRUE)

# Distribution of likes
SURVEY[treatment!="Control", round(prop.table(table(newLikesNum, matchTreatment), 2), 2)]

# *************************************************************************
# List of outlets  ----
# *************************************************************************

USERS_PAGES = merge(USERS_PAGES, SURVEY[, list(ResponseId, tookFollowup)], by="ResponseId")
SummaryTable = merge(MEDIA[backup>=0, list(outlet, group, slantBakshy, backup, pageID, surveyName)],
                     USERS_PAGES[, list(Potential = sum(potential, na.rm=TRUE), 
                                       Offered = sum(offered, na.rm=TRUE), 
                                       Subscribed = sum(newLike, na.rm=TRUE)), 
                                  by="pageID"], by="pageID", all.x=TRUE)
setorder(SummaryTable, group, -Potential)
SummaryTable = SummaryTable[, list(Outlet = surveyName, Treatment=group, Slant = slantBakshy, Potential, Offered, Subscribed)]
SummaryTable = SummaryTable[Offered>0]

setnames(SummaryTable, "Subscribed", "Sub.")

table = kableExtra::kable(SummaryTable, format = "latex", booktabs = TRUE, linesep = "",
                          align=c('l', 'l', rep('c',5)), col.names = names(SummaryTable),
                          format.args = list(big.mark = ","),
                  digits = c(0, 0, 2, 0, 0, 0, 0)) 
cat(table, file = "Output/Tables/Outlets/OutletsOffered.tex")



# ******************************************************************
# DESCRIPTIVE  -----
# ******************************************************************

# .offer ----
# .........................................................................

sharePrimaryOutlets = SURVEY[tookFollowup==TRUE, mean(primaryOutlets)]
saveToLyx(sharePrimaryOutlets, "sharePrimaryOutlets", percent = TRUE)

shareComplyMatch = mean(SURVEY[matchTreatment=="Pro", comply],  na.rm=TRUE)
shareComplyOpposing = mean(SURVEY[matchTreatment=="Counter", comply],  na.rm=TRUE)
shareComplyConservative = mean(SURVEY[treatment=="Conservative", comply],  na.rm=TRUE)
shareComplyLiberal = mean(SURVEY[treatment=="Liberal", comply],  na.rm=TRUE)

shareComplyMatchFollow = mean(SURVEY[matchTreatment=="Pro" & tookFollowup==TRUE, comply],  na.rm=TRUE)
shareComplyOpposingFollow = mean(SURVEY[matchTreatment=="Counter" & tookFollowup==TRUE, comply],  na.rm=TRUE)

lapply(list("shareComplyMatch", "shareComplyOpposing", "shareComplyConservative", "shareComplyLiberal"), 
       function(x) saveToLyx(get(x), x, percent=TRUE))

# Need for graph probably
saveToLyx(1-shareComplyMatch, "shareNonComplyMatch", percent=TRUE)
saveToLyx(1-shareComplyOpposing, "shareNonComplyOpposing", percent=TRUE)
saveToLyx(1-shareComplyConservative, "shareNonComplyConservative", percent=TRUE)
saveToLyx(1-shareComplyLiberal, "shareNonComplyLiberal", percent=TRUE)


# *************************************************************************
# REGRESSIONS ----
# *************************************************************************

# .user level ----
# .........................................................................


# Regression, comply by treatment*ideology
SURVEY[,complyWithControlZero:= ifelse(treatment=="control", 0, newLikesNum>0)]
analyzeChoiceTreatIdeoFixed   <- felmR(data=SURVEY, complyWithControlZero ~ treatIdeo + genderF +  ageWM + ageSqWM + ageHave | potentialText)


# .user*page ----
# .........................................................................

# Second type of regression at the page level 
USERS_PAGES_OFFERED = merge(USERS_PAGES[offered==1], 
                            SURVEY[, list(ResponseId, ideologyN, genderF, ageWM, ageSqWM, ageHave, 
                                          potentialText, treatIdeo)], by = "ResponseId")

# Distance from outlets
USERS_PAGES_OFFERED[, distance := ifelse(slantKnow, abs(ideologyN-slant), NA)]
USERS_PAGES_OFFERED[, distanceWithDK := ifelse(slantKnow, distance, 0)]
USERS_PAGES_OFFERED[, distanceWithDKS := scale(distanceWithDK)]

# Abs slant 
USERS_PAGES_OFFERED[, absSlantWithDK := ifelse(slantKnow, abs(slant), 0)]
USERS_PAGES_OFFERED[, absSlantWithDKS := scale(absSlantWithDK)]

analyzeChoiceBoth   <- felmR(data=USERS_PAGES_OFFERED, newLike ~ slantKnow +  absSlantWithDKS + distanceWithDKS | 0 | 0 | ResponseId)
analyzeChoiceBothFE   <- felmR(data=USERS_PAGES_OFFERED, newLike ~ slantKnow + absSlantWithDKS + distanceWithDKS + genderF +  ageWM + ageSqWM + ageHave  | potentialText + factor(outlet) | 0 | ResponseId)


stargazerR(analyzeChoiceTreatIdeoFixed, analyzeChoiceBothFE, 
           add.lines = list(c("Controls", "X", "X"),
               c("Observation Unit", c("Ind.","Ind. * Outlet Offered", "X"))),
           keep = c("treat", "distance", "lant", "Heard"), no.space=TRUE
           ,replaceCov = replaceWords, type = "latex",  file="Output/Tables/Likes/Table3_Compliance.tex")
