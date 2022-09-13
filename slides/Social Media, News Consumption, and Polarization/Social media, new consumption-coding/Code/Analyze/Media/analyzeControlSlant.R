# Author: Ro'ee Levy, Yale
# Date: Wed May 29 21:54:31 2019
# Purpose: Provide descriptive statistics based on the control group
# --------------#

rm(list=ls())
source("Code/Utils/log.R"); beginLogging()
source("Code/Utils/commonVars.R")
source("Code/Utils/segregationFunctions.R")


library(fst)
library(data.table)
library(ggplot2)
library(kableExtra)
library(scales)
options(knitr.table.format = "latex")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")
TOP500 <- readRDS("Datasets/Media/top500Outlets.rds")
SLANT_ALL <- readRDS("Datasets/Extension/slant.rds")


# *************************************************************************
# PREPARE ----
# *************************************************************************

# Add slant
SLANT_ALL = merge(SLANT_ALL, TOP500[, list(domain, alignPct, slant=avg_align, slantGroup)], by="domain")

# Add respondent details, and keep users who installed the extension for at least two weeks
SLANT_ALL = merge(SLANT_ALL, SURVEY[installTwoWeeks==TRUE, list(ResponseId, ideoLeaning, ideologyN, party7, zipReside,
                                        startPOSIX, F_startPOSIX, polAffectiveIndex, control)], 
                  by="ResponseId")


# Slant
SLANT_ALL[, slantNorm := (slant+1)/2]
SLANT_ALL[, slantBinary := ifelse(slantGroup=="Conservative", 1, ifelse(slantGroup=="Liberal", -1, 0))]


SLANT_CONTROL = SLANT_ALL[control==TRUE]
rm(SLANT_ALL)


# *************************************************************************
# CALULATE ----
# *************************************************************************

myTypes = c("Likes", "FB", "FB_Page", "FB_Friend", "FB_Ad", "Nav", "Nav_FB", "Nav_NotFB", "Nav_Friend", "Nav_Page",
            "Nav_Ad", "Shared")

# All results will be saved here
results = expand.grid(currentType = myTypes, 
                      method = c("isolation", "segregation", "absSlant", "congruence", "shareCounter"), 
                      myIdeoVar = c("ideoLeaning"), 
                      myUserAgg = c(T, F), stringsAsFactors = FALSE) %>% as.data.table()


# Calculate all segregation measures
for (i in 1:nrow(results)) {
  if (results[i,method]=="isolation") {
    results[i, value := calculateIsolation(SLANT_CONTROL[type==currentType], byAgg = "domain", indVar = "ResponseId", 
                  ideoVar = myIdeoVar, dateVar = "date", userAgg = myUserAgg)]
  } else if (results[i,method]=="segregation") {
    results[i, value := calculateSegregation(SLANT_CONTROL[type==currentType], byAgg = "domain", indVar = "ResponseId", 
                  slantVar = "slantNorm", userAgg = myUserAgg)]
  } else if (results[i,method]=="shareCounter") {
    results[i, value := calculateShareCounter(SLANT_CONTROL[type==currentType], indVar = "ResponseId", 
                  ideoVar = myIdeoVar, slantBinaryVar = "slantBinary", userAgg = myUserAgg)]
  } else if (results[i,method]=="congruence") {
    results[i, value := calculateCongruence(SLANT_CONTROL[type==currentType], indVar = "ResponseId", 
                  ideoVar = myIdeoVar, slantVar = "slant", userAgg = myUserAgg)]
  }
    else if (results[i, method]=="absSlant") {
      results[i, value := calculateAbsSlant(SLANT_CONTROL[type==currentType], indVar = "ResponseId", 
                                            slantVar = "slant", userAgg = myUserAgg)]
  } 
}


# .Clean Results Table ----
# .........................................................................

results[, roundValue := round(value, digits=3)]



# Share by usage
shareFB = getShare(SLANT_CONTROL[type %in% c("FB_Page", "FB_Friend", "FB_Ad")], var="type")
shareNavFB = getShare(SLANT_CONTROL[type %in% c("Nav_NotFB", "Nav_FB")], var="type")
shareNavFBSource = getShare(SLANT_CONTROL[type %in% c("Nav_Page", "Nav_Friend", "Nav_Ad")], var="type")

shareFBUser = getShare(SLANT_CONTROL[type %in% c("FB_Page", "FB_Friend", "FB_Ad")], var="type", userAgg = T, userVar = "ResponseId")
shareNavFBUser = getShare(SLANT_CONTROL[type %in% c("Nav_NotFB", "Nav_FB")], var="type", userAgg = T, userVar = "ResponseId")
shareNavFBSourceUser = getShare(SLANT_CONTROL[type %in% c("Nav_Page", "Nav_Friend", "Nav_Ad")], var="type", userAgg = T, userVar = "ResponseId")

shareAll = rbind(rbindlist(list(shareFB, shareNavFB, shareNavFBSource))[, myUserAgg := FALSE],
                 rbindlist(list(shareFBUser, shareNavFBUser, shareNavFBSourceUser))[, myUserAgg := TRUE])
setnames(shareAll, "group", "currentType")


# .organize table ----
# .........................................................................

niceResults = dcast(results, currentType+myUserAgg+myIdeoVar~method, value.var = c("value"))

niceResults = merge(shareAll, niceResults, by=c("currentType", "myUserAgg"), all.y=T)
niceResults[, shareChar := ifelse(is.na(share), "", gsub("%", "\\\\%", scales::percent(share, accuracy = 0.1)))]

niceResults[, groupF := factor(currentType, 
                           levels = c("Likes", "FB", "FB_Friend", "FB_Page", "FB_Ad", 
                                      "Nav", "Nav_NotFB", "Nav_FB", "Nav_Friend", "Nav_Page", "Nav_Ad", "Shared"),
                           labels = c("Subscribed", "FB Feed",  "Friends", "Pages", "Ads",
                                      "Browsing", "Not FB", "  FB", " Friends", " Pages", " Ads", "Shared")  )]

# Function to print the segregation table
createSegControlTable <- function (DT, file) {
  DT = DT[order(groupF)]
  DT[, groupF := paste0(.I, ") ", groupF)]
  keepColumns = c("groupF", "shareChar", "segregation", "absSlant", "isolation", "congruence", "shareCounter")
  colNames = c("Category", "Share", "Seg.", "Slant,\nAbs.", "Isol.", "Cong.", "Share\nCounter")
  
  kable(DT[, ..keepColumns], digits=3, booktabs=TRUE, escape = FALSE, 
        align = c("l"), col.names = linebreak(colNames)) %>%
    kableExtra::group_rows(start_row = 2, end_row = 5) %>%
    kableExtra::group_rows(start_row = 6, end_row = 11) %>%
    kableExtra::group_rows(start_row = 12, end_row = 12) %>%
    add_indent(c(3,4,5,7,8,9,10,11)) %>%
    add_indent(c(9,10,11)) %>%
    #myTexPreview(file = FALSE)  
    cat(., file = paste0("Output/Tables/Descriptive/Control/", file, ".tex")) 
}

# Save segregation tables
createSegControlTable(niceResults[myUserAgg==TRUE & myIdeoVar=="ideoLeaning"], "segregationControlUser")
createSegControlTable(niceResults[myUserAgg==FALSE & myIdeoVar=="ideoLeaning"], "segregationControl")
  



# *************************************************************************
# DESCRIPTIVE ----
# *************************************************************************


# Share FB
shareFBExt = niceResults[myUserAgg==TRUE & currentType=="Nav_FB", share][1]
saveToLyx(shareFBExt, "shareFBExt", percent=TRUE)

# .segregation measures ----
# .........................................................................

isolNavNoAgg = niceResults[currentType=="Nav" & myUserAgg==FALSE & myIdeoVar=="ideoLeaning", isolation]
saveToLyx(isolNavNoAgg, "isolNav", digits = 2)

isolNotFB = niceResults[currentType=="Nav_NotFB" & myUserAgg==TRUE & myIdeoVar=="ideoLeaning", isolation]
isolFriend = niceResults[currentType=="Nav_Friend" & myUserAgg==TRUE & myIdeoVar=="ideoLeaning", isolation]
isolPage = niceResults[currentType=="Nav_Page" & myUserAgg==TRUE & myIdeoVar=="ideoLeaning", isolation]

saveToLyx(isolNotFB, "isolNotFB", digits = 2)
saveToLyx(isolFriend, "isolFriend", digits = 2)
saveToLyx(isolPage, "isolPage", digits = 2)

segregationBrowsingExt = niceResults[currentType=="Nav" & myUserAgg==TRUE & myIdeoVar=="ideoLeaning", segregation]

# Compare more directly to Peterson
SLANT_CONTROL[, partyRep := ifelse(party7>0, 1, ifelse(party7<0, -1, NA))]
SLANT_CONTROL[, ideoRep := ifelse(ideologyN>0, 1, ifelse(ideologyN<0, -1, NA))]
segComparePeterson = calculateSegregation(SLANT_CONTROL[type=="Nav"], byAgg = "domain", 
  includeCurrent = TRUE, indVar = "ResponseId", ideoVar = "ideoLeaning", user = TRUE)
calculateSegregation(SLANT_CONTROL[type=="Nav"], byAgg = "domain", includeCurrent = TRUE,
                     indVar = "ResponseId", ideoVar = "partyRep", user = TRUE)
saveToLyx(segregationBrowsingExt, "segregationBrowsingExt", digits = 2)
saveToLyx(segComparePeterson, "segComparePeterson", digits = 2)


# Share of pages excluding ads 
table(SLANT_CONTROL$type)
sharePages = niceResults[currentType=="Nav_Page" & myUserAgg==FALSE & myIdeoVar=="ideoLeaning", share]
saveToLyx(sharePages, "sharePagesOfFBClicks", percent = TRUE)


# *************************************************************************
# PLOT  ----
# *************************************************************************


segregationPlot = niceResults[myIdeoVar=="ideoLeaning" & myUserAgg==TRUE, 
                              list(currentType , segregation, isolation, share, shareChar)]
segregationPlot = melt(segregationPlot, measure.var=c("isolation", "segregation"))

segregationPlot[, typeFacet := ifelse(!grepl("_", currentType), "General", 
                                 ifelse(grepl("FB_", currentType) , "FB", 
                                        ifelse(grepl("Nav_", currentType), "Nav", NA)))]
segregationPlot[, typeFacetF := factor(typeFacet, levels = c("General", "Nav", "FB"),
                                       labels = c("Panel 1: Isolation", "Panel 2: Isolation by\nBrowsing Referral Source",
                                                  "Panel 3:Isolation Within\nFacebook Feed"))]
segregationPlot[, shareFriendPage := 
                  ifelse(currentType %in% c("Nav_Page", "Nav_Friend"), 
                         scales::percent(share*segregationPlot[currentType=="Nav_FB", share]), 
                         gsub("\\\\", "", shareChar))]
segregationPlot[, currentTypeF := factor(currentType, 
       levels = c("FB", "FB_Ad", "FB_Friend", "FB_Page", "Likes",
                 "Nav", "Nav_FB", "Nav_NotFB", "Nav_Friend", "Nav_Page", "Nav_Ad", "Shared"),
       labels = c("Facebook Feed", "Sponsored Posts", "Posts by Friends", "Posts by Pages", "Subscribed Pages", 
                  "Browsing", "Through Facebook\nAll Posts", "Not Through\nFacebook",
                  "Through Facebook\nPosts by Friends", "Through Facebook\nPosts by Pages", "Through Facebook\nSponsored Posts",
                  "Posts Shared"))]

segregationPlot[, type := ifelse(grepl("FB_", currentType), "FB", 
                                 ifelse(grepl("Nav_", currentType), "Nav", currentType))]
segregationPlot[, typeF := factor(type, levels = c("Likes" , "FB", "Nav", "Shared"), 
                                  labels = c("Subscribed", "Facebook Feed", "Browsing", "Shared"))]

ggplot(segregationPlot[variable=="isolation" & currentType!="Nav_Ad"], 
       aes(x=reorder(currentTypeF, value), y=value, alpha=reorder(typeF, -value))) +
  geom_bar(stat="identity", color="black") +
  scale_alpha_manual(values = c(0.15, 0.4, 0.65, 1)) + 
  facet_wrap(~typeFacetF, scales="free_x") +
  labs(y="Isolation") 

currentTheme = theme(legend.position="none", axis.title.x = element_blank(), 
                     axis.text.x = element_text(angle = 90))  
ggMySaveDoc("Output/Graphs/Descriptive/Extension/Figure4_isolationMeasureByMedium.eps", 
            theme=currentTheme, device=cairo_ps, adjustHeight = 0.75)
ggMySaveBeamer("Output/Graphs/Descriptive/Extension/isolationMeasureByMedium_Beamer.eps", 
            theme=currentTheme, device=cairo_ps)

