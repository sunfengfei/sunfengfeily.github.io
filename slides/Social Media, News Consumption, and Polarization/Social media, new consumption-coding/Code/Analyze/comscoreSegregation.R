# Author: Ro'ee Levy, Yale
# Date: Wed May 29 21:29:49 2019
# Purpose: Create tables providing descriptive statistics on news consumption based on Comscore data

# --------------#

library(data.table)
library(ggplot2)
library(fst)
library(kableExtra)
options(knitr.table.format = "latex")

rm(list=ls())
source("Code/Utils/log.R"); beginLogging()
source("Code/Utils/robustReg.R")
source("Code/Utils/commonVars.R")
source("Code/Utils/segregationFunctions.R")

# *************************************************************************
# LOAD DATA ----
# *************************************************************************

TOP500 <- readRDS("Datasets/Media/top500Outlets.rds")
csNews <- readRDS("Datasets/Comscore/comScoreNews.rds")


# Add slant
csNews = merge(csNews, TOP500[, list(domain, slant = avg_align, slantPct = alignPct)], by="domain")


# *************************************************************************
# PREPARE ----
# *************************************************************************

# Compre 2007-2008 with 2017-2018
csNews[, years := ifelse(year %in% c(2007, 2008), "0708", ifelse(year %in% c(2017, 2018), "1718", NA))]

csNews[, refFacebook := ref_domain=="facebook.com"]
csNews[, countFBNews := sum(refFacebook), by="machine_id"]
csNews[, countOtherNews := sum(!refFacebook), by="machine_id"]
includeFBMachineId = csNews[countFBNews>1 & countOtherNews>1, machine_id]

# Define facebook users for comparison with my data
csNews[, FBUser := machine_id %in% includeFBMachineId]


# .user/outlet match ----
# .........................................................................

csNews[, absSlant := abs(slant)]
csNews[, slantNorm := (slant+1)/2]



# *************************************************************************
# SUMMARY MEASURES ----
# *************************************************************************

csNews[, ref := ifelse(refFacebook, "FB", "Not FB")]
csNews[, All := "All"]

channelCat = unique(csNews$channel)
fbCat = unique(csNews$ref)


results = as.data.table(expand.grid(
  method = c("segregation", "isolation", "absSlant"), 
  group = c("All", channelCat, fbCat),  
  myUserAgg = c(TRUE, FALSE), 
  userGroup = c("All", "FBRef"), 
  myYears = c("0708", "1718"), stringsAsFactors = F))

results[, category := ifelse(group %in% channelCat, "channel", 
                            ifelse(group %in% fbCat, "ref", 
                                   ifelse(group=="All", "All", NA)))]

# Don't need FB user results for 0708 or visit level
results = results[! (userGroup=="FBRef" & (myYears=="0708" | myUserAgg==FALSE))]

# Don't need 0708 if not all or aggregated
results = results[! (myYears=="0708" & (group!="All" | myUserAgg==FALSE))]

results[, myInd := "machine_id"]
results[, myAgg := "domain"]

# To make this run faster don't need to recalculate dataset if only parameters change
results[, differentMethod := .I==1 | myUserAgg!=shift(myUserAgg) | group!=shift(group) |
          userGroup!=shift(userGroup) | myYears!=shift(myYears)]

# Calculate segregation/isolation measures
for (i in 1:nrow(results)) {
  
  # Getting dataset takes a while, only needed when data changes 
  if (results[i, differentMethod]) {
    currentNews = csNews[get(results[i, category])==results[i, group] & years==results[i, myYears]]
    if (results[i, results[i, userGroup]=="FBRef"]) {
      currentNews = currentNews[FBUser==TRUE]
    }
  }
  if (results[i, method]=="segregation") {
    results[i, value := calculateSegregation(currentNews,  byAgg = "domain", indVar = myInd, 
                                             slantVar = "slantNorm", includeCurrent = FALSE, userAgg=myUserAgg)]
  }   else if (results[i, method]=="absSlant") {
    results[i, value := calculateAbsSlant(currentNews, indVar = myInd, userAgg = myUserAgg, 
                                          slantVar = "slant", weightVar=NULL)]
  }
}



# .Add share ----
# .........................................................................


# Find share in each channel for visit and for average user. 
for (i in c("0708", "1718")) {
  currentNews = csNews[years==i]
  for (j in c("All", "FBRef"))  {
    if (j=="FBRef") {
      currentNews = currentNews[FBUser==TRUE]
    }
    shareAll = rbindlist(list(getShare(currentNews, var="ref"), getShare(currentNews, var="channel")), use.names = T)
    shareAllUser = rbindlist(list(getShare(currentNews, userAgg = TRUE, var="ref", userVar = "machine_id"), 
                                getShare(currentNews, userAgg = TRUE, var="channel", userVar = "machine_id")), 
                             use.names = T)
    assign(paste0("shareAllBoth", i, j), 
           rbindlist(list(shareAll[, myUserAgg := FALSE], 
                          shareAllUser[, myUserAgg := TRUE]), use.names = T)[, myYears:=i][, userGroup:=j])
  }
}

shareAllBoth = rbindlist(list(shareAllBoth0708All, shareAllBoth0708FBRef,
                              shareAllBoth1718All, shareAllBoth1718FBRef), use.names = T)

# Find share in each channel when first aggregating at the daily level
csNewsDaily = unique(csNews[years=="1718", list(machine_id, domain, date, ref, channel)])
shareAllDaily = getShare(csNewsDaily, var="ref")


# .cleanResultsTable ----
# .........................................................................

# Merge with share in each format
results[, roundValue := round(value, digits=3)]
niceResults = dcast(results, userGroup + myYears + group + myUserAgg~method, value.var = c("roundValue"))
niceResults = merge(niceResults, shareAllBoth, by=c("group", "myUserAgg", "myYears", "userGroup"), all.x=TRUE)

# Add escape char before %
niceResults[, share := ifelse(is.na(share), "", gsub("%", "\\\\%", scales::percent(share, accuracy = 0.1)))]


niceResults[, groupF := factor(group, 
                               levels = c("All", "Direct", "Social", "Search", "Other", "FB", "Not FB"),
                               labels = c("All Browsing", "Direct", "Social", "Search", "Other", "FB", "Not FB\\phantom{aa}"))]


createSegTableCS <- function (DT, file, details=TRUE, preview=FALSE, keepHeader=TRUE) {
  DT = DT[order(groupF)]
  DT[, groupF := paste0(.I, ") ", groupF)]
  
  # Columns
  keepColumns = c("groupF", "share", "segregation", "absSlant")
  if (keepHeader) {
    colNames = linebreak(c("Category", "Share", "Seg.", "Slant,\nAbs."))
  } else  {
    colNames = NULL
  }
  table = kable(DT[, ..keepColumns], digits=3, booktabs=TRUE, escape = FALSE, 
                align = c("l"), col.names = colNames)
  
  if (details)  {
    table = table %>%
    kableExtra::group_rows(start_row = 2, end_row = 5) %>%
    kableExtra::group_rows(start_row = 6, end_row = 7) %>%
    add_indent(2:7)
  }
  
  if (preview) {  
    myTexPreview(table, file = FALSE)  
  } else {
    cat(table, file = paste0("Output/Tables/Descriptive/Comscore/", file, ".tex")) 
  }
}



# .printTables ----
# .........................................................................

# Main table
createSegTableCS(niceResults[myUserAgg==TRUE & myYears=="1718" & userGroup=="All"], 
                 "segregationComscoreUser")


# Aggregated at visit  level 
createSegTableCS(niceResults[myUserAgg==FALSE  & myYears=="1718" & userGroup=="All"], 
                 "segregationComscore")

# By year
csYears = data.table::copy(niceResults[myUserAgg==TRUE  & group=="All" & userGroup=="All"])
csYears[, groupF := ifelse(myYears=="0708", "All: 2007-2008", 
                          ifelse(myYears=="1718", "All: 2017-2018\\phantom{a}", NA))]
createSegTableCS(csYears, "segregationComscoreYears", details = F, keepHeader = TRUE, preview = FALSE)

# By FB
createSegTableCS(niceResults[myUserAgg==TRUE & myYears=="1718" & userGroup=="FBRef"], 
                 "segregationComscoreFBUser", keepHeader = TRUE)




# *************************************************************************
# DESCRIPTIVE ----
# *************************************************************************


# channels:
socialSomains = csNews[channel=="Social", .N, by="ref_domain"][order(-N)]$ref_domain
socialDomains = paste0(paste0("\"", socialSomains, "\""), collapse = ", ")

searchDomains = csNews[channel=="Search" & !grepl("google", ref_domain), .N, by="ref_domain"][order(-N)]$ref_domain
searchDomains = paste0(paste0("\"", searchDomains, "\""), collapse = ", ")

saveToLyx(searchDomains, "searchDomains")
saveToLyx(socialDomains, "socialDomains")


# sample sizes
newsUserCSTotal = csNews[years=="1718", uniqueN(machine_id)]
usersRefFBAndOther = csNews[years=="1718" & FBUser==TRUE, uniqueN(machine_id)]

saveToLyx(newsUserCSTotal, "newsUserCSTotal", digits = 0)
saveToLyx(usersRefFBAndOther, "usersRefFBAndOther", digits = 0)

# Compare slant 
csNews[domain=="wsj.com", unique(slantNorm)] - csNews[domain=="usatoday.com", unique(slantNorm)]
csNews[domain=="wsj.com", unique(slantNorm)] - csNews[domain=="washingtonpost.com", unique(slantNorm)]


# Share through Facebook
comScoreshareFB = niceResults[myUserAgg==TRUE & group=="FB" & userGroup=="All", 
                              as.numeric(gsub("\\\\%", "", share))/100]
comScoreshareFBAmongFB = niceResults[myUserAgg==TRUE & group=="FB" & userGroup=="FBRef", 
                              as.numeric(gsub("\\\\%", "", share))/100]

saveToLyx(comScoreshareFB, "comScoreshareFB", percent=TRUE)
saveToLyx(comScoreshareFBAmongFB, "comScoreshareFBAmongFB", percent=TRUE)

# Segregation through channels
segSearch = niceResults[myUserAgg==TRUE & group=="Search" & userGroup=="All", segregation]
segSocial = niceResults[myUserAgg==TRUE & group=="Social" & userGroup=="All", segregation]
segDirect = niceResults[myUserAgg==TRUE & group=="Direct" & userGroup=="All", segregation]

saveToLyx(segSearch, "segSearch")
saveToLyx(segSocial, "segSocial")
saveToLyx(segDirect, "segDirect")


# *************************************************************************
# PLOT ----
# *************************************************************************

segregationPlot = niceResults[myUserAgg==TRUE & myYears=="1718" & userGroup=="All"]
segregationPlot[, facet := ifelse(group=="All", 1, ifelse(grepl("FB", group), 3, 2))]
segregationPlot[group=="FB", group := "Facebook"]
segregationPlot[group=="Not FB", group := "Not Facebook"]
ggplot(segregationPlot[group!="All"], aes(x=reorder(group, segregation), y=segregation)) +
  geom_bar(stat="identity", color="black", width=0.3) +
  scale_y_continuous(breaks=c(0, 0.1, 0.2)) +
  facet_wrap(~facet, scales="free_x") +
  labs(y="Segregation", x="Referral Source") 

currentTheme =  theme(strip.background = element_blank(), strip.text.x = element_blank())
ggMySaveDoc("Output/Graphs/Descriptive/Comscore/Figure3_segregationMeasureBySource.eps",  
            theme = currentTheme, device=cairo_ps, adjustHeight = 0.6)


