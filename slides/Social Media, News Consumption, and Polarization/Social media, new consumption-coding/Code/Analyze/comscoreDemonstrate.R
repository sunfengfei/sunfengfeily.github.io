# Author: Ro'ee Levy, Yale
# Date: Wed May 29 21:29:49 2019
# Purpose: Plot news consumption descriptive statistics based on Comscore data

# --------------#

library(data.table)
library(ggplot2)
library(fst)

rm(list=ls())
source("Code/Utils/log.R"); beginLogging()
source("Code/Utils/robustReg.R")
source("Code/Utils/commonVars.R")

TOP500 <- readRDS("Datasets/Media/top500Outlets.rds")
csNews <- readRDS("Datasets/Comscore/comScoreNews.rds")


csNews = csNews[year %in% c(2017, 2018)]

# Add slant
csNews = merge(csNews, TOP500[, list(domain, slant = avg_align, slantPct = alignPct)], by="domain")


# *************************************************************************
# AGGREGATE ----
# *************************************************************************

csNews[, slantScale := scale(slant)]
csNews[, absSlant := abs(slant)]
csNews[, absSlantScale := scale(abs(slant))]

# .Aggregate at user Level for graphs----
# Each observation user and reference
# .........................................................................

# Count number of posts per reference and user, used to filter users later
csNews[, refFacebook := ref_domain=="facebook.com"]
csNews[, countFBNews := sum(refFacebook), by="machine_id"]
csNews[, countOtherNews := sum(!refFacebook), by="machine_id"]

# Limit to cases where visited at least two sites. Can change these to >=, graphs stay similar
includeFBMachineId = unique(csNews[countFBNews>1 & countOtherNews>1, machine_id])

# Calculate slant and number of post by references and user
csNewsUserFB = csNews[machine_id %in% includeFBMachineId,
                        list(userSlant = mean(slant), userAbsSlant = mean(absSlant), count = .N), 
                        by=c("machine_id", "refFacebook", "shareRepDonations_1618")]

# Add ideology
#csNewsUserFB = merge(csNewsUserFB, csDemo, by="machine_id")
csNewsUserFB[, slantScale := scale(userSlant)]

# With three categories 
csNewsUserFBAllConsumption = csNews[machine_id %in% includeFBMachineId,
                                    list(userSlant = mean(slant), userAbsSlant = mean(absSlant), variableName="All Online News"), 
                                    by=c("machine_id",  "shareRepDonations_1618")]
#csNewsUserFBAllConsumption = merge(csNewsUserFBAllConsumption, csDemo, by="machine_id")
csNewsUserFBAllConsumption = rbindlist(list(csNewsUserFBAllConsumption, csNewsUserFB), use.names = T, fill = T)
csNewsUserFBAllConsumption[, slantScale := scale(userSlant)]


# *************************************************************************
# PLOTS ----
# *************************************************************************

# .Prepare ----
# .........................................................................

# Prepare plots
csNews[, variableName := as.factor(ifelse(!refFacebook, "Other News", "Facebook References"))]

csNewsUserFB[, variableName := as.factor(ifelse(refFacebook, "Facebook References", "Other News"))]
csNewsUserFB[, variableName := relevel(variableName, "Facebook References")]

csNewsUserFBAllConsumption[is.na(variableName), variableName := (ifelse(refFacebook, "Facebook References", "Other News"))]
csNewsUserFBAllConsumption[, variableName := factor(variableName, 
                                                    levels = c("Facebook References", "Other News", "All Online News"))]




# .User Distribution ----
# .........................................................................

shortName = list(domain = c("salon.com", "huffingtonpost.com", "nytimes.com", "latimes.com", "bostonglobe.com",
                            "washingtonpost.com", "usatoday.com", "cnn.com", "reuters.com", "news.yahoo.com",
                            "wsj.com", "foxnews.com", "breitbart.com"), 
                 shortName = c("Salon", "HuffPost", "NYT", "LA Times", "Boston Globe", 
                               "Washington Post", "USA Today", "CNN", "Reuters", "Yahoo News",
                               "WSJ", "Fox News", "Breitbart")) %>% as.data.table()
TOP500 = merge(TOP500, shortName, by="domain", all.x=TRUE)

includeNames = c("Salon", "HuffPost", "NYT", "Boston Globe", "Washington Post", 
                 "USA Today", "Yahoo News", "WSJ", "Fox News", "Breitbart")

userDist <- function (DT, fontSize = 2.5) {
  # NPR next to HuffPost
  ggplot(DT, aes(x = userSlant, color = variableName, linetype = variableName)) + 
    stat_density(geom = "line", bw=0.05, position="identity")+
    labs(x="Slant", y="Density") +
    scale_linetype_manual(values = c(1, 2, 3)) +
    scale_color_manual(values = c("blue", "black", "grey")) +
    scale_y_continuous(expand = c(0.01, 0)) + 
    ggplot2::annotate("text", y=0, angle=90, hjust=0, color="grey17", size=fontSize,
        x=TOP500[shortName %in%  includeNames, avg_align], label = TOP500[shortName %in%  includeNames, shortName])
}

curTheme =  theme(legend.spacing.x = unit(0.2, 'cm'), legend.key.width = unit(2, "line")) 

userDist(csNewsUserFB, fontSize = 2.5)
ggMySaveBeamer("Output/Graphs/Descriptive/Comscore/comScoreSegregation_Beamer.eps", theme=curTheme)


# With all
userDist(csNewsUserFBAllConsumption, fontSize = 3) #+ guides(color=guide_legend(nrow=2), line=guide_legend(nrow=2)
ggMySaveDoc("Output/Graphs/Descriptive/Comscore/Figure5a_comScoreSegregation.eps", theme=curTheme, 
            adjustHeight = 0.7, adjustWidth = 0.85)



# .User Outlet Match ----
# .........................................................................

userIdeoScatter <- function (DT, ideoMeaure, xAxis) {
  
  # Verify that same  users and can use the same scatter plot
  stopifnot(DT[variableName=="Other News", mean(get(ideoMeaure), na.rm=TRUE)]==
              DT[grepl("References", variableName), mean(get(ideoMeaure), na.rm=TRUE)])
  
  # First creating list of users and then defining quantiles, otherwsie some users randomly split
  # to two quantiles at cutoff
  uniqueUsers = unique(DT[, list(machine_id, ideo = get(ideoMeaure))])
  uniqueUsers[, quantileIdeo := ntile(ideo, 15)]
  mergedForGraph = merge(DT, uniqueUsers[, list(machine_id, quantileIdeo)], by="machine_id")
  mergedForGraph[, ideo := get(ideoMeaure)]
  
  sumForGraph = mergedForGraph[!is.na(ideo), lapply(.SD, mean, na.rm=TRUE),
                                 keyby=c("quantileIdeo", "variableName"), .SDcols = c("slantScale", "userSlant", "ideo")]
  ggplot(sumForGraph, aes(x = ideo, y=slantScale, color = variableName, linetype = variableName, shape = variableName)) +   geom_point() +
    geom_smooth(data=sumForGraph, method = "lm", se = FALSE, na.rm=TRUE)  +
    labs(x=xAxis, y="Mean News Slant, Std. Dev.\n(Higher=More Conservative)") +
    scale_color_manual(values = c("blue", "black", "gray")) + 
    scale_x_continuous(breaks = seq(0.1, 0.9, 0.1))
}



# Include all online news
userIdeoScatter(csNewsUserFBAllConsumption, ideoMeaure = "shareRepDonations_1618", xAxis = "Republican Donations Share")
ggMySaveDoc("Output/Graphs/Descriptive/Comscore/Figure5b_comScoreIdeologyDonations.eps", theme=curTheme, 
            adjustHeight = 0.75, adjustWidth = 0.95)

curThemeBeamer =  theme(legend.position = c(0.8,0.2), plot.margin = margin(0,2,0,0), 
                        legend.spacing.x = unit(0.2, 'cm'), legend.key.width = unit(1.75, "line")) 
ggMySaveBeamer("Output/Graphs/Descriptive/Comscore/comScoreIdeologyAndSlant_Beamer.eps", theme=curThemeBeamer, adjustHeight = 0.95)



# .Site Level ----
# .........................................................................

ggplot(csNews, aes(x = slant, color = variableName, linetype = variableName)) + 
  stat_density(geom = "line", bw=0.05, position="identity")+
  ggplot2::annotate("text", y=0, angle=90, hjust=0, color="grey17", size = 2.5,
                    x=c(-0.8753, -0.6176, -0.547, -0.4, -0.0635, 0.0945, 0.2754, 0.7754, 0.9136),
                    label = c("Salon", "Huff Post", "NYT", "LA Times", "USA Today",  "Reuters", "WSJ", "Fox News", "Breitbart")) +
  labs(x="Slant", y="Density") +
  scale_linetype_manual(values = c(2, 1)) +
  scale_color_manual(values = c("black", "blue")) +
  scale_y_continuous(expand = c(0.01, 0)) 
ggMySaveBeamer("Output/Graphs/Descriptive/Comscore/comScoreSegregationSiteLevel_Beamer.eps")


# *************************************************************************
# NUMBER FOR INTUION ----
# *************************************************************************

csNewsUserFBExtreme = csNews[machine_id %in% includeFBMachineId, 
                                 list(meanSlant = mean(slant),
                                      countNews = .N, 
                                      countVeryLiberal=sum(slantPct<0.2,na.rm=TRUE),
                                      countVeryConservative=sum(slantPct>0.8,na.rm=TRUE)), 
                                 by=c("machine_id", "refFacebook")]

TOP500[domain=="washingtonpost.com"]
TOP500[domain=="wsj.com"]

csNewsUserFBExtreme[, extremePostWSJ := meanSlant > TOP500[domain=="wsj.com", avg_align] | 
                      meanSlant < TOP500[domain=="washingtonpost.com", avg_align]]
extremePostWSJNotFB = csNewsUserFBExtreme[refFacebook==FALSE, mean(extremePostWSJ)]
extremePostWSJFB = csNewsUserFBExtreme[refFacebook==TRUE, mean(extremePostWSJ)]


csNewsUserFBExtreme[, extreme2575 := meanSlant >  TOP500[alignPct==TOP500[0.75<=alignPct, min(alignPct)], avg_align] |
                      meanSlant < TOP500[alignPct==TOP500[alignPct<=0.25, max(alignPct)], avg_align]]
extremePostQuarterNotFB = csNewsUserFBExtreme[refFacebook==FALSE, mean(extreme2575)]
extremePostQuarterFB = csNewsUserFBExtreme[refFacebook==TRUE, mean(extreme2575)]

saveToLyx(extremePostWSJNotFB, "extremePostWSJNotFB", percent = TRUE)
saveToLyx(extremePostWSJFB, "extremePostWSJFB", percent = TRUE)
saveToLyx(extremePostQuarterNotFB, "extremePostQuarterNotFB", percent = TRUE)
saveToLyx(extremePostQuarterFB, "extremePostQuarterFB", percent = TRUE)




