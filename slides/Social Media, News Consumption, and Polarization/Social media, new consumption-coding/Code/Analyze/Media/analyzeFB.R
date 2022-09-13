# Author: Ro'ee Levy, Yale
# Date: Wed Dec 19 17:33:26 2018
# Purpose: Provide summary statistics on exposure to posts observed in the feed
# --------------#


rm(list=ls()); 
source("Code/Utils/log.R"); beginLogging()


library(fst)
library(data.table)
library(dplyr)


source("Code/Utils/robustReg.R")
source("Code/Utils/commonVars.R")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")
TOP500 <- readRDS("Datasets/Media/top500Outlets.rds")

USERS_PAGES <- readRDS("Datasets/UsersPages/users_pages.rds")
EXT_FB_ALL <- read_fst("Datasets/Extension/facebook/ext_facebook.fst", as.data.table=TRUE,
      columns = c("ResponseId", "newsPageID", "finalDomain500", "newsPageURL", "type", "relativeDay", 
                  "timePOSIX", "foundNav", "withinSessionRank", "hasNonFBDomain"))


# *************************************************************************
# PREPARE ----
# *************************************************************************

# Add details on post and treatment
EXT_FB_ALL = merge(EXT_FB_ALL, USERS_PAGES[, list(pageID, ResponseId, newLike)], 
                   by.x=c("newsPageID", "ResponseId"), by.y=c("pageID", "ResponseId"), all.x=TRUE)
EXT_FB_ALL[is.na(newLike), newLike := 0]

EXT_FB_ALL = merge(EXT_FB_ALL, SURVEY[, list(ResponseId, matchTreatment, treatment)], by="ResponseId")

# Only keep posts within two weeks from valid extension users
EXT_FB_ALL_2W = EXT_FB_ALL[relativeDay<14]


# .click by order ----
# .........................................................................

# Group ranks into groups of five
EXT_FB_ALL_2W[, withinSessionRank5 := floor((withinSessionRank-1)/10)+1]

# Save as logical for graph
EXT_FB_ALL_2W[, newLike := as.logical(newLike)]

# Only know if news outlets were visited so focus on those and only relevant if have link
EXT_FB_500_2W = EXT_FB_ALL_2W[(newsPageID %in% TOP500$pageID) & newsPageURL!=""]
EXT_FB_500_2W[is.na(foundNav), foundNav:=FALSE]

# Find share of clicks by length of session
clickSession5NoAdUser = EXT_FB_500_2W[withinSessionRank5<=10 & type!="Ad", 
                                      list(clickedLink=mean(foundNav), count = .N),
                                      by=c("withinSessionRank5", "newLike")]

clickSession5NoAdUser[, Outlets := factor(ifelse(newLike, "Outlets Offered And Liked", "All Other News Outlets"),
                                          levels= c("Outlets Offered And Liked", "All Other News Outlets"))]

# Plot the results
ggplot(clickSession5NoAdUser, aes(x=withinSessionRank5, y=clickedLink, linetype=Outlets, color=Outlets)) +
  geom_line(stat="identity") + 
  geom_point() +
  scale_color_manual(values = c("Purple", "Gray")) +
  labs(y="Share Clicked", x = "Order in Feed") +
  scale_x_continuous(breaks = c(1, 4, 7, 10),
                     labels = c("1-10", "31-40", "61-70", "91-100")) 

currentTheme = theme(legend.position="bottom", legend.title = element_blank())  
ggMySaveBeamer("Output/Graphs/Browsing/shareVisitsByRank_Beamer.eps", theme=currentTheme)  
ggMySaveDoc("Output/Graphs/Browsing/shareVisitsByRank.eps",  theme=currentTheme)



# *************************************************************************
# SUMMARY STAT ----
# *************************************************************************

# .Total number of posts ----
# .........................................................................

numPostsControlPerUser = EXT_FB_ALL_2W[treatment=="Control",.N] / 
  EXT_FB_ALL_2W[treatment=="Control", length(unique(ResponseId))]
numPostsNewsControlPerUser = EXT_FB_ALL_2W[finalDomain500 %in% TOP500$domain & treatment=="Control", .N] / 
  EXT_FB_ALL_2W[treatment=="Control", length(unique(ResponseId))]

saveToLyx(numPostsControlPerUser, "numPostsControlPerUser", digits=0)
saveToLyx(numPostsNewsControlPerUser, "numPostsNewsControlPerUser", digits=0)


# .Share with links ----
# .........................................................................

shareFBPostsWithLinks = EXT_FB_ALL_2W[newLike & hasNonFBDomain, .N] / EXT_FB_ALL_2W[newLike==TRUE, .N]
saveToLyx(shareFBPostsWithLinks, "shareFBPostsWithLinks", percent = TRUE)

