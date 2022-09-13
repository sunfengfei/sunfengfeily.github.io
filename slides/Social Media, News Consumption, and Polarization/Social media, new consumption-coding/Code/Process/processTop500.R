# Author:Roee Levy, Yale
# Date: Fri Dec 01 16:48:03 2017
# Purpose: Process dataset of leading news outlets

rm(list=ls())

library(data.table)

source("Code/Utils/log.R"); beginLogging()
source("Code/Utils/commonFunctions.R"); 

MEDIA <- readRDS("Datasets/Media/media.rds")
TOP500 <- readRDS("Datasets/Media/raw/top500Raw.rds")
TOP500_PAGEID <- readRDS("Datasets/Media/raw/top500PageID.rds")

# *************************************************************************
# ADD PAGE_ID ----
# *************************************************************************

TOP500 = merge(TOP500, TOP500_PAGEID, by="domain", all.x=TRUE)


# *************************************************************************
# PROCESS ----
# *************************************************************************

# Keep only slant
TOP500 <- TOP500[, list(domain, pageID, avg_align)]

# Calculate slant percentilve
TOP500[, alignPct := rank(avg_align)/500]

# Remove domains
TOP500 <- TOP500[! domain %in% c("www.youtube.com","www.amazon.com", "vimeo.com", "twitter.com", "www.whitehouse.gov", 
                                 "en.wikipedia.org", "m.youtube.com", "www.barackobama.com")]
TOP500 <- TOP500[! domain %in% c("www.msn.com","www.aol.com")]

# Remove www., after removing I have several duplicates, so remove those as well
TOP500[,domain := gsub("www.","",domain)]
TOP500[domain %in% TOP500[duplicated(TOP500,by="domain")]$domain]
TOP500[domain %in% TOP500[duplicated(TOP500,by="domain")]$domain, avg_align:= mean(avg_align),domain]
TOP500[domain %in% TOP500[duplicated(TOP500,by="domain")]$domain, alignPct:= mean(alignPct),domain]
TOP500 = unique(TOP500)

# Fix wrong pageID manually
TOP500[domain=="conservativetribune.com", pageID := "519305544814653"]

TOP500[pageID=="", pageID:=NA]


# .Define groups ----
# .........................................................................

# Find slant of outlet
TOP500[, slantGroup := ifelse(alignPct <= 0.4, "Liberal", ifelse(0.6 < alignPct, "Conservative", "Moderate"))]
TOP500[, liberal := slantGroup=="Liberal"]
TOP500[, moderate := slantGroup=="Moderate"]
TOP500[, conservative := slantGroup=="Conservative"]

# Verify that TOP500 has all the domains and IDs
allIDs = merge(MEDIA[, list(pageID, domain)],  TOP500[, list(pageID, top500Domain=domain)], all.x=TRUE) 
stopifnot(length(allIDs$domain!=allIDs$top500Domain)>0)


TOP500[pageID=="0", pageID := NA]

setkey(TOP500, domain)
stopifnot(sum(duplicated(TOP500, by=key(TOP500)))==0)


# .set labels ----
# .........................................................................

setattrLabel(TOP500, "domain", "Outlet's domain (key)")
setattrLabel(TOP500, "pageID", "Outlet's Facebook page")
setattrLabel(TOP500, "avg_align", "Slant according to Bakshy et al. (2015)")
setattrLabel(TOP500, "alignPct", "Percentile of slant")
setattrLabel(TOP500, "slantGroup", paste0("Outlet group based on slant (Liberal=below 40th percentile, ",
                                          "Moderate=between 40th and 60th percentile, Conservative=above 60th percentile)"))
setattrLabel(TOP500, "liberal", "Outlet is liberal")
setattrLabel(TOP500, "moderate", "Outlet is moderate")
setattrLabel(TOP500, "conservative", "Outlet is conservative")


attr(TOP500, "description") <- "Dataset of leading news outlets"
attr(TOP500, "source") <- "Bakshy et al. (2015)"
attr(TOP500, "access") <- "Available"

saveRDS(TOP500,file='Datasets/Media/top500Outlets.rds')

