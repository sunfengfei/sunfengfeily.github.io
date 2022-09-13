# Author: Ro'ee Levy, Yale
# Date: Tue Feb 11 14:21:49 2020
# Purpose: Process Comscore data and save one dataset for all news sites across years
# --------------#

library(fst)
library(data.table)
rm(list=ls())

source("Code/Utils/log.R"); beginLogging()
source("Code/Utils/commonFunctions.R")



TOP500 <- readRDS("Datasets/Media/top500Outlets.rds")

# Find channel
processCSNewsChannel <- function(csNews) {
  
  # Search: assume "yahoo.com" search, could have other uses
  csNews[, channel := ""]
  csNews[ref_domain %in% c("google.com", "bing.com", "ask.com", "googlesyndication.com",
                           "duckduckgo.com", "searchlock.com", "searchencrypt.com", "search.com", "safesear.ch",
                           "searchincognito.com", "myprivatesearch.com", "searchprivacy.co", "netfind.com",
                           "yahoo.com"), 
         channel := "Search"]
  csNews[grepl("google\\.", ref_domain), channel := "Search"]
  
  # Social: include Live.com  (email)
  csNews[ref_domain %in% c("facebook.com", "t.co", "twitter.com", "reddit.com", "pinterest.com", 
                           "linkedin.com", "instagram.com", "tumblr.com",
                           "live.com", "youtube.com"), 
         channel := "Social"]
  
  csNews[ref_domain == "" | ref_domain==domain, channel := "Direct"]
  csNews[channel=="", channel := "Other"]
  
  return(csNews)  
}


# *************************************************************************
# SAVE NEWS ----
# *************************************************************************

for (year in c("07Q1a", "07Q1b", "07Q2a", "07Q2b", "07Q3a", "07Q3b", "07Q4a", "07Q4b", "08a", "08b",  "17", "18")) {
  print(paste0("Processing Comscore: ", year))
  comScore <- read.fst(paste0("Datasets/Comscore/raw/comScore", year, "Raw.fst"), as.data.table=TRUE, 
                                     columns = c("machine_id", "domain_name", "timePOSIX", "ref_domain_name"))
  
  # Keep only news
  csNews = comScore[domain_name %in% TOP500$domain]
  
  rm(comScore); gc();
  
  
  # Save ref_domain as lower case for consistency and simplify names
  setnames(csNews, c("domain_name", "ref_domain_name"), c("domain", "ref_domain")); gc();
  csNews[, ref_domain := tolower(ref_domain)]
  
  csNews[, date := as.Date(timePOSIX)]
  csNews[, timePOSIX := NULL]
  csNews = processCSNewsChannel(csNews)
  
  #write.fst(csNews, paste0("Datasets/Comscore/intermediate/cs", year, "News.fst"))
  assign(paste0("csNews", year), csNews)
  rm(csNews); gc();
}
  

# *************************************************************************
# APPEND ----
# *************************************************************************


# .bind demographics ----
# .........................................................................

load(file = "Datasets/Comscore/raw/csDemo07.Rda")
load(file = "Datasets/Comscore/raw/csDemo08.Rda")
load(file = "Datasets/Comscore/raw/csDemo17.Rda")
load(file = "Datasets/Comscore/raw/csDemo18.Rda")

donations <- readRDS("Datasets/Donations/donationsZip.rds")

csDemo = rbindlist(list(csDemo07[, year:=2007], csDemo08[, year:=2008],
                        csDemo17[, year:=2017], csDemo18[, year:=2018]), use.names = TRUE)

csDemo[, countMachine := .N, by="machine_id"]

# Some cases have multiple zip code. Using last. 
csDemo[, maxYear := max(year), by="machine_id"]
csDemo = csDemo[year==maxYear]

# Add donations
csDemo = merge(csDemo[, list(machine_id, household_income, hoh_most_education, zip_code, racial_background, hoh_oldest_age)], 
               donations, by.x="zip_code", by.y="zip5", all.x=TRUE)


# .bind news, merge news ----
# .........................................................................

csNews07 = rbind(csNews07Q1a, csNews07Q1b, csNews07Q2a, csNews07Q2b, 
                 csNews07Q3a, csNews07Q3b, csNews07Q4a, csNews07Q4b)

csNews08 = rbind(csNews08a, csNews08b)

csNews = rbindlist(list(csNews07[, year:=2007], csNews08[, year:=2008],
                        csNews17[, year:=2017], csNews18[, year:=2018]), use.names = TRUE)
csNews = merge(csNews, csDemo, by=c("machine_id"))


# .remove vars ----
# .........................................................................

keepVars = c("machine_id", "domain", "ref_domain", "date", "year", "zip_code", 
             "channel", "shareRepDonations_1618")

csNews = csNews[,..keepVars]


# .labels ----
# .........................................................................

setattrLabel(csNews, "machine_id", "Identifier of machines (typically individuals) in the dataset")
setattrLabel(csNews, "domain", "Domain visited")
setattrLabel(csNews, "ref_domain", "Referring domain")
setattrLabel(csNews, "date", "Visit date")
setattrLabel(csNews, "year",  "Dataset year")
setattrLabel(csNews, "zip_code", "Zipcode of individual")
setattrLabel(csNews, "channel", "Referring channel: direct, social, search, or other")
setattrLabel(csNews, "shareRepDonations_1618", "Share of republican donations in zipcode (FEC data)")

attr(csNews, "description") <- "Visits to news domains in the Comscore 2007, 2008, 2017, 2018 datasets"
attr(csNews, "source") <- "Comscore WRDS Web Behavior Database Panel"
attr(csNews, "access") <- "Replication only (it is possible to access the Comscore data through WRDS)"

saveRDS(csNews, "Datasets/Comscore/comScoreNews.rds")
