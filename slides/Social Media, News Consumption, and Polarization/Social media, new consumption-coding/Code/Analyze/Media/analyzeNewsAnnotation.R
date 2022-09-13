# Author: Ro'ee Levy, Yale
# Date: Thu May 30 17:12:08 2019
# Purpose: Plot names that were mentioned most often by the news outlets
# --------------#

library(data.table)
library(dplyr)
library(ggplot2)
library(fst)


rm(list=ls())
source("Code/Utils/log.R"); beginLogging()

source("Code/Utils/textAnalysis.R")
source("Code/Utils/commonVars.R")

# Load media
MEDIA <- readRDS("Datasets/Media/media.rds")

# Load news posts
NEWS_POSTS <- readRDS("Datasets/News/newsPosts.rds")
NEWS_POSTS <- merge(NEWS_POSTS, MEDIA[, list(group, backup, pageID, outlet)], by = "pageID")
newsEntities <- readRDS("Datasets/News/newsEntities.rds")


# *************************************************************************
# CLEAN NAMES ----
# *************************************************************************

# Fix classification
newsEntities[entity %in% c("Cambridge Analytica", "Facebook", "Oscar", "MeToo", "Oscars"), entity_type:="ORG"]
newsEntities[entity=="Austin", entity_type:="LOC"]

# Keep only people
newsEntities = newsEntities[entity_type=="PERSON"]

# Add post details 
newsEntities <- merge(newsEntities, 
                      NEWS_POSTS[, list(postID, slant=group, last_updatePOSIX, outlet, backup, message)], 
                      by.x="id", by.y="postID") 

newsEntities[, slant := factor(slant, levels= c("Liberal","Neutral", "Conservative"), 
                               labels = c("Liberal", "Moderate", "Conservative"))]

# Remove names that appear only in one outlet
newsEntities[,count:=.N, entity]
newsEntities[,countOutlets:= length(unique(outlet)), entity]
newsEntities = newsEntities[countOutlets>1]

# Clean and add names
newsEntitiesClean = cleanNews(newsEntities)



# *************************************************************************
# CREATE DATASETS ----
# *************************************************************************

# Only keep four primary and one alternative outlet
keepBackup=1

# .all news ----
# .........................................................................

# Instead of counting words,  count sentences
NEWS_NAMES <-unique(newsEntitiesClean)

# Focus only on primary outlets and major backup
NEWS_NAMES <-  NEWS_NAMES[backup<=keepBackup]


# .descriptive ----
# .........................................................................

# Find share message with names
NEWS_POSTS[, relevantPosts := last_updatePOSIX >= min(NEWS_NAMES$last_updatePOSIX) & 
             last_updatePOSIX <= max(NEWS_NAMES$last_updatePOSIX) & 
             message!="" & !is.na(message) & backup<=keepBackup]

shareMessagesWithNames = NEWS_NAMES[, uniqueN(id)] /   NEWS_POSTS[relevantPosts==TRUE, uniqueN(postID)] 
medianMessageLength = NEWS_POSTS[relevantPosts==TRUE, median(nchar(message))] 

saveToLyx(shareMessagesWithNames, "shareMessagesWithNames", percent=TRUE)
saveToLyx(medianMessageLength, "medianMessageLength", digits = 0)


# *************************************************************************
# PLOTS ----
# *************************************************************************

NAMES = prepareNewsNames(NEWS_NAMES)
setkey(NAMES, "entity", "slant")

# Create "Figure2_NamesPrimaryOutlets.eps"
prepareNewsPlot(NAMES, "Figure2_NamesPrimaryOutlets")

