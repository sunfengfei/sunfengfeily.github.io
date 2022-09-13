# Author:Roee Levy, Yale
# Date: Mon Jan 22 18:23:36 2018
# Purpose: Analyze the content of Facebook posts, and test whether specific news stories were mentioned


library(data.table)
library(dplyr)
library(tidyr)
library(fst)

rm(list=ls())

timeStart = proc.time()


source("Code/Utils/log.R"); beginLogging()

source("Code/Utils/indexFunctions.R")
source("Code/Utils/robustReg.R")
source("Code/Utils/commonVars.R")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")
EXT_FB <- read_fst("Datasets/Extension/facebook/ext_facebook.fst", as.data.table = TRUE,
                   columns = c("ResponseId", "timePOSIX", "Clark", "Farrakhan", "Cohen", "ClintonHusband", "relativeDay"))


# *************************************************************************
# PREPARE ----
# *************************************************************************

maxDays = 7*8

# Include all posts until mid April
#EXT_FB = EXT_FB[as.Date(timePOSIX) <= as.Date("2018-04-15")]
EXT_FB = EXT_FB[relativeDay<maxDays]


# *************************************************************************
# ANALYZE TEXT ----
# *************************************************************************

# Only keep cases where there is content. If user isn't exposed to any content (e.g., see only videos)
# then by definition can't be exposed to these words
EXT_FB = EXT_FB[!is.na(Clark)]

# Find num of posts per expression for each user
sumVars = c("Farrakhan", "Clark", "ClintonHusband", "Cohen")
FB_WORD_USER <- EXT_FB[, lapply(.SD, sum, na.rm=TRUE), by="ResponseId", .SDcols=sumVars]

FB_WORD_USER = merge(FB_WORD_USER, SURVEY, by=c("ResponseId"))


Farrakhan_seen = lmR(data=FB_WORD_USER[,seenFeed:=Farrakhan], reformulate(knowledgeBasic, "seenFeed"))
Cohen_seen = lmR(data=FB_WORD_USER[, seenFeed := Cohen], reformulate(knowledgeBasic, "seenFeed"))
Clark_seen = lmR(data=FB_WORD_USER[, seenFeed := Clark], reformulate(knowledgeBasic, "seenFeed"))
Clinton_seen = lmR(data=FB_WORD_USER[, seenFeed := ClintonHusband], reformulate(knowledgeBasic, "seenFeed"))



# .only extension ----
# .........................................................................

extensionInfoFile = "Output/Tables/Information/informationExtension.tex"
extensionInfo = stargazerR(Cohen_seen, Clark_seen, Farrakhan_seen, Clinton_seen, 
           column.labels = rep(c('\\multirow{2}{1.3 cm}{Michael Cohen}', '\\multirow{2}{1.3 cm}{Clark  Shooting}', 
                                 '\\multirow{2}{1.3 cm}{Louis  Farrakhan}', '\\multirow{2}{1.3 cm}{Clinton Speech}'), 2), 
           add.lines = append(diffFValueAddRow("Cons. Treat - Lib. Treat",  list(Cohen_seen, Clark_seen, Farrakhan_seen, Clinton_seen),
                                        treatments, digits = 2),
                              list(c("Controls", rep("X", 4)),
                                   c("Expected Effect", rep("Lib. Treat", 2), rep("Cons. Treat", 2)))),
           keep = "treatment"
      , replaceCov=replaceWords,  type = "latex", file=extensionInfoFile)
addLinePrint(extensionInfo, extensionInfoFile, count=1)


