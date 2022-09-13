# Author: Ro'ee Levy, Yale
# Date: Thu May 30 17:17:09 2019
# Purpose: Analyze effect on endline survey knowledge outcomes
# --------------#



require(data.table)
require(dplyr)
library(fst)


rm(list=ls()) #Remove everything
source("Code/Utils/log.R"); beginLogging()

source("Code/Utils/robustReg.R")
source("Code/Utils/commonVars.R")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")


# *************************************************************************
# INFORMATION  ----
# *************************************************************************

# .Main table ----
# .........................................................................

# Determine correct answer
SURVEY[, F_BN_WallCorrect := ifelse(F_believeNews_Wall==999, 0.5, 1-F_believeNews_Wall)]
SURVEY[, F_BN_TaxCorrect := ifelse(F_believeNews_Tax==999, 0.5, F_believeNews_Tax)]
SURVEY[, F_BN_CriminalTargetCorrect := ifelse(F_believeNews_CriminalTarget==999, 0.5, 1-F_believeNews_CriminalTarget)]
SURVEY[, F_BN_InfluenceCorrect := ifelse(F_believeNews_Influence==999, 0.5, F_believeNews_Influence)]


# Convert to binary outcome
SURVEY[, F_BN_InfluenceCorrectBin := ifelse(F_BN_InfluenceCorrect==0.5, NA, F_BN_InfluenceCorrect==1)]
SURVEY[, F_BN_WallCorrectBin := ifelse(F_BN_WallCorrect==0.5, NA, F_BN_WallCorrect==1)]
SURVEY[, F_BN_CriminalTargetCorrectBin := ifelse(F_BN_CriminalTargetCorrect==0.5, NA, F_BN_CriminalTargetCorrect==1)]
SURVEY[, F_BN_TaxCorrectBin := ifelse(F_BN_TaxCorrect==0.5, NA, F_BN_TaxCorrect==1)]

SURVEY[, F_HN_ClarkBin := F_heardNews_Clark>=1]
SURVEY[, F_HN_ClintonBin := F_heardNews_Clinton>=1]



outcomes = as.data.table(list(dv = c("F_favorable_Cohen_Know", "F_HN_ClarkBin", "F_favorable_Farrakhan_Know", "F_HN_ClintonBin",
                                      "F_BN_InfluenceCorrectBin", "F_BN_WallCorrectBin", "F_BN_CriminalTargetCorrectBin", "F_BN_TaxCorrectBin"),
                              names = c("Heard Michael Cohen", "Heard Clark Shooting", "Heard Louis Farrakhan", "Heard Clinton Speech",
                                        "Correct Russian Influence", "Correct Wall Built", "Correct Trump Target", " Correct Tax Cut")))
outcomes[, shortName := paste0("reg", gsub("(_Know|Bin|CorrectBin)", "", gsub(".*_", "", dv)))]

for (i in 1:nrow(outcomes)) {
  assign(outcomes[i, shortName],lmR(data=SURVEY, formula = reformulate(knowledgeBasic , outcomes[i, dv])), pos=globalenv())
  assign(outcomes[i, paste0(shortName, "Ext")],lmR(data=SURVEY[installTwoWeeks==TRUE],
                                       formula = reformulate(knowledgeBasic , outcomes[i, dv])), pos=globalenv())
}



fileKnow =  "Output/Tables/Information/allPrimaryInformation.tex"
knowReg = stargazerR(regKnow, regClark, regKnow, regClinton, 
                     regInfluence, regWall, regCriminalTarget, regTax,
           dep.var.labels = paste0(paste0("\\multirow{2}{1.6 cm}{", outcomes[, names]), "}"),
           dep.var.labels.include = TRUE,
           add.lines = append(diffFValueAddRow("Cons. Treat - Lib. Treat",  lapply(outcomes[, shortName], get),
                                              treatments, digits = 2),
                                        list(  #c("\\hline \\\\[-4ex]"),
                                               #c("Control Mean", unlist(outcomes[, controlMean])), 
                                              c("Controls", rep("X", 8)), 
                                              c("Expected Effect", rep(c(rep("Lib Treat", 2), rep("Cons Treat", 2)), 2)))),
           keep = "treatment"
 , replaceCov=replaceWords, type = "latex", file = fileKnow)

addLinePrint(knowReg, fileKnow, count=2)


# .Only extension data (not in paper) ----
# .........................................................................

# Robustness Ext
stargazerR(regKnowExt, regClarkExt, regKnowExt, regClintonExt,
           regInfluenceExt, regWallExt, regCriminalTargetExt, regTaxExt,
           dep.var.labels = paste0(paste0("\\multirow{2}{1.6 cm}{", outcomes[, paste0(shortName, "Ext")]), "}"),
           dep.var.labels.include = TRUE,
           add.lines = append(diffFValueAddRow("Cons. Treat - Lib. Treat",  lapply(outcomes[, paste0(shortName, "Ext")], get),
                                               treatments, digits = 2),
                              list(  #c("\\hline \\\\[-4ex]"),
                                c("Control", rep("X", 4)),
                                c("Expected Effect", rep(c(rep("Lib Treat", 2), rep("Cons Treat", 2)), 2)))),
           keep = "treatment")




