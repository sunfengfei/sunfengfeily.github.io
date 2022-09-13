# Author: Ro'ee Levy, Yale
# Date: Mon Apr 27 19:34:26 2020
# Purpose: Analyze the perceived objective of the baseline survey
# --------------#


library(data.table)
library(quanteda)
library(kableExtra)

rm(list=ls())
source("Code/Utils/log.R"); beginLogging()
source("Code/Utils/commonVars.R"); 
source("Code/Utils/textAnalysis.R"); 

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")

# *************************************************************************
# CLEAN ----
# *************************************************************************

# Lower case, remove non ascii
SURVEY[, objClean := iconv(objective, "", "ASCII", sub="")]
SURVEY[, objClean := tolower(objClean)]


# Some differences between server and PC. Some related to hyphon at end beginning of word. Do not seem important
#ALL[ResponseId=="R_2WPfFe6MWTseu90"][, list(ResponseId, objective, objClean)]


# *************************************************************************
# ANALYZE ----
# *************************************************************************

# Create corpus
myCorpus = corpus(SURVEY[!is.na(matchTreatment) & objClean!="", list(matchTreatment, text = objClean)])

# Create dfm 
dfm1gram <- processCorpus(myCorpus, 1)
dfm1gramStat = processDfm(dfm1gram, groups="matchTreatment")

dfm2gram <- processCorpus(myCorpus, 2)
dfm2gramStat = processDfm(dfm2gram, groups="matchTreatment")

dfm3gram <- processCorpus(myCorpus, 3)
dfm3gramStat = processDfm(dfm3gram, groups="matchTreatment")

dfm4gram <- processCorpus(myCorpus, 4)
dfm4gramStat = processDfm(dfm4gram, groups="matchTreatment")


# *************************************************************************
# PRINT ----
# *************************************************************************

# .most common ----
# .........................................................................

# 3-gram
threeGramTable = dfm3gramStat[rankShare<16,  list(group, featureShare, Rank = rankShare)] %>% 
  dcast(Rank~group, value.var="featureShare", fill="")
kableExtra::kable(threeGramTable, booktabs=TRUE, escape = TRUE,  align = "l", format = "latex") %>% 
  cat(., file = "Output/Tables/AdditionalOutcomes/surveyObjective3gram.tex")

twoGramTable = dfm2gramStat[rankShare<16,  list(group, featureShare, Rank = rankShare)] %>% 
  dcast(Rank~group, value.var="featureShare", fill="")
kableExtra::kable(twoGramTable, booktabs=TRUE, escape = TRUE,  align = "l", format="latex") %>% 
  #myTexPreview(file=FALSE)
  cat(., file = "Output/Tables/AdditionalOutcomes/surveyObjective2gram.tex")



# .most different ----
# .........................................................................

chiSq <- function(dfm) {
  dfmProCounter =  dfm_subset(dfm, matchTreatment!="Control")
  chi2ProCounter = as.data.table(textstat_keyness(dfmProCounter, measure="chi2", correct="none",
                                                target=attributes(dfmProCounter)$docvars[["matchTreatment"]]=="Pro"))
  
  dfmProControl =  dfm_subset(dfm, matchTreatment!="Counter")
  chi2ProControl = as.data.table(textstat_keyness(dfmProControl, measure="chi2", correct="none",
                                                  target=attributes(dfmProControl)$docvars[["matchTreatment"]]=="Pro"))
  
  dfmCounterControl =  dfm_subset(dfm, matchTreatment!="Pro")
  chi2CounterControl = as.data.table(textstat_keyness(dfmCounterControl, measure="chi2", correct="none",
                                                  target=attributes(dfmCounterControl)$docvars[["matchTreatment"]]=="Counter"))
  
  chi2 = merge(chi2ProCounter[, list(feature, chi2ProCounter=chi2)], 
               chi2ProControl[, list(feature, chi2ProControl=chi2)], by="feature", all=TRUE)
  chi2 = merge(chi2, chi2CounterControl[, list(feature, chi2CounterControl=chi2)], by="feature", all=TRUE)
}

# Bind 1, 2, 3, 4 words
allGram = rbind(dfm4gramStat, dfm3gramStat, dfm2gramStat, dfm1gramStat)

# Find differences between treatment
allChi = rbind(chiSq(dfm1gram), chiSq(dfm2gram), chiSq(dfm3gram), chiSq(dfm4gram))
allGram = merge(allGram, allChi, by="feature")

# Each feature one observation
allGramWide = dcast(allGram, feature+chi2ProControl+chi2CounterControl+chi2ProCounter~group, 
                    value.var="niceShareWord", fill = 0)
allGramWide[, rankProControl := rank(-abs(chi2ProControl))]
allGramWide[, rankCounterControl := rank(-abs(chi2CounterControl))]
allGramWide[, rankProCounter := rank(-abs(chi2ProCounter))]
allGramWide[, feature := gsub("_", " ", feature)]

# Print results
kable(allGramWide[rankProControl<=10, list(` `=feature, Control, Pro, Counter)],
      booktabs=TRUE, escape = TRUE,  align = "l", linesep = "", format="latex") %>% 
  add_header_above(c("Expression"=1, "Share Among Phrases with the Same Length"=3)) %>%
  column_spec(column = 1, width = paste0(docWidth*0.4, "cm")) %>% 
  column_spec(column = 2:4, width = paste0(docWidth*0.2, "cm")) %>% 
  cat(., file = "Output/Tables/AdditionalOutcomes/surveyObjectiveProControl.tex")

kable(allGramWide[rankCounterControl<=10, list(Expression=feature, Control, Pro, Counter)],
      booktabs=TRUE, escape = TRUE,  align = "l", linesep = "", format="latex", col.names = NULL) %>%
  column_spec(column = 1, width = paste0(docWidth*0.4, "cm")) %>% 
  column_spec(column = 2:4, width = paste0(docWidth*0.2, "cm")) %>% 
  cat(., file = "Output/Tables/AdditionalOutcomes/surveyObjectiveCounterControl.tex")

kable(allGramWide[rankProCounter<=10, list(Expression=feature, Control, Pro, Counter)],
      booktabs=TRUE, escape = TRUE,  align = "l", linesep = "", format="latex", col.names = NULL) %>%
  column_spec(column = 1, width = paste0(docWidth*0.4, "cm")) %>% 
  column_spec(column = 2:4, width = paste0(docWidth*0.2, "cm")) %>% 
  cat(., file = "Output/Tables/AdditionalOutcomes/surveyObjectiveProCounter.tex")




