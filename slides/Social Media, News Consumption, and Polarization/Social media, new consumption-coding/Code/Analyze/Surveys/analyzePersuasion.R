# Author: Ro'ee Levy, Yale
# Date: Fri Sep 07 09:57:54 2018
# Purpose: Analyze the effects on the political opinions outcomes
# --------------#


require(data.table)
require(dplyr)
require(fst)

rm(list=ls()) #Remove everything 
source("Code/Utils/log.R"); beginLogging()

source("Code/Utils/robustReg.R")
source("Code/Utils/commonVars.R")

SURVEY <- readRDS(file="Datasets/Survey/survey.rds")


# *************************************************************************
# REGRESSIONS  ----
# *************************************************************************

# Basic regression
regPersNotAdj = lmR(data=SURVEY, formula = persIndex ~  treatment )
regPersBasicControl = lmR(data=SURVEY, formula = reformulate(controlsBasic, "persIndex"))
regPers = lmR(data=SURVEY, formula = reformulate(controlsPersWithBase, "persIndex"))

# Diff attrition 
regPersWithoutLastControl = lmR(data=SURVEY[F_withoutLastControl==TRUE], formula = reformulate(controlsPersWithBase, "persIndex"))

# Regression with controls and when exlcuding the respondents recruited last
stargazerR(regPersNotAdj, regPersBasicControl, regPers, regPersWithoutLastControl,
           order = c("treatmentConservative"),
           add.lines = append(diffFValueAddRow("Conservative - Lib. Treatment", 
                  list(regPersNotAdj, regPersBasicControl, regPers, regPersWithoutLastControl), treatments),
                  list(c("Common Controls", " ", "X", "X", "X"),
                  c("Baseline Political Opinions Controls", "", "", "X", "X"),
                  c("Ex. Last Control Group Responders", "", "", "", "X"))),
           keep = c("treatment*", "ideologyNSWM"), omit.table.layout="n"
 , replaceCov = replaceWords ,type = "latex", file = "Output/Tables/PoliticalOpinions/politicalOpinionControls.tex") 




# *************************************************************************
# PLOT ----
# *************************************************************************

# .Ind outcomes  ----
# .........................................................................

for(i in persuasionVars_ScaleS) {
  assign(paste0("regIndNoCG", i), lmR(data=SURVEY[treatment!="Control"], formula = reformulate(controlsPersWithBase, i)))
}

varsReg = paste0("regInd", persuasionVars_ScaleS)



# .index components ----
# .........................................................................

varsRegNoCG = paste0("regIndNoCG", persuasionVars_ScaleS)

mainModels = rbindlist(lapply(varsRegNoCG[1:20],  function(x) tidyRob(get(x))))
modelsForPlot = as.data.table(mainModels)
modelsForPlot = modelsForPlot[grepl("treatment", term)]
modelsForPlot[, dvName := gsub("_ScaleS", "", gsub("F_", "", dvName))]
modelsForPlot[, dvNameF := factor(dvName, levels = 
                            rev(c("favorable_Bolton_Op", "favorable_Clinton_Op", "favorable_Cohen_Op", 
                              "favorable_Daniels_Op", "favorable_Hogg_Op",
                              "favorable_McCabe_Op", "favorable_Mueller_Op", "favorable_Pruitt_Op",
                              "favOrg_FBI_Op", "favOrg_California_Op",
                              "favOrg_Immigrants_Op", "favOrg_March_Op",  "favOrg_NRA_Op", 
                              "approve", "thermo_Trump", 
                              "BN_Obstruct_Op", "investigation_Op", "mccabe_summary",
                              "tradeWar_Op", "gunPolicy_Assault_Op")),
                  labels = rev(c("Favorability: John Bolton", "Favorability: Hillary Clinton", "Favorability: Michael Cohen",  
                             "Favorability: Stormy Daniels", "Favorability: David Hogg",
                             "Favorability: Andrew McCabe", "Favorability: Robert Mueller", "Favorability: Scott Pruitt",
                             "Favorability: The FBI", "Favorability: The Government of California",
                             "Favorability: Illegal/Undocumented Immigrants", "Favorability: March for Our Lives", #\nand National School Walkout", 
                             "Favorability: The NRA",
                             "Approval: Trump", "Feeling Thermometer: Trump",
                             "Believe Obstruction", "Opinion on FBI Investigation", "Reason McCabe Fired",
                             "Trade War Likelihood", "Support Banning Assault Style Weapons")))]

currentTheme = theme(axis.title.y = element_blank(), legend.position = c("none"), panel.border = element_rect(fill=NA),
                     axis.text.y=element_text(hjust=0))

# Create plot with all political opinions outcomes
regressionPlot(ggplot(modelsForPlot, aes(x=reorder(dvNameF, estimate), y=estimate))) + 
  ylab("ITT: Conservative Minus Liberal Treatment") 
ggMySaveBeamer("Output/Graphs/PoliticalOpinions/indPoliticalOpinionsOrder_Beamer.eps", theme=currentTheme, noLinks = TRUE)
ggMySaveDoc("Output/Graphs/PoliticalOpinions/indPoliticalOpinionsOrder.eps", theme=currentTheme)
