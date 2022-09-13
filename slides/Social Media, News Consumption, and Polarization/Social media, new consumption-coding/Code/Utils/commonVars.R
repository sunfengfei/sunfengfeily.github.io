# Author: Ro'ee Levy, Yale
# Date: Fri Nov 30 11:15:46 2018
# Purpose: Common vars and basic functions used in many files, mostly for variable names and creation of plots
# --------------#

library(dplyr)
library(data.table)
library(ggplot2)

# *************************************************************************
# VARS ----
# *************************************************************************

# .controls ----
# .........................................................................


# Create controls
controlVars = c("ideologyF", "partyAllF", "trumpF", "genderF", "ageWM", "ageSqWM", "ageHave", "ideoLeaningF", "treatment")
controlPersBaseVars = c("thermo_TrumpWM", "thermo_TrumpHave", "immigrationWorryF", "MuellerFairF", "obstructF")
controlPolBaseVars = c("thermo_DiffWM", "thermo_DiffHave", "empathyDifficult_DiffHave", "empathyDifficult_DiffWM")
controlsKnowledge = c("repF", "follow")

# Political Opinions
controlsBasic = paste0(controlVars, collapse = " + ")
controlsNoTreat = gsub('treatment \\+ ', "", controlsBasic)
controlsPersWithBase = paste0(c(controlVars, controlPersBaseVars), collapse = " + ")

# Polarization
controlsMatch = gsub('treatment', "matchTreatment", controlsBasic)
controlsPolBasic = gsub("treatment", "matchTreatment", controlsBasic) 
controlsPolWithBase = paste(controlsPolBasic, paste0(controlPolBaseVars, collapse = " + "), sep= " + ")
controlsPolWithBase4 = paste0(gsub("matchTreatment", "treatIdeo + ideoLeaningF", controlsPolWithBase), " | potentialText")

knowledgeBasic = paste0(c(controlsBasic, controlsKnowledge), collapse = " + ")


allControlVars = unique(c(controlVars, controlPersBaseVars, controlPolBaseVars, controlsKnowledge, "potentialText"))
treatVars = c("treatment", "matchTreatment", "treatIdeo", "control")


# .outcomes ----
# .........................................................................

persuasionVarsFollow = c("F_approve", "F_favorable_McCabe_Op", "F_favorable_Hogg_Op", "F_favorable_Mueller_Op", "F_favorable_Daniels_Op", 
                         "F_favorable_Bolton_Op", "F_favorable_Pruitt_Op", "F_favorable_Cohen_Op", "F_favorable_Clinton_Op", "F_favOrg_NRA_Op", 
                         "F_favOrg_March_Op", "F_favOrg_California_Op", "F_favOrg_FBI_Op", "F_mccabe_summary", "F_tradeWar_Op","F_gunPolicy_Assault_Op",
                         "F_thermo_Trump", "F_favOrg_Immigrants_Op", "F_investigation_Op", "F_BN_Obstruct_Op")

persuasionSigns = c(1, -1, -1, -1, -1, 
                    1, 1, 1, -1, 1, 
                    -1, -1, -1, 1, -1, -1,
                    1, -1, 1, -1)
testthat::expect_equal(length(persuasionVarsFollow), length(persuasionSigns))

persuasionVars_ScaleS = paste0(persuasionVarsFollow, "_ScaleS")


polVarsAffectiveFollow = c("F_thermo_Diff", "F_empathyDifficult_Diff", "F_empathyImportant_Diff", "F_partyIdeas_Diff", "F_marry_Opposing")
polVarsBehavior = c("F_postMod_Like_Pol", "F_postMod_Share_Pol", "F_postMod_ShareActual_Pol")
polVars = c(polVarsAffectiveFollow, polVarsBehavior)
polVarsScale = paste0(polVars, "Scale")

knowledgeRawVars = 
  c("F_heardNews_Clark", "F_heardNews_Clinton", "F_favorable_Farrakhan_Know", "F_favorable_Cohen_Know",
    "F_believeNews_Wall", "F_believeNews_Tax", "F_believeNews_CriminalTarget", "F_believeNews_Influence")

outcomeVars = c(persuasionVars_ScaleS, "persIndex",
                "persAnderson", "persAndersonNoNeg", "persAndersonWithMissing", "persAndersonNoNegWithMissing",
                polVars, polVarsScale, 
                "polAffectiveIndex", "polIndexBehavior", "polIndexAll", "polAffectiveIndexNoMarry",
                "polAffectiveIndexOwn", "polAffectiveIndexOpposing", 
                "polAffectiveAnderson", "polAffectiveAndersonWithMissing",
                knowledgeRawVars)


# .hetro reg ----
# .........................................................................

# Other optional: "heardAllMed", (similar to seen)


heteroVars = rev(c("absIdeoAboveEqMed", "echoChamberAboveEqMed", "seenCounterAnyShareAboveEqMed",  
                   "openessPersonalityAboveEqMed", "certainAboveEqMed", "crtScoreAboveEqMed",
                   "thermo_DiffAboveEqMed", "mostNewsSM", "countInitialNewsLikesAboveMed", 
                   "meanSeenAboveMed",  "meanDistSlantUnderMed", "followAboveMed",
                   "consIdeology", "ageAboveMed", 
                   "female", "mobile", "politicsAd", "All")) 
heteroVarLabels = rev(c("Ideological", "Echo Chamber", "Seen Counter Att.", 
                        "Open Personality", "Certain", "Sophisticated", 
                        "High Feeling Thermo. Diff.", "Most News Social Media", "High News Subscriptions", 
                        "Exposed to Outlets", "Familiar with Slant", "Follow the News",
                        "Conservative", "Older", 
                        "Female", "Mobile", "Politics Ad", "All")) 
heteroVarsForAll = c("openessPersonalityAboveEqMed", "certainAboveEqMed", "echoChamberAboveEqMed", 
                     "seenCounterAnyShareAboveEqMed", "absIdeoAboveEqMed", 
                     "thermo_DiffAboveEqMed", "mostNewsSM", "countInitialNewsLikesAboveMed", 
                     "meanSeenAboveMed", "meanDistSlantUnderMed", "followAboveMed",
                     "consIdeology", "ageAboveMed", "female")
mainHeteroVars = c(heteroVarsForAll, "crtScoreAboveEqMed")


# .other ----
# .........................................................................

reweightVars =  c("democrat", "republican", "independent",  "ideologyN", "thermo_Diff", "age", "female")
expVars = c("potLib", "potCons", "potPro", "potCounter", "newLike")
mediaVars = c("liberal", "conservative", "moderate")

controlVarsForest = c(
  "follow", "registered", "empathy_Rep", "empathy_Dem", paste0("personality_", 1:10), 
  "immigration", "daca", "immigrationWorry", "thermo_Trump", "thermo_Rep", "thermo_Dem", "thermo_Local",
  "certain", "slant_Rep", "slant_Dem", "voteSupport2016F", "partyAllF", "ideologyF", "trumpF", 
  "ideoLeaningF", "thermo_Diff", "empathyDifficult_Diff", "genderF", "startPOSIX", "initialLikesNum", 
  "age", "ageSq")


# .balanceVars ----
# .........................................................................

balanceBaselineVars = c("ideologyN", "absIdeology",  
                        reweightVars[!reweightVars %in% c("female", "age", "ideologyN", "thermo_Diff")], 
                        "voteOrSupportClinton", "voteOrSupportTrump", 
                        "thermo_Rep", "thermo_Dem", "thermo_Diff", "empathy_Rep", "empathy_Dem", "empathyDifficult_Diff",
                        "follow", "mostNewsSM",  "echoChamber",
                        "certain", "openessPersonality", "seenCounterAnyShare")
balanceDeviceVars = c("mobile")   
balanceFacebookVars = c("female", "age", "initialLikesNum", "countInitialNewsLikes", "meanInitialSlant", 
                 "absInitialSlant", "postsInitialAccess")

balanceVars = c(balanceBaselineVars, balanceDeviceVars, balanceFacebookVars)
attritionVars = c("tookFollowup", "have14DaysPosts", "installTwoWeeks")


# .partOfVarNames ----
# .........................................................................

prefixVar = c("potLib", "potCons", "potPro", "potCounter", "newLike", 
           "liberal", "conservative", "moderate", "count")
prefixVarDesc = c("potential liberal outlets", "potential conservative outlets", "potential pro-attitudinal outlets",
               "potential counter-attitudinal outlets", "outlets liked in the experiment", "all liberals outlet (Bakshy et al., 2015)", 
               "all conservative outlets (Bakshy et al., 2015)", "all moderate outlets (Bakshy et al., 2015)", "all outlets (Bakshy et al., 2015)")


# *************************************************************************
# POLITICAL WORDS ----
# *************************************************************************

polNames = c("trump", "pence", "pelosi", "clinton", "obama", "biden", "mcconnell", "manafort", 
             "kushner", "tillerson", "devos", "mccabe", "bolton", "pruitt", "carson", "michael cohen")
polParties = c("liberal", "conservative", "democrat", "republican", "dnc", "the left", "the right", "gop")
polInst = c("elect", "vote", "white house", "politic", "congress", "senate")
polIssues = c("immigration", "daca", "tax cut", "sanctuary city", "sanctuary state", "gun law", "gun right",
              "school shooting", "parkland", "nra", "gun control", "walkout", "mass shooting", "ar 15")

allPolWords = c(polNames, polParties, polInst, polIssues)
politicsExpression = paste0("(", paste(allPolWords, collapse = "|"), ")" )




# *************************************************************************
# TABLES ----
# *************************************************************************

familyLevels = c("Like", "FB", "Nav", "Shared", "SharedEmpty", "SharedPolitics")
familyLabels = c("Subscriptions, number of outlets", "News exposure, posts in feed", "Browsing behavior, visits to news sites", 
           "Sharing behavior, all posts shared", 
           "Sharing behavior, posts shared with no commentary", 
           "Sharing behavior, Politics")

treatments = c("treatmentConservative", "treatmentLiberal")
matchTreatments = c("matchTreatmentPro", "matchTreatmentCounter")

treatmentLevels = c("treatmentConservative", "treatmentLiberal")
matchTreatmentLevels = c("matchTreatmentPro", "matchTreatmentCounter")
treatmentLabels = c("Conservative Treatment", "Liberal Treatment")
matchTreatmentLabels = c("Pro-Att. Treatment", "Counter-Att. Treatment")

matchLabelsOutlets = values=c("Four Counter-Attitudinal Outlets", "Four Pro-Attitudinal Outlets")

wideToGraph = function(resultsTable) {

  resultsTable = melt(resultsTable, id="matchTreatment")
  resultsTable = tidyr::extract(resultsTable, "variable", into=c("news", "family", "stat"), "([[:alnum:]]+)_([[:alnum:]]+)_([[:alnum:]]+)")
  resultsTable = dcast(resultsTable, matchTreatment + news  ~ stat + family, value.var="value")

  resultsTable =resultsTable[grepl("Day", news)]
  resultsTable[, news := factor(news, labels= matchLabelsOutlets)]
  return(resultsTable)
}


revFactor <- function(data) {return(factor(data, levels = rev(levels(data))))}


REPLACE = data.table(rbindlist( list(
  list("treatmentLiberal:scale(openess)", "Lib. Treatment * Openess"),
  list("treatmentConservative:scale(openess)", "Cons. Treatment * Openess"),
  list("scale(openess)", "Openess"),
  list("treatmentLiberal", "Liberal Treatment"),
  list("treatmentConservative", "Conservative Treatment"),
  list("matchTreatmentPro-Attitudinal Treatment", "Pro-Attitudinal Treatment"),
  list("matchTreatmentCounter-Attitudinal Treatment", "Counter-Attitudinal Treatment"),
  list("matchTreatmentPro", "Pro-Att. Treatment"),
  list("matchTreatmentCounter", "Counter-Att. Treatment"),
  list("treatIdeotreatCIdeoC", "Cons. Treat., Cons. Ideology"),
  list("treatIdeotreatCIdeoL", "Cons. Treat., Lib. Ideology"),
  list("treatIdeotreatLIdeoC", "Lib. Treat., Cons. Ideology"),
  list("treatIdeotreatLIdeoL", "Lib. Treat., Lib. Ideology"),
  list("ideologyNSWM", "Ideology (std.)"),
  list("ideoLeaningFconservativeIdeo", "Conservative Ideology"),
  list("initialLikesNum", "Number of Pages Liked"),
  list("voteClinton", "Vote Clinton"),
  list("ideologyN", "Ideology (-3, +3)"),
  list("thermo_Rep", "Feeling Therm.: Republican"),
  list("thermo_Dem", "Feeling Therm.: Democrat"),
  list("thermo_Diff", "Feeling Therm - Abs. Diff."),
  list("empathyDifficult_Diff", "Pol. Empathy - Abs. Diff. "),
  list("mostNewsSocial", "Most News Social"),
  list("installSuccess", "Install Extension"),
  list("mobile", "Mobile"),
  list("^newLike", "Subscribed"),
  list("potPro", "Pro"),
  list("liberalIdeo", "Liberal Ideology"),
  list("scale(follow)", "Follows News"),
  list("scale(absIdeologyWM)", "Absolute Ideology"),
  list("scale(fbOthersOpWM)", "Echo Chamber"),
  list("scale(openessWM)", "Openess"),
  list("scale(ageWM)", "Age"),
  list("`", ""),
  list("(fit)", ""),
  list("scale(ageSqWM)", "Age Squared"),
  list("offeredWithControl", "Offered"),
  list("newLike14potMatch", "Subscribed * Pro"),
  list("newLike14", "Subscribed"),
  list(":", ' * '),
  list('genderFmale', 'Male'),
  list('ideoLeaningFconservativeIdeo', 'Conservative Ideology'),
  list('refFacebook', 'FB News Ref.'),
  list('shareFB', 'FB Visits Share'),
  list('ZipIdeo', ' * Rep. Vote'),
  list('zipIdeo', 'Rep. Vote'),
  list('shareRefFB', '\\\\% FB News Ref.'),
  list('slant\\_FB', 'Slant of FB Feed'),
  list("shareCounterBet\\_FB ", 'FB Counter-Att. Share'),
  list("congruenceBet\\_FBScaled ", 'FB Congruence Scale, Std. Dev.'),
  list("congruenceBet\\_FB ", 'FB Congruence Scale'),
  list("likeVersion", "Subscriptions"),
  list("likeMatch", "Subscriptions * Pro-Att."),
  list("slantKnow", "Know Slant"),
  list("distanceWithDKS", "Ideological Distance (Std. Dev.)"),
  list("absSlantWithDKS", "Outlet Ideology, Abs. Value (Std. Dev.)"),
  list("countAllCounterBetweenDay\\_NavChangeScaled", "Counter-Att. Web Consumption (std. dev.)"),
  list('^^^', ':')
)))

REPLACE_REG =  data.table(rbindlist( list(
  list(".*shareCounterBet.*FBS[^&]*", 'FB Counter-Att. Share, Std. Dev.'),
  list(".*congruenceBetScaled[^&]*", "FB Congruence Scale, Std. Dev."),
  list(".*slantBetScaled[^&]*", "FB Slant, Std. Dev.")
)))

# From Hmisc
escapeRegex <- function (string) 
{
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", string)
}

# Add line in title row in a generate stargazer file
addLinePrint <- function(reg, file, count=1) {
  addLine = grep("multirow", reg)
  reg = c(reg[1:(addLine)], rep("\\\\", count), reg[(addLine+1):length(reg)])
  cat(reg, file = file, sep = "\n")
}

REPLACE_ALL = rbind(REPLACE[, regEx := FALSE], REPLACE_REG[, regEx := TRUE])
REPLACE_ALL = REPLACE_ALL %>% dplyr::rename(var=V1, newVar=V2) %>% as.data.table()
REPLACE_ALL[, newName := ifelse(regEx, var, escapeRegex(var))]

replaceWords = unlist(REPLACE_ALL[, newVar])
names(replaceWords) = REPLACE_ALL[, newName]


# *************************************************************************
# PLOTS ----
# *************************************************************************

# This is needed for output of some plots
#options(bitmapType='cairo')

matchColors2 = values=c("#009E73", "darkorange")
treatColors2 = values=c("red", "blue")


# .plotLabel ----
# .........................................................................

dvLevels = c("polAffectiveIndexOpposing", "polAffectiveIndexOwn", "polAffectiveIndexNoMarry",
             "potCounter", "potPro", "potCons", "potLib", 
             "slant", "slantScale", "slantExcluding", "slantScaleExcluding",
             "polSlant", "polSlantScale", "polSlantExcluding", "polSlantScaleExcluding")
dvLabels = c("Opposing Party", "Own Party", "Affective Polarization",
             "Counter-Att.", "Pro-Att.", "Conservative", "Liberal", 
              rep("Slant",2), rep("Slant, Excluding Experiment Outlets", 2),
             rep("Congruence Scale",2), rep("Congruence scale, Excluding Experiment Outlets", 2))

termLevels = c("treatmentConservative", "treatmentLiberal", "matchTreatmentPro", "matchTreatmentCounter",
               "treatIdeotreatCIdeoC", "treatIdeotreatLIdeoC", "treatIdeotreatCIdeoL", "treatIdeotreatLIdeoL")
termLabels = c("Conservative Treatment", "Liberal Treatment","Pro-Att. Treatment",  "Counter-Att. Treatment",
               "Cons. Treat., Cons. Ideology", "Lib. Treat., Cons. Ideology", "Cons. Treat., Lib. Ideology", "Lib. Treat., Lib. Ideology")

termLevelsNoCG = c("matchTreatmentCounter", "treatmentLiberal", "matchTreatmentPro", "treatmentConservative")
termLabelsNoCG = c("Counter - Pro Attitudinal Treatment", "Liberal - Conservative Treatment",
                   "Pro - Counter Attitudinal Treatment", "Conservative - Liberal Treatment")



# .plotting ----
# .........................................................................

regressionPlot <- function(plot) {
  return (
    plot+
      geom_errorbar(aes(ymin=conf.low, ymax=conf.high, width=0.1), position = position_dodge(width=0.2)) + 
      geom_point(size=1.5, position = position_dodge(width=0.2)) +
      geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
      coord_flip() 
  )
}

myThemeBase <- theme(plot.margin = ggplot2::margin(0,2,0,2),
                     strip.background =element_rect(fill="lightgrey"),
                     legend.title = element_blank(), legend.margin = ggplot2::margin(0,0,0,0), legend.position = "bottom") 

myThemeBeamer <- theme_classic(base_size = 8) + myThemeBase
  
myThemeDoc <- theme_classic(base_size = 11) + myThemeBase + theme(plot.margin = ggplot2::margin(0,0,4,0))

plotBeamer <- function (theme, plot = last_plot()) {
  plot+myThemeBeamer+theme
}
plotDoc <- function (theme, plot = last_plot()) {
  plot+myThemeDoc+theme
}

  
# .saving ----
# .........................................................................

beamerWidthCMAll = 12.8
beamerHeightCMAll = 9.6

# Height and Width taken from textWidth / textHeight (height adjusted somewhat)
beamerWidth = 10.23 
beamerHeight = 8.24 -0.5 
beamerWidthNM = beamerWidthCMAll -0.6
beamerHeightLinks = beamerHeight-1


# 2.5 inch width margin 
docWidth = 21.6-2.5*2.54

# Half a page
docHeight = (27.9 - 2 -2)/2

ggMySave <- function(file) ggsave(file, plot=last_plot(), height=beamerHeight ,width=beamerWidth, units="cm")
ggMySaveBeamer <- function(file, plot=last_plot(), theme=myThemeBeamer, adjustHeight=1, noLinks=FALSE, dpi=300, device=NULL) {
    currentHeight = adjustHeight*beamerHeightLinks
  if (noLinks==TRUE) {
    currentHeight = adjustHeight*beamerHeight  
  }
  if (!is.null(theme)) {
    plot = plotBeamer(theme, plot)
  }
  ggsave(file, plot, height=currentHeight, width=beamerWidth, units="cm", dpi=dpi, device=device)
}

ggMySaveDoc <- function(file, plot=last_plot(), theme=myThemeDoc, adjustHeight=1, adjustWidth=1, dpi=300, device=NULL) {
  if (!is.null(theme)) {
    plot = plotDoc(theme, plot)
  }
  
  ggsave(file, plot, height=docHeight*adjustHeight, width=docWidth*adjustWidth, units="cm", dpi=dpi, device=device)
}

loadAndDisplay <- function(file) {imager::display(imager::load.image(file))}

# utilLyx includes functions for saving values in Lyx and storing them in a paper, 
#  these functions are not needed to analyze the data this function removes them
if (file.exists("Code/Utils/utilLyx.R")) {
  source("Code/Utils/utilLyx.R")
} else  {
  saveToLyx <- function (...) {}
}
