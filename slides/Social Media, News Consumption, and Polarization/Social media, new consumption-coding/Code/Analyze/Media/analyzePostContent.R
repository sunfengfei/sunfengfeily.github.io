# Author: Ro'ee Levy, Yale
# Date: Wed May 29 21:29:49 2019
# Purpose: Analyze text in posts participants were exposed to in their feed, posts they clicked, and posts they shared

# --------------#

rm(list=ls()) #Remove everything
source("Code/Utils/log.R"); beginLogging()
source("Code/Utils/commonVars.R"); 
source("Code/Utils/textAnalysis.R"); 
source("Code/Utils/utilURL.R")

library(quanteda)
library(data.table)
library(ggplot2)
library(fst)
library(kableExtra)

# Load user details
MEDIA <- readRDS("Datasets/Media/media.rds")
SURVEY <- readRDS(file="Datasets/Survey/survey.rds")

ARTICLES_SUMMARY <-readRDS("Datasets/News/articlesSection.rds")
USERS_PAGES <- readRDS("Datasets/UsersPages/users_pages.rds")

USERS_PAGES = merge(USERS_PAGES, MEDIA[, list(outlet, domain, backup)], by="outlet")




# *************************************************************************
# PREPARE ----
# *************************************************************************

maxDays = 7*8


# .fb ----
# .........................................................................

# Load all FB posts
EXT_FB_ALL <- read_fst("Datasets/Extension/facebook/ext_facebook.fst", as.data.table=TRUE,
                       columns = c("ResponseId", "newsPageContent", "newsPageURL", "newsPageID",
                                   "foundNav", "type", "relativeDay"))
EXT_FB_ALL = EXT_FB_ALL[!is.na(newsPageID)]

# Analyze eight weeks of data
EXT_FB_ALL_INCLUDE = EXT_FB_ALL[relativeDay<maxDays]

EXT_FB_ALL_INCLUDE = merge(EXT_FB_ALL_INCLUDE, USERS_PAGES[, list(pageID, ResponseId, potential, newLike, backup, outlet,
                                                          potLib, potCons)], 
                      by.x=c("newsPageID", "ResponseId"), by.y=c("pageID", "ResponseId"), all.x=TRUE)

EXT_FB_ALL_INCLUDE[potential==TRUE & newLike==TRUE, table(backup<=1)]

shareExposedNotTopFive = EXT_FB_ALL_INCLUDE[potential==TRUE & newLike==TRUE, mean(backup>1)]
saveToLyx(shareExposedNotTopFive, "shareExposedNotTopFive", percent=TRUE)

# Only keep pages liked among first four pages and backup
FB_LIKE = EXT_FB_ALL_INCLUDE[potential==TRUE & newLike==TRUE & backup<=1 & type!="Ad"]

# Only keep cases where have url or text
FB_LIKE = FB_LIKE[newsPageURL!="" | !is.na(newsPageContent)]


# Analyzing by matchTreatment
FB_LIKE = merge(FB_LIKE, SURVEY[!is.na(matchTreatment), 
                             list(ResponseId, matchTreatment, treatment, treatIdeo)], by="ResponseId")

# Prepare text
FB_LIKE[, contentTextClean := cleanText(newsPageContent)]
FB_LIKE[, urlClean := cleanText(newsPageURL)]

# For graph
FB_LIKE[newLike==TRUE, outletGroup :=ifelse(potLib, "Liberal\nOutlets", ifelse(potCons, "Conservative\nOutlets", NA))]
keywordFB = "Exposed in Feed"



# .nav ----
# .........................................................................

NAV_LIKE = FB_LIKE[foundNav==TRUE]

# For graph
keywordNav = "Posts Visited"

saveToLyx(NAV_LIKE[matchTreatment=="Pro", .N], "postsLikedProVisitedSampleSize", digits = 0)
saveToLyx(NAV_LIKE[matchTreatment=="Counter", .N], "postsLikedCounterVisitedSampleSize", digits = 0)


# .shared ----
# .........................................................................
SHARED_POSTS_AFTER <- readRDS("Datasets/Shared/sharedPosts.rds")
SHARED_POSTS_INCLUDE = SHARED_POSTS_AFTER[!is.na(newsPageID)]


# Add treatment
SHARED_POSTS_INCLUDE = merge(SHARED_POSTS_INCLUDE, 
                             SURVEY[!is.na(matchTreatment), list(matchTreatment, treatment, ResponseId, treatIdeo)], 
                             by="ResponseId")

SHARED_PAGE = merge(SHARED_POSTS_INCLUDE, 
                    USERS_PAGES[, list(pageID, ResponseId, potential, newLike, outlet, backup,
                                       potLib, potCons)], 
                    by.x=c("newsPageID", "ResponseId"), by.y=c("pageID", "ResponseId"), all.x=TRUE)

SHARED_LIKE = SHARED_PAGE[potential==TRUE & newLike==TRUE & backup<=1]


# Prepare text
SHARED_LIKE[, descriptionClean := cleanText(newsPageDescription)]
SHARED_LIKE[, linkClean:= cleanText(newsPageLink)]

# For graph
SHARED_LIKE[, outletGroup := ifelse(potLib, "Liberal\nOutlets", ifelse(potCons, "Conservative\nOutlets", NA))]
keywordShared = "Shared Posts"



# *************************************************************************
# POLITICAL EXPRESSION ----
# *************************************************************************

lapply(c("polNames", "polParties", "polInst", "polIssues", "allPolWords"), function(x)
  saveToLyx(paste(sort(get(x)), collapse=", "), x))


# .exposure ----
# .........................................................................

# Is post about politics?
FB_LIKE[, politics := grepl(politicsExpression, contentTextClean) | grepl(politicsExpression, urlClean)]
FB_SUM_LIKED = FB_LIKE[, list(sharePolitics = mean(politics)),  by=c("matchTreatment", "outletGroup")]

# Claim that more than half political
stopifnot(FB_LIKE[, mean(politics)]>0.5)


# .browsing ----
# .........................................................................

NAV_SUM_LIKE = FB_LIKE[foundNav==TRUE, list(sharePolitics = mean(politics)), by=c("matchTreatment", "outletGroup")]

# Claim that more than half political
stopifnot(FB_LIKE[foundNav==TRUE, mean(politics)]>0.5)

# .shared ----
# .........................................................................

SHARED_LIKE[, politicsDescription := grepl(politicsExpression, descriptionClean)]
SHARED_LIKE[, politicsLink := grepl(politicsExpression, linkClean)]
SHARED_LIKE[, politics := newsPoliticsMessage | politicsDescription | politicsLink]

# Claim that more than half political
stopifnot(SHARED_LIKE[, mean(politics)]>0.5)

SHARED_SUM_LIKED = SHARED_LIKE[, list(sharePolitics = mean(politics)), by=c("matchTreatment", "outletGroup")]

libSharedProPolitics = SHARED_SUM_LIKED[outletGroup=="Liberal\nOutlets" & matchTreatment=="Pro", sharePolitics]
libSharedCounterPolitics = SHARED_SUM_LIKED[outletGroup=="Liberal\nOutlets" & matchTreatment=="Counter", sharePolitics]

saveToLyx(libSharedProPolitics, "libSharedProPolitics", percent = TRUE)
saveToLyx(libSharedCounterPolitics, "libSharedCounterPolitics", percent = TRUE)


# .graph share politics ----
# .........................................................................

SUM_POLITICS = rbindlist(list(FB_SUM_LIKED[, keyword := keywordFB], 
                              NAV_SUM_LIKE[, keyword := keywordNav],
                              SHARED_SUM_LIKED[, keyword := keywordShared]), use.names = TRUE) 

ggplot(SUM_POLITICS, aes(x=outletGroup, y=sharePolitics, fill=matchTreatment, alpha=matchTreatment)) +
  geom_col(position = "dodge", color="black") + 
  scale_alpha_manual(values = c(0.25, 1)) + 
  scale_fill_manual(values = matchColors2) + 
  facet_wrap(~keyword) + 
  scale_y_continuous(labels = function(x) scales::percent (x, accuracy = 1), breaks=seq(0.1, 0.9, 0.1)) +
  labs(y = "Share of Posts with Political Terms")

currentTheme = theme(axis.title.x = element_blank(),
                     legend.spacing.x = unit(0.2, 'cm'), legend.key.width = unit(3, "line"))
currentThemeBeamer = theme(axis.title.x = element_blank())

ggMySaveDoc("Output/Graphs/Combined/sharedPoliticalPosts.eps", theme = currentTheme, device=cairo_ps)
ggMySaveBeamer("Output/Graphs/Combined/sharedPoliticalPosts_Beamer.eps", theme = currentThemeBeamer, device=cairo_ps, adjustHeight = 0.9)



# *************************************************************************
# COMMON WORDS ----
# *************************************************************************

# .functions ----
# .........................................................................

additionalStopWords = c("http", "https", "bit", "say", "said", "can", "comment", "opinion", "write", 
         "new york time", "wall street journal", "wsj", "fox friend", "new york",
         "fox news", "times", "last week", "washington post", "journal", "nyt",
         "news", "breaking news", "write the editori board", "year old")
saveToLyx(paste0(sort(unique((additionalStopWords))), collapse = ", "), "additionalStopWords")

#myCorpus = myCorpusNav; i=2
# Receive corpus, remove stopword, calculate dfm-gram, and remove words that only appear in one outlet
returnDfm <- function(myCorpus, nwords=2) {
newStopwords = c(gsub("'", "", stopwords("english")), phrase(additionalStopWords))
  dfmgram <- processCorpus(myCorpus, nwords, stopWords = newStopwords)  
  
  dfmgramOutlet <- dfm_group(dfmgram, groups = "outlet")
  dfmgramOutlet <-  processDfm(dfmgramOutlet, groups = "outlet")
  dfmgramOutlet[, uniqueFeature := .N==1, by=feature]
  remove = dfmgramOutlet[uniqueFeature==TRUE, feature]
  
  dfmgramClean = dfm_remove(dfmgram, c(remove))
  return(dfmgramClean)
}


# Receive dfma and print the most common words
# Example values: dfmgramClean=dfmgramCleanList[[2]]; keyword = keywordShared, ; includeColNames=TRUE
printCommonWords <- function(dfmgramClean, file, keyword, includeColNames=TRUE) {
  dfmgramStat = processDfm(dfmgramClean, groups="treatIdeo")
  dfmgramStatWide = dcast(dfmgramStat[rankShare<11], rankShare~group, value.var="featureShare")

  liberal = paste0(keyword, ", Liberal Outlets")
  conservative = paste0(keyword, ", Conservative Outlets")
  if(includeColNames) {
    colNames = c("Pro", "Counter", "Pro", "Counter")
  } else {
    colNames = NULL
  }
  
  kableExtra::kable(dfmgramStatWide[rankShare<11, list(treatCIdeoC, treatCIdeoL, treatLIdeoL, treatLIdeoC)], 
                    format="latex", booktabs=TRUE, escape=TRUE, linesep = "", 
                  col.names = colNames)   %>%
    add_header_above(c(setNames(2, conservative), setNames(2, liberal))) %>%
    cat(file=file) 
}


# .fb ----
# .........................................................................

myCorpusFB = corpus(FB_LIKE[, list(treatIdeo, matchTreatment, treatment, outlet, text = contentTextClean)])
dfmgramCleanListFB <- returnDfm(myCorpusFB)

printCommonWords(dfmgramCleanListFB, file="Output/Tables/Exposure/commonWords.tex", keyword = keywordFB)


# .nav ----
# .........................................................................

myCorpusNav = corpus(NAV_LIKE[, list(treatIdeo, matchTreatment, treatment, outlet, text = contentTextClean)])
dfmgramCleanListNav = returnDfm(myCorpusNav)

printCommonWords(dfmgramCleanListNav, file="Output/Tables/Browsing/commonWords.tex", keyword = keywordNav, 
                 includeColNames = FALSE)
#myTexPreview(obj = "Output/Tables/Browsing/commonWords.tex")


# .share ----
# .........................................................................

# Create corpus
myCorpusShared = corpus(SHARED_LIKE[, list(treatIdeo, matchTreatment, treatment, outlet, text = descriptionClean)])

dfmgramCleanList = returnDfm(myCorpusShared)

# Interpretation - conservative post about illegal immigration, liberals about Trump, some unrelated areas
printCommonWords(dfmgramCleanList, file="Output/Tables/Shared/commonWords.tex", keyword = keywordShared,
                 includeColNames = FALSE)



# *************************************************************************
# OUTLET SECTION ----
# *************************************************************************

# .prepare ----
# .........................................................................

SHORT_OUTLETS = as.data.table(list(outlet = c(
  "MSNBC", "WashingtonPost", "HuffPost", "Slate", "TheNewYorkTimes",
  "FoxNews", "TheWashingtonTimes", "TheWallStreetJournal", "NationalReview", "DailyCaller"), 
  shortOutlet = c(
    "MSNBC", "WP", "HP", "Slate", "NYT", 
    "Fox", "WT", "WSJ", "NR", "DC")))


# .functions ----
# .........................................................................

# Receive datatable, count how often each section appears, create name, replace rare sections with Other
processSection <- function(DT) {
  
  # Define list of sections
  DT[is.na(articleSection) | articleSection=="", articleSection := "Unknown"]

  # count and rank sections
  DT[, countSectionOutlet := .N, by=c("outlet", "articleSection")]
  
  
  SECTIONS = unique(DT[, list(outlet, articleSection, countSectionOutlet)])
  SECTIONS[, rankSectionOutlet := rank(-countSectionOutlet), by=c("outlet")]
  
  # Provide clean name, replace rare sections with "Other"
  SECTIONS = merge(SECTIONS, SHORT_OUTLETS, by="outlet")
  SECTIONS[, sectionWithOutlet := paste0(shortOutlet, ": ", 
    ifelse(rankSectionOutlet<=50, stringr::str_to_title(articleSection), "Other"))]
  SECTIONS[, sectionWithOutlet := gsub("\\.", "", gsub("Us", "US", sectionWithOutlet))]
  
  n = nrow(DT)
  DT = merge(DT, unique(SECTIONS[, list(outlet, articleSection, sectionWithOutlet)]), 
             by=c("outlet", "articleSection"),
              all.x=TRUE)
  stopifnot(n==nrow(DT))
  return(DT)
} 

# Receive datatable, calculate share of posts from each section and rank all sections
sortArticleSection <- function(DT) {
  DT[, matchTreatmentRev := factor(matchTreatment, levels = rev(levels(matchTreatment)))]
  DT = DT[!is.na(matchTreatmentRev) & newLike, .N, 
                                      by=c("sectionWithOutlet", "matchTreatmentRev", "outletGroup")]
  
  DT[, rank := frank(-N), by=c("matchTreatmentRev", "outletGroup")]
  DT[, myOrder := rank*ifelse(matchTreatmentRev=="Counter", 1, 100)+
                       ifelse(outletGroup=="Liberal Outlets", 10000, 0)]
  DT[, share := N/sum(N), by=c("matchTreatmentRev", "outletGroup")]
  DT[, include := min(rank)<=10, by=c("sectionWithOutlet", "outletGroup")]
  return(DT)
}


# .fb ----
# .........................................................................

# Add section
FB_LIKE[, articleUrl := setArticleURL(newsPageURL)]
FB_SECTION = merge(FB_LIKE, ARTICLES_SUMMARY[, list(url, articleSection)], by.x="articleUrl", by.y="url")

# process section 
FB_SECTION_SUM = processSection(FB_SECTION)
FB_SECTION_SUM = sortArticleSection(FB_SECTION_SUM)


# .browsing ----
# .........................................................................

NAV_SECTION = merge(FB_LIKE[foundNav==TRUE], ARTICLES_SUMMARY[, list(url, articleSection)], 
                    by.x="articleUrl", by.y="url")
NAV_SECTION_SUM = processSection(NAV_SECTION)
NAV_SECTION_SUM = sortArticleSection(NAV_SECTION_SUM)


# .shared ----
# .........................................................................

SHARED_LIKE[, articleUrl := setArticleURL(newsPageLink)]

SHARED_SECTION = merge(SHARED_LIKE, ARTICLES_SUMMARY[, list(url, articleSection)], by.x="articleUrl", by.y="url")
SHARED_SECTION_SUM = processSection(SHARED_SECTION)
SHARED_SECTION_SUM = sortArticleSection(SHARED_SECTION_SUM)

# Sports
SHARED_SECTION[articleSection!="Unknown", mean(grepl("sport", articleSection))]


# .bind and plost ----
# .........................................................................

SECTION_SUM = rbindlist(list(FB_SECTION_SUM[, keyword := keywordFB], 
                             NAV_SECTION_SUM[, keyword := keywordNav], 
                             SHARED_SECTION_SUM[, keyword := keywordShared]), use.names = TRUE)

myGuides = guides(fill = guide_legend(reverse=TRUE, nrow = 2, keyheight=0.15,default.unit="inch"), 
                  alpha = guide_legend(reverse=TRUE, nrow = 2, keyheight=0.15,default.unit="inch"))
currentTheme = theme(axis.title.y = element_blank(), legend.position = "top", legend.title = element_blank(),
                     legend.spacing.x = unit(0.2, 'cm'), legend.spacing.y = unit(1, 'cm'), legend.key.width = unit(3, "line"))
currentThemeBeamer = theme(axis.title.y = element_blank(), legend.position = c(0.9, 0.1), legend.title = element_blank(),
                           legend.box.background = element_rect(colour = "black"))

# DT = SECTION_SUM[include==TRUE & keyword==keywordFB]; file="Output/Graphs/Exposure/outletAndSection.eps"
saveSectionGraph <- function(DT, file) {
  ggplot(DT, aes(x=reorder(sectionWithOutlet, -myOrder), y=share, fill=matchTreatmentRev, alpha=matchTreatmentRev)) +
    geom_bar(stat="identity", color="black", width=0.7, position=position_dodge(width=0.8)) + 
    scale_alpha_manual(values=c(1, 0.25)) + 
    scale_fill_manual(values=rev(matchColors2)) + 
    scale_y_continuous(labels = function(x) scales::percent (x, accuracy = 1)) +
    labs(y = "Share of Posts") + 
    coord_flip() + 
    facet_wrap(~outletGroup, scales="free") + 
    myGuides
  
  ggMySaveBeamer(gsub("\\.eps", "_Beamer\\.eps", file), theme = currentThemeBeamer, device=cairo_ps, noLinks = TRUE)
  ggMySaveDoc(file, theme = currentTheme, device=cairo_ps, adjustHeight=1.5)
}

saveSectionGraph(SECTION_SUM[include==TRUE & keyword==keywordFB], "Output/Graphs/Exposure/outletAndSection.eps")
saveSectionGraph(SECTION_SUM[include==TRUE & keyword==keywordNav], "Output/Graphs/Browsing/outletAndSection.eps")
saveSectionGraph(SECTION_SUM[include==TRUE & keyword==keywordShared], "Output/Graphs/Shared/outletAndSection.eps")
                  
