# Author: Ro'ee Levy, Yale
# Date: Sun Sep 13 22:49:20 2020
# Purpose: Userful functions to process text throughout the project
# --------------#


library(quanteda)

# Clean non-ascii and other text 
cleanText <- function(char) {
  
  # Lowercase, replace ' in don't and . in U.S. for example
  charClean = tolower(iconv(char, "", "ASCII", sub=""))
  charClean = gsub("-", " ", charClean)
  charClean = gsub("[\\.']", "", charClean)
  
  #In The New York Times Opinion Section, Marcy Wheeler writes
  charClean = gsub("in the new york times opinion section.*writes", "", charClean)
  return(charClean)
}


# Convert text to number
textToNumber <- function(x) {
  # Removing many that say a lot
  temp = tolower(x)
  temp = gsub("[[:space:]]", "", temp)
  
  # Replace 5-7 with 5to7 and any 5+ 5> 5< as NA
  temp = gsub("(.*[0-9]+)[-/]([0-9]+.*)", "\\1to\\2", temp)
  
  # 30+- -> 30, 30+ 4- -> ""
  temp = gsub("\\+/?-", "", temp)
  temp[grepl("[<>\\+%=]", temp)]=""
  temp = gsub("-$", "", temp)
  
  # Now can remove punctuation
  temp = gsub("[^0-9a-z\\.]", "", temp)
  
  # approx.
  temp = gsub("\\.+$", "", temp)
  temp = gsub("\\.\\.+", "", temp)
  temp = gsub("times|about|around|orso|apx|approx|ish|est|probably|perhaps|maybe", "", temp)
  
  # 30x = 30 times
  temp = gsub("([0-9]+)x[s]?", "\\1", temp)
  
  temp[temp %in% c("notonce", "o", "none")] = 0
  temp[grepl("unsure|unsure|often|yes|several|more|less|few|many|bunch|lot|ton|all|least|multiple|rare|uncertain|no|plus|over", 
             temp, ignore.case = TRUE)] = ""
  temp[grepl("hour|daily|tablet|watch|ipad|dozens|numerous|every|often|some|min|idk|na|week|important", 
             temp, ignore.case = TRUE)] = ""
  
  temp = stringr::str_replace_all(temp, c("zero"="0", "never"="0", "once"="1", "one"="1", 
                                          "acouple"="2", "couple"="2", "two"="2", "twice"="2", "three"="3", "four"="4", 
                                          "five"="5", "six"="6","seven"="7", "eight"="8", "nine"="9", "ten"="10"))
  
  # 5or6 -> 5.5
  or = grepl("([0-9]+)or|to([0-9]+)", temp)
  temp[or] = (as.numeric(gsub("([0-9]+)[a-z]+[0-9]+", "\\1", temp[or])) + 
                as.numeric(gsub("[0-9]+[a-z]+([0-9]+)", "\\1", temp[or])))/2
  
  # Start with zero and explain. 0, don't have wifi -> 0
  temp[grepl("^0[^-1-9\\.].*", temp)] = 0
  
  # If still have character then ""
  temp[grepl("[a-z]", temp)]=""
  temp = as.numeric(temp)
  return(temp)
}




# Receive corpus and return dfm
processCorpus <- function(corpus, ngrams, stopWords=NULL, stem=TRUE, groups=NULL) {
  tokens <- tokens(corpus, remove_punct=TRUE, remove_symbols=TRUE)
  if (stem) {
    tokens <- tokens_wordstem(tokens)
  }
  if(is.null(stopWords)) {
    tokens <- tokens_remove(tokens, stopwords("english"))
  } else {
    tokens <- tokens_remove(tokens, stopWords)
  }
  
  # Remove less than 3 chars
  tokens <- tokens_keep(tokens, min_nchar=3)
  
  tokens <- tokens_ngrams(tokens, ngrams)
  if (is.null(groups)) {
    dfm = dfm(tokens)
  } else{
    dfm = dfm(token, groups=groups)
  }
  return(dfm)
}


# Receive dfm and return summary stats
processDfm <- function(dfm, groups=NULL) {
  dfmStat = textstat_frequency(dfm, groups=attributes(dfm)$docvars[[groups]])
  dfmStat = as.data.table(dfmStat)
  dfmStat[, rank := NULL]
  setnames(dfmStat, c("frequency","docfreq"), c("freqWord", "freqDoc"))
  
  # Share of phrase in all phrases used
  dfmStat[, totalWord := sum(freqWord), by="group"]
  dfmStat[, shareWord := freqWord/totalWord]
  
  # Share of documents mentioning phrase in all documents used
  totalDocs = dcast(as.data.table(list(group=attributes(dfm)$docvars[[groups]])), group~., value.var="group", fun.aggregate = length)
  setnames(totalDocs, c("group", "totalDoc"))
  dfmStat = merge(dfmStat, totalDocs, by="group")
  dfmStat[, shareDoc := freqDoc/totalDoc]
  
  # Rank and print
  dfmStat[, niceShareWord := scales::percent(shareWord, accuracy = 0.01)]
  dfmStat = dfmStat[order(feature)]
  dfmStat[, rankShare := frank(-shareDoc, ties.method = "first"), by="group"]
  dfmStat[, featureShare := paste0(gsub("_", " ", feature), " (", scales::percent(shareDoc, accuracy = 0.01), ")")]
  
  return(dfmStat)
}


# *************************************************************************
# NEWS ANNOTATION RELATED FUNCTION ----
# *************************************************************************


# Clean all names in news, merge similar names, find first name when only last names exists, etc.
cleanNews <- function(newsEntities) {
  
  # Clean whitespace 
  newsEntities[,entity:= trimws(entity)]
  
  # Deal with possesive s
  newsEntities[,entity := gsub("[ ']s$", "", entity)] # Remove 's at the end
  newsEntities[,entity := gsub("s[']$", "s", entity)] # Remove 's at the end
  
  # Replace all characater besides letters . - and space
  newsEntities[, entity := gsub("[^-a-zA-Z\\.\\ ]", "", entity)]
  
  
  newsEntities[grepl("Donald",entity) & grepl("Trump",entity) & ! grepl("Jr.",entity),entity:="Donald Trump"]
  
  # Many only say Obama, checked manually seem to refer to Barak
  newsEntities[grepl("Michelle",entity) & grepl("Obama",entity),entity:="Michelle Obama"]
  newsEntities[entity=="Obama" & grepl("President", message),entity:="Barack Obama"]
  newsEntities[entity=="Obama" & grepl("Obama[- ]era", message),entity:="Barack Obama"]
  newsEntities[entity=="Obama" & grepl("Obama years", message),entity:="Barack Obama"]
  newsEntities[entity=="Obama" & grepl("[aA]dministration", message),entity:="Barack Obama"]
  newsEntities[entity=="Obama", entity:="Barack Obama"]
  
  newsEntities[entity=="Clinton" & grepl("[pP]resident",message),entity:="Bill Clinton"]
  newsEntities[entity=="Clinton" & grepl("Gore|Bill|Carter|Reagan",message),entity:="Bill Clinton"]
  newsEntities[entity=="Clinton" & grepl("Clinton-appointed",message),entity:="Bill Clinton"]
  newsEntities[entity=="Clinton" & grepl("e-mail",message),entity:="Hillary Clinton"]
  newsEntities[entity=="Clinton" & grepl("email|Comey|investigation",message),entity:="Hillary Clinton"] # One refers to the Clintons, fine
  newsEntities[entity=="Clinton" & grepl("voted|candidate|election",message),entity:="Hillary Clinton"]
  newsEntities[entity=="Clinton" & grepl("Secretary|administration",message), entity:="Hillary Clinton"]
  newsEntities[entity=="Clinton" & grepl("Clinton campaign",message), entity:="Hillary Clinton"]
  newsEntities[entity=="Clinton" & grepl("Hillary|Mrs",message), entity:="Hillary Clinton"]
  newsEntities[entity=="Clinton" & grepl("Morton|Missouri",message), entity:=NA] # location
  
  # Fix Manually
  newsEntities[entity=="Andy McCabe", entity := "Andrew McCabe"]
  newsEntities[entity %in% c("Bob Mueller","Robert S. Mueller III"),entity:="Robert Mueller"]
  
  # Checked manually actually Chuck and not Amy
  newsEntities[entity=="Schumer", entity:="Chuck Schumer"] 
  
  # Sometimes has middle name, sometimes doesn't
  newsEntities[grepl("Sarah.*Sanders",entity),entity:="Sarah Sanders"]
  
  # If trailing s, and exists without, remove trailing s
  newsEntities[,existsNoS := grepl("s$", entity) & gsub("s$", "", entity) %in% newsEntities$entity]
  newsEntities[existsNoS==TRUE, entity := gsub("s$", "", entity)]
  
  # If only one alternative, change to alternative. Using stringr::str_detect instead of grepl
  {
    newsEntities[, oneWord := !grepl(" ", entity)]
    newsEntities[, countEntity := .N, "entity"]
    
    allCombination = as.data.table(expand.grid(shortName = unique(newsEntities[oneWord==TRUE, entity]),
                                               newFullName = unique(newsEntities[oneWord==FALSE, entity])))
    allCombination[, potential := stringr::str_detect(newFullName, stringr::coll(shortName))]
    allCombination = allCombination[potential==TRUE]
    
    allCombination = merge(allCombination, unique(newsEntities[, list(countEntity, entity)]), by.x="newFullName", by.y= "entity")
    allCombination[, sharePotential := countEntity / sum(countEntity), by="shortName"]
    allCombination = allCombination[sharePotential>0.9]
    
    newsEntities = merge(newsEntities, allCombination[, list(shortName, newFullName)], by.x="entity", by.y = "shortName", all.x=TRUE)
    newsEntities[!is.na(newFullName), entity := newFullName]
    newsEntities[, countEntity := NULL]
  }
  
  return(newsEntities)
}



# prepare news names for plot
prepareNewsNames <- function(DT, removeDup=TRUE) {
  
  DT[,countNameSlant := .N, c("slant","entity")]
  
  # Remove duplicates
  DT = unique(DT[,  list(entity, slant, countNameSlant)])
  
  # Complete table 
  DT <- tidyr::complete(DT, entity, slant, fill=list(countNameSlant=0)) %>% as.data.table()
  DT = DT[slant!="Moderate"] #Need again because it is completed..
  
  # Count
  DT[,countName := sum(countNameSlant), by=c("entity")] 
  DT[,shareName := countName/sum(countName), by=c("slant")] 
  DT[,shareNameSlant := countNameSlant/sum(countNameSlant), c("slant")] 
  
  DT[, rankShareSlant := rank(desc(shareNameSlant)), by="slant"]
  DT[, include := min(rankShareSlant)<=10, by="entity"]
  
  # Top names by diff
  DT[, shareSlantDiff := (max(shareNameSlant*(slant=="Conservative")) -
                            max(shareNameSlant*(slant=="Liberal"))), by="entity"]
  DT[,rankSlantDiff := rank(desc(abs(shareSlantDiff))), by=c("slant")]
  
  DT[,rankSlantConservative := max(ifelse(slant=="Conservative", rankShareSlant, 0)), by="entity"]
}


# Prepare plot of figures mentioned in news
prepareNewsPlot <- function(DT, fileName) {
  data = unique(DT[include==TRUE][order(-rankSlantConservative), list(slant, entity, shareNameSlant, shareSlantDiff, rankSlantConservative)])
  
  #data[entity %in% c("Donald Trump", "Trump", "President Trump"), shareNameSlant := sum(shareNameSlant), slant]
  #data = data[! entity %in% c("Trump", "President Trump")]
  
  data[, trump:=ifelse(entity=="Donald Trump", 1, 0)]
  data[, entityF := factor(entity, levels=unique(data$entity[order(data$shareSlantDiff)]))]
  data[, slantF := factor(slant, levels=c("Conservative", "Liberal"))]
  
  noTrump = ggplot(data[trump==0], aes(reorder(entityF, -shareSlantDiff), abs(shareNameSlant), fill=slantF, alpha=slantF)) +
    geom_bar(stat="identity",position="dodge", width=.6) +
    coord_flip() +
    scale_y_continuous(limits = c(0,0.055)) +
    scale_fill_manual(values=c("Red", "Blue")) + 
    scale_alpha_manual(values=c(1, 0.3)) + 
    theme_bw(base_size = 10) +
    theme(legend.position = "none", legend.title = element_blank(), plot.margin = margin(0,0,0,0)) + 
    guides(fill=guide_legend(reverse = TRUE), alpha=guide_legend(reverse = TRUE)) +
    xlab("") + ylab("")
  
  # Keeping legend for same proportion but making it invisible
  withTrump = ggplot(data[trump==1], aes(entityF, abs(shareNameSlant), fill=slantF, alpha=slantF)) +
    geom_bar(stat="identity",position="dodge", width=.6) +
    coord_flip() +
    scale_y_continuous(breaks=scales::pretty_breaks(3), limits = c(0,0.3)) +
    scale_fill_manual(values=c("Red", "Blue")) + 
    scale_alpha_manual(values=c(1, 0.3)) + 
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.key.height = unit(0.05, "in"), legend.spacing.x = unit(0.15, "in"),
          legend.margin = margin(-5, 0, 0, 0), plot.margin = margin(0,0,0,0)) + 
    guides(fill=guide_legend(reverse = TRUE), alpha=guide_legend(reverse = TRUE)) +
    xlab("") + ylab("Share of Individuals Mentioned")
  
  # Can't call this on cluster since all masks ggplot, unless use actual or virtual display
  plotFull = cowplot::plot_grid(noTrump, withTrump, align = "v", nrow=2, rel_heights = c(0.75, 0.25))
  plotFullDoc = cowplot::plot_grid(noTrump, withTrump, align = "v", nrow=2, rel_heights = c(0.8, 0.2))
  ggMySaveBeamer(file = paste0("Output/Graphs/News/", fileName, "_Beamer.eps"), plot = plotFull, theme=NULL, adjustHeight = 1.1, device=cairo_ps)
  ggMySaveDoc(file = paste0("Output/Graphs/News/", fileName, ".eps"), plot = plotFullDoc, theme=NULL, device=cairo_ps)
}
