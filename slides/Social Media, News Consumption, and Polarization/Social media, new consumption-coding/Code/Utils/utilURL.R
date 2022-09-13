# Author: Ro'ee Levy, Yale
# Date: Sun Sep 13 22:51:18 2020
# Purpose: Userful functions when working with URLs
# --------------#



setArticleURL <- function(url) {
  articleUrl = gsub("&utm_term=\\.[0-9a-z]*", "", url)
  articleUrl = gsub("\\?utm_term=\\.[0-9a-z]*$", "", articleUrl)
  articleUrl = gsub("\\?utm_term=\\.[0-9a-z]*&", "\\?", articleUrl)
  return(articleUrl)
}


getDomain <- function(url) {
  # something, then :// then include the following: [anything up to /] and then / and then anything
  # Notice the ? after .* makes the search non-greedy, this mean we stop at first :// useful when there are a couple of http:// in address
  temp = sub(".*?://","",url)
  temp = sub("([^/]*)/.*","\\1",temp)
  #temp = sub(".*://([^/]*\\.[^/]*)/.*","\\1",url) 
  return (gsub("www[0-9]?.", "", temp)) #replace www. or www1.
}



# 1. Remove http 2. Remove last / 3. For most urls remove everything after last ?. Not perfect method, e.g. "www8.miamidade.gov/global/news-item.page?Mduid_news=news1511298061044259&Mduid_organization=org1496346070566655&Value.1=org1496346070566655&utm_campaign=09-21-2018-weekly-news.htm&utm_medium=email&utm_source=Eloqua"
# But still merges many articles so worth it, e.g. "www.washingtonpost.com/local/immigration/2018/04/24/cfb41578-4816-11e8-8b5a-3b1697adcc2a_story.html"
# Not removing for house/senate, e.g.   https://www.lee.senate.gov/public/index.cfm/press-releases?ID=02CAB3BA-E884-4730-84A3-88141E3140A2
# Using bytes=TRUE because the server failed with some strange url without it
getSimplifiedUrl <- function(url) {
  # replace http and https, two function with fixed=TRUE to save time
  url = gsub("https://", "", gsub("http://", "", url, fixed=TRUE, useBytes=TRUE), fixed=TRUE, useBytes=TRUE)
  url = gsub("/$", "", url)
  firstQuestionMark = regexpr("?", url, fixed = TRUE,  useBytes=TRUE)
  houseSenateURL = grepl("house.gov", url, fixed=TRUE,  useBytes=TRUE) |  grepl("senate.gov", url, fixed=TRUE,  useBytes=TRUE)
  url[firstQuestionMark>40 & !houseSenateURL] = sub("\\?.*", "", url[firstQuestionMark>40 & !houseSenateURL])
  
  # After removing ? again remove final /
  url = gsub("/$", "", url)
  return (url)
}

addArtilceSlant <- function(DT, urlColumn, newColumn) {
  URLS_CONGRESS <- read_fst("Datasets/Congress/congressUrls.fst", as.data.table=TRUE, columns = c("url", "meanIdeoURL"))
  
  #First need to simplify URL (takes a while)
  DT[eval(urlColumn)!="", simpleURL := getSimplifiedUrl(get(urlColumn))]
  
  DT <- merge(DT, URLS_CONGRESS[, list(url, tempName=meanIdeoURL)], by.x="simpleURL", by.y="url", all.x=TRUE)
  setnames(DT, "tempName", newColumn)
  DT[, simpleURL := NULL]
  return (DT)
}


