# Author: Ro'ee Levy, Yale
# Date: Sun Sep 13 22:42:17 2020
# Purpose: Functions used to create news consumption descriptive stat.: isolation, segregation, congruence, share of counter-attitudinal news
# --------------#


library(data.table)

# *************************************************************************
# SEG FUNCTIONS ----
# *************************************************************************


# Calculate isolation scale in data.table DT
# byAgg - the variable defining the media outlet (e.g., domain)
# slantVar - variable defining slant of post/website
# ideoVar - variable defining the ideology of user
# indVar - variable defining the individual identifier
# userAgg - aggregate first at the user level by var 'userVar' 
# weightsVar - variable used to weight the results
# minVisitors - keep websites with at least minVisitors number of visitors
# includeCurrent - include current user (FALSE = adjusted segregation scale)
#DT = NEWS_PARTISAN; byAgg="domain"; indVar="caseid"; ideoVar="partyTest"; includeCurrent = FALSE; slantVar = "slant"; userAgg=FALSE; daily=FALSE; minVisitors=2; weightVar = NULL; dateVar=NULL
calculateIsolation <- function(DT, byAgg, indVar, ideoVar, dateVar=NULL, weightVar = NULL,
                               includeCurrent=FALSE, minVisitors=0, check=FALSE, daily = TRUE, userAgg=FALSE, returnShares = FALSE) { 
  
  # Work with local data.table so global one won't be affected. Remove cases where ideology is not know
  keepCols = c(byAgg, indVar, ideoVar, dateVar, weightVar); keepCols = keepCols[keepCols!="NULL"]
  currentDT <- DT[!is.na(get(ideoVar)), ..keepCols] 
  
  # Move to daily level 
  if (daily) {
    currentDT = unique(currentDT)
  }
  
  # Only include cases with at least one visitors.
  if(includeCurrent==FALSE & minVisitors==0){
    minVisitors=2
  }
  # Keep websites with at least X unique visit
  currentDT[, uniqueVisitors := uniqueN(get(indVar)), by=byAgg]
  currentDT <- currentDT[uniqueVisitors>=minVisitors]
  
  # Define weights if not defined
  if(is.null(weightVar) || weightVar=="NULL") {
    currentDT[, userDomainVisit := .N, by = c(byAgg, indVar)]
    weightVar = "weight"
    currentDT[, weight := 1]
  } else   {
    # Can use only this line, but prefer using .N with no weights to save time
    currentDT[, userDomainVisit := sum(get(weightVar)), by = c(byAgg, indVar)]
  }
  
  #### START CALCULATION 
  
  # Total visits of conservative and liberal to websites. cons_m and lib_m (Gentzkow and Shpiro 2011, pages 1810, 1834)
  totalCons = currentDT[get(ideoVar)==1, sum(get(weightVar))]
  totalLib = currentDT[get(ideoVar)==-1, sum(get(weightVar))]
  
  # Total visits of conservatives and liberals by domain. cons_j lib_j
  currentDT[, domainVisitCon := sum(get(weightVar)*(get(ideoVar)==1)), by=byAgg]
  currentDT[, domainVisitLib := sum(get(weightVar)*(get(ideoVar)==-1)), by=byAgg]
  
  # Share of conservative in this domain among all sites visited by conservative. Same for liberals
  currentDT[, domainShareAmongCon := domainVisitCon / totalCons]
  currentDT[, domainShareAmongLib := domainVisitLib / totalLib]
  
  # Total visits to domain. visits_j
  currentDT[, domainVisit := domainVisitCon + domainVisitLib]
  
  if   (!includeCurrent) {
    # x_ij Calculate total number of liberal and conservative visit in each website at the user-domain level 
    currentDT[, userDomainVisitCon := userDomainVisit*(get(ideoVar)==1)]
    currentDT[, userDomainVisitLib := userDomainVisit*(get(ideoVar)==-1)]
    
    # conservative and total visits to domain excluding current: cons_j - x_ij, visit_j-x_ij
    currentDT[, domainVisitConEx := domainVisitCon-userDomainVisitCon, by=byAgg]
    currentDT[, domainVisitEx := domainVisit-userDomainVisit]
    
    currentDT[, shareConsDomain := weighted.mean(domainVisitConEx / domainVisitEx, get(weightVar)), by=c(ideoVar, byAgg)]
    #currentDT[, shareConsDomain := weighted.mean(domainVisitConEx / domainVisitEx, get(weightVar)), by=c(ideoVar)]
  } else {
    # Share of conservative in domain x
    currentDT[, shareConsDomain := domainVisitCon / domainVisit]
  }
  
  # Now calculation isolation index
  
  # Isolation at the visit level - main measure used by Genkzkow and Shapiro
  if (!userAgg) {
    # Keep list of domain and then sum over each j
    DOMAINS = unique(currentDT[, list(domain, ideo=get(ideoVar), domainShareAmongCon, domainShareAmongLib, shareConsDomain)])
    stopifnot(sum(duplicated(DOMAINS, by=c("domain", "ideo")))==0)
    
    # domainShareAmongCon - the weight, how often conservatives visit this site among all sites visited *
    # shareConDomain - conservative exposure, how many conservatives do they "meet" in these sites
    repShare = DOMAINS[ideo==1, sum(domainShareAmongCon*shareConsDomain)]
    libShare = DOMAINS[ideo==-1, sum(domainShareAmongLib*shareConsDomain)]
    isol = repShare - libShare
  } else {
    
    # Isolation at the user liver
    USERS = currentDT[, list(exposureConU = mean(shareConsDomain), totalVisitU = .N, userWeight = mean(get(weightVar))), 
                        by=c(indVar, ideoVar)]
    
    # Calculate mean exposure to conservative content across users
    repShare = USERS[get(ideoVar)==1, weighted.mean(exposureConU, userWeight)]
    libShare = USERS[get(ideoVar)==-1, weighted.mean(exposureConU, userWeight)]
    isol = repShare - libShare
  }
  
  if (check) {
    # Can calculate mean conservative content at user level and weight by total visit to get the same result as get when 
    # vinding isolation at the visit level
    DOMAINS = unique(currentDT[, list(domain, ideo=get(ideoVar), domainShareAmongCon, domainShareAmongLib, shareConsDomain)])
    USERS = currentDT[, list(exposureConU = mean(shareConsDomain), totalVisitU = sum(get(weightVar))), by=c(indVar, ideoVar)]
    
    isolDomain = DOMAINS[ideo==1, sum(domainShareAmongCon*shareConsDomain)] - DOMAINS[ideo==-1, sum(domainShareAmongLib*shareConsDomain)]
    isolDomainUserCalc = USERS[get(ideoVar)==1, weighted.mean(exposureConU, totalVisitU)] - USERS[get(ideoVar)==-1, weighted.mean(exposureConU, totalVisitU)]
    stopifnot(abs(isolDomain - isolDomainUserCalc)<0.0001)
  }
  
  if (returnShares) {
    return (list(repShare = repShare, libShare = libShare, isol=isol))
  } else {
    return(isol)
  }
}






# Calculate segregation scale in data.table DT
# byAgg - the variable defining the media outlet (e.g., domain)
# slantVar - variable defining slant of post/website
# ideoVar - variable defining the ideology of user
# indVar - variable defining the individual identifier
# userAgg - aggregate first at the user level by var 'userVar' 
# weightsVar - variable used to weight the results
# minVisitors - keep websites with at least minVisitors number of visitors
# includeCurrent - include current user (FALSE = adjusted segregation scale)
calculateSegregation <- function(DT, byAgg, indVar, ideoVar = NULL, slantVar = NULL, weightVar = NULL, 
                                 includeCurrent=FALSE, minVisitors=1, userAgg=TRUE, slantDT = NULL) { 
  library(Hmisc)
  
  if (is.null(ideoVar) && is.null(slantVar)) {
    error("need either slanVar or ideoVar to calculate slant of visits")
  }
  
  # Work with local data.table so global one won't be affected and since cleaner
  keepCols = c(byAgg, indVar, ideoVar, slantVar, weightVar); keepCols = keepCols[keepCols!="NULL"]
  currentDT <- DT[, ..keepCols] 
  
  initialWeightZero = 0
  if(is.null(weightVar) || weightVar=="NULL") {
    weightVar = "weight"
    initialWeightZero = 1
    currentDT[, weight := 1]
  }
  
  # Keep websites with at least X unique visit
  if(includeCurrent==FALSE & minVisitors==0){
    minVisitors=2
  }
  currentDT[, uniqueVisitors := uniqueN(get(indVar)), by=byAgg]
  currentDT <- currentDT[uniqueVisitors>=minVisitors]
  
  # If no slant need to calculate based on visits 
  if (is.null(slantVar) || slantVar=="NULL") {
    
    if (is.null(slantDT)) { 
      findSlantDT = currentDT[!is.na(get(ideoVar))]
    } else  {
      findSlantDT = slantDT[!is.na(get(ideoVar))]
    }
    
    # Splitting up this command to save time
    if(initialWeightZero==1) {
      findSlantDT[, weight := 1]
      findSlantDT[, userVisit := as.double(.N), by = c(byAgg, indVar)]
    } else {
      findSlantDT[, userVisit := sum(get(weightVar)), by = c(byAgg, indVar)]
    }
    
    # Calculate total number of conservative visit in each website
    findSlantDT[, userVisitCon := userVisit*get(ideoVar)==1]
    
    # Calculate the share of liberal and conservative visits
    if (includeCurrent) {
      findSlantDT[, shareConsDomain := weighted.mean(get(ideoVar)==1, get(weightVar)), by=byAgg]
      keepSlantDT = c(byAgg, "shareConsDomain")
      currentDT = merge(currentDT, unique(findSlantDT[, ..keepSlantDT]), by=byAgg)
      
    } else {
      findSlantDT[, shareConsDomain := (sum((get(ideoVar)==1)*get(weightVar))-userVisitCon)/ (sum(get(weightVar))-userVisitCon), by=byAgg]
      keepSlantDT = c(byAgg, indVar, "shareConsDomain")
      currentDT = merge(currentDT, unique(findSlantDT[, ..keepSlantDT]), by=c(byAgg, indVar))
    }
    slantVar = "shareConsDomain"
    
  }
  
  # Don't need weights here, within individual
  USER_SEG = currentDT[, list(slantVarU=mean(get(slantVar)), userWeight = mean(get(weightVar))), by=indVar]
  
  if (userAgg) {
    # Flexman  apge 308, Peterson, page 9. Need either wtd.var, or a function for wt.st and then parenthesis around 2 
    seg = sqrt(2*Hmisc::wtd.var(USER_SEG$slantVarU, USER_SEG$userWeight, normwt = TRUE))
    #seg = sqrt(2)*SDMTools::wt.sd(USER_SEG$slantVarU, USER_SEG$userWeight)
  } else {
    seg = sqrt(2*Hmisc::wtd.var(currentDT[[slantVar]], currentDT[[weightVar]], normwt = TRUE))
    #seg = sqrt(2)*SDMTools::wt.sd(currentDT[[slantVar]], currentDT[[weightVar]])
  }
  return(seg)
}



# Calaculate absolute value of slant
# slantVar - variable defining slant of post/website
# ideoVar - variable defining the ideology of user
# indVar - variable defining the individual identifier
# userAgg - aggregate first at the user level by var 'userVar' 
# weightsVar - variable used to weight the results
# minCount - exclude users with num of observations<minCount
calculateAbsSlant <- function(DT, slantVar, indVar, weightVar=NULL, userAgg=FALSE, minCount=1) {
  
  # Work with local data.table so global one won't be affected. Keep non-NA slant
  keepCols = c(slantVar, indVar, weightVar); keepCols = keepCols[keepCols!="NULL"]
  currentDT <- DT[!is.na(get(slantVar)), ..keepCols] 
  
  if(is.null(weightVar) || weightVar=="NULL") {
    weightVar = "weight"
    currentDT[, weight := 1]
  }
  
  
  # How many data points required per user
  currentDT[, countUser := .N, by=indVar]
  currentDT = currentDT[countUser>=minCount]
  
  
  if (userAgg)  {
    #USER_ABS_SLANT = currentDT[, list(absSlantVarU=mean(get(absSlantVar)), numVisits = .N),by=c(indVar, weightVar)]
    USER_SLANT = currentDT[, list(slantU=mean(get(slantVar)), numVisits = .N),by=c(indVar, weightVar)]
    USER_SLANT[, absSlantU := abs(slantU)]
    stopifnot(sum(duplicated(USER_SLANT, by=indVar))==0)
    return (USER_SLANT[, weighted.mean(absSlantU, get(weightVar))])
  } else {
    return (currentDT[, weighted.mean(abs(get(slantVar)), get(weightVar))])
  }
}


# Calaculate congruenceScale
# slantVar - variable defining slant of post/website
# ideoVar - variable defining the ideology of user
# indVar - variable defining the individual identifier
# userAgg - aggregate first at the user level by var 'userVar' 
# weightsVar - variable used to weight the results
# minCount - exclude users with num of observations<minCount
calculateCongruence <- function(DT, slantVar, ideoVar, indVar, weightVar=NULL, userAgg=FALSE, minCount=2) {
  
  # Work with local data.table so global one won't be affected. Keep non-NA
  keepCols = c(slantVar, ideoVar, indVar, weightVar); keepCols = keepCols[keepCols!="NULL"]
  currentDT <- DT[!is.na(get(ideoVar)), ..keepCols] 
  
  if(is.null(weightVar) || weightVar=="NULL") {
    weightVar = "weight"
    currentDT[, weight := 1]
  }
  
    # How many data points required per user
  currentDT[, countUser := .N, by=indVar]
  currentDT = currentDT[countUser>=minCount]
  
  # Define congruence scale
  
  
  if (userAgg)  {
    USER_CONGRUENCE = currentDT[, list(slantU=mean(slant), numVisits = .N),by=c(indVar, weightVar, ideoVar)]
    stopifnot(sum(duplicated(USER_CONGRUENCE, by=indVar))==0)
    USER_CONGRUENCE[, congruenceU := slantU * get(ideoVar)]
    return (USER_CONGRUENCE[, weighted.mean(congruenceU, get(weightVar))])
  } else {
    currentDT[, congruence := get(slantVar) * get(ideoVar) ]
    return (currentDT[, weighted.mean(congruence, get(weightVar))])
  }
}



# Calaculate share of counter-attitudinal media engagements in data.table DT
# slantBinaryVar - is post/site liberal or conservative
# ideoVar - ideology of user
# userAgg - aggregate first at the user level by var 'userVar' 
# weightsVar - variable used to weight the results
# minCount - exclude users with num of observations<minCount
calculateShareCounter <- function(DT, slantBinaryVar, ideoVar, indVar, weightVar=NULL, userAgg=FALSE, minCount=1) {
  
  # Work with local data.table so global one won't be affected and since cleaner
  keepCols = c(slantBinaryVar, ideoVar, indVar, weightVar); keepCols = keepCols[keepCols!="NULL"]
  currentDT <- data.table::copy(DT[, ..keepCols])
  
  if(is.null(weightVar) || weightVar=="NULL") {
    weightVar = "weight"
    currentDT[, weight := 1]
  }
  
  currentDT = currentDT[!is.na(get(slantBinaryVar)) & !is.na(get(ideoVar)) & get(slantBinaryVar)!=0]
  
  currentDT[, countUser := .N, by=indVar]
  currentDT = currentDT[countUser>=minCount]
  
  currentDT[, counter := (get(slantBinaryVar)==1 & get(ideoVar)==-1) |  (get(slantBinaryVar)==-1 & get(ideoVar)==1)]
  
  if (userAgg)  {
    USER_COUNTER = currentDT[, list(counterU=mean(counter), numVisits = .N),by=c(indVar, weightVar)]
    stopifnot(sum(duplicated(USER_COUNTER, by=indVar))==0)
    return (USER_COUNTER[, weighted.mean(counterU, get(weightVar))])
  } else {
    return (currentDT[, weighted.mean(counter, get(weightVar))])
  }
}
  
  




# Return share of all values in var 'var' in data.table DT. 
# userAgg - aggregate first at the user level by var 'userVar' 
getShare <- function (DT, var, userAgg=FALSE, userVar = NULL) { 
  if (!userAgg) {
    DT[, N := .N]
    shareDT = unique(DT[, .N/N, by=get(var)])
    #shareDT = melt(DT[, prop.table(table(get(var)))])
    shareDT = setnames(shareDT, c("get", "V1"), c("group", "share"))
  } else  {
    keepCols = c(var, userVar)
    shareDT <- DT[, ..keepCols] 
    shareDT[, user := as.character(get(userVar))]
    setnames(shareDT, var, "group")
    
    shareDT = shareDT[, .N, by=c("group", "user")]
    shareDT = as.data.table(tidyr::complete(shareDT, group, user, fill=list(N=0)))
    shareDT = shareDT[, share := N/ sum(N), by="user"][, list(share = mean(share)), by="group"]
  }
  
  #shareAll = as.data.table(rbind(shareFB, shareChannel))
  return(shareDT)
}




