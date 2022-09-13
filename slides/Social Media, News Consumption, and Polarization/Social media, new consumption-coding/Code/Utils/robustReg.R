# Author: Ro'ee Levy, Yale
# Date: Sun Sep 13 22:31:06 2020
# Purpose: Functions to find robust standard errors, print regression tables with robust standard errors, find differences between coefficients
# --------------#

# Important note: this project uses the function lmR, felmR and similar function to for robust standard errors. 
# This function are similar to functions such as estimatr::lm_robust. The main advantage is that they store
# the robust standard errors in a new object called se. 
# When printing the regression tables I call stargazerR, a wrapper to the stargazer function which recognizes the se
# object and prints robust standard errors. 


library("multiwayvcov") #needed for clustering
library("lmtest") # for coeftest
library("sandwich") # for vcovHC
library("stargazer")
library("lfe") #needed for felm


# Return regression coefficient 
getCoef <- function(model, coef) {
  if (class(model)=="felm") {
    return(model$coefficients[which(attributes(model$coefficients)$dimnames[[1]]==coef)])
  }
  if (class(model) %in% c("lm", "glm")) {
    return(coefficients(model)[names(coefficients(model))==coef])
  }
  if (class(model)=="coeftest") {
    return(model[which(attributes(model)$dimnames[[1]]==coef)])
  }
  else (stop("Unrecognized class"))
}


# Create robust lm function 
lmR <-function (formula, data, weights=NULL) {
  #reg = do.call("lm",dots,quote=TRUE) # Takes longer
  if (!is.null(weights)) {
    data$tempWeights <- weights
    reg <- lm(formula=formula, data=data, weights=tempWeights)
  } else {
    reg <- lm(formula=formula, data=data)
  }
  coefResults = coeftest(reg, vcov = vcovHC(reg, "HC1"))
  reg$se <- coefResults[,2]
  reg$pvalues <-coefResults[,4]
  return(reg)
}


# Robust glm function
glmR <-function (formula, data, weights=NULL, family = family) {
  if (!is.null(weights)) {
    data$tempWeights <- weights
    reg <- glm(formula=formula, data=data, weights=tempWeights, family = family)
  } else {
    reg <- glm(formula=formula, data=data, family = family)
  }
  
  coefResults <- coeftest(reg, vcov = vcovHC(reg, "HC1"))
  reg$se <- coefResults[,2]
  reg$pvalues <- coefResults[,4]
  
  return (reg)
}

# Robust negative binomial function
glm.nbR <-function (formula, data, weights=NULL) {
  if (!is.null(weights)) {
    data$tempWeights <- weights
    reg <- MASS::glm.nb(formula=formula, data=data, weights=tempWeights)
  } else {
    reg <- MASS::glm.nb(formula=formula, data=data)
  }
  #reg <- MASS::glm.nb(...)
  
  coefResults <- coeftest(reg, vcov = vcovHC(reg, "HC1"))
  reg$se <- coefResults[,2]
  reg$pvalues <- coefResults[,4]
  
  return (lmtest::coeftest(reg))
}


# Robust felm function 
felmR <-function (formula, data, weights=NULL) {
  if(!is.null(weights)) {
    reg <- felm(formula, data, weights=weights)
  } else {
    reg <- felm(formula, data)
  }
  
  coefResults = coef(summary(reg, robust=TRUE))
  
  reg$se <- coefResults[,2]
  reg$pvalues <- coefResults[,4]
  
  return (reg)
}

# Linear regression with clustered standard errors
lmC <-function (..., cluster) {
  #reg <- lm(..., weights=weights, data = data)
  dots <- list(...)
  argNames = names(dots)
  if(! "data" %in% argNames)  {stop ("error: data arg required")}
  
  reg = do.call("lm",dots,quote=TRUE)
  
  myVOC<-cluster.vcov(reg, dots[["data"]][[cluster]])
  reg$se <-coeftest(reg, vcov = myVOC)[,2]
  reg$pvalues <-coeftest(reg, vcov = myVOC)[,4]
  return (reg)
}



# Probit with cluster
lrmC <-function (...,data,cluster) {
  reg <- lrm(...,data)
  
  newReg <- robcov(reg, data[[cluster]]) 
  return (newReg)
}


# glm with clustered standard errors
glmC <-function (..., data, cluster) {
  reg <- glm(..., data)
  coef <- coeftest(reg,  cluster.vcov(reg, cluster = data[[cluster]]))
  reg$se <- coef[, 2]
  reg$pvalues <- coef[, 4]
  
  return (reg)
}



# Create robust IV
ivregR <-function (...) {
  reg <- ivreg(...)
  
  sink("temp.txt");  # Don't display output
  reg$se=robust.se(reg)[,2]
  reg$pvalues=robust.se(reg)[,4]
  sink()
  
  return (reg)
}

# Robust IV with cluster
ivregC <-function (...,data,cluster) {
  reg <- ivreg(...,data=data)
  naList = na.action(reg)
  if (length(naList)>0) {
    clusterSE = cluster.robust.se(reg, data[!na.action(reg)][[cluster]])
  } else {
    clusterSE = cluster.robust.se(reg, data[[cluster]])
  }
  reg$se = clusterSE[,2]
  reg$pvalues = clusterSE[,4]
  return (reg)
}


# Robust probit
lrmR <-function (...) {
  reg <- lrm(..., x=TRUE, y=TRUE)
  reg <- robcov(reg)
  return (reg)
}



# *************************************************************************
# PRINT REGRESSIONS ----
# *************************************************************************

# StargazerR - this is a wrapper for stargazer that prints robust standard errors. 
# This function works after calling the functions specified in this file to run the regressions
# The function also sets default options for stargazer (float=FALSE, omit stats and constant, remove dep variables, type=text)
# New args:
# file - outputs latex to this file (probably can't use Stargazer out's command if making other chances to latex code)
# replaceCov - replaced covariate names with a named list
# threePartNotes - uses threeparttable package for notes, looks much better
# addTitle - add title in first table row, useful with panels
# removeLatex - grep a specific row and remove it from latex output, useful for panels, 
#   can create two tables and remove the end of the first table and beginning of next table before appending them
stargazerR <- function(..., file=NULL) {
  dots <- list(...)
  seList <- list()
  pvaluesList <- list()
  dotsPrint <- substitute(list(...))
  x = list(NULL)
  #print (dotsPrint) #[-1:-4]

  
  if (class(dots[[1]])=="list") {
    # Note: using a list within stargazer takes much longer and doesn't work properly
    print(class(dots[[1]]))
    print(length(dots[[1]]))
    
    stop("StargazerR doesn't work with lists")
    dots = c(unlist(dots[[1]], recursive = FALSE), dots[-1])
    
    print(dots[[1]])
    print(names(dots[[1]]))
  } 
  

  for (i in seq_along(dots)) {
    if (is.null(names(dots)) || names(dots)[i]=="") {
      if ("se" %in% names(dots[[i]])) {
        seList <- c(seList,list(dots[[i]]$se))
      }
      else {
        seList <- c(seList, list(NULL)) # Null uses default standard errrors if non-specified 
      }
      if ("pvalues" %in% names(dots[[i]])) {
        pvaluesList <- c(pvaluesList,list(dots[[i]]$pvalues))
        
        # This is not needed, but adding quotes saves A LOT of time
        x[[i]] = dots[[i]]
        if (i==1) {dots[[i]] = quote(x[[1]])}
        if (i==2) {dots[[i]] = quote(x[[2]])}
        if (i==3) {dots[[i]] = quote(x[[3]])}
        if (i==4) {dots[[i]] = quote(x[[4]])}
        if (i==5) {dots[[i]] = quote(x[[5]])}
        if (i==6) {dots[[i]] = quote(x[[6]])}
        if (i==7) {dots[[i]] = quote(x[[7]])}
        if (i==8) {dots[[i]] = quote(x[[8]])}
        if (i==9) {dots[[i]] = quote(x[[9]])}
        if (i==10) {dots[[i]] = quote(x[[10]])}
      } else {
        pvaluesList <- c(pvaluesList,list(NULL))
      }
    }
  }
  
  stopifnot(length(seList)==length(pvaluesList))
  dots <-c(dots,se=list(seList),p=list(pvaluesList))

  argNames = names(dots)
  #print(argNames)
  # Model.names=TRUE/FALSE: OLS/probit/ etc. (default TRUE)
  # dep.var.labels.include=TRUE/FALSE: Name of dep variables (default TRUE, change to FALSE)
  # model.numbers=TRUE/FALSE: (1)/(2)/etc.  (default TRUE)
  # dep.var.labels.include=TRUE/FALSE: Name of dep variables (default TRUE, change to FALSE). Identical dependent variables grouped together (but not identical labels)
  # column.labels = additional labels, I use this groups (defualt NULL), column.separate = (2, 2, 1, 3) - define group for labels. 
  if(! "df"                     %in% argNames)  {dots["df"]                     = FALSE}
  if(! "dep.var.labels.include" %in% argNames)  {dots["dep.var.labels.include"] = FALSE}
  if(! "float"                  %in% argNames)  {dots["float"]                  = FALSE}
  if(! "dep.var.caption"        %in% argNames)  {dots["dep.var.caption"]        = ""}
  if(! "omit.stat"              %in% argNames)  {dots[["omit.stat"]]            = c("f","rsq","adj.rsq","ser", "ll", "aic")}
  if(! "omit"                   %in% argNames)  {dots[["omit"]]                 = c("Constant")}
  if(! "type"                   %in% argNames)  {dots["type"]                   = "text"}
  if(! "notes"                  %in% argNames)  {dots["notes"]                  = ""}
  if(! "notes.append"           %in% argNames)  {dots["notes.append"]           = FALSE}
  if(! "notes.label"            %in% argNames)  {dots["notes.label"]            = ""}
  
  if(! "replaceCov"             %in% argNames)  {replaceCov = NULL} else {
    replaceCov = dots["replaceCov"]
    replaceCov = unlist(unlist(replaceCov))
    dots["replaceCov"]             = NULL
  }
  
  threePartNotes = NULL; removeLatex = NULL; addTitle = NULL
  if( "threePartNotes"             %in% argNames)  {
    threePartNotes = unlist(dots["threePartNotes"])
    dots["threePartNotes"]             = NULL
  }
  if( "removeLatex"             %in% argNames)  {
    removeLatex = unlist(dots["removeLatex"])
    dots["removeLatex"]             = NULL
  }
  if( "addTitle"             %in% argNames)  {
    addTitle = unlist(dots["addTitle"])
    dots["addTitle"]             = NULL
  }

  if (!is.null(replaceCov)) {
    if (dots["notes.label"] == "Notes:") {
      dots["notes.label"] = "Notes^^^"
    }
    names(replaceCov) = gsub("replaceCov\\.", "", names(replaceCov))
  
    # Don't display output
    sink(tempfile());  
    result <- do.call("stargazer",dots,quote=FALSE)
    sink()

    result = stringr::str_replace_all(result, unlist(replaceCov))
    if (is.null(file)) {
      print(result)
    }
  }
  else {
    result = do.call("stargazer",dots,quote=FALSE)
  }
  
  if(!is.null(addTitle)) {
    where = min(grep("tabular", result))+2
    rows = length(result)
    result = c(result[1:where], addTitle, result[(where+1):rows])
  }
  if(!is.null(threePartNotes)) {
    result = c("\\begin{threeparttable}", 
               result,
               "\\begin{tablenotes}[flushleft]",
                   "\\small",
               paste("\\item", threePartNotes),
               "\\end{tablenotes}",
               "\\end{threeparttable}")
  }
  if(!is.null(removeLatex)) {
    result = result[!grepl(removeLatex, result)]
  }
  
  if (!is.null(file) ) {
    write(result, file=file)
  }
  if ("type"  %in% argNames & dots[["type"]]=="latex") {
    return (result)
  }
}


# *************************************************************************
# COLLECT DATA ----
# *************************************************************************


# Collect data for a regressions in a tidy dataset
tidyRob <- function (model, name=NULL, ci = 0.9) {
  library(dplyr)
  if (class(model)=="lm") {
    result = (cbind(broom::tidy(coeftest(model, vcov = vcovHC(model, type="HC1"))),
                    #broom::confint_tidy(model, vcov = vcovHC(model), conf.level = ci),
                    confint_robust(model, ci)))
    result = result %>%  mutate(dvName = names(model[["model"]])[1])
  } else if(class(model)=="felm") {
    result = (broom_felm(model, ci))
  }
  result = as.data.table(result)
  if (!is.null(name)) {
    result[, modelName := name]
  }
  return (result[term != "(Intercept)"]) 
}


# Find roobst confidence intervals
confint_robust <- function(model, ci) {
  robSe = coeftest(model, vcov = vcovHC(model, type="HC1"))[,2]
  tt <-qt(c((1-ci)/2, 0.5+0.5*ci),summary(model)$df[2])
  coefs = coef(model, vcov = vcovHC(model, type="HC1"))
  coefs = coefs[!is.na(coefs)]
  robCI <- coefs + (robSe %o% tt)
  
  #robCI = as.data.table(robCI)
  colnames(robCI) = c("conf.low", "conf.high")
  return (robCI)
}

# model = regseen_Pro_All
broom_felm <- function(model, ci=0.9) {
  summaryFE = summary(model, robust=TRUE)
  returnBroom = as.data.table(summaryFE$coefficient)
  returnBroom[, term := rownames(summaryFE$coefficient)]
  returnBroom[, dvName := all.vars(summaryFE$call$formula)[1]]
  print(names(returnBroom))
  s.e.Name = names(returnBroom)[grepl('s.e', names(returnBroom))]
  setnames(returnBroom, old = c("Estimate", s.e.Name, "t value", "Pr(>|t|)"), 
           new = c("estimate", "std.error", "statistic", "p.value"))
  
  tt <-qt(c(0.05,0.95),summaryFE$df[2])
  returnBroom[,conf.low := estimate + std.error * tt[1]]
  returnBroom[,conf.high := estimate + std.error * tt[2]]
  
  return(returnBroom)
}




# *************************************************************************
# FIND DIFFERENCE BETWEEN TWO COEFFCIENTS ----
# *************************************************************************

addParenthesis <-  function(value, digits) {
  paste0("(", formatC(value, digits=digits, format="f"), ")" )
}

# Add star based on pvalue
addStar <-  function(value, pvalue, digits, superscipt=TRUE) {
  paste0(formatC(value, digits=digits, format="f"), 
         ifelse(superscipt, '$^{', ""),
         symnum(pvalue, cutpoints = c(0, .01, .05, .1, 1), symbols = c("***","**","*"," ")),
         ifelse(superscipt, '}$', ""))
}

# .find difference ----
# .........................................................................

# Calaculate F-value for difference between two coefficients in regressions
diffFValue <- function(regList, coefs, returnValues=FALSE, returnSE=FALSE, digits = 3) {
  coef1 = coefs[1]
  coef2 = coefs[2]
  if (class(regList)!="list") {
    regList = list(regList)
  }
  sapply(regList, function(x) { 
    if (class(x)=="felm")  {
      var1 = x$coefficients[which(attributes(x$coefficients)$dimnames[[1]]==coef1)]
      var2 = x$coefficients[which(attributes(x$coefficients)$dimnames[[1]]==coef2)]
      
      # In some cases ' actually appears in variable name. E.g. regPolAdjAffectiveIV = felmR(data=ALL, formula = reformulate(controlsPolBasicFEIV, "polIndexAffective"))
      # In other cases I pass it due to : in interactions
      cleanCoef1 = gsub("`", "", coef1)
      cleanCoef2 = gsub("`", "", coef2)
      
      vcvR = x$robustvcv
      
      # Important note: white-adjusted standard errors do not work for felm. I think it's because the hccm function does not work
      #pValue = car::linearHypothesis(x, equal, white.adjust = "hc2")[["Pr(>Chisq)"]][2]
      
      if (coef1 %in% rownames(vcvR)) {
        wald = lfe::waldtest(x, as.formula(paste("~", cleanCoef1, "-", cleanCoef2)),  type="robust")
        
        # Manually calculate robust se for the difference se(beta1-beta2)=sqrt(var(beta1-beta2))=sqrt[var(beta1)+var(beta2)-2*cov(beta1,beta2)]
        se =  sqrt(vcvR[coef1,coef1] + vcvR[coef2, coef2] - 2*vcvR[coef1, coef2])
      } else { 
        wald = lfe::waldtest(x, as.formula(paste("~", coef1, "-", coef2)),  type="robust")
        se =  sqrt(vcvR[cleanCoef1,cleanCoef1] + vcvR[cleanCoef2, cleanCoef2] - 2*vcvR[cleanCoef1, cleanCoef2])
      }
      
      pValue = wald[["p"]]
      
    } else if(class(x) %in% c("lm", "ivreg")) {
      var1 = x$coefficients[[coef1]] 
      var2 = x$coefficients[[coef2]]
      
      if (class(x)=="ivreg") {
        if(returnValues==FALSE) {
          stop("Don't know how to calculate F Test for ivreg")
        } else {
          return(var1-var2)
        }
      }
      testEqual = car::linearHypothesis(x, paste0(coef1, "=", coef2), white.adjust = "hc1") #hc2
      pValue = testEqual[["Pr(>F)"]][2]
      se = sqrt(attributes(testEqual)[["vcov"]][1])
    } else {
      stop ("unknown class")
    }
    stopifnot(!is.null(pValue))
    
    diff = var1-var2
    
    if (returnValues) {
      return(diff)
    } else if(returnSE) {
      return(addParenthesis(se, digits))
    }
    else {
      return(addStar(diff, pValue, digits))
    }
  })
}


# .print difference ----
# .........................................................................

# Add to table a row with difference between two coefficients
diffFValueAddRow <- function(name, regList, coefs, length=NULL, col=NULL, digits = 3) {
  coef1 = coefs[1]
  coef2 = coefs[2]
  values = diffFValue(regList, coefs, digits= digits)
  se = diffFValue(regList, coefs, returnSE=TRUE, digits= digits)
  if (!is.null(length)) {
    vectorValues = c(name, rep("-", length))
    vectorSe     = c("", rep("-", length))
    counter=1
    for (i in col) {
      vectorValues[i+1] = values[counter]
      vectorSe[i+1] = se[counter]
      counter=counter+1
    }
  } else {
    vectorValues = c(name, values)
    vectorSe     = c("", se)
  }
  
  return(list( c(vectorValues), c(vectorSe)))
}





