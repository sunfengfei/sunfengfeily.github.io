# Author: Ro'ee Levy, Yale
# Date: Sat Dec 01 00:12:37 2018
# Purpose: Useful functions when creating an outcome index
# --------------#

library(data.table)

# count non-na values in x
countNonNA <- function(x) sum(!is.na(x))

# Replace NA with mean
replaceNaMean <- function(x) {
  if (is.numeric(x)) {
    if (sum(is.na(x))>0) {
      print(paste0("replacing na ", mean(x, na.rm = TRUE)))
    }
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  } else {
    x
  }
}



# Scale all data with respect to a control group
scaleControl <- function(data, control = NULL) {
  if (!is.list(data)) {
    data = list(data)
  }
  if (is.null(control)) {
    return(scale(data))
  }
  
  returnData = lapply(data, function(x) return((x-mean(x[control==TRUE], na.rm=TRUE))/sd(x[control==TRUE], na.rm=TRUE)))
  return (returnData)
}


# Create an index with inverse-covariance weights (Anderson 2008) for covariates x 
# directions - if directions are provided all variables are first reoriented to have the same interpretation. directions should be 1 or -1
# scale - should each covariate be standardized before creating the index
# scaleIndex - should the final index be standardized 
# control - if a control group is provided and the index should be standardized, it is standardized with respect to the control group
createAndersonIndex <- function(x, control=NULL, directions=NULL, scale=TRUE, scaleIndex=TRUE) {
  if (!is.null(directions)) {
    x = x * matrix(rep(directions, nrow(x)), byrow = TRUE, ncol=ncol(x) )
  }
  
  if (scale) {
    scaled = simplify2array(scaleControl(x, control))
  } else {
    scaled = simplify2array(x)
  }
  inversedCov = solve(cov(scaled)) # After taking into account positive value in X[1,1] and X[2,2], X[1,2] will be negative, we already expect them to go together
  weights = rowSums(inversedCov)
  normWeights = weights / sum(weights)
  index = t(normWeights %*% t(scaled))
  
  if (scaleIndex==TRUE) {
    if (!is.null(control)) {
      index = scaleControl(index, control)
    } else {
      index = scale(index)
    }
  }
  else {
    index = list(index)
  }
  
  return (list(index = index, weights=normWeights))
}


# Create an index with equal weihgts for covariates x 
# directions - if directions are provided all variables are first reoriented to have the same interpretation. directions should be 1 or -1
# scale - should each covariate be standardized before creating the index
# scaleIndex - should the final index be standardized 
# control - if a control group is provided and the index should be standardized, it is standardized with respect to the control group
createStandardIndex <- function(x, control=NULL, directions=NULL, scale=TRUE, scaleIndex=TRUE) {
  if (!is.null(directions)) {
    x = x * matrix(rep(directions, nrow(x)), byrow = TRUE, ncol=ncol(x) )
  }
  if (scale) {
    scaled = simplify2array(scaleControl(x, control))
  } else {
    scaled = simplify2array(x)
  }
  index = rowMeans(scaled, na.rm=TRUE)
  
  if (scaleIndex==TRUE) {
    if (!is.null(control)) {
      index = scaleControl(index, control)
    } else {
      index = scale(index)
    }
  } else {
    index = list(index)
  }
  
  return(index)
}

# Create an index for covariates x 
# directions - if directions are provided all variables are first reoriented to have the same interpretation. directions should be 1 or -1
# weights - the weights of each variable in the index
# scaleIndex - should the final index be standardized 
# control - if a control group is provided and the index should be standardized, it is standardized with respect to the control group
createIndex <- function  (x, control=NULL, directions=NULL, weights, scaleIndex=TRUE) {
  if (!is.null(directions)) {
    x = x * matrix(rep(directions, nrow(x)), byrow = TRUE, ncol=ncol(x) )
  }
  scaled = simplify2array(scaleControl(x, control))
  testthat::expect_equal(ncol(x), length(weights))
  findNAs = x %>% mutate_all(function(x) !is.na(x))
  index = (weights %*% t(scaled))
  
  if (scaleIndex==TRUE) {
    if (!is.null(control)) {
      index = scaleControl(index, control)
    } else {
      index = scale(index)
    }
  } 
  
  return (as.vector(unlist(index)))
}


# Create missing variable in data.table data. 
# vars - the list of vars to create. For each var two new vars are created:
#     - varHave which is an indicator for whether the variable is not missing
#     - varWM is the variable with a constant where the original variable is missing
createMissing <- function(data, vars, warn=FALSE) {
  if(is.list(vars)) {
    vars = unlist(vars)
  }

  for (var in vars) {
    if (nrow(data[is.na(get(var))])==0) {
      if (warn) {
        print("No missing values")
      }
    }  else {
      data[, eval(paste0(var,"Have")) := !is.na(get(var))]
      data[, eval(paste0(var,"WM")) := ifelse(is.na(get(var)), 0, get(var))]
    }
  }
  return(data)
}


# Create weights for data DT based on means provided in NATIONAL. 
# id - the indiviual level identifier
# uesWeights - the names of covariate to use for creating the weights
# The function calls a stata script and return the weights created
addWeights <- function (DT, NATIONAL, useWeights, id="ResponseId") {
  
  if ("SampleWeight" %in% names(DT)) {
    stop("weights already exist")
  }
  
  reweightVarsWithId = c(useWeights, id)
  DATA = data.table::copy(DT[, ..reweightVarsWithId])
  DATA = DATA[, lapply(.SD, replaceNaMean)]
  
  NATIONAL[, eval(id) := NA]
  
  NEW_DATA = rbindlist(list(DATA[, ..useWeights][, national := 0], 
                            NATIONAL[, ..useWeights][, national := 1],
                            NATIONAL[, ..useWeights][, national := 1]), use.names = TRUE)
  treatment = NEW_DATA[, national]
  NEW_DATA[, national := NULL]
  
  library(ebal)
  results = ebalance(Treatment=treatment, X=NEW_DATA, print.level = 3)
  returnWeight = as.data.table(list(tempName=DATA[, get(id)], SampleWeight = results$w))
  setnames(returnWeight, "tempName", id)
  return (returnWeight)
}
