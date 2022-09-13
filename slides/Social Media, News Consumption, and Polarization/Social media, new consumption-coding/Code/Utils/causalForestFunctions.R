# Author: Ro'ee Levy, Yale
# Date: Sun Sep 13 21:54:36 2020
# Purpose: Functions to run causal forest analysis
# --------------#


findVarImportance <- function(forest, cov, countImportance) {
  # Important of variables
  covSummary = variable_importance(forest)
  covSummary = data.table(name = colnames(cov), importance = covSummary[,1])
  
  if(! NA %in% cov) {
    project = best_linear_projection(forest, cov) 
    projectDT = as.data.table(as.matrix.data.frame(project, rownames.force = T), keep.rownames = TRUE)
    names(projectDT)[2:5] = colnames(project)[1:4]
    covSummary = merge(covSummary, projectDT, by.x = "name", by.y = "rn")
    
  } else {
    maxCount = min(countImportance, nrow(covSummary)-1)
    checkVarNames = covSummary[name!="(Intercept)"][order(-importance), name][1:maxCount]
    for (checkVarName in checkVarNames) {
      
      colNum = which(colnames(cov)==checkVarName)
      testVar = colnames(cov)[colNum]
      
      testVarValues = unique(cov[, colNum])
      testVarValues = testVarValues[!is.na(testVarValues)]
      
      if (length(testVarValues)>9) { 
        testVarValues = quantile(cov[, colNum], seq(0.1, 0.9, 0.1), na.rm=TRUE)
      }
      
      X.test <- matrix(0, nrow(cov)*length(testVarValues), ncol(cov))
      X.test[, colNum] <- rep(testVarValues, nrow(cov))
      
      # This takes a while, with 9*11000 rows * 82 = 8.3 M - 45 seconds, with 9*11K*469=46M 55sec, binary 12
      # but works well, people with more likes less likely to see
      testPredict <- predict(forest, X.test)
      testPredict = as.data.table(list(prediction = testPredict$predictions, 
                                       varValue = rep(testVarValues, nrow(cov))))
      
      assign(paste0("reg", testVar), lmR(data=testPredict, prediction ~ varValue))
      covSummary[name==checkVarName, c("manualEstimate", "p.value") := 
                   tidyRob(get(paste0("reg", testVar)))[, c("estimate", "p.value")]]
      
      #stargazerR(testVarReg)
      #testPredict[, mean(prediction), keyby="varValue"]
    }
  }
  replaceNumeric = names(covSummary)[sapply(covSummary, class)=="numeric"]
  covSummary[, c(replaceNumeric) := lapply(.SD, function(x) round(x, digits = 8)),  .SDcols = replaceNumeric ]
  covSummary = covSummary[order(-importance)]
  return(covSummary)
}



projectForest <- function(cov, outcome, treat, predictCov=NULL,  predictCovEx=NULL, importance=TRUE, countImportance=5,
                          returnCalbiration = FALSE) {
  
  # Run causal forest
  forest <- causal_forest(X = cov, Y=outcome, W=treat)
  
  # Store results
  results = as.data.table(list(name=c(c("ate","predicted", "predicted_error", "projected", "projected_combined",
                                        "mean.forest.prediction", "differential.forest.prediction")), 
                               value=as.numeric(NA), se=as.numeric(NA), pvalue=as.numeric(NA)))
  
  # Calibration
  if (returnCalbiration) {
    calibration = test_calibration(forest)
    results[name=="mean.forest.prediction", c("value", "se", "pvalue") := 
              as.list(calibration["mean.forest.prediction",c("Estimate", "Std. Error", "Pr(>t)")]) ]
    results[name=="differential.forest.prediction", c("value", "se", "pvalue") := 
              as.list(calibration["differential.forest.prediction",c("Estimate", "Std. Error", "Pr(>t)")]) ]
  }
  
  # Return important of specfic variables, takes a long time
  if (importance) {
    covSummary = findVarImportance(forest, cov, countImportance)
  }
  
  # ATE
  results[name=="ate", c("value", "se") := as.list(average_treatment_effect(forest))]
  
  # This predict again using out of sample
  predictEndline <- predict(forest,  estimate.variance=TRUE)
  results[name=="predicted", value := mean(predictEndline$predictions)]
  results[name=="predicted", se := mean(sqrt(predictEndline$variance.estimates))]
  
  # Excess error should be negligible compared to variance estimate, can grow more trees if not negligible
  results[name=="predicted_error", value := mean(predictEndline$excess.error / predictEndline$variance.estimates)]
  
  # Find heterogeneity
  if (!is.null(predictCov)) {
    predictBaseline <- predict(forest, predictCov)
    results[name=="projected", value := mean(predictBaseline$predictions)]
  }
  
  if (!is.null(predictCovEx)) {
    predictBaselineEx <- predict(forest, predictCovEx)
    results[name=="projected_combined", value := mean(c(predictBaselineEx$predictions, predictEndline$predictions))]  
  }
  
  if (importance) {
    return(list(results, covSummary))
  } else {
    return(list(results))
  }
}

# DATA = DATA; depName = "slant_FBScale"; covNames = controlVars; matchTreat=FALSE; importance=TRUE; countImportance=5
projectForestWrapper <- function(DATA, depName, covNames, matchTreat=FALSE, importance=TRUE, countImportance=5) {
  
  # Need to remove attributes for forest
  attributes(DATA[[depName]]) = NULL
  
  # Define outcome, covariates, and treatment
  outcome = DATA[!is.na(get(depName)), get(depName)]
  cov = model.matrix(~., data=DATA[!is.na(get(depName)), ..covNames])
  if (matchTreat) {
    treat = DATA[!is.na(get(depName)), ifelse(matchTreatment=="Pro", 1, 0)]
  } else  {
    treat = DATA[!is.na(get(depName)), ifelse(treatment=="Conservative", 1, 0)]
  }
  
  predictCov = model.matrix(~., data=DATA[, ..covNames])
  #predictCovEx = model.matrix(~., data=DATA[is.na(get(depName)), ..covNames])
  
  return(projectForest(cov=cov, outcome = outcome, treat = treat, predictCov = predictCov,  
                       importance=importance, countImportance = countImportance))
  
}

