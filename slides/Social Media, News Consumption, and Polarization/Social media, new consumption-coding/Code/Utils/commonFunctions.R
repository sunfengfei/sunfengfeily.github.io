# Author: Ro'ee Levy, Yale
# Date: Sun Sep 13 22:00:26 2020
# Purpose: Two common functions: define a new factor variable and set the label attributes
# --------------#


# Create factor based on labels attribute
factorWithLabel <- function(x, exclude=NA) {  
  levels = attr(x, "labels")
  labels = names(attr(x, "labels"))
  additionalLevels = unique(x)[! unique(x) %in% c(levels, exclude)]
  
  return (factor(x, levels=c(levels, additionalLevels), labels=c(labels, additionalLevels), exclude=exclude))
}


# Turn a variable 'var' into a factor in dataset 'DT'
# copyAtt - copy label and labels attribtes
# removeOrig - remove origianl variable from dataset DT
# exclude - exclude specific values
setFactor <- function(DT, var, exclude=NA, copyAtt=FALSE, removeOrig=FALSE) {
  factorVar = paste0(var,"F")
  
  if ("labels" %in% names(attributes(DT[[var]]))) {
    DT[, (factorVar) := factorWithLabel(get(var), exclude=exclude)]
  } else {
    DT[, (factorVar) := factor(get(var), exclude=exclude)]
  }
  
  if (copyAtt) {
    setattr(DT[[factorVar]], "label", attributes(DT[[var]])$label)
    setattr(DT[[factorVar]], "labels", attributes(DT[[var]])$labels)
  }
  
  if (removeOrig) {
    DT[, (var) := NULL]
  }
  
  return(DT)
}


# Set 'label' attributes in data.table DT
setattrLabel <- function(DT, var, label) {
  setattr(DT[[var]], "label", label)
}
