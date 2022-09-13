# Author: Ro'ee Levy, Yale
# Date: Wed Sep 16 11:06:01 2020
# Purpose: Setup R environment
# --------------#

rm(list=ls())


options(install.packages.check.source = "no")

pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    print(paste0("\n\n\n*****Installing package ", x))
    install.packages(x)
    if(!require(x,character.only = TRUE)) { 
      stop("Package not found")
    }
  }
  return("OK")
}


listOfPackages = c("data.table", "dplyr", "broom", "tidyr", "fst", # managing dataset
                   "scales", "stargazer", "kableExtra", "Hmisc", # printing results 
                   "ggplot2", "ggpubr",  # Graphics 
                   "lfe", "sandwich", "multiwayvcov", "lmtest", "car", "grf", "ebal", # stat. analysis
                   "logging", # logging
                   "quanteda") # text analysis



results <- sapply(as.list(listOfPackages), pkgTest)
