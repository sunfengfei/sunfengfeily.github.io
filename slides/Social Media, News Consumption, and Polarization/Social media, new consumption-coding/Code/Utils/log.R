# Author: Ro'ee Levy, Yale
# Date: Sun Sep 13 22:25:53 2020
# Purpose: Functions to generate logs
# --------------#


library(logging)
#logging::basicConfig(level='FINEST')

addHandler(writeToFile, file=paste("Logs/runAll", Sys.Date(), ".log"), level='INFO')

beginLogging <- function () {
  if (!dir.exists(("Logs"))) {
    dir.create("Logs")
  }
  for (i in sys.parents()) {
    currentFile = sys.frame(i)$ofile
    if(!is.null(currentFile) && !grepl("(dailyAnalysis|master)", currentFile)) {
      break;
      print("break")
    }
  }
  currentRelFile = gsub(paste0(getwd(),"/"), "", currentFile)
  if (!is.null(currentFile)) {
    logging::loginfo(paste0("Beginning file ", currentFile))
    print(paste0("Beginning file ", currentFile))
  }
}

endLogging <- function () {
  for (i in sys.parents()) {
    currentFile = sys.frame(i)$ofile
    if(!is.null(currentFile) && !grepl("(dailyAnalysis|master)", currentFile)) {
      break;
    }
  }
  
  if (length(currentFile)>0) {
    logging::loginfo(paste0("---Finished file ", currentFile))
  }
}


