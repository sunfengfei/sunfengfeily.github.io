# Author: Ro'ee Levy, Yale
# Date: Thu May 30 17:26:58 2019
# Purpose: Import 2007, 2008, 2017, 2018 Comscore demographic and browsing data 
# --------------#
  
library(data.table)
library(fst)

rm(list=ls())
source("Code/Utils/log.R"); beginLogging()

windows = Sys.info()[["sysname"]]=="Windows"


# *************************************************************************
# SESSIONS ----
# *************************************************************************

dir.create("Datasets/Comscore", showWarnings = FALSE)
dir.create("Datasets/Comscore/raw", showWarnings = FALSE)


# .2007 - 25 minutes----
# .........................................................................

for (i in 1:4) {
  print(paste0("Importing Comscore 2017 Q", i))
  # unzip fails in unix with large files
  if (windows) {
    zipList = unzip(paste0("Input/ComScore/07/sessions07Q", i, ".zip"), list=TRUE)
    comScore07 <- fread(unzip(paste0("Input/ComScore/07/sessions07Q", i, ".zip")), 
                        select = c("machine_id", "ref_domain_name", "domain_name", "event_time", "event_date"),
                        colClasses = c("machine_id"="factor"))
    file.remove(zipList$Name)
  } else {
    comScore07 <- fread(paste0("Input/ComScore/07/sessions07Q", i, ".csv"), 
                        select = c("machine_id", "ref_domain_name", "domain_name", "event_time", "event_date"),
                        colClasses = c("machine_id"="factor"))
  }
  
  comScore07[, timePOSIX := fasttime::fastPOSIXct(paste0("2007:", substr(event_date,5,6), ":", substr(event_date,7,8), ":", event_time), required.components = 6, tz="GMT")]
  comScore07[, c("event_date", "event_time") :=NULL]
  
  # 2007 too big to handle later saving as two files
  N = comScore07[, .N]
  half = ceiling(N/2)
  
  write.fst(comScore07[1:half], paste0("Datasets/Comscore/raw/comScore07Q", i, "aRaw.fst"))
  write.fst(comScore07[(half+1):(N)], paste0("Datasets/Comscore/raw/comScore07Q", i, "bRaw.fst"))
  
  rm(comScore07); gc()
}


# .2008----
# .........................................................................

print("Importing Comscore 2008")

if (windows) {
  zipList = unzip(paste0("Input/ComScore/08/sessions08.zip"), list=TRUE)
  comScore08 <- fread(unzip("Input/ComScore/08/sessions08.zip"), 
                    select = c("machine_id", "ref_domain_name", "domain_name", "event_time", "event_date"),
                    colClasses = c("machine_id"="factor"))
  file.remove(zipList$Name)
  
} else {
  comScore08 <- fread(unzip("Input/ComScore/08/sessions08.zip"), 
                    select = c("machine_id", "ref_domain_name", "domain_name", "event_time", "event_date"),
                    colClasses = c("machine_id"="factor"))
}


comScore08[, timePOSIX := fasttime::fastPOSIXct(paste0("2008:", substr(event_date,5,6), ":", substr(event_date,7,8), ":", event_time), required.components = 6, tz="GMT")]
comScore08[, c("event_date", "event_time") :=NULL]

# 2008 too big to handle later saving as two files
N = comScore08[, .N]
half = ceiling(N/2)

write.fst(comScore08[1:half], paste0("Datasets/Comscore/raw/comScore08aRaw.fst"))
write.fst(comScore08[(half+1):(N)], paste0("Datasets/Comscore/raw/comScore08bRaw.fst"))

rm(comScore08); gc()



# 2017, 2018 ----
# .........................................................................


for (i in c("17", "18")) {
  print(paste0("Importing Comscore ", i))
  
  if (windows) {
    # get name of csv to delete it late
    zipList = unzip(paste0("Input/ComScore/", i, "/sessions", i, ".zip"), list=TRUE)
    comScore <- fread(unzip(paste0("Input/ComScore/", i, "/sessions", i, ".zip")), 
                      select = c("machine_id", "ref_domain__name", "domain_name", "event_time", "event_date"),
                      colClasses = c("machine_id"="factor"))
    file.remove(zipList$Name)
  } else {
    comScore <- fread(paste0("Input/ComScore/", i, "/sessions", i, ".csv"), 
                      select = c("machine_id", "ref_domain__name", "domain_name", "event_time", "event_date"),
                      colClasses = c("machine_id"="factor"))
  }
  
  gc()
  comScore[, timePOSIX := fasttime::fastPOSIXct(paste0("20", i, ":", substr(event_date,5,6), ":", substr(event_date,7,8), ":", event_time), required.components = 6, tz="GMT")]
  
  comScore[, c("event_date", "event_time") :=NULL]
  
  setnames(comScore, "ref_domain__name", "ref_domain_name")
  
  write.fst(comScore, paste0("Datasets/Comscore/raw/comScore", i, "Raw.fst"))
  
  rm(comScore); gc()
}


# *************************************************************************
# DEMOGRAPHIS ----
# *************************************************************************

for (i in c("07", "08", "17", "18")) {
  assign(paste0("csDemo", i), fread(paste0("Input/ComScore/", i, "/demographics", i, ".csv"), 
                                    colClasses = c("zip_code"="character", "machine_id"="factor")))
  save(list = paste0("csDemo", i), file = paste0("Datasets/Comscore/raw/csDemo", i, ".Rda"))
}
  
  


