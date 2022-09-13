# Author: Ro'ee Levy, Yale
# Date: Thu May 30 19:29:04 2019
# Purpose: Import FEC donation data for 2016, 2018 election cycles
# --------------#

rm(list=ls())
source("Code/Utils/log.R"); beginLogging()

library(fst)
library(data.table)

# Directory to store results
dir.create("Datasets/Donations", showWarnings = FALSE)
dir.create("Datasets/Donations/raw", showWarnings = FALSE)



# import zip codes
colNames = names(fread("Input/Donations/Ind/indivHeader.csv"))
dropNames = c("IMAGE_NUM", "OTHER_ID", "TRAN_ID", "FILE_NUM", "SUB_ID", "MEMO_TEXT", "RPT_TP", "TRANSACTION_DT")
dropNumbers = which(colNames %in% dropNames)
keepNames = colNames[!colNames %in% dropNames]

years = c("16", "18")

for (year in years) {
  DONATE <- fread(paste0("Input/Donations/Ind/indiv", year, ".txt"),
                  drop = dropNumbers, col.names = keepNames)

  write_fst(DONATE, paste0("Datasets/Donations/raw/indDonations", year, ".fst"))
  rm(DONATE); gc()
}


# Import committees
comHeader = names(fread("Input/Donations/Committee/cmMasterHeader.csv"))

for (year in years) {
  COM <- fread(paste0("Input/Donations/Committee/cmMaster", year, ".txt"), quote = "")
  names(COM) <- comHeader
  COM = unique(COM)
  save(COM, file = paste0("Datasets/Donations/raw/com", year, ".Rda"))
}


# Import Candidates
candHeader = names(fread("Input/Donations/Cand/candMasterHeader.csv"))

for (year in years) {
  CAND <- fread(paste0("Input/Donations/Cand/candMaster", year, ".txt"))
  names(CAND) <- candHeader
  CAND = unique(CAND)
  save(CAND, file = paste0("Datasets/Donations/raw/cand", year, ".Rda"))
}


  
