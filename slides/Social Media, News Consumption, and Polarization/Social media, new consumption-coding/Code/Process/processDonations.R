# Author: Ro'ee Levy, Yale
# Date: Sat May 30 09:42:52 2020
# Purpose: Process donation data to determine ideology by zipcode
# --------------#


rm(list=ls())
library(fst)
library(data.table)

source("Code/Utils/log.R")
beginLogging()


years = c("16", "18")
for (year in years) {
  DONATE <- read_fst(paste0("Datasets/Donations/raw/indDonations", year, ".fst"), as.data.table = TRUE, 
                      columns = c("CMTE_ID", "NAME", "CITY", "STATE", "ZIP_CODE"))

  load(file = paste0("Datasets/Donations/raw/com", year, ".Rda"))
  load(file = paste0("Datasets/Donations/raw/cand", year, ".Rda"))
  
  COM = merge(COM, CAND, by="CAND_ID", all.x=TRUE)
  rm(CAND); gc(); 
  
  COM[, party := ifelse(!is.na(CAND_PTY_AFFILIATION), CAND_PTY_AFFILIATION, CMTE_PTY_AFFILIATION)]
  
  # Keep only democrats and republicans
  COM = COM[party %in% c("DEM", "REP"), list(CMTE_ID, party)]; gc()

  DONATE = merge(DONATE[, list(CMTE_ID, NAME, CITY, STATE, ZIP_CODE)], COM, by="CMTE_ID")
  
  assign(paste0("DONATE_", year), DONATE)
  rm(DONATE); gc()
}
  

DONATE = rbindlist(list(DONATE_16[, period := "1618"], DONATE_18[, period := "1618"]), use.names = TRUE)
rm(COM, DONATE_16, DONATE_18); gc()



# We will use 5 digit zipcode for comscore
DONATE[, zip5 := substr(ZIP_CODE, 1, 5)]



# Keep only valid zip code (some invalid from out of the country), only donations that can associate with a party, about 15 seconds
donationsZip <- DONATE[ZIP_CODE!="" & !grepl("[a-zA-Z]", ZIP_CODE), list(party, NAME, CITY, STATE, ZIP_CODE, zip5, period)]

# Include each donor only once
donationsZip = unique(donationsZip)

# Count donors by party
donationsZip = donationsZip[, list(countZipParty = .N), by=c("zip5", "party", "period")]
donationsZip = dcast(donationsZip, zip5+period~party, value.var = "countZipParty") 
setnames(donationsZip, c("DEM", "REP"), c("dem", "rep"))

donationsZip[is.na(dem), dem:=0]
donationsZip[is.na(rep), rep:=0]
donationsZip[, shareRepDonations := rep/(rep+dem)]
donationsZip[, totalDonors := rep+dem]
donationsZip = dcast(donationsZip, zip5~period, value.var = c("shareRepDonations", "totalDonors"))

donationsZip = donationsZip[, list(zip5, shareRepDonations_1618)]
attr(donationsZip$zip5, 'label') <- "Zip Code"
attr(donationsZip$shareRepDonations_1618, 'label') <- "Share Donations to Rebpublicans 2016-2018"


attr(donationsZip, "description") <- "Share of Republican Donations by Zip Code"
attr(donationsZip, "source") <- "FEC Donation Data"
attr(donationsZip, "access") <- "Available"

saveRDS(donationsZip, file = "Datasets/Donations/donationsZip.rds")
