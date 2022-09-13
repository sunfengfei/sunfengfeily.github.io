# Author: Ro'ee Levy, Yale
# Date: Thu Nov 15 23:14:53 2018
# Purpose: Import outlet dataset
# --------------#

rm(list=ls())

library(data.table)

source("Code/Utils/log.R")
beginLogging()


## Import file with 500 sites for Bakshy etal (2015)
TOP500 <- fread('Input/Media/top500.csv', stringsAsFactors = F, data.table=TRUE)

dir.create("Datasets/Media/raw", showWarnings = FALSE)
saveRDS(TOP500, file='Datasets/Media/raw/top500Raw.rds')

