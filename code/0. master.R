rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

setwd(paste0("C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/GitHub/UW-RTSL-100MLives/"))

#libraries
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(readxl)   

wd <- "C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/GitHub/UW-RTSL-100MLives/"

wd_code <- paste0(wd,"code/")

# Raw data not available on GitHub
wd_raw <- paste0("C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/","100MLives/Data/raw/")

# Processed data (from base rates and tps)
wd_data <- paste0(wd,"data/processed/")
wd_outp <- paste0(wd,"output/")

#...........................................................
# functions-----
#...........................................................
source("functions_review_6.R")

#...........................................................
# 1. Base rates (State and Risks)-----
#...........................................................

#...........................................................
# 2. Transition Probabilities-----
#...........................................................

#...........................................................
# 3. Calibration-----
#...........................................................

#...........................................................
# 4. Model-----
#...........................................................

#...........................................................
# 5. Output-----
#...........................................................
