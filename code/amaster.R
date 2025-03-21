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
wd_raw <- paste0("C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/","100MLives/Data/raw/")
wd_inpu <- paste0(wd,"input/")
wd_outp <- paste0(wd,"output/")

#...........................................................
# functions-----
#...........................................................

source("functions_review_6.R")

#...........................................................
# Data inputs-----
#...........................................................

# Blood pressure data
source("get_bp2022.R")

#...........................................................
# Estimation-----
#...........................................................

#...........................................................
# Projection-----
#...........................................................

#...........................................................
# Output-----
#...........................................................


