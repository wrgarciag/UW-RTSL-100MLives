rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#libraries
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(RColorBrewer)


#...........................................................
# GBD 2019 Data
#...........................................................

#data permalink
#https://vizhub.healthdata.org/gbd-results?params=gbd-api-2019-permalink/463a40833819742df724b697ba2cc03f
dt<-bind_rows(read.csv("data/IHME-GBD_2019_DATA-d0d7a8c8-1.csv", stringsAsFactors = F),
              read.csv("data/IHME-GBD_2019_DATA-d0d7a8c8-2.csv", stringsAsFactors = F))
dt<-data.table(dt)
dt[, upper:=NULL]
dt[, lower:=NULL]

unique(dt$location)
unique(dt$cause)
unique(dt$age)

#using GBD 2019 population estimates
pop0<-fread("IHME_GBD_2019_POP_SYA_2000_Y2021M01D28.csv")
pop1<-fread("IHME_GBD_2019_POP_SYA_2001_Y2021M01D28.csv")
pop2<-fread("IHME_GBD_2019_POP_SYA_2002_Y2021M01D28.csv")
pop3<-fread("IHME_GBD_2019_POP_SYA_2003_Y2021M01D28.csv")
pop4<-fread("IHME_GBD_2019_POP_SYA_2004_Y2021M01D28.csv")
pop5<-fread("IHME_GBD_2019_POP_SYA_2005_Y2021M01D28.csv")
pop6<-fread("IHME_GBD_2019_POP_SYA_2006_Y2021M01D28.csv")
pop7<-fread("IHME_GBD_2019_POP_SYA_2007_Y2021M01D28.csv")
pop8<-fread("IHME_GBD_2019_POP_SYA_2008_Y2021M01D28.csv")
pop9<-fread("IHME_GBD_2019_POP_SYA_2009_Y2021M01D28.csv")
pop10<-fread("IHME_GBD_2019_POP_SYA_2010_Y2021M01D28.csv")
pop11<-fread("IHME_GBD_2019_POP_SYA_2011_Y2021M01D28.csv")
pop12<-fread("IHME_GBD_2019_POP_SYA_2012_Y2021M01D28.csv")
pop13<-fread("IHME_GBD_2019_POP_SYA_2013_Y2021M01D28.csv")
pop14<-fread("IHME_GBD_2019_POP_SYA_2014_Y2021M01D28.csv")
pop15<-fread("IHME_GBD_2019_POP_SYA_2015_Y2021M01D28.csv")
pop16<-fread("IHME_GBD_2019_POP_SYA_2016_Y2021M01D28.csv")
pop17<-fread("IHME_GBD_2019_POP_SYA_2017_Y2021M01D28.csv")
pop18<-fread("IHME_GBD_2019_POP_SYA_2018_Y2021M01D28.csv")
pop19<-fread("IHME_GBD_2019_POP_SYA_2019_Y2021M01D28.csv")

gbdpop<-rbindlist(list(pop0,pop1,pop2,pop3,pop4,pop5,pop6,pop7,
                       pop8,pop9,pop10,pop11,pop12,pop13,pop14,
                       pop15,pop16,pop17,pop18,pop19))%>%filter(sex_name!="both" & location_id!=533) #Georgia the state

gbdpop$age_group<-as.numeric(gbdpop$age_group_name)
gbdpop$age_group[gbdpop$age_group_name=="<1 year"]<-0
gbdpop$age_group[gbdpop$age_group_name=="95 plus"]<-95

names(gbdpop)[2]<-"location"
totalpop<-gbdpop%>%mutate(iso3 = countrycode::countrycode(location, "country.name", "iso3c"))
#totalpop[,location:=NULL]

totalpop$sex_name[totalpop$sex_name=="male"]<-"Male"
totalpop$sex_name[totalpop$sex_name=="female"]<-"Female"

# GBD rates As a function----

#Country<-"India"
#yr<-2000

project.all <- function(Country, yr){
  
  gbd_data<-dt[year==yr]
  IHME_DEMO<-totalpop%>%filter(year_id==yr)

pop.df <- IHME_DEMO %>% rename(sex = sex_name, age = age_group, Nx=val) %>%
  select(location, sex, age, Nx) %>% filter(location==Country & age > 19)

## function to get the rates ----

other.rates <- function(met, meas, name, sel){
  df    <- gbd_data %>% filter(metric==met & measure==meas & location==Country) %>%
    mutate(midptage = as.numeric(substr(age,1,2)) + 2) %>% arrange(sex, cause, midptage) %>%
    select(sex, midptage, val, cause) %>% spread(cause, val) %>% 
    mutate(ihd     = `All causes` - `Ischemic heart disease`,  
           istroke = `All causes` - `Ischemic stroke`, 
           hstroke = `All causes` - `Intracerebral hemorrhage`,
           hhd     = `All causes` - `Hypertensive heart disease`,
           dm      = `All causes` - `Diabetes mellitus type 2`,
           ckd1    = `All causes` - `Chronic kidney disease due to hypertension`,
           ckd2    = `All causes` - `Chronic kidney disease due to diabetes mellitus type 2`) 
  
  if (sel == 1){df  <- df %>%  mutate(ihd = `Ischemic heart disease`, 
                                      istroke = `Ischemic stroke`, 
                                      hstroke = `Intracerebral hemorrhage`, 
                                      hhd = `Hypertensive heart disease`, 
                                      all =`All causes`,
                                      dm = `Diabetes mellitus type 2`,
                                      ckd1 = `Chronic kidney disease due to hypertension`,
                                      ckd2 = `Chronic kidney disease due to diabetes mellitus type 2`)}
  
  df   <- df %>% select(sex, midptage, ihd, istroke, hstroke, hhd, all, dm, ckd1, ckd2) %>% arrange(sex, midptage)
  
  interpolate.rate <- function(y){approx(c(seq(22,92,5), 95) ,y, xout=20:95, rule=2, method="linear")$y}
  
  df.rates <- df %>% filter(sex=="Female") %>% select(ihd, istroke, hstroke, hhd, all, dm, ckd1, ckd2) %>% as.matrix() %>%
    apply(., 2, interpolate.rate) %>% data.table() %>% mutate(sex = "Female", age = 20:95)
  df.rates <- df %>% filter(sex=="Male") %>% select(ihd, istroke, hstroke, hhd, all, dm, ckd1, ckd2) %>% as.matrix() %>%
    apply(., 2, interpolate.rate) %>% data.table() %>% mutate(sex = "Male", age = 20:95) %>%
    rbind(df.rates) %>% gather(cause, val, -sex, -age) %>% mutate(val = val/100000, location = Country, year = yr)
  setnames(df.rates, "val", name)
  df.rates
}


## background rates -----

prev.rates  <- other.rates("Rate","Prevalence","PREVt0", 1)
death.rates <- other.rates("Rate","Deaths","DIS.mx.t0", 1)

bg.rates<-death.rates%>%spread(cause, DIS.mx.t0)
bg.rates$BG.mx.all<-bg.rates$all-(bg.rates$ihd+ bg.rates$hhd+bg.rates$istroke+bg.rates$hstroke+bg.rates$dm+bg.rates$ckd1++bg.rates$ckd2)
bg.rates$BG.mx.ihd<-bg.rates$all-bg.rates$ihd
bg.rates$BG.mx.hhd<-bg.rates$all-bg.rates$hhd
bg.rates$BG.mx.istroke<-bg.rates$all-bg.rates$istroke
bg.rates$BG.mx.hstroke<-bg.rates$all-bg.rates$hstroke
bg.rates$BG.mx.dm<-bg.rates$all-bg.rates$dm
bg.rates$BG.mx.ckd1<-bg.rates$all-bg.rates$ckd1
bg.rates$BG.mx.ckd2<-bg.rates$all-bg.rates$ckd2

bg.rates<-bg.rates%>%select(-c(hhd, ihd, istroke, hstroke, dm, ckd1, ckd2))%>%
  gather(cause, BG.mx, -BG.mx.all, -age, -sex, -location, -year, -all)

bg.rates$cause[bg.rates$cause=="BG.mx.ihd"]<-"ihd"
bg.rates$cause[bg.rates$cause=="BG.mx.hhd"]<-"hhd"
bg.rates$cause[bg.rates$cause=="BG.mx.istroke"]<-"istroke"
bg.rates$cause[bg.rates$cause=="BG.mx.hstroke"]<-"hstroke"
bg.rates$cause[bg.rates$cause=="BG.mx.dm"]<-"dm"
bg.rates$cause[bg.rates$cause=="BG.mx.ckd1"]<-"ckd1"
bg.rates$cause[bg.rates$cause=="BG.mx.ckd2"]<-"ckd2"


bg.rates<-bg.rates%>%rename(ALL.mx = all)
names(bg.rates)

jvars          <- c("age","sex","location","year")

baseline_rates<-left_join(bg.rates, prev.rates, by = c(jvars, "cause")) %>% # background + prev
  left_join(death.rates, by = c(jvars, "cause")) %>% # death rates
  left_join(pop.df, by = c("location", "sex", "age")) %>%
  rename(Nx = Nx) %>% arrange(sex, cause, age)

baseline_rates$location <- Country

baseline_rates

}

## export file----
locs<-unique(dt$location[dt$location!="Global"])

for(is in locs){
data.out<-project.all(is, 2000)

for(j in 1:19){
dat<- project.all(is, 2000+j)
data.out<-bind_rows(dat, data.out)
}

any(is.na(data.out))
unique(data.out$cause)

data.out<-data.out%>%mutate(cause = ifelse(cause=="dm", "Diabetes mellitus type 2",
                                    ifelse(cause=="ihd", "Ischemic heart disease",
                                    ifelse(cause=="hhd", "Hypertensive heart disease",
                                    ifelse(cause=="istroke", "Ischemic stroke", 
                                    ifelse(cause=="hstroke", "Intracerebral hemorrhage",
                                    ifelse(cause=="ckd1", "Chronic kidney disease due to hypertension",
                                           "Chronic kidney disease due to diabetes mellitus type 2"
                            )))))))

unique(data.out$cause)

write.csv(data.out, paste0("baseline_rates_", is, ".csv"), row.names = FALSE)

}

#...........................................................
# Sodium data ----
#...........................................................

#...........................................................
# Blood Pressure data ----
#...........................................................
source("get_bp2022.R")


#...........................................................
# Lipid data ----
#...........................................................


#...........................................................
# TFA data ----
#...........................................................

