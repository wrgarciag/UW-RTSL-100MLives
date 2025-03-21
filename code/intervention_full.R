rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd("../data_preprocessing")
setwd(paste0("C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/GitHub/UW-RTSL-100MLives/"))

#libraries
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

wd <- "C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/GitHub/UW-RTSL-100MLives/"

wd_code <- paste0(wd,"code/")
wd_inpu <- paste0(wd,"input/")
wd_outp <- paste0(wd,"output/")

#...........................................................
# Data inputs-----
#...........................................................

countrylist <- read.csv("super_regions.csv", stringsAsFactors=FALSE)%>%filter(location!="Global", 
                                                                              location!="American Samoa",
                                                                              location!="Andorra",
                                                                              location!= "Bermuda",
                                                                              location!= "Dominica",
                                                                              location!="Greenland",
                                                                              location!="Marshall Islands",
                                                                              location!="Northern Mariana Islands",
                                                                              location!="Palestine",
                                                                              location!="Taiwan (Province of China)",
                                                                              location!="Guam",
                                                                              location!="Puerto Rico",
                                                                              location!="South Sudan",
                                                                              location!="Virgin Islands, U.S.")%>%pull(location)


names<-read.csv("Country_groupings_extended.csv", stringsAsFactors = F)%>%
  select(location_gbd, gbd2019, iso3)%>%
  rename(location = location_gbd)

#add covid mx data
load("wpp.adj.Rda")

#Covid mx ~= excess mortality
# b_rates <- read.csv(file=paste0('C:/Users/wrgar/OneDrive - UW/02Work/ResolveToSaveLives/100MLives/Data/',"base_rates_2022.csv"))
# b_rates <- as.data.table(b_rates)
# b_rates <- b_rates[location %in% c('Colombia','United States'),]
# 
# saveRDS(b_rates,file=paste0("base_rates_2022.rds"))
b_rates<- readRDS(file=paste0("base_rates_2022.rds"))

b_rates<-left_join(b_rates, wpp.adj%>%
                     rename(location = location_name)%>%
                     select( -Nx, -mx, -iso3))

#add location-specific CFAARC estimates
df<-read.csv("IHME-CFR.csv", stringsAsFactors = F)
cfr<-df%>%select(-c(lower, upper, measure, age, metric))%>%
  filter(cause!="All causes")%>%
  spread(year, val)%>%
  mutate(CFAARC = 1+log(`2019`/`2009`)/10)

cfr$cause[cfr$cause == "Ischemic heart disease"]<-"ihd"
cfr$cause[cfr$cause == "Hypertensive heart disease"]<-"hhd"
cfr$cause[cfr$cause == "Ischemic stroke"]<-"istroke"
cfr$cause[cfr$cause == "Intracerebral hemorrhage"]<-"hstroke"

cfr<-left_join(cfr%>%rename(gbd2019 = location), names)%>%
  filter(location %in% countrylist)%>%
  select(-c(gbd2019, iso3, `2019`, `2009`))

##add reduction in bg.mx over time
all<-df%>%select(-c(lower, upper, measure, age, metric))%>%
  filter(cause=="All causes")%>%
  spread(year, val)%>%
  mutate(BGAARC = 1+log(`2019`/`2009`)/10)

all<-left_join(all%>%rename(gbd2019 = location), names)%>%
  filter(location %in% countrylist)%>%
  select(-c(gbd2019, iso3, `2019`, `2009`, cause))

b_rates<-left_join(b_rates, all)%>%select(-c(CFAARC))
b_rates<-left_join(b_rates, cfr)

#adjust rates
b_rates[year>=2020, CF :=CF*(CFAARC^(year-2019))]
b_rates[year>=2020, BG.mx :=BG.mx*(BGAARC^(year-2019))]
b_rates[year>=2020, BG.mx.all :=BG.mx.all*(BGAARC^(year-2019))]

#add age-20 cohort projections from demographic model
pop20<-read.csv("PopulationsAge20_2050.csv", stringsAsFactors = F)

b_rates<-left_join(b_rates, pop20%>%rename(Nx2=Nx, year=year_id)%>%filter(year>=2017), 
                   by=c("location", "year", "sex", "age"))%>%
  mutate(Nx = ifelse(is.na(Nx2), Nx, Nx2), pop=ifelse(is.na(Nx2), pop, Nx2))%>%
  select(-c(Nx2))

#blood pressure data calculated in file: "Blood pressure.R"
data.in<-fread("bp_data6.csv")%>%rename(location = location_gbd)%>%select(-Year, -Country)
data.in$salt[data.in$location=="China"]<-4.83*2.54
length(unique(data.in$location))

#add scale-up data
inc<-read.csv("covfxn2.csv", stringsAsFactors = F)%>%
  select(iso3, location, Year, aroc, p_change, a_change, refwsalt, aspwsalt, reach_base,
         aroc2, p_change2, a_change2, ideal)

bpcats<-c("<120", "120-129", "130-139", 
       "140-149", "150-159", "160-169", 
       "170-179", "180+")

data.in<-merge(bpcats, data.in)%>%rename(bp_cat = x)

source("functions_review_6.R")

#ref<-fread("global_pop_0122.csv")
#check rates
b_rates[is.na(covid.mx), covid.mx:=0]
b_rates[covid.mx>=1, covid.mx:=0.9]

#rebalance TPs w/ covid such that they sum to less than 1
#especially @ old ages where covid deaths are high
b_rates[,check_well := BG.mx+covid.mx+IR]
b_rates[,check_sick := BG.mx+covid.mx+CF]

#first ensure that background mortality + covid <1
b_rates[check_well>1 | check_sick>1, covid.mx:=ifelse(1-BG.mx<covid.mx, 1-BG.mx, covid.mx)]
#then proportionally reduce rates by check_well
b_rates[check_well>1, covid.mx:= covid.mx - covid.mx*(check_well-1)/(covid.mx+BG.mx+IR)]
b_rates[check_well>1, BG.mx   := BG.mx    - BG.mx*   (check_well-1)/(covid.mx+BG.mx+IR)]
b_rates[check_well>1, IR      := IR       - IR*      (check_well-1)/(covid.mx+BG.mx+IR)]

b_rates[,check_well := BG.mx+covid.mx+IR]
b_rates[check_well>1]

#same process for check_sick
b_rates[check_sick>1, covid.mx:= covid.mx - covid.mx*(check_sick-1)/(covid.mx+BG.mx+CF)]
b_rates[check_sick>1, BG.mx   := BG.mx    - BG.mx*   (check_sick-1)/(covid.mx+BG.mx+CF)]
b_rates[check_sick>1, CF      := CF       - CF*      (check_sick-1)/(covid.mx+BG.mx+CF)]

b_rates[,check_sick := BG.mx+covid.mx+CF]
b_rates[check_sick>1]

#check that no BG.mx.all+covid>1
b_rates[covid.mx+BG.mx.all>1]


#fxn
repYear<-function(row){
  2017+floor((row-1)/224)
}

data.in<-data.table(data.in%>%select(-age)%>%rename(age=Age.group))
b_rates[, newcases:=0]
#not adjusting IR because we're essentially doing that with bau coverage
#b_rates[year>2019, IR:=IR*(0.997)^(year-2019)]


#.........................
# Model function
#.........................

# This function set the intervention effect
intervention.set <- function(df,year.inte,target.mam,target.cbe,target.trt,year.target) {
  
  intervention_rates<-as.data.table(left_join(intervention_rates, inc%>%rename(year=Year), by=c("location","year")))
  
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="ihd",     CF:=CF*(1-0.24*aroc)]
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="istroke", CF:=CF*(1-0.36*aroc)]
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="hstroke", CF:=CF*(1-0.76*aroc)]
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="hhd",     CF:=CF*(1-0.20*aroc)]
  
  if(drugcov=="p75"){
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="ihd",     CF:=CF*(1-0.24*p_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="istroke", CF:=CF*(1-0.36*p_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hstroke", CF:=CF*(1-0.76*p_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hhd",     CF:=CF*(1-0.20*p_change)]
  }
  
  if(drugcov=="p975"){
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="ihd",     CF:=CF*(1-0.24*a_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="istroke", CF:=CF*(1-0.36*a_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hstroke", CF:=CF*(1-0.76*a_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hhd",     CF:=CF*(1-0.20*a_change)]
  }
  
  if(drugcov=="ideal"){
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="ihd",     CF:=CF*(1-0.24*ideal)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="istroke", CF:=CF*(1-0.36*ideal)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hstroke", CF:=CF*(1-0.76*ideal)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hhd",     CF:=CF*(1-0.20*ideal)]
  }

  return(intervention_rates)
}
  
  
  
project.all <- function(Country, saltmet, salteff, saltyear2, drugcov){
 
  base_rates<-b_rates[location==Country]#[, -c("year")]
  
  #intervention scenarios
  
  DT<-unique(data.in[location==Country][,Year:=2017][,-c("Lower95", "Upper95")])
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  
  bp_prob_salt<-get.bp.prob(DT.in, salteff, saltmet, 2023, saltyear2, 1, "baseline")
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_drug<-get.bp.prob(DT.in, 0, "app", 2023, saltyear2, 1, drugcov)
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_both<-get.bp.prob(DT.in, salteff, saltmet, 2023, saltyear2, 1, drugcov)
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_base<-get.bp.prob(DT.in, 0, "app", 2023, saltyear2, 0, "baseline")
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_bau<-get.bp.prob(DT.in, 0, "app", 2023, saltyear2, 1, "baseline")
  
  bp_prob_salt[,intervention:="Salt reduction"]
  bp_prob_drug[,intervention:="Antihypertensive therapy"]
  bp_prob_both[,intervention:="Both"]
  bp_prob_bau[,intervention:="b.a.u"]

  setnames(bp_prob_base, "prob", "prob_0")
  
  bp_probs<-bind_rows(bp_prob_both, bp_prob_drug, bp_prob_salt, bp_prob_bau)
  bp_probs<-merge(bp_probs, bp_prob_base, by=c("age","sex", "bp_cat", "Year", "location")) #change to "Year"
  
  #duplicating data to be age-specific
  bp_probs[, age:=as.numeric(substr(age, 1,2))]
  bp_probs<-bp_probs[rep(seq_len(nrow(bp_probs)), each=5)]
  bp_probs[,age2:=rep(1:5, nrow(bp_probs)/5)][,age:=age+age2-1]
  
  over90<-bp_probs[age==89]
  
  over90<-over90[rep(seq_len(nrow(over90)), each=6)]
  over90[,age2:=rep(1:6, nrow(over90)/6)][,age:=age+age2]
  
  #bind  
  bp_probs<-rbindlist(list(bp_probs, over90))
  
  ##add RRis##
  addRR<-function(RR, bp){
    if(bp=="<120"){1}
    else if (bp=="120-129"){1/RR}
    else if (bp=="130-139"){1/RR^2}
    else if (bp=="140-149"){1/RR^3}
    else if (bp=="150-159"){1/RR^4}
    else if (bp=="160-169"){1/RR^5}
    else if (bp=="170-179"){1/RR^6}
    else {1/RR^7}
  }
  
  bp_probs[, RRi_IHD:=sapply(bp_cat, addRR, RR=0.83)]
  bp_probs[, RRi_HHD:=sapply(bp_cat, addRR, RR=0.72)]
  bp_probs[, RRi_stroke:=sapply(bp_cat, addRR, RR=0.73)]
 
  
  ##add alphas##
  alphas<-bp_probs[,.(ihd=sum(prob_0*RRi_IHD), istroke=sum(prob_0*RRi_stroke), 
                  hstroke=sum(prob_0*RRi_stroke), hhd=sum(prob_0*RRi_HHD)), 
                  by=.(age, sex, location, intervention, Year)] #change to "Year"
  
  alphas<-melt(alphas, id.vars=c("age", "sex", "location", "intervention", "Year"), measure.vars=c(), variable.name = "cause",
       value.name="alpha")#change to "Year"
  
  
  rris<-bp_probs[,list(age, sex, Year, location, intervention, bp_cat, prob, RRi_IHD, RRi_HHD, RRi_stroke)]#change to "Year"
  rris[,hstroke:=RRi_stroke]
  
  setnames(rris, c("RRi_IHD", "RRi_HHD", "RRi_stroke"), c("ihd", "hhd","istroke"))
  rris<-melt(rris, id.vars=c("age", "sex", "location", "intervention", "bp_cat", "prob", "Year"), measure.vars=c(), variable.name = "cause",
               value.name="RRi")#change to "Year"
  
  bp_probs<-merge(rris, alphas, by=c("age", "sex", "location", "intervention","cause", "Year"))#change to "Year"
  setnames(bp_probs, "Year", "year")
  
  ####adding baseline_rates
  intervention_rates<-merge(bp_probs, base_rates, by=c("age", "sex", "location", "cause", "year"))

  #calculating yi*pi
  intervention_rates[, yixpi:=(RRi*IR/alpha)*prob]
  intervention_rates[, IR:=sum(yixpi), by=.(age, sex, location, intervention, cause, CF, 
                                            BG.mx, BG.mx.all, PREVt0, DIS.mx.t0, Nx, year, ALL.mx)]#change to "Year"

  intervention_rates<-unique(intervention_rates[,-c("prob", "bp_cat", "yixpi", "RRi", "alpha")])
  
  ##add CF effects##
  #this is ugly code
  
  intervention_rates<-as.data.table(left_join(intervention_rates, inc%>%rename(year=Year), by=c("location","year")))
  
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="ihd",     CF:=CF*(1-0.24*aroc)]
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="istroke", CF:=CF*(1-0.36*aroc)]
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="hstroke", CF:=CF*(1-0.76*aroc)]
  intervention_rates[intervention%in%c("b.a.u", "Salt reduction") & cause=="hhd",     CF:=CF*(1-0.20*aroc)]
  
  if(drugcov=="p75"){
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="ihd",     CF:=CF*(1-0.24*p_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="istroke", CF:=CF*(1-0.36*p_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hstroke", CF:=CF*(1-0.76*p_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hhd",     CF:=CF*(1-0.20*p_change)]
  }
  
  if(drugcov=="p975"){
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="ihd",     CF:=CF*(1-0.24*a_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="istroke", CF:=CF*(1-0.36*a_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hstroke", CF:=CF*(1-0.76*a_change)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hhd",     CF:=CF*(1-0.20*a_change)]
  }
  
  if(drugcov=="ideal"){
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="ihd",     CF:=CF*(1-0.24*ideal)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="istroke", CF:=CF*(1-0.36*ideal)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hstroke", CF:=CF*(1-0.76*ideal)]
    intervention_rates[intervention%in%c("Both", "Antihypertensive therapy") & cause=="hhd",     CF:=CF*(1-0.20*ideal)]
  }

  ## calculate initial states for the incoming year 2000 and all years for age 20 population
  intervention_rates[year==2017 | age==20, sick:=Nx*PREVt0]
  intervention_rates[year==2017 | age==20, dead:=Nx*DIS.mx.t0]
  intervention_rates[year==2017 | age==20, well:=Nx*(1-(PREVt0+ALL.mx))]
  
  
  #base_rates<-base_rates[location %in% countrylist]
  intervention_rates[age==20 | year==2017, pop:=Nx]
  intervention_rates[age==20 | year==2017, all.mx:=Nx*ALL.mx]
  
  intervention_rates[CF>0.99, CF:=0.99]
  intervention_rates[IR>0.99, IR:=0.99]
  
  #STATE TRANSITIONS#
  for(i in 1:41){
    
    b2<-intervention_rates[year<=2017+i & year>=2017+i-1]
    b2[,age2:=age+1]
    
    #newcases
    b2[, newcases2:=shift(well)*IR, by=.(sex, location, cause, age, intervention)]
    
    #sick
    b2[, sick2:=shift(sick)*(1-(CF+BG.mx+covid.mx)) + shift(well)*IR, by=.(sex, location, cause, age, intervention)]
    b2[sick2<0, sick2:=0]
    
    #dead
    b2[, dead2:=shift(sick)*CF, by=.(sex, location, cause, age, intervention)]
    b2[dead2<0, dead2:=0]
    
    #pop
    b2[,pop2:=shift(pop)-shift(all.mx), by=.(sex, location, cause, age, intervention)]
    b2[pop2<0, pop2:=0] #prevent negatives
    
    #all dead envelope
    b2[,all.mx2:=sum(dead2), by=.(sex, location, year, age, intervention)]
    b2[,all.mx2:=all.mx2+(pop2*BG.mx.all)+(pop2*covid.mx)] #UPDATE w/ covid data
    b2[all.mx2<0, all.mx:=0]
    
    #well
    b2[, well2:=pop2-all.mx2-sick2]
    b2[well2<0, well2:=0] #prevent negatives
    
    #re-combined into original data.table
    b2<-b2[year==2017+i & age2<96, c("age2", "newcases2", "sick2", "dead2", "well2", "pop2", 
                                     "all.mx2", "sex", "location", "cause", "intervention")]
    setnames(b2, "age2", "age")
    intervention_rates[year==2017+i & age>20, newcases:=b2[, newcases2]]
    intervention_rates[year==2017+i & age>20, sick:=b2[,sick2]]
    intervention_rates[year==2017+i & age>20, dead:=b2[,dead2]]
    intervention_rates[year==2017+i & age>20, well:=b2[,well2]]
    intervention_rates[year==2017+i & age>20, pop:=b2[,pop2]]
    intervention_rates[year==2017+i & age>20, all.mx:=b2[,all.mx2]]
    
  }
  
  out.df<-intervention_rates[, c("age", "cause", "sex", "year", "well", "sick", "newcases",
                         "dead", "pop", "all.mx", "intervention", "location")]
  
  return(out.df)
  
  
}#as a fxn


#test that it doesn't crash before running for ~15 mins
#this takes ~15 seconds
test<-project.all("Colombia",  "percent", 0.3, 2024, "p75")
p<-test%>%group_by(year, intervention)%>%
  summarise(dead = sum(dead),
            sick=sum(sick))

ggplot(p, aes(x=year, y=dead, color=intervention))+
  geom_point()

ggplot(p, aes(x=year, y=sick, color=intervention))+
  geom_point()

save(test,file = paste0(wd_outp,"BCA_BHealthOutcomesProgressive.rds"))

fwrite(test,file = paste0(wd_outp,"BCA_BHealthOutcomesProgressive.csv"))

#Inputs GBD 2021
data.in <- as.data.table(data.in)
dt_gbd19 <- data.in[location=="Colombia",]

dt_base <- b_rates[year==2021,]
fwrite(dt_gbd19,file = "BaseInputRiskFactorGBD2019.csv")
fwrite(dt_base,file = "BaseInputStatesGBD2019.csv")