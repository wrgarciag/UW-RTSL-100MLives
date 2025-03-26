#################################################################################################
#################################################################################################
###########################################
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#################################################################################################

#libraries
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(readxl)

#################################################################################################

#add covid mx data
load("htn_model/wpp.adj.Rda")

wpp.adj<-wpp.adj%>%
      mutate(location_name = ifelse(location_name=="North Korea", "Democratic People's Republic of Korea", location_name))
#Covid mx ~= excess mortality
b_rates<-fread("tps_adjusted_searo.csv")
b_rates<-left_join(b_rates, wpp.adj%>%
                     rename(location = location_name)%>%
                     select( -Nx, -mx, -iso3))

locs<-unique(b_rates$location)
#add age-20 cohort projections from demographic model
pop20<-read.csv("htn_model/PopulationsAge20_2050.csv", stringsAsFactors = F)%>%
      mutate(location = ifelse(location=="North Korea", "Democratic People's Republic of Korea", location))%>%
      filter(location %in% locs)

unique(pop20$location)

b_rates<-left_join(b_rates, pop20%>%rename(Nx2=Nx, year=year_id)%>%filter(year>=2017), 
                   by=c("location", "year", "sex", "age"))%>%
  mutate(Nx = ifelse(is.na(Nx2), Nx, Nx2), pop=Nx)%>%
  select(-c(Nx2))

#blood pressure data calculated in file: "Blood pressure.R"
data.in<-fread("htn_model/bp_data6.csv")%>%rename(location = location_gbd)%>%select(-Year, -Country)
#data.in<-fread("bp_data3.csv")%>%select(-Year, -V1)%>%rename(Lower95 = Low95CI, Upper95 = High95CI)
#data.in<-left_join(data.in, names)
data.in$salt[data.in$location=="China"]<-4.83*2.54
length(unique(data.in$location))

unique(data.in$location)
#add scale-up data
inc<-read.csv("htn_model/covfxn2.csv", stringsAsFactors = F)%>%
  select(iso3, location, Year, aroc, p_change, a_change, refwsalt, aspwsalt, reach_base,
         aroc2, p_change2, a_change2, ideal)%>%
      mutate(location = ifelse(location=="North Korea", "Democratic People's Republic of Korea", location))%>%
      filter(location %in% locs)

bpcats<-c("<120", "120-129", "130-139", 
       "140-149", "150-159", "160-169", 
       "170-179", "180+")

data.in<-merge(bpcats, data.in)%>%rename(bp_cat = x)%>%
      mutate(location = ifelse(location=="North Korea", "Democratic People's Republic of Korea", location))%>%
      filter(location %in% locs)


unique(data.in$location)
any(is.na(data.in))
#source("functions_review_4.R")
source("htn_model/functions_review_6.R")

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

############
###fxn
repYear<-function(row){
  2017+floor((row-1)/224)
}

data.in<-data.table(data.in%>%select(-age)%>%rename(age=Age.group))
b_rates[, newcases:=0]

##repeat rates for years 2020-2050
rep<-b_rates%>%filter(year==2019)

for (i in 2020:2050){
  b_rates<-bind_rows(b_rates, rep%>%mutate(year=i))
}


#Pop estimates for 2023, age 30-79
#pop<-b_rates%>%filter(year==2023, cause=="Ischemic heart disease", age>=30, age<80)%>%
#      group_by(location)%>%
#      summarise(pop = sum(Nx))

#write.csv(pop, "pop_for_targets.csv", row.names = F)

#Pop estimates for 2023, age 30-95
pop<-b_rates%>%filter(year==2023, cause=="Ischemic heart disease", age>=30, age<96)%>%
  group_by(location, age)%>%
  summarise(pop = sum(Nx))

write.csv(pop, "pop_for_targets3.csv", row.names = F)
#save.image(file = "dm-cvd/appData.Rda")



#SEARO scenario 1 (change sheet for other scenarios)
#set targets for HTN/DM model
targets<-read_excel("IND HTN_DM_targets_2June.xlsx", sheet="SEARO1")%>%
              filter(metric %in% c("Treated/HTN"))%>%
              select(location, `baseline (%)`, `target (%)`)%>%
              rename(htn.trt.base = `baseline (%)`,
                     htn.trt.target = `target (%)`)%>%
  left_join(., read_excel("IND HTN_DM_targets_2June.xlsx", sheet="SEARO1")%>%
              filter(metric %in% c("Controlled/HTN"))%>%
              select(location, `baseline (%)`, `target (%)`)%>%
              rename(htn.ctrl.base = `baseline (%)`,
                     htn.ctrl.target = `target (%)`))%>%
  left_join(., read_excel("IND HTN_DM_targets_2June.xlsx", sheet="SEARO1")%>%
              filter(metric %in% c("Treated/DM"))%>%
              select(location, `baseline (%)`, `target (%)`)%>%
              rename(dm.trt.base = `baseline (%)`,
                     dm.trt.target = `target (%)`))%>%
  left_join(., read_excel("IND HTN_DM_targets_2June.xlsx", sheet="SEARO1")%>%
              filter(metric %in% c("Controlled/DM"))%>%
              select(location, `baseline (%)`, `target (%)`)%>%
              rename(dm.ctrl.base = `baseline (%)`,
                     dm.ctrl.target = `target (%)`))

inc<-left_join(inc, targets)%>%
                  mutate(aroc = 0,
                  htn_ctrl = ifelse(Year>2022, (htn.ctrl.target - htn.ctrl.base)*(Year-2022)/(2025-2022), 0),
                  htn_ctrl = ifelse(Year>=2025, htn.ctrl.target - htn.ctrl.base, htn_ctrl),
                  htn_trt = ifelse(Year>2022, (htn.trt.target - htn.trt.base)*(Year-2022)/(2025-2022), 0),
                  htn_trt = ifelse(Year>=2025, htn.trt.target - htn.trt.base, htn_trt),
                  htn_cov = htn_ctrl + htn_trt/2, #assume half theimpact for treatment w/o control #2nd prevention
                  dm_trt = ifelse(Year>2022, (dm.trt.target - dm.trt.base)*(Year-2022)/(2025-2022), 0),
                  dm_trt = ifelse(Year>=2025, dm.trt.target - dm.trt.base, dm_trt),
                  dm_ctrl = ifelse(Year>2022, (dm.ctrl.target - dm.ctrl.base)*(Year-2022)/(2025-2022), 0),
                  dm_ctrl = ifelse(Year>=2025, dm.ctrl.target - dm.ctrl.base, dm_ctrl),
                  dm_cov = dm_ctrl + dm_trt/2)%>%  #assume half the impact for treatment w/o control #2nd prevention
      select(iso3, location, Year, aroc, htn_ctrl, htn_trt, htn_cov, dm_cov)%>%
      mutate(htn_ctrl = ifelse(htn_ctrl<0, 0, htn_ctrl),
             htn_trt = ifelse(htn_trt<0, 0, htn_trt),
             htn_cov = ifelse(htn_cov<0, 0, htn_cov),
             dm_cov = ifelse(dm_cov<0, 0, dm_cov))%>%
      filter(location %in% locs)%>%
  arrange(location)

unique(inc$location)
#Country<-"India"
#saltmet<-"app" 
#salteff<-0
#saltyear2<-2027
#drugcov<-"p975"

#Issue with North KOrea IR an CF for ages under 40 for some reason...
#Fix with zeros for now...

b_rates<-b_rates%>%
      mutate(IR = ifelse(is.na(IR), 0, IR),
             CF = ifelse(is.na(CF), 0, CF))

#################################################################################################
# As a function
#################################################################################################
project.all <- function(Country, saltmet, salteff, saltyear2, drugcov){
  #################################################################################################
  base_rates<-b_rates[location==Country & year>=2017]#[, -c("year")]
  #################################################################################################
  #################################################################################################
  #intervention scenarios
  DT<-unique(data.in[location==Country][,Year:=2017][,-c("Lower95", "Upper95")])
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  
  bp_prob_dm<-get.bp.prob(DT.in, salteff, saltmet, 2023, saltyear2, 1, "baseline")
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_htn<-get.bp.prob(DT.in, 0, "app", 2023, saltyear2, 1, drugcov)
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_both<-get.bp.prob(DT.in, salteff, saltmet, 2023, saltyear2, 1, drugcov)
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_base<-get.bp.prob(DT.in, 0, "app", 2023, saltyear2, 0, "baseline")
  DT.in<-as.data.table(left_join(DT[rep(seq(1,nrow(DT)), 34)][, Year:=repYear(.I)], inc%>%select(-location), by=c("iso3","Year")))
  bp_prob_bau<-get.bp.prob(DT.in, 0, "app", 2023, saltyear2, 1, "baseline")
  
  bp_prob_dm[,intervention:="Diabetes treatment only"]
  bp_prob_htn[,intervention:="Antihypertensive therapy only"]
  bp_prob_both[,intervention:="Both"]
  bp_prob_bau[,intervention:="b.a.u"]

  setnames(bp_prob_base, "prob", "prob_0")
  
  bp_probs<-bind_rows(bp_prob_both, bp_prob_htn, bp_prob_dm, bp_prob_bau)
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
  bp_probs[, RRi_ckd:=sapply(bp_cat, addRR, RR=0.95)]
 
  
  ##add alphas##
  alphas<-bp_probs[,.(ihd=sum(prob_0*RRi_IHD), istroke=sum(prob_0*RRi_stroke), 
                  hstroke=sum(prob_0*RRi_stroke), hhd=sum(prob_0*RRi_HHD),
                  ckd=sum(prob_0*RRi_ckd)),
                  by=.(age, sex, location, intervention, Year)] #change to "Year"
  
  alphas<-melt(alphas, id.vars=c("age", "sex", "location", "intervention", "Year"), measure.vars=c(), variable.name = "cause",
       value.name="alpha")#change to "Year"
  
  
  rris<-bp_probs[,list(age, sex, Year, location, intervention, bp_cat, prob, RRi_IHD, RRi_HHD, RRi_stroke, RRi_ckd)]#change to "Year"
  rris[,hstroke:=RRi_stroke]
  
  setnames(rris, c("RRi_IHD", "RRi_HHD", "RRi_stroke", "RRi_ckd"), c("ihd", "hhd","istroke", "ckd"))
  rris<-melt(rris, id.vars=c("age", "sex", "location", "intervention", "bp_cat", "prob", "Year"), measure.vars=c(), variable.name = "cause",
               value.name="RRi")#change to "Year"
  
  bp_probs<-merge(rris, alphas, by=c("age", "sex", "location", "intervention","cause", "Year"))#change to "Year"
  setnames(bp_probs, "Year", "year")
  
  #rename causes
  unique(bp_probs$cause)
  bp_probs<-bp_probs%>%mutate(cause = ifelse(cause=="ihd" , "Ischemic heart disease",
                                              ifelse(cause=="hhd", "Hypertensive heart disease",
                                                     ifelse(cause=="istroke", "Ischemic stroke", 
                                                            ifelse(cause=="hstroke", "Intracerebral hemorrhage", 
                                                                   "Chronic kidney disease due to hypertension")))))
  
  #unique(bp_probs$year)
  #unique(base_rates$year)
  #unique(bp_probs$cause)
  #unique(base_rates$cause)
  
  #need to add lines for diabetes and ckd due to dm
  
  add_dm<-bp_probs%>%filter(cause == "Ischemic heart disease")%>%
    mutate(RRi = 1,
           alpha = 1,
           cause = "Diabetes mellitus type 2")
  
  add_ckd<-bp_probs%>%filter(cause == "Ischemic heart disease")%>%
        mutate(RRi = 1,
               alpha = 1,
               cause = "Chronic kidney disease due to diabetes mellitus type 2")
  
  bp_probs<-bind_rows(bp_probs, add_dm, add_ckd)
  
  ####adding baseline_rates
  intervention_rates<-merge(bp_probs, base_rates, by=c("age", "sex", "location", "cause", "year"))

  #calculating yi*pi
  #for ages 30-95
  intervention_rates[, yixpi:=(RRi*IR/alpha)*prob]
  intervention_rates[age>30, IR:=sum(yixpi), by=.(age, sex, location, intervention, cause, CF, 
                                            BG.mx, BG.mx.all, PREVt0, DIS.mx.t0, Nx, year, ALL.mx)]#change to "Year"

  intervention_rates<-unique(intervention_rates[,-c("prob", "bp_cat", "yixpi", "RRi", "alpha")])
  
  ##add CF effects##

  intervention_rates<-as.data.table(left_join(intervention_rates, inc%>%rename(year=Year), by=c("location","year")))
  
  #intervention_rates[intervention%in%c("b.a.u", "Diabetes treatment only") & cause=="Ischemic heart disease",     CF:=CF*(1-0.24*aroc2)]
  #intervention_rates[intervention%in%c("b.a.u", "Diabetes treatment only") & cause=="Ischemic stroke", CF:=CF*(1-0.36*aroc2)]
  #intervention_rates[intervention%in%c("b.a.u", "Diabetes treatment only") & cause=="Intracerebral hemorrhage", CF:=CF*(1-0.76*aroc2)]
  #intervention_rates[intervention%in%c("b.a.u", "Diabetes treatment only") & cause=="Hypertensive heart disease",  CF:=CF*(1-0.20*aroc2)]
  
  intervention_rates[age>=30 & intervention%in%c("Diabetes treatment only") & cause=="Ischemic heart disease", IR:=IR*(1-0.17*dm_cov)]
  intervention_rates[age>=30 & intervention%in%c("Diabetes treatment only") & cause=="Diabetes mellitus type 2", CF:=CF*(1-0.28*dm_cov)]
  intervention_rates[age>=30 & intervention%in%c("Diabetes treatment only") & cause=="Chronic kidney disease due to diabetes mellitus type 2", CF:=CF*(1-0.16*dm_cov)]
  
    intervention_rates[age>=30 & intervention%in%c("Both", "Antihypertensive therapy only") & cause=="Ischemic heart disease",     CF:=CF*(1-0.24*htn_cov)]
    intervention_rates[age>=30 & intervention%in%c("Both", "Antihypertensive therapy only") & cause=="Ischemic stroke", CF:=CF*(1-0.36*htn_cov)]
    intervention_rates[age>=30 & intervention%in%c("Both", "Antihypertensive therapy only") & cause=="Intracerebral hemorrhage", CF:=CF*(1-0.76*htn_cov)]
    intervention_rates[age>=30 & intervention%in%c("Both", "Antihypertensive therapy only") & cause=="Hypertensive heart disease",     CF:=CF*(1-0.20*htn_cov)]
    
    intervention_rates[age>=30 & intervention%in%c("Both") & cause=="Ischemic heart disease",     IR:=IR*(1-0.17*dm_cov)]
    intervention_rates[age>=30 & intervention%in%c("Both") & cause=="Diabetes mellitus type 2",     CF:=CF*(1-0.28*dm_cov)]  
    intervention_rates[age>=30 & intervention%in%c("Both") & cause=="Chronic kidney disease due to diabetes mellitus type 2", CF:=CF*(1-0.16*dm_cov)]

    
  #any(is.na(intervention_rates))  
  ##########################################################################################

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

test<-project.all("Bangladesh",  "app", 0, 2025, "p975")

p<-test%>%group_by(year, intervention)%>%
  summarise(dead = sum(dead),
            sick=sum(sick),
            newcases = sum(newcases))

ggplot(p, aes(x=year, y=dead, color=intervention))+
  geom_point()

ggplot(p, aes(x=year, y=newcases, color=intervention))+
  geom_point()

locs[2:11]
time1<-Sys.time()

for (is in locs[2:11]){
      
      temp<-project.all(is,  "app", 0, 2025, "p975")
      test<-bind_rows(temp, test)
}
time2<-Sys.time()

time2-time1
#2 mins

write.csv(test, "output/searo_all_1.csv", row.names = F)
###############################################################
#Tables and figures
###############################################################
test<-read.csv("output/searo_all_1.csv", stringsAsFactors = F)

ratio<-read.csv("data/IHME-GBD_2019_DATA-b30c6916-1.csv")%>%
  select(-upper, -lower, -metric, -year, -age, -sex)%>%
  spread(measure, val)%>%
  mutate(daly2death = `DALYs (Disability-Adjusted Life Years)`/Deaths)%>%
  select(location, cause, daly2death)

DA<-test%>%filter(year==2030)%>%
  group_by(intervention, location)%>%
  summarise(dead = sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
         DM_averted= (`b.a.u` - `Diabetes treatment only`),
         Both_averted= (`b.a.u` - `Both`),
         year = "in 2030")%>%
      bind_rows(.,test%>%filter(year<=2030)%>%
  group_by(intervention, location)%>%
  summarise(dead = sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
         DM_averted= (`b.a.u` - `Diabetes treatment only`),
         Both_averted= (`b.a.u` - `Both`),
         year = "2023-2030"))%>%
      mutate(measure = "Deaths")

DALY<-test%>%filter(year==2030)%>%
  left_join(., ratio)%>%
  group_by(intervention, location)%>%
  summarise(daly = sum(dead*daly2death))%>%
  spread(intervention, daly)%>%
  mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
         DM_averted= (`b.a.u` - `Diabetes treatment only`),
         Both_averted= (`b.a.u` - `Both`),
         year = "in 2030")%>%
  bind_rows(.,test%>%filter(year<=2030)%>%
              left_join(., ratio)%>%
              group_by(intervention, location)%>%
              summarise(daly = sum(dead*daly2death))%>%
              spread(intervention, daly)%>%
              mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
                     DM_averted= (`b.a.u` - `Diabetes treatment only`),
                     Both_averted= (`b.a.u` - `Both`),
                     year = "2023-2030"))%>%
  mutate(measure = "DALYs")

DA_all<-test%>%filter(year==2030)%>%
  group_by(intervention)%>%
  summarise(dead = sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
         DM_averted= (`b.a.u` - `Diabetes treatment only`),
         Both_averted= (`b.a.u` - `Both`),
         year = "in 2030")%>%
  bind_rows(.,test%>%filter(year<=2030)%>%
              group_by(intervention)%>%
              summarise(dead = sum(dead))%>%
              spread(intervention, dead)%>%
              mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
                     DM_averted= (`b.a.u` - `Diabetes treatment only`),
                     Both_averted= (`b.a.u` - `Both`),
                     year = "2023-2030"))%>%
  mutate(measure = "Deaths")%>%
  mutate(location = "SEARO")


DALY_all<-test%>%filter(year==2030)%>%
  left_join(., ratio)%>%
  group_by(intervention)%>%
  summarise(daly = sum(dead*daly2death))%>%
  spread(intervention, daly)%>%
  mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
         DM_averted= (`b.a.u` - `Diabetes treatment only`),
         Both_averted= (`b.a.u` - `Both`),
         year = "in 2030")%>%
  bind_rows(.,test%>%filter(year<=2030)%>%
              left_join(., ratio)%>%
              group_by(intervention)%>%
              summarise(daly = sum(dead*daly2death))%>%
              spread(intervention, daly)%>%
              mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
                     DM_averted= (`b.a.u` - `Diabetes treatment only`),
                     Both_averted= (`b.a.u` - `Both`),
                     year = "2023-2030"))%>%
  mutate(measure = "DALYs")%>%
  mutate(location = "SEARO")

####################################
#Table 1#
####################################

tab1<-test%>%filter(year==2030)%>%
  group_by(intervention, location)%>%
  summarise(dead = sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
         DM_averted= (`b.a.u` - `Diabetes treatment only`),
         Both_averted= (`b.a.u` - `Both`),
         year = "in 2030")%>%
  bind_rows(.,test%>%filter(year<=2030)%>%
              group_by(intervention, location)%>%
              summarise(dead = sum(dead))%>%
              spread(intervention, dead)%>%
              mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
                     DM_averted= (`b.a.u` - `Diabetes treatment only`),
                     Both_averted= (`b.a.u` - `Both`),
                     year = "2023-2030"))%>%
  bind_rows(.,test%>%filter(year==2023)%>%
              group_by(intervention, location)%>%
              summarise(dead = sum(dead))%>%
              spread(intervention, dead)%>%
              mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
                     DM_averted= (`b.a.u` - `Diabetes treatment only`),
                     Both_averted= (`b.a.u` - `Both`),
                     year = "in 2023"))%>%
  mutate(measure = "Deaths")

tab1_india<-tab1%>%filter(location=="India")
write.csv(tab1_india, "output/Table1_india.csv")

tab1_searo<-tab1%>%group_by(year, measure)%>%
  summarise(`Antihypertensive therapy only` = sum(`Antihypertensive therapy only`),
            `b.a.u` = sum(`b.a.u`),
            Both = sum(Both),
            `Diabetes treatment only` = sum(`Diabetes treatment only`),
            HTN_averted = sum(HTN_averted),
            DM_averted = sum(DM_averted),
            Both_averted = sum(Both_averted))

write.csv(tab1_searo, "output/Table1_searo.csv")

####################################
#Figure 1#
####################################
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

fig1_india<-test%>%filter(year<=2030, location=="India")%>%
  group_by(intervention, location, cause)%>%
  summarise(dead = sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
         DM_averted= (`b.a.u` - `Diabetes treatment only`),
         Both_averted= (`b.a.u` - `Both`),
         year = "2023-2030")

ggplot(fig1_india, aes(x="", y=Both_averted, fill=cause)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0)+
  theme_void()+
  scale_fill_manual(values = cbPalette)

ggsave("output/Figure1_india.jpeg", height=4, width=6)


fig1_searo<-test%>%filter(year<=2030)%>%
  group_by(intervention, cause)%>%
  summarise(dead = sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
         DM_averted= (`b.a.u` - `Diabetes treatment only`),
         Both_averted= (`b.a.u` - `Both`),
         year = "2023-2030")

ggplot(fig1_searo, aes(x="", y=Both_averted, fill=cause)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0)+
  theme_void()+
  scale_fill_manual(values = cbPalette)

ggsave("output/Figure1_searo.jpeg", height=4, width=6)

#40q30
CVD<-test%>%group_by(age, sex, year,location, intervention)%>%
  filter(age>=30 & age<70)%>%summarise(pop=sum(pop)/7, dead=sum(dead)) #divide pop by 7 to avoid over counting for each cause
CVD$age.group<-NA
CVD$age.group[CVD$age>=30 & CVD$age<35]<-"30-34"
CVD$age.group[CVD$age>=35 & CVD$age<40]<-"35-39"
CVD$age.group[CVD$age>=40 & CVD$age<45]<-"40-44"
CVD$age.group[CVD$age>=45 & CVD$age<50]<-"45-49"
CVD$age.group[CVD$age>=50 & CVD$age<55]<-"50-54"
CVD$age.group[CVD$age>=55 & CVD$age<60]<-"55-59"
CVD$age.group[CVD$age>=60 & CVD$age<65]<-"60-64"
CVD$age.group[CVD$age>=65 & CVD$age<70]<-"65-69"

#by region
WB_50q30<-CVD%>%group_by(age.group, year, intervention)%>%
  summarise(pop=sum(pop), dead=sum(dead))
WB_50q30$mx<-WB_50q30$dead/WB_50q30$pop
any(is.na(WB_50q30))

WB_50q30<-WB_50q30%>%group_by(year, intervention)%>%
  summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))

#by country
WB_50q30_c<-CVD%>%group_by(age.group, year, intervention, location)%>%
      summarise(pop=sum(pop), dead=sum(dead))
WB_50q30_c$mx<-WB_50q30_c$dead/WB_50q30_c$pop
any(is.na(WB_50q30_c))

WB_50q30_c<-WB_50q30_c%>%group_by(year, intervention, location)%>%
      summarise(x50q30 = 1-prod(1-(5*mx/(1+2.5*mx))))

##########

plot<-WB_50q30%>%
  mutate(intervention = ifelse(intervention == "b.a.u", "Baseline", intervention),
         intervention = ifelse(intervention =="Antihypertensive therapy only", "HTN treatment only", intervention))%>%
  rename(Scenario = intervention)%>%
  mutate(Scenario = factor(Scenario, levels=c("Baseline",
                                              "Diabetes treatment only",
                                              "HTN treatment only",
                                              "Both")))

plot_c<-WB_50q30_c%>%
      mutate(intervention = ifelse(intervention == "b.a.u", "Baseline", intervention),
             intervention = ifelse(intervention =="Antihypertensive therapy only", "HTN treatment only", intervention))%>%
      rename(Scenario = intervention)%>%
      mutate(Scenario = factor(Scenario, levels=c("Baseline",
                                                  "Diabetes treatment only",
                                                  "HTN treatment only",
                                                  "Both")))

##formatted graph##
library(ggthemes)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "black")
#from: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette


ggplot(plot%>%filter(year<=2030, Scenario!="Diabetes treatment only", Scenario!="HTN treatment only")%>%
         mutate(Scenario = ifelse(Scenario=="Both", "Intervention", "Baseline")), 
       aes(x=year, y=x50q30, color=Scenario))+
  geom_smooth(method = "loess", span=0.5, se=FALSE, width=0.1)+
  theme_calc()+
  scale_colour_manual(values=cbPalette[c(7,4,2,3)])+
  xlab("Year")+
  ylab("Probability of dying between ages 30 and 70")+
  ylim(0.1, 0.15)+
  ggtitle("CVD-DM 40q30")

ggsave("output/line_plot_searo1.jpeg", height=4, width=6)

####################################
#Figure 2#
####################################
ggplot(plot%>%filter(year<=2030), 
       aes(x=year, y=x50q30, color=Scenario))+
  geom_smooth(method = "loess", span=0.5, se=FALSE, width=0.1)+
  theme_calc()+
  scale_colour_manual(values=cbPalette[c(7,4,2,3)])+
  xlab("Year")+
  ylab("Probability of dying between ages 30 and 70")+
  ylim(0.1, 0.15)

ggsave("output/Figure2_searo.jpeg", height=4, width=6)

ggplot(plot_c%>%filter(year<=2030, location=="India"), 
       aes(x=year, y=x50q30, color=Scenario))+
  geom_smooth(method = "loess", span=0.5, se=FALSE, width=0.1)+
  theme_calc()+
  scale_colour_manual(values=cbPalette[c(7,4,2,3)])+
  xlab("Year")+
  ylab("Probability of dying between ages 30 and 70")+
  ylim(0.1, 0.15)

ggsave("output/Figure2_india.jpeg", height=4, width=6)

####################################
# Table 2 #
####################################
events<-read.csv("cvd_events.csv")

cvd<-test%>%filter(year==2030, cause %in% c("Ischemic heart disease", "Ischemic stroke",
                                            "Intracerebral hemorrhage", "Hypertensive heart disease"))%>%
  group_by(intervention, location)%>%
  summarise(dead = sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
         DM_averted= (`b.a.u` - `Diabetes treatment only`),
         Both_averted= (`b.a.u` - `Both`),
         year = "in 2030")%>%
  bind_rows(., test%>%filter(year<=2030, cause %in% c("Ischemic heart disease", "Ischemic stroke",
                                                      "Intracerebral hemorrhage", "Hypertensive heart disease"))%>%
              group_by(intervention, location)%>%
              summarise(dead = sum(dead))%>%
              spread(intervention, dead)%>%
              mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
                     DM_averted= (`b.a.u` - `Diabetes treatment only`),
                     Both_averted= (`b.a.u` - `Both`),
                     year = "2023-2030"))%>%
  cross_join(., events%>%filter(event!="RRT"))%>%
  mutate(HTN_averted = val*HTN_averted,
         DM_averted = val*DM_averted,
         Both_averted = val*Both_averted) 

tab2_india<-cvd%>%filter(location=="India")

ckd<-test%>%filter(year==2030, cause %in% c("Chronic kidney disease due to diabetes mellitus type 2",
                                            "Chronic kidney disease due to diabetes mellitus type 1"))%>%
  group_by(intervention, location)%>%
  summarise(dead = sum(dead))%>%
  spread(intervention, dead)%>%
  mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
         DM_averted= (`b.a.u` - `Diabetes treatment only`),
         Both_averted= (`b.a.u` - `Both`),
         year = "in 2030")%>%
  bind_rows(., test%>%filter(year<=2030, cause %in% c("Chronic kidney disease due to diabetes mellitus type 2",
                                                      "Chronic kidney disease due to diabetes mellitus type 1"))%>%
              group_by(intervention, location)%>%
              summarise(dead = sum(dead))%>%
              spread(intervention, dead)%>%
              mutate(HTN_averted = (`b.a.u` - `Antihypertensive therapy only`),
                     DM_averted= (`b.a.u` - `Diabetes treatment only`),
                     Both_averted= (`b.a.u` - `Both`),
                     year = "2023-2030"))%>%
  cross_join(., events%>%filter(event=="RRT"))%>%
  mutate(HTN_averted = val*HTN_averted,
         DM_averted = val*DM_averted,
         Both_averted = val*Both_averted) 

tab2_india<-bind_rows(tab2_india, ckd%>%filter(location=="India"))

write.csv(tab2_india%>%arrange(event), "output/Table2_india.csv")

#searo
tab2_searo<-cvd%>%group_by(year, event)%>%
  summarise(HTN_averted = sum(HTN_averted),
            DM_averted = sum(DM_averted),
            Both_averted = sum(Both_averted))%>%
  bind_rows(., ckd%>%group_by(year, event)%>%
              summarise(HTN_averted = sum(HTN_averted),
                        DM_averted = sum(DM_averted),
                        Both_averted = sum(Both_averted)))%>%
  arrange(event)

write.csv(tab2_searo, "output/Table2_searo.csv")

####################################

QA<-bind_rows(plot%>%mutate(location="SEARO"), plot_c)%>%
      filter(year==2023 | year==2030)%>%
      spread(Scenario, x50q30)%>%
      mutate(measure = "40q30",
             year = ifelse(year==2023, "in 2023", "in 2030"))%>%
      rename(`Antihypertensive therapy only` = `HTN treatment only`)

out.df<-bind_rows(DA, DA_all)%>%
      rename(Baseline = `b.a.u`)%>%
  bind_rows(., DALY%>%rename(Baseline = `b.a.u`))%>%
  bind_rows(., DALY_all%>%rename(Baseline = `b.a.u`))%>%
      bind_rows(., QA)%>%
  arrange(desc(measure), location)%>%
  rename(`Both treatments` = Both)

write.csv(out.df, "output/results_searo1.csv", row.names = F)


