rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

##############################
#data required:
  #incoming population (if not using fertility)
  #transition probabilities
  #initial state populations
##############################

locs<-c("Bangladesh", "Bhutan", "Democratic People's Republic of Korea",
        "India", "Indonesia", "Maldives", "Myanmar", "Nepal", "Sri Lanka",
        "Thailand", "Timor-Leste")

#baseline rates calculated in file:
b_rates<-fread("tps_Bangladesh.csv")

for(is in locs[2:11]){
b_rates<-bind_rows(b_rates, fread(paste0("tps_", is,".csv")))
}

b_rates[CF>0.8, CF:=0.8]
b_rates[IR>0.8, IR:=0.8]
b_rates[CF<0, CF:=0]
b_rates[IR<0, IR:=0]

pop20<-fread("PopulationsAge20_full.csv")%>%filter(location %in% locs)
pop20<-pop20[year_id>=2009 & year_id<=2040]

setnames(pop20, c("year_id", "Nx"), c("year", "Nx20"))

##############################
#Run model to calibrate TPs 2009-2019#
##############################
#IRadjust<-1
#CFadjust<-1
#any(is.na(base_rates))

state.transition<-function(b_rates, pop20,  IRadjust, CFadjust){ 
      
      base_rates<-merge(b_rates, pop20[year<=2019], by=c("year", "location", "sex", "age"), all=TRUE)
      base_rates[age==20 & year>2009, Nx:=Nx20]
      base_rates[, Nx20:=NULL]
      
      ## calculate initial states for the incoming year 2000 and all years for age 20 population
      base_rates[year==2009 | age==20, sick:=Nx*PREVt0]
      base_rates[year==2009 | age==20, dead:=Nx*DIS.mx.t0]
      base_rates[year==2009 | age==20, well:=Nx*(1-(PREVt0+ALL.mx))]
      
      base_rates[age==20 | year==2009, pop:=Nx]
      base_rates[age==20 | year==2009, all.mx:=Nx*ALL.mx]
      
      base_rates[, IR:=IR*IRadjust]
      base_rates[, CF:=CF*CFadjust]
      
      base_rates[,IRadjust:=IRadjust]
      base_rates[,CFadjust:=CFadjust]
      
      base_rates[CF>0.9, CF:=0.9]
      base_rates[IR>0.9, IR:=0.9]
      
      #STATE TRANSITIONS#
      for(i in 1:11){
            
            b2<-base_rates[year<=2009+i & year>=2009+i-1]
            b2[,age2:=age+1]
            
            #sick
            b2[, sick2:=shift(sick)*(1-(CF+BG.mx)) + shift(well)*IR, by=.(sex, location, cause, age)]
            #b2[age2>=95, sick2:=sick2+shift(sick2, type="lead"), by=.(sex, location, cause, year)]
            b2[sick2<0, sick2:=0] #prevent possible negatives
            
            #dead
            b2[, dead2:=shift(sick)*CF, by=.(sex, location, cause, age)]
            #b2[age2>=95, dead2:=dead2+shift(dead2, type="lead"), by=.(sex, location, cause, year)]
            b2[dead2<0, sick2:=0] #prevent possible negatives
            
            #pop
            b2[,pop2:=shift(pop)-shift(all.mx), by=.(sex, location, cause, age)]
            #b2[age2>=95, pop2:=pop2+shift(pop2, type="lead"), by=.(sex, location, cause, year)]
            b2[pop2<0, pop2:=0] #prevent possible negatives
            
            #all dead
            b2[, all.mx2:=sum(dead2), by=.(sex, location, year, age)]
            b2[,all.mx2:=all.mx2+(pop2*BG.mx.all)]
            b2[all.mx2<0, all.mx2:=0]
            
            #well
            b2[, well2:=pop2-all.mx2-sick2]
            b2[well2<0, well2:=0] #prevent possible negatives
            
            #re-combined into original data.table
            b2<-b2[year==2009+i & age2<96, c("age2", "sick2", "dead2", "well2", "pop2", "all.mx2", "sex", "location", "cause")]
            setnames(b2, "age2", "age")
            base_rates[year==2009+i & age>20, sick:=b2[,sick2]]
            base_rates[year==2009+i & age>20, dead:=b2[,dead2]]
            base_rates[year==2009+i & age>20, well:=b2[,well2]]
            base_rates[year==2009+i & age>20, pop:=b2[,pop2]]
            base_rates[year==2009+i & age>20, all.mx:=b2[,all.mx2]]
            
      }
      
      base_rates%>%select(year, location, sex, age, cause, IR, CF, well, sick, dead, pop, all.mx, IRadjust, CFadjust)
}


out.df<-data.table(year = numeric(),
                   location = character(),
                   sex = character(),
                   age = numeric(),
                   cause = character(),
                   IR = numeric(),
                   CF = numeric(),
                   well = numeric(),
                   sick = numeric(),
                   dead = numeric(),
                   pop = numeric(),
                   all.mx = numeric(),
                   IRadjust = numeric(),
                   CFadjust = numeric())

##############################
#Run model 121 times
##############################

time1<-Sys.time()
for(i in -10:10){
      for(j in -10:10){
            temp<-state.transition(b_rates, pop20, 1+(i/100), 1+(j/100))%>%filter(year>=2009)
            out.df<-bind_rows(out.df, temp)
      }
}

time2<-Sys.time()
time2-time1 # ~120 minutes

any(is.na(out.df))
#unique(out.df$CFadjust)

#Remove any scenarios where CF>0.9 or CF<0
#Remove any scenarios where IR>0.9 or IR<0

out.df<-out.df%>%filter(CF<=0.9 & CF>0 & IR>0 & IR<=0.9)

#################################
#Minimize root mean squared error compared to GBD 2009-2019
#Weight fatal estimates 2x non-fatal estimates
#################################

gbd<-bind_rows(read.csv("IHME-GBD_2019_DATA-d0d7a8c8-1.csv", stringsAsFactors = F),
               read.csv("IHME-GBD_2019_DATA-d0d7a8c8-2.csv", stringsAsFactors = F))%>%
      select(-upper, -lower)%>%
      filter(metric=="Number" & measure!="Incidence" & location %in% locs & year>=2009)%>%
      spread(measure, val)%>%
      select(-metric)

unique(gbd$location)
unique(gbd$age)

age_match<-data.frame(age=20:95)%>%
      mutate(age.group = ifelse(age<25, "20-24 years", NA),
             age.group = ifelse(age>=25 & age<30, "25-29 years", age.group),
             age.group = ifelse(age>=30 & age<35, "30-34 years", age.group),
             age.group = ifelse(age>=35 & age<40, "35-39 years", age.group),
             age.group = ifelse(age>=40 & age<45, "40-44 years", age.group),
             age.group = ifelse(age>=45 & age<50, "45-49 years", age.group),
             age.group = ifelse(age>=50 & age<55, "50-54 years", age.group),
             age.group = ifelse(age>=55 & age<60, "55-59 years", age.group),
             age.group = ifelse(age>=60 & age<65, "60-64 years", age.group),
             age.group = ifelse(age>=65 & age<70, "65-69 years", age.group),
             age.group = ifelse(age>=70 & age<75, "70-74 years", age.group),
             age.group = ifelse(age>=75 & age<80, "75-79 years", age.group),
             age.group = ifelse(age>=80 & age<85, "80-84", age.group),
             age.group = ifelse(age>=85 & age<90, "85-89", age.group),
             age.group = ifelse(age>=90 & age<95, "90-94", age.group),
             age.group = ifelse(age==95, "95+ years", age.group))

#Put modeled results into 5-year age groups
dt<-as.data.table(out.df)
dt[,intervention:=paste0("IR", IRadjust, "CF", CFadjust)]
rm(out.df)

dt<-left_join(dt, age_match)%>%
      group_by(cause, sex, year, intervention, location, age.group)%>%
      summarise(Prevalence = sum(sick),
                Deaths = sum(dead))%>%
      rename(age = age.group)

unique(dt$age)
unique(dt$year)

dt<-as.data.table(dt)
#############################
#Compare to GBD
#############################
gbd2<-data.table(gbd%>%rename(gbdDeaths = Deaths, gbdPrev = Prevalence))
data2<-left_join(dt[year<2020], gbd2[year<2020 & cause!="All causes"])
any(is.na(data2))
unique(data2$age)

data2<-data2%>%group_by(year, intervention, sex, cause, location, age)%>%
      summarise(Deaths=sum(Deaths), Prevalence=sum(Prevalence), gbdDeaths=sum(gbdDeaths),
                gbdPrev=sum(gbdPrev))

data2$derror<-(data2$gbdDeaths-data2$Deaths)^2
data2$perror<-(data2$gbdPrev-data2$Prevalence)^2

data2<-data2%>%group_by(location, sex, cause, intervention, age)%>%summarise(RMSE_deaths=sqrt(mean(derror)),
                                                                        RMSE_prev=sqrt(mean(perror)))

data.adj<-as.data.table(data2)
data.adj[, error:=2*RMSE_deaths + RMSE_prev]
test<-data.adj[ , .SD[which.min(error)], by=.(location, sex, cause, age)]

###########################
#Plot and pull adjustments
###########################

library(stringr)
test[, IRadjust:=NA]
test[, CFadjust:=NA]

test[, IRadjust:=as.numeric(gsub(".*IR(.+)CF.*", "\\1", intervention))]
test[, CFadjust:=as.numeric(gsub(".*CF(.+).*", "\\1", intervention))]

data2<-left_join(dt[year<2020], gbd2[year<2020 & cause!="All causes"])
plot<-data2%>%group_by(year, intervention, sex, cause, location, age)%>%
      summarise(Deaths=sum(Deaths), Prevalence=sum(Prevalence), gbdDeaths=sum(gbdDeaths),
                gbdPrev=sum(gbdPrev))%>%
      right_join(., test)%>%
      ungroup()%>%
      select(year, sex, cause, location, age, Deaths, Prevalence, gbdDeaths, gbdPrev)%>%
      gather(metric, val, -location, -sex, -age,-year,-cause)
#

is<-locs[1]

ggplot(plot%>%filter(cause=="Ischemic heart disease",sex=="Female", metric%in%c("Deaths", "gbdDeaths"), location==is), 
       aes(x=year, y=val, color=metric))+
      geom_point()+
      facet_wrap(~age)

ggplot(plot%>%filter(cause=="Ischemic heart disease",sex=="Female", metric%in%c("Prevalence", "gbdPrev"), location==is), 
       aes(x=year, y=val, color=metric))+
      geom_point()+
      facet_wrap(~age)

adjustments<-test[, c("sex", "location", "cause", "IRadjust", "CFadjust", "age")]


#########
#run it again
#########

b_rates<-left_join(b_rates, age_match)%>%
      left_join(adjustments%>%rename(age.group = age))%>%
      mutate(CF = CF*CFadjust,
             IR = IR*IRadjust)%>%
      select(-age.group, -IRadjust, -CFadjust)

write.csv(b_rates, "tps_adjusted_searo.csv", row.names = F)

out.df<-data.table(year = numeric(),
                   location = character(),
                   sex = character(),
                   age = numeric(),
                   cause = character(),
                   IR = numeric(),
                   CF = numeric(),
                   well = numeric(),
                   sick = numeric(),
                   dead = numeric(),
                   pop = numeric(),
                   all.mx = numeric(),
                   IRadjust = numeric(),
                   CFadjust = numeric())


##############################
#Run model 121 times
##############################

time1<-Sys.time()
for(i in -10:10){
      for(j in -10:10){
            temp<-state.transition(b_rates, pop20, 1+(i/100), 1+(j/100))%>%filter(year>=2009)
            out.df<-bind_rows(out.df, temp)
      }
}

time2<-Sys.time()
time2-time1 # ~60 minutes

any(is.na(out.df))
unique(out.df$CFadjust)

#Remove any scenarios where CF>0.9 or CF<0
#Remove any scenarios where IR>0.9 or IR<0

out.df<-out.df%>%filter(CF<=0.9 & CF>0 & IR>0 & IR<=0.9)


##################################################
#Put modeled results into 5-year age groups
dt<-as.data.table(out.df)
dt[,intervention:=paste0("IR", IRadjust, "CF", CFadjust)]
rm(out.df)

dt<-left_join(dt, age_match)%>%
      group_by(cause, sex, year, intervention, location, age.group)%>%
      summarise(Prevalence = sum(sick),
                Deaths = sum(dead))%>%
      rename(age = age.group)

unique(dt$age)
unique(dt$year)

dt<-as.data.table(dt)
#############################
#Compare to GBD
#############################
gbd2<-data.table(gbd%>%rename(gbdDeaths = Deaths, gbdPrev = Prevalence))
data2<-left_join(dt[year<2020], gbd2[year<2020 & cause!="All causes"])
any(is.na(data2))
unique(data2$age)

data2<-data2%>%group_by(year, intervention, sex, cause, location, age)%>%
      summarise(Deaths=sum(Deaths), Prevalence=sum(Prevalence), gbdDeaths=sum(gbdDeaths),
                gbdPrev=sum(gbdPrev))

data2$derror<-(data2$gbdDeaths-data2$Deaths)^2
data2$perror<-(data2$gbdPrev-data2$Prevalence)^2

data2<-data2%>%group_by(location, sex, cause, intervention, age)%>%summarise(RMSE_deaths=sqrt(mean(derror)),
                                                                             RMSE_prev=sqrt(mean(perror)))

data.adj<-as.data.table(data2)
data.adj[, error:=2*RMSE_deaths + RMSE_prev]
test<-data.adj[ , .SD[which.min(error)], by=.(location, sex, cause, age)]


###########################
#Plot and pull adjustments
###########################


library(stringr)
test[, IRadjust:=NA]
test[, CFadjust:=NA]

test[, IRadjust:=as.numeric(gsub(".*IR(.+)CF.*", "\\1", intervention))]
test[, CFadjust:=as.numeric(gsub(".*CF(.+).*", "\\1", intervention))]

data2<-left_join(dt[year<2020], gbd2[year<2020 & cause!="All causes"])
plot<-data2%>%group_by(year, intervention, sex, cause, location, age)%>%
      summarise(Deaths=sum(Deaths), Prevalence=sum(Prevalence), gbdDeaths=sum(gbdDeaths),
                gbdPrev=sum(gbdPrev))%>%
      right_join(., test)%>%
      ungroup()%>%
      select(year, sex, cause, location, age, Deaths, Prevalence, gbdDeaths, gbdPrev)%>%
      gather(metric, val, -location, -sex, -age,-year,-cause)

#is<-locs[1]
cse<-unique(plot$cause)

for (is in locs){
      for(c in cse){
      
      ggplot(plot%>%filter(cause==c, sex=="Female", metric%in%c("Deaths", "gbdDeaths"), location==is), 
             aes(x=year, y=val, color=metric))+
            geom_point()+
            facet_wrap(~age)
      
      ggsave(paste0("check plots/", is,"_", c, "_mort.jpeg"), height=6, width=8)
      
      ggplot(plot%>%filter(cause==c, sex=="Female", metric%in%c("Prevalence", "gbdPrev"), location==is), 
             aes(x=year, y=val, color=metric))+
            geom_point()+
            facet_wrap(~age)
      
      ggsave(paste0("check plots/", is,"_", c, "_prev.jpeg"), height=6, width=8)
      }
}

adjustments<-test[, c("sex", "location", "cause", "IRadjust", "CFadjust", "age")]%>%
      rename(age.group = age)%>%
      left_join(., age_match)%>%
      select(-age.group)

#write.csv(adjustments, "adjustments.csv", row.names = F)

b_rates<-left_join(b_rates, adjustments)%>%
      mutate(IR = IR*IRadjust,
             CF = CF*CFadjust)%>%
      select(-IRadjust, -CFadjust)

write.csv(b_rates, "tps_adjusted_searo.csv", row.names = F)

