rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, ggplot2, readxl)   
##############################################

#data permalink
#https://vizhub.healthdata.org/gbd-results?params=gbd-api-2019-permalink/463a40833819742df724b697ba2cc03f
dt<-bind_rows(read.csv("IHME-GBD_2019_DATA-d0d7a8c8-1.csv", stringsAsFactors = F),
              read.csv("IHME-GBD_2019_DATA-d0d7a8c8-2.csv", stringsAsFactors = F))%>%
      filter(location!="Global")
dt<-data.table(dt)
dt[, upper:=NULL]
dt[, lower:=NULL]

unique(dt$cause)

#Get average annual rates of change by cohort
#as a function#
get.new.rates<-function(dt, year1, year2){
  #year1<-1995
  #year2<-2000
prevyear1<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Number" & year==year1)%>%
                select(-c(measure, metric, year))%>%
                  rename(prev14 = val))

prevrtyear1<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Rate" & year==year1)%>%
  select(-c(measure, metric, year))%>%
  rename(prevrt14 = val))

deathyear1<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Number" & year==year1)%>%
  select(-c(measure, metric, year))%>%
  rename(death14 = val))

deathrtyear1<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Rate" & year==year1)%>%
  select(-c(measure, metric, year))%>%
  rename(deathrt14 = val))

prevyear2<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Number" & year==year2)%>%
  select(-c(measure, metric, year))%>%
  rename(prev19 = val))

prevrtyear2<-as.data.table(dt%>%filter(measure=="Prevalence" & metric=="Rate" & year==year2)%>%
  select(-c(measure, metric, year))%>%
  rename(prevrt19 = val))

deathyear2<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Number" & year==year2)%>%
  select(-c(measure, metric, year))%>%
  rename(death19 = val))

deathrtyear2<-as.data.table(dt%>%filter(measure=="Deaths" & metric=="Rate" & year==year2)%>%
  select(-c(measure, metric, year))%>%
  rename(deathrt19 = val))

mymerge<-function(x,y){merge.data.table(x,y, by=c("location", "age", "sex", "cause"))}

dt14<-Reduce(mymerge, list(prevyear1,prevrtyear1,deathyear1,deathrtyear1))
dt19<-Reduce(mymerge, list(prevyear2,prevrtyear2,deathyear2,deathrtyear2))

dt14[, pop14:=death14/deathrt14*100000]
dt14<-dt14%>%mutate(age2 = ifelse(age=="20-24 years", 1, NA),
                    age2 = ifelse(age=="25-29 years", 2, age2),
                    age2 = ifelse(age=="30-34 years", 3, age2),
                    age2 = ifelse(age=="35-39 years", 4, age2),
                    age2 = ifelse(age=="40-44 years", 5, age2),
                    age2 = ifelse(age=="45-49 years", 6, age2),
                    age2 = ifelse(age=="50-54 years", 7, age2),
                    age2 = ifelse(age=="55-59 years", 8, age2),
                    age2 = ifelse(age=="60-64 years", 9, age2),
                    age2 = ifelse(age=="65-69 years", 10, age2),
                    age2 = ifelse(age=="70-74 years", 11, age2),
                    age2 = ifelse(age=="75-79 years", 12, age2),
                    age2 = ifelse(age=="80-84", 13, age2),
                    age2 = ifelse(age=="85-89", 14, age2),
                    age2 = ifelse(age=="90-94", 15, age2),
                    age2 = ifelse(age=="95+ years", 16, age2))

dt19[, pop19:=death19/deathrt19*100000]
dt19<-dt19%>%mutate(age2 = ifelse(age=="20-24 years", 0, NA),
                    age2 = ifelse(age=="25-29 years", 1, age2),
                    age2 = ifelse(age=="30-34 years", 2, age2),
                    age2 = ifelse(age=="35-39 years", 3, age2),
                    age2 = ifelse(age=="40-44 years", 4, age2),
                    age2 = ifelse(age=="45-49 years", 5, age2),
                    age2 = ifelse(age=="50-54 years", 6, age2),
                    age2 = ifelse(age=="55-59 years", 7, age2),
                    age2 = ifelse(age=="60-64 years", 8, age2),
                    age2 = ifelse(age=="65-69 years", 9, age2),
                    age2 = ifelse(age=="70-74 years", 10, age2),
                    age2 = ifelse(age=="75-79 years", 11, age2),
                    age2 = ifelse(age=="80-84", 12, age2),
                    age2 = ifelse(age=="85-89", 13, age2),
                    age2 = ifelse(age=="90-94", 14, age2),
                    age2 = ifelse(age=="95+ years", 15, age2))

dt19[, age:=NULL]

dt<-merge(dt14, dt19, by=c("age2", "location", "sex", "cause"))

allcause<-dt[cause=="All causes"]
setnames(allcause, c("death14", "deathrt14", "death19", "deathrt19"),
         c("alldeath14", "alldeathrt14", "alldeath19", "alldeathrt19"))
allcause[,c("cause", "prev14", "prevrt14", "prev19", "prevrt19",
            "pop14", "pop19"):=NULL]

dt<-merge(dt, allcause, by=c("age2", "sex", "location", "age"))
dt<-dt[cause!="All causes"]

dt[, othermx14:=alldeath14-death14]
dt[, othermx19:=alldeath19-death19]
dt[, othermxrt14:=alldeathrt14 - deathrt14]
dt[, othermxrt19:=alldeathrt19 - deathrt19]

dt[, well14:=pop14-prev14-alldeath14]
dt[,well19:=pop19-prev19-alldeath19]

dt[, wellAARC:=log((well19/pop19)/(well14/pop14))/5]
dt[, sickAARC:=log(prevrt19/prevrt14)/5]
dt[, deadAARC:=log(deathrt19/deathrt14)/5]
dt[, deadotherAARC:=log(othermxrt19/othermxrt14)/5]

dt[sickAARC<0, sickAARC:=0]
dt[sickAARC>1, sickAARC:=0.99]
dt[deadAARC<0, deadAARC:=0]
dt[deadAARC>1, deadAARC:=0.99]
dt[deadotherAARC<0, deadotherAARC:=0]
dt[deadotherAARC>1, deadotherAARC:=0.99]


dt[,age2:=1]

rows<-as.numeric(nrow(dt))

reprow<-function(row){
  floor((row-1)/rows)
}

DT<-dt[rep(seq(1,nrow(dt)), 5)][, age2:=age2+reprow(.I)]

DT[age2==1, Well:=well14]
DT[age2==1, Sick:=prev14]
DT[age2==1, Dead:=death14]
DT[age2==1, DeadOther:=othermx14]

for(i in 2:5){
DT2<-DT[age2<=i &age2>=i-1]
DT2[, Well2:=shift(Well)*(1+wellAARC), by=.(age, sex, location, cause)]
DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "Well2")]
DT[age2==i, Well:=DT2[,Well2]]
}

for(i in 2:5){
  DT2<-DT[age2<=i &age2>=i-1]
  DT2[, Sick2:=shift(Sick)*(1+sickAARC), by=.(age, sex, location, cause)]
  DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "Sick2")]
  DT[age2==i, Sick:=DT2[,Sick2]]
}

for(i in 2:5){
  DT2<-DT[age2<=i &age2>=i-1]
  DT2[, Dead2:=shift(Dead)*(1+deadAARC), by=.(age, sex, location, cause)]
  DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "Dead2")]
  DT[age2==i, Dead:=DT2[,Dead2]]
}

for(i in 2:5){
  DT2<-DT[age2<=i &age2>=i-1]
  DT2[, DeadOther2:=shift(DeadOther)*(1+deadotherAARC), by=.(age, sex, location, cause)]
  DT2<-DT2[age2==i, c("age", "sex", "location", "cause", "DeadOther2")]
  DT[age2==i, DeadOther:=DT2[,DeadOther2]]
}

DT[, IR:=(Sick-(shift(Sick)-Dead))/shift(Well),  by=.(age, sex, location, cause)]
DT[, CF:=Dead/shift(Sick),  by=.(age, sex, location, cause)]

DT[ , avgIR:=mean(na.omit(IR)), by=.(age, sex, location, cause)]
DT[ , avgCF:=mean(na.omit(CF)), by=.(age, sex, location, cause)]
DT[ , midptage:=as.numeric(substr(age,1,2))+2]

DT_final<-unique(DT[,c("midptage", "age", "sex", "location", "cause", "avgIR", "avgCF")])
DT_final[, year:=year2]

DT_final[avgIR<0 | is.na(avgIR), avgIR:=0]
DT_final[avgCF>1, avgCF:=0.9]
DT_final[avgIR>1, avgIR:=0.9]
}
#end of function

newrates<-get.new.rates(dt, 1995,2000)
unique(newrates$cause)

for(i in 1:19){
  DT_final<-  get.new.rates(dt, 1995+i, 2000+i)
  newrates<-rbindlist(list(DT_final, newrates), use.names = T)
}

DT_final<-newrates #store for debugging
any(is.na(DT_final))

over95<-DT_final[age=="90-94"]
over95[, age:="95+ years"]
over95[, midptage:=97]

new<-rbindlist(list(over95, DT_final))
new<-new[order(location, sex, midptage)]
unique(new$midptage)
setnames(new, c("avgIR", "avgCF"), c("IR", "CF"))

get.data <- function(sx, cse, var, dfin, yr, country){
  x        <- c(seq(22,92,5), 95)
  y        <- dfin %>% filter(sex==sx & cause==cse) %>% pull(var)
  d        <- approx(x,y, xout=20:95, rule=2, method="linear")
  df       <- data.table(age = d$x, dname = d$y, cause = cse, sex = sx, location = country, year = yr)
  setnames(df, "dname", var)
  df
}

get.single.age.rates<-function(data, yr, country){
  #data<-new
  #yr<-2000
  #country<-"India"
data<-data[year==yr & location==country]
  
d<-get.data("Female", "Ischemic heart disease", "IR", data, yr, country)
d2<-get.data("Male", "Ischemic heart disease", "IR", data, yr, country)
d3<-get.data("Female", "Hypertensive heart disease", "IR", data, yr, country)
d4<-get.data("Male", "Hypertensive heart disease", "IR", data, yr, country)
d5<-get.data("Female", "Ischemic stroke", "IR", data, yr, country)
d6<-get.data("Male", "Ischemic stroke", "IR", data, yr, country)
d7<-get.data("Female", "Intracerebral hemorrhage", "IR", data, yr, country)
d8<-get.data("Male", "Intracerebral hemorrhage", "IR", data, yr, country)
d9<-get.data("Female", "Diabetes mellitus type 2", "IR", data, yr, country)
d10<-get.data("Male", "Diabetes mellitus type 2", "IR", data, yr, country)
d11<-get.data("Female", "Chronic kidney disease due to diabetes mellitus type 2", "IR", data, yr, country)
d12<-get.data("Male", "Chronic kidney disease due to diabetes mellitus type 2", "IR", data, yr, country)
d13<-get.data("Female", "Chronic kidney disease due to hypertension", "IR", data, yr, country)
d14<-get.data("Male", "Chronic kidney disease due to hypertension", "IR", data, yr, country)

IRs<-rbindlist(list(d,d2,d3,d4,d5,d6,d7,d8, d9, d10, d11, d12, d13, d14))

d<-get.data("Female", "Ischemic heart disease", "CF", data, yr, country)
d2<-get.data("Male", "Ischemic heart disease", "CF", data, yr, country)
d3<-get.data("Female", "Hypertensive heart disease", "CF", data, yr, country)
d4<-get.data("Male", "Hypertensive heart disease", "CF", data, yr, country)
d5<-get.data("Female", "Ischemic stroke", "CF", data, yr, country)
d6<-get.data("Male", "Ischemic stroke", "CF", data, yr, country)
d7<-get.data("Female", "Intracerebral hemorrhage", "CF", data, yr, country)
d8<-get.data("Male", "Intracerebral hemorrhage", "CF", data, yr, country)
d9<-get.data("Female", "Diabetes mellitus type 2", "CF", data, yr, country)
d10<-get.data("Male", "Diabetes mellitus type 2", "CF", data, yr, country)
d11<-get.data("Female", "Chronic kidney disease due to diabetes mellitus type 2", "CF", data, yr, country)
d12<-get.data("Male", "Chronic kidney disease due to diabetes mellitus type 2", "CF", data, yr, country)
d13<-get.data("Female", "Chronic kidney disease due to hypertension", "CF", data, yr, country)
d14<-get.data("Male", "Chronic kidney disease due to hypertension", "CF", data, yr, country)

CFs<-rbindlist(list(d,d2,d3,d4,d5,d6,d7,d8,d9,d10, d11, d12, d13, d14))

newrates2<-merge(IRs, CFs, by=c("age", "sex", "cause", "location", "year"))
newrates2
}

locs<-unique(dt$location)

for(is in locs){
newrates<-get.single.age.rates(new, 2000, is)
newrates<-rbindlist(list(newrates,
                         get.single.age.rates(new, 2001, is), get.single.age.rates(new, 2002, is),
                         get.single.age.rates(new, 2003, is), get.single.age.rates(new, 2004, is),
                         get.single.age.rates(new, 2005, is),
                         get.single.age.rates(new, 2006, is), get.single.age.rates(new, 2007, is),
                         get.single.age.rates(new, 2008, is), get.single.age.rates(new, 2009, is),
                         get.single.age.rates(new, 2010, is), get.single.age.rates(new, 2011, is),
                         get.single.age.rates(new, 2012, is), get.single.age.rates(new, 2013, is),
                         get.single.age.rates(new, 2014, is), get.single.age.rates(new, 2015, is),
                         get.single.age.rates(new, 2016, is), get.single.age.rates(new, 2017, is),
                         get.single.age.rates(new, 2018, is), get.single.age.rates(new, 2019, is)))


############# Combine merge with other rates #############
other<-fread(paste0("baseline_rates_", is, ".csv"))
dataout<-merge(other, newrates, by=c("age", "sex", "location", "year", "cause"))
any(is.na(dataout))

write.csv(dataout, paste0("tps_", is,".csv"), row.names = F)
}

#Inspect#
ggplot(newrates%>%filter(year==2019), 
       aes(x=age, y=IR, color=cause))+
      facet_wrap(~sex)+
      geom_point()

ggplot(newrates%>%filter(year==2019), 
       aes(x=age, y=CF, color=cause))+
      facet_wrap(~sex)+
      geom_point()
