
library(data.table)
library(reshape2)
library(rstatix)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(raincloudplots)

rm(list=ls())
load('./data/empirical_data/df.rdata')
df<-na.omit(df)


df%>%group_by(subject)%>%summarise(mean_acc=mean(acc))

model= glmer(stay ~ rewrad_oneback+(1| subject), 
             data = df, 
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
















