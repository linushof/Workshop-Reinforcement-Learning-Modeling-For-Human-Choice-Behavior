rm(list=ls())
library(data.table)
library(tidyverse)
df<-data.table(read.csv('empirical data/df.csv'))

###### house keeping ----------------------
#sort trials
df    =df[order(df$subj,df$blk,df$trl),]
df$trl=df$trl+1
rownames(df) <- 1:nrow(df)

#add columns


df$rw[df$cond=="neg"]    <-df$rw[df$cond=="neg"] +1        #add absolute outcome (1\0) column to tab
df$cond                  <-(df$cond=='pos')*1
df$acc                   <-(df$prob1>df$prob2)*1 #prob1 is prob.ch
df$trial.total           <-df$trl+(df$blk-1)*50


df=df%>%mutate(delta_exp_value        = (prob1-prob2),
               offer1=frcA,
               offer2=frcB,
               choice=ch,
               unchosen=ifelse(ch==frcA,frcB,frcA),
               reward=rw,
               subject=subj)%>%
  mutate(delta_exp_value_oneback=lag(delta_exp_value),
               reoffer_ch             =(offer1==lag(choice)|offer2==lag(choice)),
               reoffer_unch           =(offer1==lag(unchosen)|offer2==lag(unchosen)),
               stay_frc_ch            =(choice==lag(choice)),
               stay_frc_unch          =(choice==lag(unchosen)),
               reward_oneback         =lag(reward),
               acc_oneback            =lag(acc),
               prob1_oneback          =lag(prob1),
               prob2_oneback          =lag(prob2),
               rt_oneback             =lag(rt))

#replace -1 rt with values
df$rt[df$rt==-1]<-NA

####### omit subjects due to no response probability higher then 5%----------------------
#check proportion
x           <-df[,mean(is.na(rt)==T), by=subj]
colnames(x) <- c("subject", "no_response")
sum(x$no_response>0.05)

#remove subjects with more than 5% no response trials
df<-df[df$subj %in% x[(x$no_response>0.05)==FALSE]$subj]

#remove all no response trials
df<-na.omit(df)



####### omit subjects due to staying with the same key more then 90% of the trials----------------------
#check key stay probability 
x<-df[,mean(stay_key), by=c('subj')]
colnames(x) <- c("subject", "key_rep")
sum(x$key_rep>=0.9)



#######omit subjects with more the 10% very long or very short RTs----------------------
#check long rt probability
x<-df[,mean(rt>4000), by=subj]
colnames(x) <- c("subject", "long_rt")
sum(x$long_rt>=0.1)

#check short rt probability
x<-df[,mean(rt<200)>0.1, by=subj]
colnames(x) <- c("subject", "short_rt")
sum(x$short_rt)

#remove participants with more than 10% short rt trials
df<-df[df$subj %in% x[x$short_rt==FALSE]$subj]



#######omit trials with quick RTs, slow RTs and first trial ----------------------
#count how many trials we are going to keep
dim(df[rt>200 & rt<4000])[1]/dim(df)[1]

#remove all short and long trials
df<-df[rt>200 & rt<4000]

#remove first trial of every block
df<-df[trl>1]

save(df, file='data/empirical_data/df.rdata')
