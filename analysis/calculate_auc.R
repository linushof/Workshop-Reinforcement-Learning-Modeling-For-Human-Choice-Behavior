rm(list=ls())
source('./functions/my_packages.R')
source('./functions/my_starter.R')
#load fit and data
fit=readRDS(paste0(path$data,'/modelfit_empirical.rds'))
load('./data/empirical_data/df.rdata')
p_ch_action=fit$draws(variables ='p_ch_action',format='draws_matrix')

#remove missing trials
p_ch_action[p_ch_action==0]=NA
p_ch_action = p_ch_action[, !colSums(is.na(p_ch_action))]
#transform to aligned df
p_ch_action=as.data.frame(t(p_ch_action))
#calculate roc
#print example
roc=roc(df$selected_offer,p_ch_action[,1])
plot(roc)
auc(roc)
#iterate on all samples
Nsamples=ncol(p_ch_action)
auc_all_list = list()
auc_unchosen_list=list()
#you can add a grouping variable to compare your fit for different conditions
filter_variable=df$reoffer_ch==F&df$reoffer_unch==T
for (sample in 1:Nsamples){
  print(paste0("sample: ",sample))
  roc_unchosen=roc(df%>%filter(filter_variable)%>%pull(selected_offer),p_ch_action[filter_variable,sample])
  auc_unchosen=auc(roc_unchosen)
  auc_unchosen_list=append(auc_unchosen_list,auc_unchosen)
  roc_all=roc(df%>%filter(!filter_variable)%>%pull(selected_offer),p_ch_action[!filter_variable,sample])
  auc_all=auc(roc_all)
  auc_all_list=append(auc_all_list,auc_all)
}
posterior_unchosen_auc=unlist(auc_unchosen_list)
hist(posterior_unchosen_auc)
posterior_all_auc=unlist(auc_all_list)
hist(posterior_all_auc)

save(posterior_auc,file=paste0(path$data,'/auc.Rdata'))