data2=data_uc_c2_64098d74981b0b9242e435f8_SESSION_2023_06_30_14h46_12_799_1_%>%filter(trial_name=="show_reward2")
data2$block=ceiling((data2$trial_num+1)/50)
data2=data2%>%filter(phase=="exp")
data2=data2%>%select(block,trial_num,left_company,right_company,company_selected,key_selected,first_fruit,reward1,second_fruit,reward2,prob_fruit1,prob_fruit2,prob_fruit3,prob_fruit4,prob_fruit5,counterbalance)
data2_rt=data_uc_c2_64098d74981b0b9242e435f8_SESSION_2023_06_30_14h46_12_799_1_%>%filter(trial_name=="choice",phase=="exp")%>%select(rt)
data2=data2%>%mutate(rt=data2_rt$rt)
write.csv(data2, file = "data2.csv", row.names = FALSE)

#learning
data=data_uc_l1_64098d74981b0b9242e435f8_SESSION_2023_06_26_15h18_05_203
data=data%>%select(rt,trial_num,is_correct,phase,trial_name,correct_response,counter_correct,counter_errors,counterbalance)
data=data[7:366,]
write.csv(data, file = "data_learning.csv", row.names = FALSE)

#preprocessing
company_fruit <- matrix(c(2, 4, 0, 3, 1, 4, 0, 2, 1, 3), nrow = 5, ncol = 2, byrow = TRUE)
data1=data1%>%mutate(company_not_selected=if_else(company_selected==left_company,right_company,left_company),unchosen_fruit1=company_fruit[company_not_selected+1,1],
                     unchosen_fruit2=company_fruit[company_not_selected+1,2],common_fruit=if_else((unchosen_fruit1==first_fruit|unchosen_fruit1==second_fruit),unchosen_fruit1,unchosen_fruit2),
                     unique_fruit_chosen=if_else(common_fruit==first_fruit,second_fruit,first_fruit),unique_fruit_unchosen=if_else(unchosen_fruit1==common_fruit,unchosen_fruit2,unchosen_fruit1),
                     prob_unique_fruit_chosen=case_when(unique_fruit_chosen==0~prob_fruit1,
                                                        unique_fruit_chosen==1~prob_fruit2,
                                                        unique_fruit_chosen==2~prob_fruit3,
                                                        unique_fruit_chosen==3~prob_fruit4,
                                                        unique_fruit_chosen==4~prob_fruit5),
                     prob_unique_fruit_unchosen=case_when(unique_fruit_unchosen==0~prob_fruit1,
                                                        unique_fruit_unchosen==1~prob_fruit2,
                                                        unique_fruit_unchosen==2~prob_fruit3,
                                                        unique_fruit_unchosen==3~prob_fruit4,
                                                        unique_fruit_unchosen==4~prob_fruit5),
                     accuracy=prob_unique_fruit_chosen>prob_unique_fruit_unchosen)
