rm(list=ls())
source('./functions/my_starter.R')

path = set_workingmodel()

load(paste0(path$data,'/artificial_data.Rdata'))

df=df%>%
  mutate(reward=factor(reward),
         reward_oneback=lag(reward),
         repeat_state = state==lag(state),
         stay1=choice1==lag(choice1),
         stay2=choice2==lag(choice2),
         transition=if_else(state_prob==0.7,"common","rare"),
         previous_transition=lag(transition))%>%na.omit()

df%>%group_by(reward_oneback,previous_transition)%>%summarise(mean(stay1))


model=brm(
  formula=stay1 ~ reward_oneback*previous_transition+(1+reward_oneback*previous_transition| subject),
  data = df,
  family = bernoulli(link = "logit"),
  warmup = 500,
  iter = 1000,
  chains = 4,
  cores = 4,
  seed = 123,
  backend = "cmdstanr"
)

custom_colors <- c("common" = "blue", "rare" = "red")
custom_labels <- c("common" = "Common", "rare" = "Rare")
plot(conditional_effects(model), plot = FALSE)[[3]]+theme_bw()+ylab("Stay Probability")+
  xlab("Previous outcome")+
  scale_x_discrete(labels = c("Unrewarded", "Rewarded"))+
  scale_color_manual(values = custom_colors,labels=custom_labels)+labs(color = "Transition Type")+
  guides(fill = "none", color = guide_legend(title = "Transition Type"))
save(model,file="data/regression/model_tst.rdata")

# Extract conditional effects
ce <- conditional_effects(model)

# If you want to extract a specific effect, you can do it like this:
ce_specific <- conditional_effects(model, effects = "reward_oneback:previous_transition")

# Extract the data frame from the conditional effects
ce_data <- ce_specific$`reward_oneback:previous_transition`

library(ggplot2)

ggplot(ce_data, aes(x = reward_oneback, y = estimate__, fill = previous_transition )) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("blue", "red")) +
  scale_x_discrete(labels=c("0"="Unrewarded", "1"="Rewarded"),limits = rev(levels(factor( ce_data$reward_oneback))))+
  geom_errorbar(aes(ymin = lower__, ymax = upper__), position = position_dodge(width = 0.9), width = 0.25, color = "black") +
  labs(x = "Previous outcome", y = "Stay Probability", fill = "Transition Type") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  )






