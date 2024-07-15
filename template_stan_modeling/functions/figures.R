library(brms)
library(ggplot2)
# behavioral signature ----------------------------------------------------

figure1 <- function(plot_object, name,mf) {
  plot_object= plot_object +
    theme_bw() +
    ylim(0.425, 0.525) +
    scale_x_discrete(labels = c("Unrewarded", "Rewarded"))+
    xlab("Previous unique outcome")
  if(mf){
    plot_object =plot_object + ylab("P(ch_prev_counterfactual_person)")
    }
  else{
    plot_object = plot_object + ylab("P(ch_prev_counterfactual_product)")
  }
  print(plot_object)
    ggsave(filename = paste0("C:/Users/Ido/My Drive/graphics/mf_mb_unchosen/signatures/",name,".eps"),plot=plot_object, width = 270 / 72, height = 288 / 72)
}
#full model
load("C:/Users/Ido/My Drive/R/unchosen_mf_mb/data/stanmodel_mb_mf_unchosen_ca_beta/regression/model_free_unch_full.Rdata")
load("C:/Users/Ido/My Drive/R/unchosen_mf_mb/data/stanmodel_mb_mf_unchosen_ca_beta/regression/model_based_unch_full.Rdata")

p=plot(conditional_effects(mf_unch_b), plot = FALSE)[[1]]
figure1(p,"full_mf",mf=T)
p=plot(conditional_effects(mb_unch_b), plot = FALSE)[[1]]
figure1(p,"full_mb",mf=F)


#mf_only model
load("C:/Users/Ido/My Drive/R/unchosen_mf_mb/data/stanmodel_mb_mf_unchosen_ca_beta/regression/model_free_unch_mf.Rdata")
load("C:/Users/Ido/My Drive/R/unchosen_mf_mb/data/stanmodel_mb_mf_unchosen_ca_beta/regression/model_based_unch_mf.Rdata")

p=plot(conditional_effects(mf_unch_b), plot = FALSE)[[1]]
figure1(p,"mf_mf",mf=T)
p=plot(conditional_effects(mb_unch_b), plot = FALSE)[[1]]
figure1(p,"mf_mb",mf=F)

#mb_only model
load("C:/Users/Ido/My Drive/R/unchosen_mf_mb/data/stanmodel_mb_mf_unchosen_ca_beta/regression/model_free_unch_mb.Rdata")
load("C:/Users/Ido/My Drive/R/unchosen_mf_mb/data/stanmodel_mb_mf_unchosen_ca_beta/regression/model_based_unch_mb.Rdata")

p=plot(conditional_effects(mf_unch_b), plot = FALSE)[[1]]
figure1(p,"mb_mf",mf=T)
p=plot(conditional_effects(mb_unch_b), plot = FALSE)[[1]]
figure1(p,"mb_mb",mf=F)

#none model
load("C:/Users/Ido/My Drive/R/unchosen_mf_mb/data/stanmodel_mb_mf_unchosen_ca_beta/regression/model_free_unch_none.Rdata")
load("C:/Users/Ido/My Drive/R/unchosen_mf_mb/data/stanmodel_mb_mf_unchosen_ca_beta/regression/model_based_unch_none.Rdata")

p=plot(conditional_effects(mf_unch_b), plot = FALSE)[[1]]
figure1(p,"none_mf",mf=T)
p=plot(conditional_effects(mb_unch_b), plot = FALSE)[[1]]
figure1(p,"none_mb",mf=F)


# figure2-modulation ------------------------------------------------------
load("C:/Users/Ido/My Drive/R/unchosen_mf_mb/data/stanmodel_mb_mf_unchosen_ca_beta/regression/model_free_unch_modulation_full.Rdata")
load("C:/Users/Ido/My Drive/R/unchosen_mf_mb/data/stanmodel_mb_mf_unchosen_ca_beta/regression/model_based_unch_modulation_full.Rdata")

figure2 <- function(plot_object, name,mf) {
  plot_object= plot_object +
    theme_bw() +scale_fill_manual(
      values=c("#F8766D","#619CFF"),
      labels = c("Unrewarded", "Rewarded"))+
    scale_color_manual(
      values=c("#F8766D","#619CFF"),
      labels = c("Unrewarded", "Rewarded"))+
    guides(fill = guide_legend(title = "Previous unique outcome"), color = guide_legend(title = "Previous unique outcome"))
  if(mf){
    plot_object =plot_object + ylab("P(ch_prev_counterfactual_person)")
    plot_object =plot_object + xlab(expression("C"["mf_unch"]))
    
  }
  else{
    plot_object = plot_object + ylab("P(ch_prev_counterfactual_product)")
    plot_object =plot_object + xlab(expression("C"["mb_unch"]))
  }
  print(plot_object)
  ggsave(filename = paste0("C:/Users/Ido/My Drive/graphics/mf_mb_unchosen/signatures/modulation/",name,".svg"),plot=plot_object, width = 468 / 72, height = 351 / 72)
}
p=plot(conditional_effects(mf_unch_modulation_b), plot = FALSE)[[3]]
figure2(p,"mf_mod",mf=T)
p=plot(conditional_effects(mb_unch_modulation_b), plot = FALSE)[[3]]
figure2(p,"mb_mod",mf=F)
# precision ---------------------------------------------------------------

library(ggplot2)

file_paths <- c(
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n25.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n50.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n100.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n150.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n200.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n300.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n400.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mf_unch_n500.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n25.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n50.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n100.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n150.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n200.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n300.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n400.Rdata",
  "data/stanmodel_mb_mf_unchosen_ca_beta/precision/mb_unch_n500.Rdata")

results <- lapply(file_paths, function(path) {
  e <- new.env()
  load(path, envir = e)  # Load the .Rdata file into a new environment
  # Assume the object name you need to work with is known, here we use `fit` as an example
  if(is.null(e$mf_unch_b)){
    fit <- e$mb_unch_b  # Access the brmsfit object from the new environment{
    analysis="mb"    
  }
  else{
    fit <- e$mf_unch_b  # Access the brmsfit object from the new environment{
    analysis="mf"    
  }
  
  if ("previous_unique_reward1" %in% rownames(summary(fit)$fixed)) {
    lower_ci <- summary(fit)$fixed["previous_unique_reward1", "l-95% CI"]
    upper_ci <- summary(fit)$fixed["previous_unique_reward1", "u-95% CI"]
    ci_width <- abs(upper_ci - lower_ci) 
  }
  num_participants <- length(unique(fit$data$subject))
  data.frame(
    analysis=analysis,
    sample_size = num_participants,
    ci_width = ci_width)
})
results_df <- do.call(rbind, results)

p=ggplot(results_df, aes(x = sample_size, y = ci_width,color=analysis)) +
  geom_line() +
  geom_point() +
  theme_bw()+ylab("HDI95% Width")+xlab("Sample size")+
  labs(color = "Analysis")+scale_color_manual(
    values=c("green","blue"),
    labels = c("MB", "MF"))
ggsave(filename = "C:/Users/Ido/My Drive/graphics/mf_mb_unchosen/precision/precision_agnostic.eps", plot = p, width = 400 / 72, height = 300 / 72)
