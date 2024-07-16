#load
load("C:/Users/Ido/My Drive/R/unchosen_mf_mb/data/model_recovery/data_baseline/modelfit_like_per_trial_recovery_mf.rdata")
mf=like
load("C:/Users/Ido/My Drive/R/unchosen_mf_mb/data/model_recovery/data_baseline/modelfit_like_per_trial_recovery_mb.rdata")
mb=like

load("C:/Users/Ido/My Drive/R/unchosen_mf_mb/data/model_recovery/data_baseline/modelfit_like_per_trial_recovery_baseline.rdata")
baseline=like

load("C:/Users/Ido/My Drive/R/unchosen_mf_mb/data/model_recovery/data_baseline/modelfit_like_per_trial_recovery_full.rdata")
full=like

#waic and loo
library(loo)
# w_mf=loo::waic(mf)
# w_mb=WAIC(mb)
# # w_full=WAIC(full)
# w_baseline=WAIC(baseline)
options(mc.cores = 4)
loo_mf=loo(mf)
loo_mb=loo(mb)
loo_full=loo(full)
loo_baseline=loo(baseline)
loo_compare(loo_full,loo_mf,loo_mb,loo_baseline)
#sample sums
baseline=rowMeans(baseline)
full=rowMeans(full)
mf=rowMeans(mf)
mb=rowMeans(mb)
# Convert lists to data frames
baseline_df <- data.frame(value = unlist(baseline))
full_df <- data.frame(value = unlist(full))
mf_df <- data.frame(value = unlist(mf))
mb_df <- data.frame(value = unlist(mb))

# Add a column to distinguish the datasets
baseline_df$group <- 'baseline'
full_df$group <- 'full'
mf_df$group <- 'mf'
mb_df$group <- 'mb'

# Combine the data frames
combined_data <- rbind(full_df,mf_df,mb_df,baseline_df)

# Load the ggplot2 library
library(ggplot2)

# Create the density plot
ggplot(combined_data, aes(x = exp(value), fill = group)) +
  geom_density(alpha = 0.85, color = NA) +  # Remove border lines of densities
  labs(y = NULL) +  # Remove y-axis label
  theme_bw() +
  theme(axis.text.y = element_blank(),  # Remove y-axis text
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.title.y = element_blank(),  # Remove y-axis title
        panel.grid.major.y = element_blank(),  # Remove major grid lines on y-axis
        panel.grid.minor.y = element_blank()) +  # Remove minor grid lines on y-axis
  guides(fill = guide_legend(title = "group"))  # Adjust legend
