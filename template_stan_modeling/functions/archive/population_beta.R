fit = readRDS(paste0(path$data, '/modelfit_empirical.rds'))
mytheme =
  theme_pubclean() +
  theme(
    panel.border   = element_blank(),
    axis.line      = element_line(color = 'gray'),
    text           = element_text(size = 14,  family = "serif"),
    axis.title     = element_text(size = 14),
    legend.position = "right",
    plot.title     = element_text(hjust = 0.5)
  )
p=list()
samples    = fit$draws(variables = "f_p",
                       format    = 'matrix')
sample_value = mean(model_parameters$artificial_individual_parameters[,"pr"])
p[[10]]=ggplot(data.frame(samples = as.numeric(unlist(samples))), aes(x = samples)) +
  ggdist::stat_halfeye(
    point_interval = 'median_hdi',
    .width = c(0.89, 0.97),
    fill = 'grey'
  ) +
  xlab("f_p_ind") +
  mytheme +
  
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+scale_x_continuous(limits=c(0,1))

p[[10]]
do.call("grid.arrange", c(p, ncol = 5))
