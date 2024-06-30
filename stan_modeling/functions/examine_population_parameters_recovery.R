examine_population_parameters_recovery <- function(path, datatype) {
  library(ggplot2)
  library(bayestestR)
  library(stringr)
  library(ggpubr)
  
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
  
  #load recovered parameters
  if (datatype == 'empirical') {
    print('using empirical data')
    fit = readRDS(paste0(path$data, '/modelfit_empirical.rds'))
  }
  else if (datatype == 'artificial') {
    print('using artificial data')
    fit = readRDS(paste0(path$data, '/modelfit_recovery.rds'))
  }
  
  #load artificial parameters
  source(paste0(path$model, '_parameters.r'))
  load(paste0(path$data, '/model_parameters.Rdata'))
  
  Nparameters = length(model_parameters$artificial_population_location)
  p = list()
  for (i in 1:Nparameters) {
    samples    = fit$draws(variables = paste0('population_locations[', i, ']'),
                           format    = 'matrix')
    
    if (model_parameters$transformation[i] == 'logit') {
      samples = plogis(samples)
    }
    if (model_parameters$transformation[i] == 'exp') {
      samples = exp(samples)
    }
    
    samples    = data.frame(samples = unlist(samples))
    if (datatype == 'artificial') {
    true_value = model_parameters$artificial_population_location[i]
    }
    else
    {
      true_value = NULL
    }
    
    p[[i]] =
      ggplot(data.frame(samples = as.numeric(unlist(samples))), aes(x = samples)) +
      ggdist::stat_halfeye(
        point_interval = 'median_hdi',
        .width = c(0.89, 0.97),
        fill = 'pink'
      ) +
      geom_vline(
        xintercept = true_value,
        linetype = "dotted",
        color = "blue",
        linewidth = 1.5
      ) +
      xlab(model_parameters$names[i]) +
      mytheme +
      
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank())
    
  }
  do.call("grid.arrange", c(p, ncol = 1))
}