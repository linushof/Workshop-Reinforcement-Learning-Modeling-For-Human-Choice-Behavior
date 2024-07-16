examine_population_parameters_recovery <- function(path, datatype,ncolumns=1,format) {
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
  
  p = list()
  if(format!="beta"){
    Nparameters = length(model_parameters$artificial_population_location)
    
    for (i in 1:Nparameters) {
      samples    = fit$draws(variables = paste0('population_locations[', i, ']'),
                             format    = 'matrix')
      if (datatype == 'artificial'){
        sample_value = mean(model_parameters$artificial_individual_parameters[,i])
        true_value=model_parameters$artificial_population_location[i]
        if (model_parameters$transformation[i]=='logit'){
          samples = plogis(samples)
          sample_value = plogis(mean(model_parameters$artificial_individual_parameters[,i]))
          true_value=plogis(model_parameters$artificial_population_location[i])
        }
      }
      else{
        true_value = NULL
        sample_value=NULL
      }
      
      samples    = data.frame(samples = unlist(samples))
      
      p[[i]] =
        ggplot(data.frame(samples = as.numeric(unlist(samples))), aes(x = samples)) +
        ggdist::stat_halfeye(
          point_interval = 'median_hdi',
          .width = c(0.95),
          fill = 'grey'
        ) +   
        geom_vline(xintercept = true_value, 
                   linetype="dotted",
                   color = "blue", 
                   linewidth=1.5)+
        geom_vline(xintercept = sample_value, 
                   linetype="dotted",
                   color = "lightblue", 
                   linewidth=1.5)+
        xlab(model_parameters$names[i]) +
        mytheme +
        
        theme(axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank())
      if (model_parameters$transformation[i] == "logit") {
        p[[i]] = p[[i]] + scale_x_continuous(limits = c(0, 1))
      } 
      else {
        p[[i]] = p[[i]] + scale_x_continuous(limits = c(-1, 1))
      } 
    }
  }
  else{
    Nparameters = sum(model_parameters$transformation=="none")
    Nparameters_transformed = sum(model_parameters$transformation!="none")
    p = list()
    for (i in 1:Nparameters) {
      
      samples    = fit$draws(variables = paste0('population_locations[', i, ']'),
                             format    = 'matrix')
      
      if (datatype == 'artificial'){
        sample_value = mean(model_parameters$artificial_individual_parameters[,i])
        true_value=model_parameters$artificial_population_location[i]
        
      }
      else{
        true_value = NULL
        sample_value=NULL
      }
      
      samples    = data.frame(samples = unlist(samples))
      
      p[[i]] =
        ggplot(data.frame(samples = as.numeric(unlist(samples))), aes(x = samples)) +
        ggdist::stat_halfeye(
          point_interval = 'median_hdi',
          .width = c(0.95),
          fill = 'grey'
        ) +   
        geom_vline(xintercept = true_value, 
                   linetype="dotted",
                   color = "blue", 
                   linewidth=1.5)+
        geom_vline(xintercept = sample_value, 
                   linetype="dotted",
                   color = "lightblue", 
                   linewidth=1.5)+
        xlab(model_parameters$names[i]) +
        mytheme +
        
        theme(axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank())
      
      p[[i]] = p[[i]] + scale_x_continuous(limits = c(-1, 10))
    }
    for (i in 1:Nparameters_transformed) {
      samples    = fit$draws(variables = paste0('population_locations_transformed[', i, ']'),
                             format    = 'matrix')
      
      if (datatype == 'artificial'){
        sample_value = mean(model_parameters$artificial_individual_parameters[,i+Nparameters])
        true_value=model_parameters$artificial_population_location[i+Nparameters]
      }
      else{
        true_value = NULL
        sample_value=NULL
      }
      
      
      p[[i+Nparameters]] =
        ggplot(data.frame(samples = as.numeric(unlist(samples))), aes(x = samples)) +
        ggdist::stat_halfeye(
          point_interval = 'median_hdi',
          .width = c(0.95),
          fill = 'grey'
        ) +   
        geom_vline(xintercept = true_value, 
                   linetype="dotted",
                   color = "blue", 
                   linewidth=1.5)+
        geom_vline(xintercept = sample_value, 
                   linetype="dotted",
                   color = "lightblue", 
                   linewidth=1.5)+
        xlab(model_parameters$names[i+Nparameters]) +
        mytheme +
        
        theme(axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_blank())
      p[[i+Nparameters]] = p[[i+Nparameters]] + scale_x_continuous(limits = c(0, 1))
    } 
    
  }
  do.call("grid.arrange", c(p, ncol = ncolumns))
  
}
