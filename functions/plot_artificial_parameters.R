#Aim:
#create a quick graphical summary of the artificial parameters that were sampled

plot_artifical_parameters<-function(model_parameters,plot_method){
  check_packages(c('ggplot2','gridExtra'))
  
    data = as.data.frame(model_parameters$artificial_individual_parameters)
  

    
    #generate a table with population parameters
    x1=data.frame(names     = model_parameters$names,
                  location  = model_parameters$artificial_population_location,
                  scale     = model_parameters$artificial_population_scale,
                  row.names = NULL)
    
    
    p1=tableGrob(x1, rows = NULL)
    p1    = tableGrob(x1,rows = NULL)
    title = textGrob("   \n   \npopulation parameters true values", y=1,gp = gpar(fontsize = 10, fontface = "bold"))
    p1    = arrangeGrob(p1, top = title,heights = unit.c(unit(5, "lines"), unit(1, "null")))
    
    #generate a table with observed statistics of the population parameters
    
    x2=data.frame(names     = model_parameters$names,
                  location  = apply(data, 2, mean),
                  scale     = apply(data, 2, sd),
                  row.names = NULL)
    
    
    p2    = tableGrob(x2,rows = NULL) 
    title = textGrob("   \n   \npopulation parameters estimate from sample", y=1, gp = gpar(fontsize = 10, fontface = "bold"))
    p2    = arrangeGrob(p2, top = title,heights = unit.c(unit(5, "lines"), unit(1, "null")))
    
    #draw histograms from individual parameters
    data_long <- data %>%                        
      pivot_longer(colnames(data)) %>% 
      as.data.frame()
    
    p3 <- ggplot(data_long, aes(x = value)) 
    #dot method
    if(plot_method=='hist'){
      p3=p3+geom_histogram(color='navy',fill='blue') 
    }
    
    #dot method
    if(plot_method=='dot'){
      p3=p3+geom_dotplot(color='navy',fill='blue') 
    }
    
    #dot method
    if(plot_method=='density'){
      p3=p3+geom_density(color='navy',fill='blue') 
    }
    
    p3=p3+facet_wrap(~ name, scales = "free") + ggtitle("True individual parameters")
    
    
    #plot everything together
   
    grid.arrange(p1,p2,p3,
                 ncol = 2, 
                 layout_matrix = matrix(c(1, 3, 2, 3), 2, 2)
                 )
    
  
}