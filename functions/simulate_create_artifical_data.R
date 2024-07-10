
simulate_artifical_data <-function(path,cfg){

  #load parameters
  load(paste0(path$data,'/model_parameters.Rdata'))

  #set sample size
  Nsubjects =dim(model_parameters$artificial_individual_parameters)[1] 
  
  df=data.frame()
  source(paste0(path$model,'.r'))
  # for faster performance use this
  # 
  # n.cores <- parallel::detectCores() - 1
  # 
  # my.cluster <- parallel::makeCluster(
  #   n.cores, 
  #   type = "PSOCK"
  # )
  # 
  # doParallel::registerDoParallel(cl = my.cluster)

  # df<-
  #   foreach(
  #     subject = 1:Nsubjects,
  #     .combine = rbind
  #   ) %do% {
  #     sim.block(subject=subject, 
  #               parameters=model_parameters$artificial_individual_parameters[subject,],
  #               cfg=cfg)
  #   }
  
  for (subject in 1:Nsubjects) {
    df=rbind(df, sim.block(subject=subject, parameters=model_parameters$artificial_individual_parameters[subject,],cfg=cfg))
  }

  data_path = paste0(path$data,'/artificial_data.Rdata')

  #save
  save(df,file=data_path)
  write.csv(df, paste0(path$data,'/artificial_data.csv'))
  cat(paste0('[stan_modeling]:  "artificial_data.Rdata" was saved at "',path$data,'"'))
  
}