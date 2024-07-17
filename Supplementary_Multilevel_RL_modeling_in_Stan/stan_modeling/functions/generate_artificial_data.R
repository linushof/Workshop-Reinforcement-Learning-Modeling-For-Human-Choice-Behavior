### This function uses task configuration to simulate artificial data using model parameters and task configuration.
### the data is saved in a file ready for stan run, you can choose the data file with set_standata_file().
### to manually examine the data generated in this code, use "get_df" function

generate_artificial_data <- function(cfg) {
  
  # generate parameters
  simulate_parameters(path,cfg,plotme=T)

  # generate trial-by-trial data
  simulate_artifical_data(path,cfg)
  
}