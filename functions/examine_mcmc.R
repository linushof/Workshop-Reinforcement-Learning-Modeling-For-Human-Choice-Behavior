examine_mcmc <- function(path, mypars, datatype) {
  library(purrr)
  if (datatype == 'empirical') {
    print('using empirical data, please wait a bit. Do not use results having rhat>=1.01')
    fit = readRDS(paste0(path$data, '/modelfit_empirical.rds'))
  }
  else if (datatype == 'artificial') {
    print('using artificial data, please wait a bit. Do not use results having rhat>=1.01')
    fit = readRDS(paste0(path$data, '/modelfit_recovery.rds'))
  }
  p = list()
  for (i in seq_along(mypars)) {
    pars_matrix <- fit$draws(variables = mypars[i], format = 'matrix')
    # Trace plots
    p[[i]] = mcmc_trace(pars_matrix)
  }
  print(fit)
  draws = fit$draws(variables = mypars)
  pairs = mcmc_pairs(draws)
  combined_plots <- c(p, list(pairs))
  do.call("grid.arrange", c(combined_plots, ncol = 2))
}