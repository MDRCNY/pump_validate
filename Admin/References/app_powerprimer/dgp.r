####################################################################################
#####
##
#                 Helper Functions for the Power Primer App
##
#####
###################################################################################
dgp = function(N, beta_0, beta_1, seed, tau = 5){
  
  # N : Number of sample size
  # beta_0 : Intercept/The baseline value
  # beta_1 : The group mean difference of X1 covariate
  # tau : treatment effect
  # seed : To be set for consistency in the data generating process
  
  set.seed(seed = seed)
  X = rnorm(seq(N), mean = 65, sd = 3)
  Y_0 = beta_0 + beta_1 * X + 0 + rnorm(seq(N), mean = 0, sd = 1)
  Y_1 = beta_0 + beta_1 * X + tau + rnorm(seq(N), mean = 0, sd = 1)
  dat = data.frame(cbind(seq(N), X, Y_0,Y_1))
  colnames(dat) <- c("Index", "X", "Y0", "Y1")
  return(dat)
  
}#dgp function ends