#------------------------------------------------------------------#
# script that produces output results for one case
#------------------------------------------------------------------#

library(here)

# simulation and user parameters
source(here::here("Validation/Simulations", "params.R"))

overwrite <- TRUE

q <- as.numeric(as.character(Sys.getenv("q")))
if(is.na(q)) { q <- 1 }

user.params.list[['K']] <- 1
user.params.list[['ICC.3']] <- NULL
user.params.list[['omega.3']] <- NULL
user.params.list[['R2.3']] <- NULL
user.params.list[['omega.2']] <- 0

# params to help have a decent power
user.params.list[['ICC.2']] <- rep(0.1, M)
user.params.list[['J']] <- 60

user.params.list[['nbar']] <- 100
power.results <- validate_power(user.params.list, sim.params.list, design = "simple_c2_2r", q = q, overwrite)