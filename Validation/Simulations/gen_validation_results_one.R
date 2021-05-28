#------------------------------------------------------------------#
# script that produces output results for one case
#------------------------------------------------------------------#

library(here)

# simulation and user parameters
source(here::here("Validation/Simulations", "params.R"))

overwrite <- TRUE

q <- as.numeric(as.character(Sys.getenv("q")))
if(is.na(q)) { q <- 1 }

user.params.list[['omega.2']] <- 0

user.params.list[['R2.3']] <- rep(0, M)
user.params.list[['ICC.3']] <- rep(0, M)
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_c2_3f", q = q, overwrite)
