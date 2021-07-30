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

power.results <- validate_power(user.params.list, sim.params.list, design = "d2.1_m2fr", q = q, overwrite)

print(power.results)
