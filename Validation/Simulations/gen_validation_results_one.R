#------------------------------------------------------------------#
# script that produces output results for one case
#------------------------------------------------------------------#

library(here)

# simulation and user parameters
source(here::here("Validation/Simulations", "params.R"))
params.default <- user.params.list

overwrite <- TRUE


q <- as.numeric(as.character(Sys.getenv("q")))
if(is.na(q)) { q <- 1 }

sim.params.list[['Q']] <- 1
sim.params.list[['S']] <- 10
sim.params.list[['B']] <- 10

user.params.list <- params.default
user.params.list[['nbar']] <- 50

user.params.list[['K']] <- 1
user.params.list[['ICC.3']] <- NULL
user.params.list[['omega.3']] <- NULL
user.params.list[['R2.3']] <- NULL

sim.params.list[['procs']] <- c("Bonferroni", "BH", "Holm", "WY-SS", "WY-SD")

# user.params.list[['omega.2']] <- 0
# user.params.list[['ICC.2']] <- rep(0, M)
# power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite)
# user.params.list[['omega.2']] <- params.default[['omega.2']]
# power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2f", q = q, overwrite)
# user.params.list[['ICC.2']] <- params.default[['ICC.2']]
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2r", q = q, overwrite)