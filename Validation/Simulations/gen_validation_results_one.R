#------------------------------------------------------------------#
# script that produces output results for one case
#------------------------------------------------------------------#

library(here)

# simulation and user parameters
source(here::here("Validation/Simulations", "params.R"))

overwrite <- TRUE

q <- as.numeric(as.character(Sys.getenv("q")))
if(is.na(q)) { q <- 1 }

model.params.list[['K']] <- 1
model.params.list[['ICC.3']] <- 0
model.params.list[['omega.3']] <- 0
model.params.list[['R2.3']] <- 0
model.params.list[['numCovar.3']] <- 0
model.params.list[['numCovar.2']] <- 0
model.params.list[['R2.2']] <- 0

power.results <- validate_power(model.params.list, sim.params.list, design = "d2.1_m2fr", q = q, overwrite)

print(power.results)
