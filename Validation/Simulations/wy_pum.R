library(here)

# simulation and user parameters
source(here::here("Validation/Simulations", "params.R"))

design = "blocked_i1_2c"
sim.params.list[['parallel']] <- TRUE
sim.params.list[['B']] = 10000 
sim.params.list[['S']] = 1


# --------------------------------------------------------------------- #
# generate data
# --------------------------------------------------------------------- #
if(sim.params.list[['parallel']])
{
  cl <- makeSOCKcluster(rep("localhost", sim.params.list[['ncl']]))
} else
{
  cl <- NULL
}

start.time.wy = Sys.time()

pum_results <- power_blocked_i1_2c(
  M = user.params.list[['M']], MTP = 'WY-SD',
  MDES = user.params.list[['ATE_ES']],
  J = user.params.list[['J']], n.j = user.params.list[['n.j']],
  p = sim.params.list[['p.j']],
  alpha = sim.params.list[['alpha']], numCovar.1 = 0, numCovar.2 = 0,
  R2.1 = user.params.list[['R2.1']], R2.2 = user.params.list[['R2.2']],
  ICC = user.params.list[['ICC']], sigma = NULL,
  rho = user.params.list[['rho.default']], omega = NULL,
  tnum = sim.params.list[['tnum']], snum = sim.params.list[['B']],
  cl = cl
)

stop.time.wy = Sys.time()
wy.time = difftime(stop.time.wy, start.time.wy, units = 'mins')

print('WY time diff')
print(wy.time)

if(sim.params.list[['parallel']])
{
  parallel::stopCluster(cl)
}
