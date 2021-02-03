source(here::here("Validation/Simulations", "params.R"))
user.params.list[['K']] <- 1
user.params.list[['omega.2']] <- 0
overwrite = FALSE
q <- 1

# plot accuracy and time of power as a function of tnum, snum
sim.params.list[['procs']] <- c('WY-SS', 'WY-SD')
sim.params.list[['runPump']] <- TRUE
sim.params.list[['runSim']] <- FALSE
sim.params.list[['runPowerUp']] <- FALSE

# first run a beefy one to get the 'true' value
sim.params.list[['B']] <- 50000
sim.params.list[['tnum']] <- 50000
t1 <- Sys.time()
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite = TRUE)
t2 <- Sys.time()

wy.data.ss <- data.frame(
  MTP = 'WY-SS',
  tnum = sim.params.list[['tnum']], B = sim.params.list[['B']],
  power = power.results[power.results$MTP == 'WY-SS' & power.results$power_type == 'D1indiv' & power.results$method == 'pum', 'value'],
  time = difftime(t2, t1, 'secs')[[1]]
)
wy.data.sd <- data.frame(
  MTP = 'WY-SD',
  tnum = sim.params.list[['tnum']], B = sim.params.list[['B']],
  power = power.results[power.results$MTP == 'WY-SD' & power.results$power_type == 'D1indiv' & power.results$method == 'pum', 'value'],
  time = difftime(t2, t1, 'secs')[[1]]
)
wy.data <- rbind(wy.data.ss, wy.data.sd)

# now iterate through different tnum and fixed B
# tnum.seq <- c(2, 4)
tnum.seq <- c(3000, 5000, 7000, 10000)
B.seq <- c(3000, 5000, 7000, 10000)

for(i in 1:length(tnum.seq))
{
  for(j in 1:length(B.seq))
  {
    if(B.seq[j] <= tnum.seq[i])
    {
      sim.params.list[['tnum']] <- tnum.seq[i]
      sim.params.list[['B']] <- B.seq[j]
      
      t1 <- Sys.time()
      power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite = TRUE)
      t2 <- Sys.time()
      
      wy.data.ss <- data.frame(
        MTP = 'WY-SS',
        tnum = sim.params.list[['tnum']], B = sim.params.list[['B']],
        power = power.results[power.results$MTP == 'WY-SS' & power.results$power_type == 'D1indiv' & power.results$method == 'pum', 'value'],
        time = difftime(t2, t1, 'secs')[[1]]
      )
      wy.data.sd <- data.frame(
        MTP = 'WY-SD',
        tnum = sim.params.list[['tnum']], B = sim.params.list[['B']],
        power = power.results[power.results$MTP == 'WY-SD' & power.results$power_type == 'D1indiv' & power.results$method == 'pum', 'value'],
        time = difftime(t2, t1, 'secs')[[1]]
      )
      wy.data <- rbind(wy.data, wy.data.ss, wy.data.sd)
    }
  }
}

saveRDS(wy.data, here::here("Validation", "wy_test.RDS"))

