source(here::here("Validation/Simulations", "params.R"))

overwrite = FALSE
q <- 1

user.params.list[['K']] <- 1
user.params.list[['ICC.3']] <- rep(0, M)
user.params.list[['omega.3']] <- 0
user.params.list[['omega.2']] <- 0
user.params.list[['ICC.2']] <- rep(0, M)

# plot accuracy and time of power as a function of tnum
sim.params.list[['runPump']] <- TRUE
sim.params.list[['runSim']] <- FALSE
sim.params.list[['runPowerUp']] <- FALSE
sim.params.list[['procs']] <- c("Bonferroni", "BH", "Holm")

# first run a beefy one to get the 'true' value
sim.params.list[['tnum']] <- 50000
t1 <- Sys.time()
power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite = TRUE)
t2 <- Sys.time()

results.all <- power.results[power.results$power_type == 'D1indiv' & power.results$method == 'pum', c('MTP', 'value')]
results.all$tnum <- sim.params.list[['tnum']]
results.all$time <- difftime(t2, t1, 'secs')[[1]]

# now iterate through different tnum
tnum.seq <- c(3000, 5000, 7000, 10000)
# tnum.seq <- c(100, 200, 300)

for(i in 1:length(tnum.seq))
{
  sim.params.list[['tnum']] <- tnum.seq[i]

  t1 <- Sys.time()
  power.results <- validate_power(user.params.list, sim.params.list, design = "blocked_i1_2c", q = q, overwrite = TRUE)
  t2 <- Sys.time()
      
  results <- power.results[power.results$power_type == 'D1indiv' & power.results$method == 'pum', c('MTP', 'value')]
  results$tnum <- sim.params.list[['tnum']]
  results$time <- difftime(t2, t1, 'secs')[[1]]
  
  results.all <- rbind(results.all, results)
}

saveRDS(results.all, here::here("Validation", "tnum_test.RDS"))

ggplot(results.all, aes(x = tnum, y = value)) +
  geom_point() +
  facet_wrap(MTP~., scales = 'free')

