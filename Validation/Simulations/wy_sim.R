library(here)

# simulation and user parameters
source(here::here("Validation/Simulations", "params.R"))

design <- "blocked_i1_2c"

sim.params.list[['parallel']] <- TRUE
sim.params.list[['B']] <- 100
sim.params.list[['S']] <- 10

# --------------------------------------------------------------------- #
# generate data
# --------------------------------------------------------------------- #

model.params.list <- convert.params(user.params.list)
# save out some commonly used variables
M <- model.params.list[['M']]
S <- sim.params.list[['S']]
J <- model.params.list[['J']]
p.j <- sim.params.list[['p.j']]
alpha <- sim.params.list[['alpha']]

# cheating! only generate data once

# generate full, unobserved sample data
samp.full <- gen_full_data(model.params.list, check = sim.params.list[['check']])
S.jk <- samp.full$ID$S.jk
S.k  <- samp.full$ID$S.k

# blocked designs
if(design %in% c('blocked_i1_2c', 'blocked_i1_2f', 'blocked_i1_2r')) {
  T.ijk <- randomizr::block_ra( S.jk, prob = p.j )
  # cluster designs
} else if(design %in% c('simple_c2_2r'))  {
  T.ijk <- randomizr::cluster_ra( S.jk, prob = p.j )
} else {
  stop(print(paste('Design', design, 'not implemented yet')))
}

# convert full data to observed data
samp.obs = samp.full
samp.obs$Yobs = gen_Yobs(samp.full, T.ijk)

mdat <- makelist.samp(M, samp.obs, T.ijk, model.params.list, design = design) # list length M
rawp <- get.rawp(mdat, design = design, n.j = model.params.list[['n.j']], J = J) # vector length M
rawt <- get.rawt(mdat, design = design, n.j = model.params.list[['n.j']], J = J) # vector length M

start.time.wy = Sys.time()

if(sim.params.list[['parallel']])
{
  cl <- makeSOCKcluster(rep("localhost", sim.params.list[['ncl']]))
} else
{
  cl <- NULL
}

proc <- "WY-SD"
px <- 100
# begin loop through all samples to be generated
for (s in 1:S) {
  
  t1 <- Sys.time()
  if (s %% px == 0){ message(paste0("Now processing sample ", s, " of ", S))}

  pvals <- get.adjp(proc, rawp, rawt, mdat, sim.params.list, model.params.list, design, cl)
}

stop.time.wy = Sys.time()
wy.time = difftime(stop.time.wy, start.time.wy, units = 'mins')

print(paste('WY time diff for S =', sim.params.list[['S']], ', B =', sim.params.list[['B']], 'is', round(wy.time[[1]],3), 'mins'))

if(sim.params.list[['parallel']])
{
  parallel::stopCluster(cl)
}
