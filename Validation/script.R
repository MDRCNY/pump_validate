# base parameters
M = 3
xi.scalar = delta.scalar = gamma.scalar = 1
tau0.sq.scalar = tau1.sq.scalar = eta0.sq.scalar = eta1.sq.scalar = 1
rho.D.scalar = rho.w.scalar = rho.z.scalar = 0
rho.X.scalar = rho.u.scalar = rho.v.scalar = 0
rho.C.scalar = rho.r.scalar = 0

n.jk = 10
J = 4
K = 2

S.j = rep(NA, N)
start.index = 1
end.index = n.jk
for(j in 1:J)
{
  S.j[start.index:end.index] = j
  start.index = end.index + 1
  end.index = end.index + n.jk
}

S.k = rep(NA, N)
start.index = 1
n.k = N/K
end.index = n.k
for(k in 1:K)
{
  S.k[start.index:end.index] = k
  start.index = end.index + 1
  end.index = end.index + n.k
}

model.params.list = list()

model.params.list[['M']]        = M
model.params.list[['Xi0']]      = 0
model.params.list[['Xi1']]      = 1

model.params.list[['S.j']]      = S.j
model.params.list[['S.k']]      = S.k

model.params.list[['xi']]       = rep(xi.scalar, M)
model.params.list[['rho.D']]    = gen.corr.matrix(M, rho.D.scalar)
model.params.list[['eta0.sq']]  = rep(eta0.sq.scalar, M)
model.params.list[['eta1.sq']]  = rep(eta1.sq.scalar, M)
model.params.list[['rho.w']]    = gen.corr.matrix(M, rho.w.scalar)
model.params.list[['rho.z']]    = gen.corr.matrix(M, rho.z.scalar)
model.params.list[['theta.wz']] = matrix(0, M, M)

model.params.list[['delta']]    = rep(delta.scalar, M)
model.params.list[['rho.X']]    = gen.corr.matrix(M, rho.X.scalar)
model.params.list[['tau0.sq']]  = rep(tau0.sq.scalar, M)
model.params.list[['tau1.sq']]  = rep(tau1.sq.scalar, M)
model.params.list[['rho.u']]    = gen.corr.matrix(M, rho.u.scalar)
model.params.list[['rho.v']]    = gen.corr.matrix(M, rho.v.scalar)
model.params.list[['theta.uv']] = matrix(0, M, M)

model.params.list[['gamma']]    = rep(gamma.scalar, M)
model.params.list[['rho.C']]    = gen.corr.matrix(M, rho.C.scalar)
model.params.list[['rho.r']]    = gen.corr.matrix(M, rho.r.scalar)

full.data <- gen_full_data(model.params.list, check = FALSE)

# generate treatment assignments and observed data
T.ijk = rbinom(N, 1, 0.5)
samp.obs = full.data
samp.obs$Yobs = gen_Yobs(full.data, T.ijk)
Yobs <- gen_Yobs(full.data, T.ijk)


