icc.3.fun = function(){ return( (xi^2 + eta0.sq) / (xi^2 + eta0.sq + delta^2 + tau0.sq + gamma^2 + 1 ))}
icc.2.fun = function(){ return( (delta^2 + tau0.sq) / (xi^2 + eta0.sq + delta^2 + tau0.sq + gamma^2 + 1 ))}


gamma.fun = function(){ return( (R2.1 / (1 - R2.1)) ) }
delta.fun = function(){ return( (R2.2 / (1 - R2.1)) * ( ICC.2 / (1 - ICC.2 - ICC.3) ) ) }
xi.fun = function(){ return( (R2.3 / (1 - R2.1)) * ( ICC.3 / (1 - ICC.2 - ICC.3) ) ) }
tau0.sq.fun = function(){ return( ( (1 - R2.2) / (1- R2.1)) * ( ICC.2 / (1 - ICC.2 - ICC.3) ) ) }
eta0.sq.fun = function(){ return( ( (1 - R2.3) / (1- R2.1)) * ( ICC.3 / (1 - ICC.2 - ICC.3) ) ) }

r21.fun = function(){ return( 1 - 1/(gamma^2 + 1) )}
r22.fun = function(){ return( 1 - tau0.sq/(delta^2 + tau0.sq) )}
r23.fun = function(){ return( 1 - eta0.sq/(xi^2 + eta0.sq) )}

xi = 2
eta0.sq = 3
delta = 4
tau0.sq = 5
gamma = 6

icc.3 = icc.3.fun()
print(icc.3)
icc.2 = icc.2.fun()
print(icc.2)
r21 = r21.fun()
print(r21)
r22 = r22.fun()
print(r22)
r23 = r23.fun()
print(r23)

ICC.3 = 0.1076923
ICC.2 = 0.3230769
R2.1 = 0.972973
R2.2 = 0.7619048
R2.3 = 0.5714286

print(paste('Gamma:', gamma, 'Estimate:', sqrt(gamma.fun())))
print(paste('Delta:', delta, 'Estimate:', sqrt(delta.fun())))
print(paste('Xi:', xi, 'Estimate:', sqrt(xi.fun())))
print(paste('Eta0:', eta0.sq, 'Estimate:', eta0.sq.fun()))
print(paste('Tau0:', tau0.sq, 'Estimate:', tau0.sq.fun()))
