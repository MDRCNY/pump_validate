library(PUMP)

set.seed( 0440044 )

#------------------------------------------------------------#
# Calculating MDES
#------------------------------------------------------------#

m <- pump_mdes(
  design = "d3.2_m3fc2rc",      # choice of design and analysis strategy
  MTP = "Holm",                 # multiple testing procedure
  target.power = 0.80,          # desired power
  power.definition = "D1indiv", # power type
  M = 5,                        # number of outcomes
  J = 3,                        # number of schools per block
  K = 21,                       # number districts
  nbar = 258,                   # average number of students per school
  Tbar = 0.50,                  # prop treated
  alpha = 0.05,                 # significance level
  numCovar.1 = 5,               # number of covariates at level 1
  numCovar.2 = 3,               # number of covariates at level 2
  R2.1 = 0.1, R2.2 = 0.7,       # explanatory power of covariates for each level
  ICC.2 = 0.05, ICC.3 = 0.4,    # intraclass correlation coefficients
  rho = 0.4 )
print( m )

# for table in manuscript
knitr::kable( m, digits = 3, format = 'latex', booktabs = TRUE,
              position = "h!", caption = "MDES Estimate" )

m2 <- update( m, power.definition = "min1" )
print( m2 )

m3 <- update( m2, numZero = 2 )
print( m3 )


#------------------------------------------------------------#
# Determining necessary sample size
#------------------------------------------------------------#

smp <- pump_sample(
  design = "d3.2_m3fc2rc",
  MTP = "Holm",
  typesample = "K",
  target.power = 0.80, power.definition = "min1", tol = 0.01,
  MDES = 0.10, M = 5, nbar = 258, J = 3,
  Tbar = 0.50, alpha = 0.05, numCovar.1 = 5, numCovar.2 = 3,
  R2.1 = 0.1, R2.2 = 0.7, ICC.2 = 0.05, ICC.3 = 0.40, rho = 0.4 )

print( smp )

p_check <- update( smp, type = "power", tnum = 100000,
                   long.table = TRUE )

knitr::kable( p_check, digits = 2, format = 'latex', booktabs = TRUE,
              row.names = FALSE, position = "h!", caption = "Power table" )


plot_power_curve( smp )

#------------------------------------------------------------#
# Comparing adjustment procedures
#------------------------------------------------------------#

p2 <- update( p_check,
              MTP = c( "Bonferroni", "Holm", "WY-SD" ),
              tnum = 10000,
              parallel.WY.clusters = 3 )
plot( p2 )


#------------------------------------------------------------#
# Exploring sensitivity to design parameters
#------------------------------------------------------------#

#-------------------------------------#
# Exploring power with update()
#-------------------------------------#

p_b <- update( p_check, ICC.2 = 0.20, ICC.3 = 0.25 )
print( p_b )

p_d <- update( p_check,
               R2.1 = c( 0.1, 0.3, 0.1, 0.2, 0.2 ),
               R2.2 = c( 0.4, 0.8, 0.3, 0.2, 0.2 ) )
print( p_d )

summary( p_d )

#-------------------------------------#
# Exploring the impact of the ICC
#-------------------------------------#

grid <- update_grid( p_check,
                     ICC.2 = seq( 0, 0.3, 0.05 ),
                     ICC.3 = seq( 0, 0.60, 0.20 ),
                     tnum = 5000 )

grid$ICC.3 <- as.factor( grid$ICC.3 )
grid <- dplyr::filter( grid, MTP == "Holm" )

ggplot2::ggplot( grid, aes( ICC.2, min1, group = ICC.3, col = ICC.3 ) ) +
  geom_line() + geom_point() +
  labs( y = "Min-1 Power" ) +
  coord_cartesian( ylim = c(0, 1) )


#-------------------------------------#
# Exploring the impact of rho
#-------------------------------------#

gridRho <- update_grid( p_check,
                        MTP = c( "Bonferroni", "WY-SD" ),
                        rho = seq( 0, 0.9, by = 0.15 ),
                        tnum = 500,
                        B = 10000 )

gridL <- dplyr::filter( gridRho, MTP != "None" ) %>%
  tidyr::pivot_longer( cols = c(indiv.mean, min1, min2, complete),
                       names_to = "definition", values_to = "power" ) %>%
  dplyr::mutate( definition = factor( definition,
                                      levels = c("indiv.mean", "min1", "min2", "complete" ) ) )

ggplot2::ggplot( gridL, aes( rho, power, col = MTP ) ) +
  facet_grid( . ~ definition ) +
  geom_line() + geom_point() +
  geom_hline( yintercept = c( 0, 0.80 ) ) + 
  theme_minimal() +
  coord_cartesian( ylim = c( 0, 1 ) )

#-------------------------------------#
# Exploring the impact of null outcomes
#-------------------------------------#

gridZero <- update_grid( p_check,
                         numZero = 0:4,
                         M = 5 )

gridL <- dplyr::filter( gridZero, MTP != "None" ) %>%
  tidyr::pivot_longer( cols = c(indiv.mean, min1, min2, min3, min4 ),
                       names_to = "definition", values_to = "power" ) %>%
  dplyr::mutate( definition = as.factor( definition ) )

ggplot2::ggplot( gridL, aes( numZero, power ) ) +
  facet_grid( . ~ definition ) +
  geom_line() + geom_point() +
  geom_hline( yintercept = 0.80, col = "darkgrey" ) +
  coord_cartesian( ylim = c(0,1) ) +
  theme_minimal()
