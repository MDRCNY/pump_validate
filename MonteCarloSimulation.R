#' ---
#' title: "Monte Carlo Simulation Code"
#' output: html_notebook
#' ---
#' 
#' This code generates the Monte Carlo simulations for validating methods in the paper (table C.3) and calls items from I:\Multiplicity\Archive\Domino Copy\ECmethods\R.
#' 
## ------------------------------------------------------------------------
#source("libraries.install.R")

library(RcppEigen)
library(snow)
library(lme4)

#require(RcppEigen)
#require(snow)
#require(lme4)

source("http://bioconductor.org/biocLite.R")
biocLite("multtest")
#require(multtest)
library(multtest)

source("gen.blocked.data.R")
source("adjust.WY.R")
source("functions.R")

funct<-"fixfastLm"; mod.type<-"fixed"
source("make.model.R")

rho<-0.8

ncl<-24

procs<-c("Bonferroni", "BH", "Holm", "WY") 
M<-6
MDES<-rep(0.125, M)
p.j.range<-c(0.5,0.5)
S=2000
#S=3
B=10000
J=20;n.j=100
theta<-matrix(0,M,M); diag(theta)<-0; omega <- rep(0,M)
Gamma.00<-rep(0,M);sig.sq<-rep(1,M); alpha<-0.05
ICC<-rep(0, M); R2.2<-rep(0,M); R2.1<-rep(0, M)

rho.0_lev1<-matrix(rho,M,M); diag(rho.0_lev1)<-1 
rho.0_lev2<-matrix(0.5,M,M); diag(rho.0_lev2)<-1
rho.1_lev2<-matrix(0.5,M,M); diag(rho.1_lev2)<-1



simname<-paste0("M", M, "n.j", n.j, "J", J, "ICC", ICC[1], "MDES", MDES[1], "rho", rho, "_S", S, "B", B, "_R2.1", R2.1[1],"_R2.2",R2.2[1], mod.type, "_sim.Rda")
simpwr<-est.power(procs=procs, M=M, DMDES=MDES, n.j=n.j, J=J, rho.0_lev1=rho.0_lev1, 
                  rho.0_lev2=rho.0_lev2, rho.1_lev2=rho.1_lev2, theta=theta, ICC=ICC, 
                  alpha=alpha, Gamma.00=Gamma.00, sig.sq=sig.sq, p.j.range=p.j.range, 
                  R2.1=R2.1, R2.2=R2.2, check=FALSE, omega=omega, funct=mod.type, S=S, ncl=ncl, B=B,maxT=FALSE)
#save(simpwr, file=simname)



