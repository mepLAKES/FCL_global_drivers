################################################################################
# Manuscript Title: Climate constrains whether food chain length increases with ecosystem size via body-size effects
# Script Title : Model simulations: theoretical effect of the drivers on food chain length
# Description:  This script defines and runs Schneider et al. (2016)-based food web simulations
# Date:         2026-04-27
# Version:      3.0
# Notes:        Simulations are computationally intensive and are not executed automatically.
# Dependencies: here, ATNr, deSolve, future.apply, future, doFuture
# Author: Elisa Thebault _ elisa.thebault@sorbonne-universite.fr
################################################################################


################################################################################
# 1. SETUP ----------
################################################################################
## Clear environment
rm(list = ls())

## Libraries
library(here)

library(ggplot2)
library(tidyr)
library(ATNr)
library(deSolve)
library(future.apply)
library(future)
library(doFuture)

################################################################################
# 2. FUNCTIONS ----------
################################################################################

### Basic function to calculate species trophic levels in food webs 
GetTL2 <- function(web){  
  tweb <- t(web)
  rs <- rowSums(tweb)
  for(i in 1:length(tweb[,1]))
    tweb[i,tweb[i,]>0] = tweb[i,tweb[i,]>0]/rs[i]
  nb.TL <- try(solve(diag(length(tweb[,1])) - tweb), T) 
  if(class(nb.TL)[1]=="try-error")
    nbTL <- rep(NA, length(tweb[,1]))
  if(class(nb.TL)[1]!="try-error")
    nbTL <- rowSums(nb.TL)
  nbTL
}

### Function to calculate maximum FCL from food web model outputs 
TL_max_model_Schneider<-function(mod0,final.bioms){
  fwquanti<-mod0$fw
  S<-mod0$nb_s
  n_basal<-mod0$nb_b
  matrixbiom<-matrix(rep(final.bioms^mod0$q[1],S),S,S)
  fwquanti[,-(1:n_basal)]<-mod0$b*matrixbiom[,-(1:n_basal)]
  ah<-mod0$b*mod0$h
  sum_ahB<-(final.bioms^mod0$q[1])%*%ah+1
  matrixSum_ahB<-matrix(rep(sum_ahB,each=S),S,S-n_basal)
  fwquanti[,-(1:n_basal)]<-1/matrixSum_ahB*(fwquanti[,-(1:n_basal)])
  fwquanti<-fwquanti*matrix(rep(final.bioms,each=S),S,S)
  TLq<-GetTL2(fwquanti)
  return(max(TLq))
}

### Initialize food web model for given parameter values
Model_Schneider_FWsize<-function(nb_s,n_basal,BM_min_basal,BM_max_basal,BM_min,BM_max,
                                 Temp, Snut, q){
  
  masses <- 10 ^ c(sort(runif(n_basal, BM_min_basal, BM_max_basal)),
                   sort(runif(nb_s - n_basal, BM_min, BM_max)))
  
  # create the food web matrix (method = Schneider et al. 2016)
  L <- create_Lmatrix(masses, n_basal, Ropt = 100, gamma = 2, th = 0.01)
  # create the 0/1 version of the food web
  fw2 <- L
  fw2[fw2 > 0] <- 1
  
  # create the food web model with default ATNr parameters
  mod0 <- create_model_Unscaled_nuts(nb_s = nb_s,
                                     nb_b = n_basal,
                                     nb_n=2,
                                     BM = masses,
                                     fw = fw2)
  mod0 <- initialise_default_Unscaled_nuts(mod0,fw2,temperature=Temp)
  
  #set no interference 
  mod0$c <- rep(0, mod0$nb_s - mod0$nb_b) 
  
  #set Snut and q
  mod0$q<-rep(q,mod0$nb_s - mod0$nb_b)
  mod0$S<-mod0$S*Snut
  
  #set dependence of growth rate on temperature from Sherman et al. 2016 E=-0.28
  T0 <- 293.15
  k <- 8.6173324e-5
  T.K <- Temp + 273.15
  mod0$r<-(masses[1:n_basal]^-0.25)*exp(-0.28 * (T0 - T.K) / (k * T.K * T0))
  
  return(mod0)
}

### Define the food web model with sinusoidal disturbances on additional mortality
Model_disturb<-function(t,y,params){
  model<-params[[1]]
  zmean<-params[[2]][1]
  omega<-params[[2]][2]
  zdist<-zmean*sin(omega*t)+zmean
  zanimal<-zdist*y[-(1:model$nb_n)]
  ztot<-c(rep(0,model$nb_n),-1*zanimal)
  dy<-model$ODE(y,0.0)+ztot 
  return(list(dy))
}



################################################################################
# 3. PREPARE SIMULATIONS ----------
################################################################################


# Function for running food web model with given parameters
# outputs giving maximum FCL (=TLmax) and maximum body-mass (maxBM)
simul_single <- function(i, Temp, BMmaxval, S, K) {
  set.seed(i)  # Avoid repeated random seeds across runs
  mod0 <- Model_Schneider_FWsize(S, ceiling(S*0.1), 0, 3, 2, BMmaxval, Temp, K, 1.2)
  biomasses <- runif(mod0$nb_s + mod0$nb_n, 1, 2)
  times <- seq(1, 5e4, by = 1e2)
  sol <- lsoda_wrapper(times, biomasses, mod0, verbose = FALSE)
  final.bioms <- sol[nrow(sol), -(1:3)]
  TLres <- TL_max_model_Schneider(mod0, final.bioms)
  maxBM <- max(mod0$BM * final.bioms)
  return(data.frame(Temp=Temp, S=S, K=K, BMmax=BMmaxval, TLmax=TLres, maxBM=maxBM))
}

# Function for running food web model with disturbances for given parameters
# outputs giving maximum FCL (=TLmax) and maximum body-mass (maxBM)
simul_single_disturb <- function(i, Temp, BMmaxval, S, K,Zval,omeg) {
  set.seed(i)  # Avoid repeated random seeds across runs
  times <- seq(1, 5e4, by = 1e2)
  timesbis<-seq(1, 2e3, by = 1e1)
  mod0<-Model_Schneider_FWsize(S,ceiling(S*0.1),0,3,2,BMmaxval,Temp,K,1.2)
  biomasses <- runif(mod0$nb_s+mod0$nb_n, 1, 2)
  sol <- lsoda_wrapper(times, biomasses, mod0, verbose = FALSE)
  biomasses <- sol[nrow(sol), -(1)]
  out<-lsoda(biomasses,timesbis,Model_disturb,list(mod0,c(Zval,omeg)))
  minval<-apply(out[,-1],2,min)
  biomasses<-out[nrow(out), -(1)]
  biomasses[which(minval<mod0$ext)]<-0
  out<-lsoda(biomasses,timesbis,Model_disturb,list(mod0,c(Zval,omeg)))
  final.bioms = out[nrow(out), -(1:3)]
  TLres<-TL_max_model_Schneider(mod0,final.bioms)
  maxBM <- max(mod0$BM * final.bioms)
  return(data.frame(Temp=Temp, S=S, K=K, BMmax=BMmaxval, Z=Zval,omega=omeg, TLmax=TLres, maxBM=maxBM))
}


################################################################################
# 4. SIMULATION WRAPPERS ----------
################################################################################

## 4.1 Function for simulation #1: variation of temperature and nutrient inputs ----
Simul_Nut_Temp_parallel <- function(nbrep) {
  Tempe<-c(15,20,25)
  Kval<-c(0.1,0.25,0.5,1)
  S <- 100
  BMmax<-6
  # Generate all combinations of parameter values
  params <- expand.grid(i = 1:nbrep, Temp = Tempe, K = Kval)
  
  # Plan parallel calculations (number of workers can be adjusted as needed)
  plan(multisession, workers = 15)
  
  # Run food web simulations in parallel
  results <- future.apply::future_lapply(1:nrow(params), function(j) {
    row <- params[j, ]
    tryCatch({
      simul_single(row$i, row$Temp, BMmax, S, row$K)
    }, error = function(e) {
      message(paste("Error for line", j, ":", e$message))
      return(NA)
    })},future.seed=NULL)
  
  
  # Combine all results
  result <- do.call(rbind, Filter(NROW, results)) 
  
  # Save results
  write.table(result,
              here("Theoretical modelling", "data", "result_Nut_Temp.txt"),
              sep = "\t",
              row.names = FALSE)
  return(result)
}

## 4.2 Function for simulation #2: variation of temperature and max body mass ----
Simul_maxBS_temp_parallel <- function(nbrep) {
  Tempe <- c(15,20, 25)
  BMmax <- c(4, 5, 6, 7)
  S <- 100
  K <- 1
  
  # Generate all combinations of parameter values
  params <- expand.grid(i = 1:nbrep, Temp = Tempe, BMmaxval = BMmax)
  
  # Plan parallel calculations (number of workers can be adjusted as needed)
  plan(multisession, workers = 15)
  
  # Run food web simulations in parallel
  results <- future.apply::future_lapply(1:nrow(params), function(j) {
    row <- params[j, ]
    tryCatch({
      simul_single(row$i, row$Temp, row$BMmaxval, S, K)
    }, error = function(e) {
      message(paste("Error for line", j, ":", e$message))
      return(NA)
    })},future.seed=NULL)
  
  
  # Combine all results
  result <- do.call(rbind, Filter(NROW, results)) 

  # Save results
  write.table(result,
              here("Theoretical modelling", "data", "result_BSmax_Temp.txt"),
              sep = "\t",
              row.names = FALSE)
  return(result)
}


## 4.3 Function for simulation #3: variation of disturbance parameters ----
Simul_disturb_parallel <- function(nbrep) {
  Zdist<-c(0,0.05,0.1)
  omega_val<-c(0.1,0.5,1)
  S <- 100
  K <- 1
  Temp<-20
  K<-1
  BMmax<-6
  
  # Generate all combinations of parameter values
  params <- expand.grid(i = 1:nbrep, Zval = Zdist, omeg = omega_val)
  
  # Plan parallel calculations (number of workers can be adjusted as needed)
  plan(multisession, workers = 15)
  
  # Run food web simulations with disturbances in parallel
  results <- future.apply::future_lapply(1:nrow(params), function(j) {
    row <- params[j, ]
    tryCatch({
      simul_single_disturb(row$i, Temp, BMmax, S, K, row$Zval, row$omeg)
    }, error = function(e) {
      message(paste("Error for line", j, ":", e$message))
      return(NA)
    })},future.seed=NULL)
  
  # Combine all results
  result <- do.call(rbind, Filter(NROW, results))
  
  # Save
  write.table(result,
              here("Theoretical modelling", "data", "result_Disturb.txt"),
              sep = "\t",
              row.names = FALSE)
  return(result)
}

################################################################################
# 5. RUN SIMULATIONS (MANUAL) ----------
################################################################################
# Each simulation set takes hours. Start with lower nbrep (e.g. nbrep = 1) for tests.
# This section is intentionally commented to avoid accidental execution.

# Simul_Nut_Temp_parallel(nbrep = 100)
# Simul_maxBS_temp_parallel(nbrep = 100)
# Simul_disturb_parallel(nbrep = 100)


