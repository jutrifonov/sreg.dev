#%##%##%##%###%##%##%##%###%##%##%##%###%##%
### This R file provides the template   ####
###     for Monte-Carlo simulations     ####
### under CAR with multiple treatments  ####
#%##%##%##%###%##%##%##%###%##%##%##%###%##%
####      The code is developed by      ####
####      @Juri Trifonov, UChicago      ####
####            Supervisors:            ####
####      @Azeem Shaikh, UChicago       ####
####    @Max Tabord-Meehan, UChicago    ####
#%##%##%##%###%##%##%##%###%##%##%##%###%##%
#%##%##%##%##
#%# v.4.1 #%#
#%##%##%##%##
#-------------------------------------------------------------------
library(sandwich) 
library(lubridate)
library(compiler)
library(extraDistr)
library(VGAM)
library(Matrix)
library(parallel)
library(progress)
#install.packages(c(
#  "compiler",
#  "extraDistr",
#  "VGAM",
#  "Matrix",
#  "parallel",
#  "progress"), dependencies = TRUE)
#%##%##%##%###%##%##%##%###%##%##%##%###%##%#%##%##%##%###%##%##%##%##
#%##%##%##%###%##%##%##%###%##%##%##%###%##%#%##%##%##%###%##%##%##%##
#         Please, provide the path to the corresponding source
#                  file with functions on your PC
#                    ↓↓↓↓↓↓↓↓↓↓↓HERE↓↓↓↓↓↓↓↓↓↓↓
#source('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.func_v.3.0.R')
#source('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.func_v.4.2.R')   #variance for the first parameter is underestimated, for the second is perfectly fine
#source('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.func_v.4.2.1.R') #for both parameters variance is seriously overestimated
#source('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.func_v.4.2.2.R')
source('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.func_v.4.5.R')

enableJIT(3)
#source('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.func_v.1.4.R')
#%##%##%##%###%##%##%##%###%##%##%##%###%##%#%##%##%##%###%##%##%##%##
#%##%##%##%###%##%##%##%###%##%##%##%###%##%#%##%##%##%###%##%##%##%##

options(scipen = 999)        # disable scientific notation

# Set up a cluster with available CPU cores
num_cores <- detectCores()
cl <- makeCluster(num_cores,outfile = "")

######## ^^^ SIMULATION PARAMETERS SHOULD BE DEFINED HERE ^^^ ##########
clusterEvalQ(cl, {
  library(sandwich) 
  library(lubridate)
  library(compiler)
  library(extraDistr)
  library(VGAM)
  library(Matrix)
  library(progress)
  library(parallel)
  source('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.func_v.4.5.R')
})

# The main function for the Lapply loop
sim.func <- function(sim.id)
{
  G = 100;
  Nmax=500;
  tau.vec <- c(0)
  n.treat <- length(tau.vec)
  max.support = Nmax/10-1;
  gamma.vec <-c(0.4, 0.2, 1)
  n.strata <- 4
  
  seed <- 1000 + sim.id
  set.seed(seed)
  Ng <- gen.cluster.sizes(G, max.support)[,1]
  #Ng <- rep(Nmax, G)
  data.pot <- gen.data.pot(Ng=Ng, tau.vec = (tau.vec / 0.5), G = G, gamma.vec = gamma.vec, n.treat=n.treat)
  strata <- form.strata(data.pot, n.strata)
  strata.set <- data.frame(strata)
  strata.set$S <- max.col(strata.set)
  pi.vec <- rep(c(1 / (n.treat + 1)), n.treat)   
  data.sim <- dgp.obs(data.pot, I.S = strata, pi.vec, n.treat)
  
  finale <- data.frame('Y'= data.sim$Y, 'D' = data.sim$D)
  Y <- data.sim$Y
  D <- data.sim$D
  S <- data.sim$S
  X <- data.sim$X
  Ng <- data.sim$Ng
  G.id <- data.sim$G.id
  
  #model <- lm.iter(Y,D,S,G.id,Ng,X, exp.option =T)
  #fit <- tau.hat(Y,D,S,G.id,Ng,X,model, exp.option = T)\
  #fit$tau.hat
  #as.var <- as.var(Y,S,D,X,model, fit)
  result <- tryCatch({sreg(Y,D,S,G.id,Ng,X, exp.option = F)}, error = function(e) {
    # Print the error message if an error occurs
    cat("Simulation", sim.id, "encountered an error:", conditionMessage(e), "\n")
    # Return a default value or NULL when an error occurs
    NA
  })
  
  #result <- sreg(Y,D,S,G.id,Ng,X, exp.option = F)
  
  
  if (anyNA(result) == TRUE)
  {
    tau      <- NA
    se       <- NA
    tstat    <- NA
    CI.left  <- NA
    CI.right <- NA
    ci.hit   <- NA
    results <- list(tau = tau,
                    se = se,
                    tstat = tstat,
                    CI.left = CI.left,
                    CI.right = CI.right,
                    ci.hit = ci.hit)
  } else{
    tau      <- result$tau.hat
    se       <- result$se.rob
    tstat    <- result$t.stat
    CI.left  <- result$CI.left
    CI.right <- result$CI.right
    ci.hit   <- as.numeric(tau.vec >= result$CI.left & 
                             tau.vec <= result$CI.right)
    results <- list(tau = tau,
                    se = se,
                    tstat = tstat,
                    CI.left = CI.left,
                    CI.right = CI.right,
                    ci.hit = ci.hit)}
  message(paste("Simulation", sim.id, "completed succesfully"))
  #setTxtProgressBar(pb, sim.id)
  return(results)
  
}

# Parallelize the simulations and store the results
simres <- parLapply(cl, 1:5000, sim.func)
mb <- microbenchmark(parLapply(cl, 1:100, sim.func), times = 1)

# Close the cluster
stopCluster(cl)

# Now, results is a list of results from each simulation

# Extract beta and se from the results
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
mean(tau)
sd(tau)
mean(se)
mean(ci.hit)
length(tau)
install.packages("microbenchmark")
library(microbenchmark)