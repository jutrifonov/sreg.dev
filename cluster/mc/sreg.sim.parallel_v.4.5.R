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
install.packages(c(
                 "compiler",
                 "extraDistr",
                 "VGAM",
                 "Matrix",
                 "parallel",
                 "progress"), dependencies = TRUE)
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
####### ↓↓↓ SIMULATION PARAMETERS SHOULD BE DEFINED HERE ↓↓↓ ##########
# TRUE VALUES OF PARAMETERS FOR DGP #
G = 2000;
Nmax=500;
tau.vec <- c(0)
n.treat <- length(tau.vec)
max.support = Nmax/10-1;
gamma.vec <-c(0.4, 0.2, 1)
n.strata <- 4
n.sim <- 100

# Set up a cluster with available CPU cores
num_cores <- detectCores()
cl <- makeCluster(num_cores,outfile = "")

######## ^^^ SIMULATION PARAMETERS SHOULD BE DEFINED HERE ^^^ ##########
#pb <- txtProgressBar(min = 0,          # Minimum value of the progress bar
#                     max = n.sim,      # Maximum value of the progress bar
#                     style = 3,        # Progress bar style (also available style = 1 and style = 2)
#                     width = n.sim,    # Progress bar width. Defaults to getOption("width")
#                     char = "=")       # Character used to create the bar

#init <- numeric(n.sim)
#end <- numeric(n.sim)
#pb <- progress_bar$new(
#  format = "[:bar] :percent Elapsed: :elapsed ETA: :eta",
#  total = n.sim
#)

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
  #pb <- txtProgressBar(min = 0, max = 100, style = 3)
  sim.completed <- 0
})
# Add the progress package and create a progress bar on the workers
sim.completed <- 0
  
sim.func <- function(sim.id)
{
  #init[sim.id] <- Sys.time()
  G = 1500;
  Nmax=500;
  tau.vec <- c(0)
  n.treat <- length(tau.vec)
  max.support = Nmax/10-1;
  gamma.vec <-c(0.4, 0.2, 1)
  n.strata <- 10
  
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
  result <- sreg(Y,D,S,G.id,Ng,X, exp.option = F)
  
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
                  ci.hit = ci.hit)
  #end[s] <- Sys.time()
  
  #setTxtProgressBar(pb, s)
  #time <- round(seconds_to_period(sum(end - init)), 0)
  
  # Estimated remaining time based on the
  # mean time that took to run the previous iterations
  #est <- n.sim * (mean(end[end != 0] - init[init != 0])) - time
  #remainining <- round(seconds_to_period(est), 0)
  
  #cat(paste(" // Execution time:", time,
  #          " // Estimated time remaining:", remainining,
  #          " // SIMULATION NUMBER:", s), "")
  # Access the cluster object within the worker's environment
  #clusterEvalQ(cl, {
  #  pb_worker$tick()  # Update the progress bar on the worker
  #})
  #print(cat("Simulation", sim.id, "completed\n"))
  # Access the cluster object within the worker's environment
  #cluster <- get("cl", envir = .GlobalEnv)
  #sim.completed <- sim.completed + 1
  #clusterEvalQ(cluster, {
  #  sim.completed <- sim.completed + 1
  # })
  
  message(paste("Simulation", sim.id, "completed succesfully"))
  #setTxtProgressBar(pb, sim.id)
  return(results)

  }

# Parallelize the simulations and store the results
simres <- parLapply(cl, 1:3000, sim.func)
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













 tau.mtrx <- matrix(NA, ncol = length(tau.vec), nrow = n.sim)
  sigma.mtrx <- tau.mtrx
  tstat.mtrx <- tau.mtrx
  ci.left.mtrx <- tau.mtrx
  ci.right.mtrx <- tau.mtrx
  ci.hit <- tau.mtrx
  seed.vec <- rep(NA, n.sim)
  
  pb <- txtProgressBar(min = 0,          # Minimum value of the progress bar
                       max = n.sim,      # Maximum value of the progress bar
                       style = 3,        # Progress bar style (also available style = 1 and style = 2)
                       width = n.sim,    # Progress bar width. Defaults to getOption("width")
                       char = "=")       # Character used to create the bar
  
  init <- numeric(n.sim)
  end <- numeric(n.sim)
  
  for (s in 1:n.sim)
  {
    init[s] <- Sys.time()
    
    seed <- 1000 + s
    set.seed(seed)
    seed.vec[s] <- seed
    
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
    #fit <- tau.hat(Y,D,S,G.id,Ng,X,model, exp.option = T)
    #as.var <- as.var(Y,S,D,X,model, fit)
    result <- sreg(Y,D,S,G.id,Ng,X, exp.option = F)
    #model <- lm.iter(Y,D,S,G.id,Ng,X, exp.option=FALSE)
    #lin.adj(1, data.bin.mu, model, exp.option = TRUE)
    #testim <- tau.hat(Y,D,S,G.id,Ng,X,model, exp.option = TRUE)
    #testim$tau.hat
    #var.testim <- as.var(model, testim)
    #final.testim <- sreg(Y,D,S,G.id,Ng,X)
    tau.mtrx[s, ]      <- result$tau.hat
    sigma.mtrx[s, ]    <- result$se.rob
    tstat.mtrx[s, ]    <- result$t.stat
    ci.left.mtrx[s, ]  <- result$CI.left
    ci.right.mtrx[s, ] <- result$CI.right
    ci.hit[s, ]        <- as.numeric(tau.vec >= result$CI.left & 
                                       tau.vec <= result$CI.right)
    print(tau.mtrx)
    end[s] <- Sys.time()
    
    setTxtProgressBar(pb, s)
    time <- round(seconds_to_period(sum(end - init)), 0)
    
    # Estimated remaining time based on the
    # mean time that took to run the previous iterations
    est <- n.sim * (mean(end[end != 0] - init[init != 0])) - time
    remainining <- round(seconds_to_period(est), 0)
    
    cat(paste(" // Execution time:", time,
              " // Estimated time remaining:", remainining,
              " // SIMULATION NUMBER:", s), "")
    
  }
  close(pb)
}

colMeans(na.omit(tau.mtrx)) 
sapply(na.omit(as.data.frame(tau.mtrx)), sd)
colMeans(na.omit(sigma.mtrx))
colMeans(na.omit(ci.hit))

sreg(Y,D,S,G.id,Ng,X)
model <- lm.iter(Y,D,S,G.id,Ng,X)
fit <- tau.hat(Y,D,S,G.id,Ng,X, model)
as.var(model, fit)
fit$tau.hat
