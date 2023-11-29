#%##%##%##%###%##%##%##%###%##%##%##%###%##%
### This R file provides quick examples ####
### of ATE estimation under CAR with    ####
###        multiple treatments          ####
#%##%##%##%###%##%##%##%###%##%##%##%###%##%
####      The code is developed by      ####
####      @Juri Trifonov, UChicago      ####
####            Supervisors:            ####
####      @Azeem Shaikh, UChicago       ####
####    @Max Tabord-Meehan, UChicago    ####
#%##%##%##%###%##%##%##%###%##%##%##%###%##%
#%##%##%##%##
#%# v.4.0 #%#
#%##%##%##%##

options(scipen = 999)                                                            # disable scientific notation


#%##%##%##%###%##%##%##%###%##%##%##%###%##%#%##%##%##%###%##%##%##%##
#%##%##%##%###%##%##%##%###%##%##%##%###%##%#%##%##%##%###%##%##%##%##
#         Please, provide the path to the corresponding source
#                  file with functions on your PC
#                    ↓↓↓↓↓↓↓↓↓↓↓HERE↓↓↓↓↓↓↓↓↓↓↓
source('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.git/main/sreg.func_v.4.3.R')
#%##%##%##%###%##%##%##%###%##%##%##%###%##%#%##%##%##%###%##%##%##%##
#%##%##%##%###%##%##%##%###%##%##%##%###%##%#%##%##%##%###%##%##%##%##
#-------------------------------------------
# Example with multiple treatments
#-------------------------------------------
#%##%##%##%##
#set.seed(333)                                                                    # fix the random seed

# Simulation of the DGP
#------------------------------------------------
n.treat <-3
G=200;
Nmax=200;
tau.vec <- c(0.2, 0.5, 0.9);
max.support = Nmax/10-1;
gamma.vec = c(0.4, 0.2, 1, 0.1, 0.8)

Ng <- gen.cluster.sizes(G, max.support)[,1]
data.pot <- gen.data.pot(Ng, tau.vec = tau.vec, G = G, gamma.vec, n.treat = n.treat)
strata <- form.strata(data.pot, 10)
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

result <- sreg(Y,D,S,G.id, Ng, X, exp.option = FALSE)
result$tau.hat
result$se.rob
summary.sreg(result)
lm.iter(Y,D,S,G.id,Ng,X)



