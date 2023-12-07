library(ggplot2)
load('/Users/trifonovjuri/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Sim/sim100.RData')
tau.100 <- colMeans(na.omit(tau.mtrx)) 
se.num.100 <- sapply(na.omit(as.data.frame(tau.mtrx)), sd)
se.analyt.100 <- colMeans(na.omit(sigma.mtrx))
ci.hit.100 <- colMeans(na.omit(ci.hit))

load('/Users/trifonovjuri/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Sim/sim200.RData')
tau.200 <- colMeans(na.omit(tau.mtrx)) 
se.num.200 <- sapply(na.omit(as.data.frame(tau.mtrx)), sd)
se.analyt.200 <- colMeans(na.omit(sigma.mtrx))
ci.hit.200 <- colMeans(na.omit(ci.hit))

load('/Users/trifonovjuri/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Sim/sim300.RData')
tau.300 <- colMeans(na.omit(tau.mtrx)) 
se.num.300 <- sapply(na.omit(as.data.frame(tau.mtrx)), sd)
se.analyt.300 <- colMeans(na.omit(sigma.mtrx))
ci.hit.300 <- colMeans(na.omit(ci.hit))

load('/Users/trifonovjuri/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Sim/sim400.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.400 <- mean(tau)
se.num.400 <- sd(tau)
se.analyt.400 <- mean(se)
ci.hit.400 <- mean(ci.hit)


load('/Users/trifonovjuri/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Sim/sim500.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.500 <- mean(tau)
se.num.500 <- sd(tau)
se.analyt.500 <- mean(se)
ci.hit.500 <- mean(ci.hit)

load('/Users/trifonovjuri/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Sim/sim750.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.750 <- mean(tau)
se.num.750 <- sd(tau)
se.analyt.750 <- mean(se)
ci.hit.750 <- mean(ci.hit)

load('/Users/trifonovjuri/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Sim/sim1000.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.1000 <- mean(tau)
se.num.1000 <- sd(tau)
se.analyt.1000 <- mean(se)
ci.hit.1000 <- mean(ci.hit)


load('/Users/trifonovjuri/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Sim/sim1500.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.1500 <- mean(tau)
se.num.1500 <- sd(tau)
se.analyt.1500 <- mean(se)
ci.hit.1500 <- mean(ci.hit)

load('/Users/trifonovjuri/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Sim/sim2000.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.2000 <- mean(tau)
se.num.2000 <- sd(tau)
se.analyt.2000 <- mean(se)
ci.hit.2000 <- mean(ci.hit)

load('/Users/trifonovjuri/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Sim/sim3000.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.3000 <- mean(tau)
se.num.3000 <- sd(tau)
se.analyt.3000 <- mean(se)
ci.hit.3000 <- mean(ci.hit)

load('/Users/trifonovjuri/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Sim/sim5000.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.5000 <- mean(tau)
se.num.5000 <- sd(tau)
se.analyt.5000 <- mean(se)
ci.hit.5000 <- mean(ci.hit)

tau.vec <- c(tau.100, tau.200, tau.300, tau.400,
             tau.500, tau.750, tau.1000, tau.1500, 
             tau.2000, tau.3000, tau.5000)
se.num.vec <- c(se.num.100, se.num.200, se.num.300, se.num.400,
                se.num.500, se.num.750, se.num.1000, se.num.1500,
                se.num.2000, se.num.3000, se.num.5000)
se.analyt.vec <- c(se.analyt.100, se.analyt.200, se.analyt.300, se.analyt.400,
                   se.analyt.500, se.analyt.750, se.analyt.1000, se.analyt.1500,
                   se.analyt.2000, se.analyt.3000, se.analyt.5000)
ci.hit.vec <- c(ci.hit.100, ci.hit.200, ci.hit.300, ci.hit.400,
                ci.hit.500, ci.hit.750, ci.hit.1000, ci.hit.1500,
                ci.hit.2000, ci.hit.3000, ci.hit.5000)
sample.set <- c(100, 200, 300, 400, 500, 750, 1000, 1500, 2000, 3000, 5000)

graph.data <- data.frame('s.size' = sample.set, 'tau' = tau.vec, 'se.num' = se.num.vec, 'se.analyt' = se.analyt.vec, 'se.diff' = abs(se.num.vec - se.analyt.vec), 'ci.hit' = ci.hit.vec)

ggplot(graph.data, aes(x = s.size, y = se.diff)) +
  geom_line(size = 1, color = "red") + geom_point(size = 3, color = "red") +
  labs(x = "Sample Size", y = "SE Difference") +
  ggtitle("Graph of Sample Size vs. SE Difference")

ggplot(graph.data) +
  geom_line(aes(x = s.size, y = se.num, color = "SE Num"), size = 1) +
  geom_point(aes(x = s.size, y = se.num, color = "SE Num"), size = 3) +
  geom_line(aes(x = s.size, y = se.analyt, color = "SE Analyt"), size = 1) +
  geom_point(aes(x = s.size, y = se.analyt, color = "SE Analyt"), size = 3) +
  labs(x = "Sample Size", y = "SE") +
  scale_color_manual(values = c("SE Num" = "blue", "SE Analyt" = "red")) +
  theme_minimal()

ggplot(graph.data, aes(x = s.size, y = ci.hit)) +
  geom_line(color = 'red') + geom_point(color = 'red') +
  labs(x = "Sample Size", y = "Coverage %")



### 10 strata design
library(ggplot2)

load('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.git/data/server/10strata/sim1000.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.1000 <- mean(tau)
se.num.1000 <- sd(tau)
se.analyt.1000 <- mean(se)
ci.hit.1000 <- mean(ci.hit)

load('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.git/data/server/10strata/sim1500.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.1500 <- mean(tau)
se.num.1500 <- sd(tau)
se.analyt.1500 <- mean(se)
ci.hit.1500 <- mean(ci.hit)

load('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.git/data/server/10strata/sim2000.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.2000 <- mean(tau)
se.num.2000 <- sd(tau)
se.analyt.2000 <- mean(se)
ci.hit.2000 <- mean(ci.hit)


load('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.git/data/server/10strata/sim3000.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.3000 <- mean(tau)
se.num.3000 <- sd(tau)
se.analyt.3000 <- mean(se)
ci.hit.3000 <- mean(ci.hit)

load('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.git/data/server/10strata/sim4000.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.4000 <- mean(tau)
se.num.4000 <- sd(tau)
se.analyt.4000 <- mean(se)
ci.hit.4000 <- mean(ci.hit)

load('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.git/data/server/10strata/sim5000.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.5000 <- mean(tau)
se.num.5000 <- sd(tau)
se.analyt.5000 <- mean(se)
ci.hit.5000 <- mean(ci.hit)

tau.vec <- c(tau.1000, tau.1500, tau.2000, tau.3000,
             tau.4000, tau.5000)
se.num.vec <- c(se.num.1000, se.num.1500, se.num.2000, se.num.3000,
                se.num.4000, se.num.5000)
se.analyt.vec <- c(se.analyt.1000, se.analyt.1500, se.analyt.2000, se.analyt.3000,
                   se.analyt.4000, se.analyt.5000)
ci.hit.vec <- c(ci.hit.1000, ci.hit.1500, ci.hit.2000, ci.hit.3000,
                ci.hit.4000, ci.hit.5000)
sample.set <- c(1000, 1500, 2000, 3000, 4000, 5000)

# Uncomment if want to combine the results for large samples with the results for small samples
#tau.vec <- c(tau.100, tau.200, tau.300, tau.400,
#             tau.500, tau.750, tau.1000, tau.1500, 
#             tau.2000, tau.3000, tau.4000, tau.5000)
#se.num.vec <- c(se.num.100, se.num.200, se.num.300, se.num.400,
#                se.num.500, se.num.750, se.num.1000, se.num.1500,
#                se.num.2000, se.num.3000, se.num.4000, se.num.5000)
#se.analyt.vec <- c(se.analyt.100, se.analyt.200, se.analyt.300, se.analyt.400,
#                   se.analyt.500, se.analyt.750, se.analyt.1000, se.analyt.1500,
#                   se.analyt.2000, se.analyt.3000, se.analyt.4000, se.analyt.5000)
#ci.hit.vec <- c(ci.hit.100, ci.hit.200, ci.hit.300, ci.hit.400,
#                ci.hit.500, ci.hit.750, ci.hit.1000, ci.hit.1500,
#                ci.hit.2000, ci.hit.3000, ci.hit.4000, ci.hit.5000)
#sample.set <- c(100, 200, 300, 400, 500, 750, 1000, 1500, 2000, 3000, 4000, 5000)

graph.data <- data.frame('s.size' = sample.set, 'tau' = tau.vec, 'se.num' = se.num.vec, 'se.analyt' = se.analyt.vec, 'se.diff' = abs(se.num.vec - se.analyt.vec), 'ci.hit' = ci.hit.vec)

ggplot(graph.data, aes(x = s.size, y = se.diff)) +
  geom_line(size = 1, color = "red") + geom_point(size = 3, color = "red") +
  labs(x = "Sample Size", y = "SE Difference") +
  ggtitle("Graph of Sample Size vs. SE Difference")

ggplot(graph.data) +
  geom_line(aes(x = s.size, y = se.num, color = "SE Num"), size = 1) +
  geom_point(aes(x = s.size, y = se.num, color = "SE Num"), size = 3) +
  geom_line(aes(x = s.size, y = se.analyt, color = "SE Analyt"), size = 1) +
  geom_point(aes(x = s.size, y = se.analyt, color = "SE Analyt"), size = 3) +
  labs(x = "Sample Size", y = "SE") +
  scale_color_manual(values = c("SE Num" = "blue", "SE Analyt" = "red")) +
  theme_minimal()

ggplot(graph.data, aes(x = s.size, y = ci.hit)) +
  geom_line(color = 'red') + geom_point(color = 'red') +
  labs(x = "Sample Size", y = "Coverage %") #+ scale_y_continuous(limits = c(0.6, 1))





### v.4.7 version
library(ggplot2)

load('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.git/data/unified/100.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.100 <- mean(tau)
se.num.100 <- sd(tau)
se.analyt.100 <- mean(se)
ci.hit.100 <- mean(ci.hit)

load('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.git/data/unified/300.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.300 <- mean(tau)
se.num.300 <- sd(tau)
se.analyt.300 <- mean(se)
ci.hit.300 <- mean(ci.hit)

load('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.git/data/unified/500.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.500 <- mean(tau)
se.num.500 <- sd(tau)
se.analyt.500 <- mean(se)
ci.hit.500 <- mean(ci.hit)


load('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.git/data/unified/1000.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.1000 <- mean(tau)
se.num.1000 <- sd(tau)
se.analyt.1000 <- mean(se)
ci.hit.1000 <- mean(ci.hit)

load('/Users/trifonovjuri/Desktop/pkg.sreg/sreg.git/data/unified/2000.RData')
tau <- na.omit(as.numeric(sapply(simres, function(simres) simres$tau)))
se <- na.omit(sapply(simres, function(simres) simres$se))
ci.hit <- na.omit(sapply(simres, function(simres) simres$ci.hit))
tau.2000 <- mean(tau)
se.num.2000 <- sd(tau)
se.analyt.2000 <- mean(se)
ci.hit.2000 <- mean(ci.hit)



tau.vec <- c(tau.100, tau.300, tau.500, tau.1000,
             tau.2000)
se.num.vec <- c(se.num.100, se.num.300, se.num.500, se.num.1000,
                se.num.2000)
se.analyt.vec <- c(se.analyt.100, se.analyt.300, se.analyt.500, se.analyt.1000,
                   se.analyt.2000)
ci.hit.vec <- c(ci.hit.100, ci.hit.300, ci.hit.500, ci.hit.1000,
                ci.hit.2000)
sample.set <- c(100, 300, 500, 1000, 2000)

# Uncomment if want to combine the results for large samples with the results for small samples
#tau.vec <- c(tau.100, tau.200, tau.300, tau.400,
#             tau.500, tau.750, tau.1000, tau.1500, 
#             tau.2000, tau.3000, tau.4000, tau.5000)
#se.num.vec <- c(se.num.100, se.num.200, se.num.300, se.num.400,
#                se.num.500, se.num.750, se.num.1000, se.num.1500,
#                se.num.2000, se.num.3000, se.num.4000, se.num.5000)
#se.analyt.vec <- c(se.analyt.100, se.analyt.200, se.analyt.300, se.analyt.400,
#                   se.analyt.500, se.analyt.750, se.analyt.1000, se.analyt.1500,
#                   se.analyt.2000, se.analyt.3000, se.analyt.4000, se.analyt.5000)
#ci.hit.vec <- c(ci.hit.100, ci.hit.200, ci.hit.300, ci.hit.400,
#                ci.hit.500, ci.hit.750, ci.hit.1000, ci.hit.1500,
#                ci.hit.2000, ci.hit.3000, ci.hit.4000, ci.hit.5000)
#sample.set <- c(100, 200, 300, 400, 500, 750, 1000, 1500, 2000, 3000, 4000, 5000)

graph.data <- data.frame('s.size' = sample.set, 'tau' = tau.vec, 'se.num' = se.num.vec, 'se.analyt' = se.analyt.vec, 'se.diff' = abs(se.num.vec - se.analyt.vec), 'ci.hit' = ci.hit.vec)

ggplot(graph.data, aes(x = s.size, y = se.diff)) +
  geom_line(size = 1, color = "red") + geom_point(size = 3, color = "red") +
  labs(x = "Sample Size", y = "SE Difference") +
  ggtitle("Graph of Sample Size vs. SE Difference")

ggplot(graph.data) +
  geom_line(aes(x = s.size, y = se.num, color = "SE Num"), size = 1) +
  geom_point(aes(x = s.size, y = se.num, color = "SE Num"), size = 3) +
  geom_line(aes(x = s.size, y = se.analyt, color = "SE Analyt"), size = 1) +
  geom_point(aes(x = s.size, y = se.analyt, color = "SE Analyt"), size = 3) +
  labs(x = "Sample Size", y = "SE") +
  scale_color_manual(values = c("SE Num" = "blue", "SE Analyt" = "red")) +
  theme_minimal()

ggplot(graph.data, aes(x = s.size, y = ci.hit)) +
  geom_line(color = 'red') + geom_point(color = 'red') +
  labs(x = "Sample Size", y = "Coverage %") + scale_y_continuous(limits = c(0.7, 1))

