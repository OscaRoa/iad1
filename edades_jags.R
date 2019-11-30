rm(list = ls())
library("R2jags")
setwd("~/Documents/fac/iad1/")
ages <- read.csv("data/edades.csv")

# Varianza = sigma ** 2
# Precision = lambda = 1/(sigma**2)
# pdf <- (1/(sigma*(2 * pi)**1/2))*exp(-((x - mu)**2)/(2*sigma**2))
# pdf <- dnorm(x, mu, sigma)

# plot(x, pdf, col = 'orange', type = 'l', lwd = 5)



x <- ages$edad
# x <- rep(20.6, 100)
n_edades <- length(x)
# sigma <- 1.0

observables <- list('x', 'n_edades')
unobservables <- c('mu', 'lambda', 'sigma')

write("
      model{
        mu ~ dnorm(0, 0.001)
        sigma ~ dunif(0, 10)
        lambda <- 1/pow(sigma, 2)
        # Datos
        for (i in 1:n_edades) {
          # Ejercicio 4.1.3
          # x[i] ~ dnorm(mu, 1)
          
          # Ejercicio 4.1.4
          # x[i] ~ dnorm(0, lambda)
          
          # Sin condiciones
          x[i] ~ dnorm(mu, lambda)
          x_post[i] ~ dnorm(mu, lambda)
        }
      }
      ", "ages.bug")

bayes <- jags(
  data = observables,
  parameters.to.save = unobservables,
  n.iter = 50000,
  n.chains = 3,
  n.burnin = 0,
  n.thin = 1,
  model.file = "ages.bug"
  )

nds <- bayes$BUGSoutput$sims.list

# Plots
layout(matrix(1:4, nrow = 2))
par(mar = rep(4, 4))

hist(
  x,
  xlim = c(min(x), max(x)), 
  freq = F
)
hist(
  nds$mu,
  xlim = c(min(x), max(x)),
  freq = F
)
hist(
  nds$sigma,
  freq = F
)

layout(1)

plot(nds$mu, nds$sigma)
