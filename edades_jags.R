rm(list = ls())
library("R2jags")
setwd("~/Documents/fac/iad1/")
ages <- read.csv("data/edades.csv")

# Varianza = sigma**2
# Precision = lambda = 1/(sigma**2)
# pdf <- (1/(sigma * (2 * pi)**1/2)) * exp(-((x - mu)**2)/(2 * sigma**2))
# pdf <- dnorm(x, mu, sigma)

# plot(x, pdf, col = 'orange', type = 'l', lwd = 5)


x <- ages$edad
n_edades <- length(x)

observables <- list('x', 'n_edades')
unobservables <- c('mu_prior', 'lambda_prior', 'sigma_prior',
                   'mu_post', 'lambda_post', 'sigma_post',
                   'x_post', 'x_prior')

write("
      model{
        # Prior 
        mu_prior ~ dnorm(0, 0.001)
        sigma_prior ~ dunif(0, 10)
        lambda_prior <- 1/pow(sigma_prior, 2)
        
        # Post
        mu_post~ dnorm(0, 0.001)
        sigma_post ~ dunif(0, 10)
        lambda_post <- 1/pow(sigma_post, 2)
        # Datos
        for (i in 1:n_edades) {
          # Ejercicio 4.1.3
          # x[i] ~ dnorm(mu, 1)
          
          # Ejercicio 4.1.4
          # x[i] ~ dnorm(0, lambda)
          
          # Sin condiciones
          x[i] ~ dnorm(mu_post, lambda_post)
        }
        x_post ~ dnorm(mu_post, lambda_post)
        x_prior ~ dnorm(mu_prior, lambda_prior)
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

# Plot con las predicciones posteriores
hist(
  x,
  xlim = c(min(x), max(x)), 
  freq = F
)
hist(
  nds$mu_post,
  xlim = c(min(x), max(x)),
  freq = F
)
hist(
  nds$x_post,
  freq = F
)
hist(
  nds$sigma_post,
  freq = F
)

# Plot con las predicciones prior
hist(
  nds$x_prior,
  freq = F
)
hist(
  nds$mu_prior,
  freq = F
)
hist(
  nds$sigma_prior,
  freq = F
)

# Plots de las conjuntas prior y posteriores de sigma y mu
layout(matrix(1:2, ncol = 2))
plot(
  nds$mu_post,
  nds$sigma_post,
  main = "P(mu_post, sigma_post)"
  )
plot(
  nds$mu_prior,
  nds$sigma_prior,
  main = "P(mu_prior, sigma_prior)"
  )
