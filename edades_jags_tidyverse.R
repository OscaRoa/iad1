rm(list = ls())
library(R2jags)
library(tidyverse)
library(cowplot)

setwd("~/Documents/fac/iad1/")

ages <- read_csv(
  "data/edades.csv",
  col_types = cols(
    edad = col_double()
  )
)

# Varianza = sigma ** 2
# Precision = lambda = 1/(sigma**2)
# pdf <- (1/(sigma*(2 * pi)**1/2))*exp(-((x - mu)**2)/(2*sigma**2))
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

nds <- as_tibble(bayes$BUGSoutput$sims.list)

# Posterior predictions
x_plot <- ggplot(ages) +
  geom_histogram(
    mapping = aes(x = edad, y = ..density..),
    fill = "white", color = "black",
    breaks = seq(min(ages$edad), max(ages$edad), by = 1.25),
    binwidth = .5)

mu_post_plot <- ggplot(nds) +
  geom_histogram(
    mapping = aes(x = mu_post, y = ..density..),
    fill = "white", color = "black",
    breaks = seq(min(ages$edad), max(ages$edad), by = 0.1),
    binwidth = .5
  )

x_post_plot <- ggplot(nds) +
  geom_histogram(
    mapping = aes(x = x_post, y = ..density..),
    fill = "white", color = "black",
    breaks = seq(min(nds$x_post), max(nds$x_post), by = 1.25),
    binwidth = .5
  )

sigma_post_plot <- ggplot(nds) +
  geom_histogram(
    mapping = aes(x = sigma_post, y = ..density..),
    fill = "white", color = "black",
    breaks = seq(min(nds$sigma_post), max(nds$sigma_post), by = 0.05),
    binwidth = .5
  )

plot_grid(x_plot, x_post_plot, mu_post_plot, sigma_post_plot)

# Prior predictions
mu_prior_plot <- ggplot(nds) +
  geom_histogram(
    mapping = aes(x = mu_prior, y = ..density..),
    fill = "white", color = "black",
    breaks = seq(min(nds$mu_prior), max(nds$mu_prior), by = 12),
    binwidth = .5
  )

x_prior_plot <- ggplot(nds) +
  geom_histogram(
    mapping = aes(x = x_prior, y = ..density..),
    fill = "white", color = "black",
    breaks = seq(min(nds$x_prior), max(nds$x_prior), by = 12),
    binwidth = .5
  )

sigma_prior_plot <- ggplot(nds) +
  geom_histogram(
    mapping = aes(x = sigma_prior, y = ..density..),
    fill = "white", color = "black",
    breaks = seq(min(nds$sigma_prior), max(nds$sigma_prior), by = 0.5),
    binwidth = .5
  )

plot_grid(x_prior_plot, sigma_prior_plot, mu_prior_plot)

# Joint prior and posterior of sigma and mu
post_joint_plot <- ggplot(nds) +
  geom_point(
    mapping = aes(x = mu_post, y = sigma_post)
    )

prior_joint_plot <- ggplot(nds) +
  geom_point(
    mapping = aes(x = mu_prior, y = sigma_prior)
  )

plot_grid(post_joint_plot, prior_joint_plot)
