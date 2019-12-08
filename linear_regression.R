# Beta1 pendiente
# Beta0 ordenada al origen
# Forma general de la ecuacion y = B1*x + B0

# > Encontrar los valores de pendiente e intercecto mas probables suponiendo una relacion
# entre dos variables.

# > Cada valor proviene de una distribución dada.
rm(list = ls())
library("R2jags")
setwd("~/Documents/fac/iad1")

data <- read.csv("data/Linear_Regression_1.csv")
plot(data$x, data$y, xlim = c(0, 40), ylim = c(0, 80))

x <- data$x
y <- data$y
n_obs <- nrow(data)

observables <- list('x', 'y', 'n_obs')
unobservables <- c('b0', 'b1', 'sigma', 'mu', 'tau')

write(
    "
    model {
        b0 ~ dunif(-40, 40)
        b1 ~ dunif(0,10)
        sigma ~ dunif(0, 20)
        tau <- 1/pow(sigma, 2)
        
        for (i in 1:n_obs) {
            mu[i] <- b1*x[i] + b0
            y[i] ~ dnorm(mu[i], tau)
        }
    }    
    ",
    "lr.bug"
)

bayes <- jags(
    data = observables,
    parameters.to.save = unobservables,
    model.file = "lr.bug",
    n.iter = 100000,
    n.thin = 3,
    n.burnin = 0,
    n.chains = 3
)

nds <- bayes$BUGSoutput$sims.list

plot(
    NULL,
    xlim = c(0, 40), ylim = c(0, 80)
)
posiciones <- sample(dim(nds$b0)[1], size = 5000)
x_plot <- seq(0, 40, 0.1)

for (indx in posiciones) {
    y_plot <- nds$b1[indx]*x_plot + nds$b0[indx]
    lines(x_plot, y_plot, col = "orange")
}

points(x, y, pch = 16)
