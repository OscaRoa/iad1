rm(list = ls())
library("R2jags")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

x_observada <- c(0,0,0,5,0,0,0,0,3,0,0,1,
                 1,3,0,0,1,7,2,0,0,0,1,0)
num_obs <- length(x_observada)

# Variables no observables
variables_no_observables <- c('theta')

# Varibales observables
variables_observables <- list('x_observada', 'num_obs')

# Modelo
write("
      model{
        theta~dbeta(0.05, 0.05)
        for (i in 1:num_obs) {
          x_observada[i] ~dnegbin(theta, 1)
        }
      }
      ", "model_name.bug")

# Inferencia
jags(data = variables_observables,
     parameters.to.save = variables_no_observables,
     model.file = "model_name.bug",
     n.chains = 3,
     n.iter = 1000,
     n.thin = 1)
