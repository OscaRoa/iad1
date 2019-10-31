rm(list = ls())
library("R2jags")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

x_observada <- c(0,0,0,5,0,0,0,0,3,0,0,1,
                 1,3,0,0,1,7,2,0,0,0,1,0)
num_obs <- length(x_observada)

# Variables no observables
variables_no_observables <- c('theta', 'theta_prior')

# Varibales observables
variables_observables <- list('x_observada', 'num_obs')

# Modelo
write("
      model{
        theta_prior ~dbeta(1, 2)
        theta ~dbeta(1,2)
        for (i in 1:num_obs) {
          x_observada[i] ~dnegbin(theta, 1)
        }
      }
      ", "model_name.bug")# Inferencia
bayes <- jags(
  data = variables_observables,
  parameters.to.save = variables_no_observables,
  model.file = "model_name.bug",
  n.chains = 3,
  n.iter = 30000,
  n.thin = 1
)
# 'bayes' contiene la informaci?n del output de jags
# incluidas las distribuciones predictivas

# Informacion de los parametros no observables del modelo
nodes <- bayes$BUGSoutput$sims.list

hist(nodes$theta, xlim = c(0, 1), breaks = 100, freq = F, col = "#0000ee88")
hist(nodes$theta_prior, add = T, breaks = 100, freq = F, col = "#ee000088")
mtext("Beta(1, 2)", side = 3, line = 3)

# traceplot(bayes)

