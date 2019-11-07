###################################################
#   TODO: Obtener y graficar expectativas a priori entre de theta, n
#         y las predictivas
rm(list = ls())
library("R2jags")

k_observada <- 24

variables_no_observables <- c('theta_post', 'n_post')

# Variables observables
variables_observables <- list('k_observada')

# Model
write("
  model {
    theta_post ~dbeta(1,1)
    vector <- rep(1/150, 150)
    n_post ~dcat(vector)
    
    k_observada ~dbin(theta_post, n_post)
  }
  ", "model_name.bug")

bayes <- jags(
  data = variables_observables,
  parameters.to.save = variables_no_observables,
  model.file = "model_name.bug",
  n.chains = 3,
  n.iter = 10000,
  n.thin = 1
)

nodes <- bayes$BUGSoutput$sims.list

prior_color <- "#ee000088"
post_color <- "#0000ee88"
data_color <- "#ee0088"
joint_color <- "#00ee0088"

layout(1:2)
par(mar = rep(2, 4))

hist(nodes$theta_post, col = post_color, freq = F, xlim = c(0, 1), main = "Theta post")
hist(nodes$n_post, col = post_color, freq = F, xlim = c(0, 150), main = "N post")

layout(1)
# Grafica de la distribucion conjunta entre los dos parametros no observables
plot(nodes$n_post, nodes$theta_post, pch = 21, bg = joint_color)
