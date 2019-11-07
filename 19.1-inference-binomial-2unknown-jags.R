###################################################
#   TODO: Obtener y graficar expectativas a priori entre de theta, n
#         y las predictivas
#         DONE
#   TODO: Descubrir lo que puede hacerse sobre las predicciones prior y post de K
rm(list = ls())
library("R2jags")

k_observada <- 24

variables_no_observables <- c('theta_post_pred', 'theta_prior_pred', 'n_post_pred', 'n_prior_pred')

# Variables observables
variables_observables <- list('k_observada')

# Model
write("
  model {
    theta_post_pred ~dbeta(1,1)
    theta_prior_pred ~dbeta(1,1)
    vector <- rep(1/150, 150)
    n_post_pred ~dcat(vector)
    n_prior_pred ~dcat(vector)
    k_observada ~dbin(theta_post_pred, n_post_pred)
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

hist(nodes$theta_post_pred, col = post_color, freq = F, xlim = c(0, 1), main = "Theta post")
hist(nodes$theta_prior_pred, col = prior_color, freq = F, xlim = c(0, 1), main = "Theta prior")

hist(nodes$n_post_pred, col = post_color, freq = F, xlim = c(0, 150), main = "N post")
hist(nodes$n_prior_pred, col = prior_color, freq = F, xlim = c(0, 150), main = "N prior")

layout(1)
# Graficas de la distribucion conjunta entre los dos parametros no observables
plot(nodes$n_post_pred, nodes$theta_post_pred, pch = 21, bg = joint_color, main = "P(n_post_pred, theta_post_pred)")
plot(nodes$n_prior_pred, nodes$theta_prior_pred, pch = 21, bg = joint_color, main = "P(n_prior_pred, theta_prior_pred)")
