rm(list = ls())
library("R2jags")

x_observada <- c(0,0,0,5,0,0,0,0,3,0,0,1,
                 1,3,0,0,1,7,2,0,0,0,1,0)
num_obs <- length(x_observada)

# Variables no observables
variables_no_observables <- c('theta_post', 'theta_prior', 'x_post_pred', 'x_prior_pred')

# Varibales observables
variables_observables <- list('x_observada', 'num_obs')

# Modelo
write("
      model{
        theta_prior ~dbeta(15,2)
        theta_post ~dbeta(15,2)
        for (i in 1:num_obs) {
          x_observada[i] ~dnegbin(theta_post, 1)
        }
        x_prior_pred ~dnegbin(theta_prior, 1)
        x_post_pred ~dnegbin(theta_post, 1)
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

prior_color <- "#ee000088"
post_color <- "#0000ee88"
data_color <- "#ee0088"

plot_mat <- rbind(1:2,
                  3:4)
layout(plot_mat)
hist(nodes$theta_prior, freq = F, col = prior_color, xlim = c(0,1))
hist(nodes$theta_post, freq = F, col = post_color, xlim = c(0,1))
hist(nodes$x_prior_pred,
     breaks = seq(-0.5, max(nodes$x_prior_pred) + 0.5, 1),
     freq = F, col = prior_color,
     xlim = c(min(x_observada), max(x_observada)))
hist(nodes$x_post_pred,
     breaks = seq(-0.5, max(nodes$x_post_pred) + 0.5, 1),
     freq = F, col = post_color,
     xlim = c(min(x_observada), max(x_observada)))
hist(x_observada,
     breaks = seq(-0.5, max(x_observada) + 0.5, 1),
     plot = F) -> ht_post
lines(
  ht_post$mids,
  ht_post$density,
  type = 'o', pch = 16,
  col = data_color, cex = 1)
# traceplot(bayes)
