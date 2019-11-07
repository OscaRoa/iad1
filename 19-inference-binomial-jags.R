rm(list = ls())
library("R2jags")

k_observada <- c(1, 1, 4, 3, 2, 4, 5, 3, 4, 3, 3, 3, 3, 5, 1, 2, 1, 5, 4, 3, 2, 3)
num_obs <- length(k_observada)
n <- 5

# Variables no observables
variables_no_observables <- c('theta_prior', 'theta_post', 'k_post_pred', 'k_prior_pred')

# Variables observables
variables_observables <- list('n', 'k_observada', 'num_obs')

# Model
write(
  "
    model {
      theta_prior ~dbeta(10, 25)
      theta_post ~dbeta(10,25)
      for (i in 1:num_obs) {
        k_observada[i] ~dbin(theta_post, n)
      }
      k_post_pred ~dbin(theta_post, n)
      k_prior_pred ~dbin(theta_prior, n)
    }
    ", "model_name.bug"
)

bayes <- jags(
  data = variables_observables,
  parameters.to.save = variables_no_observables,
  model.file = "model_name.bug",
  n.chains = 3,
  n.iter = 50000,
  n.thin = 1
)

nodes <- bayes$BUGSoutput$sims.list

prior_color <- "#ee000088"
post_color <- "#0000ee88"
data_color <- "#ee0088"
plot_mat <- rbind(1:2,
                  3:4)
layout(plot_mat)

hist(nodes$theta_prior, freq = F, col = prior_color, xlim = c(0,1))
hist(nodes$theta_post, freq = F, col = post_color, xlim = c(0,1))
hist(nodes$k_prior_pred,
     breaks = seq(-0.5, max(nodes$k_prior_pred) + 0.5, 1),
     freq = F, col = prior_color,
     xlim = c(min(k_observada), max(k_observada)))
hist(nodes$k_post_pred,
     breaks = seq(-0.5, max(nodes$k_post_pred) + 0.5, 1),
     freq = F, col = post_color,
     xlim = c(min(k_observada), max(k_observada)))
hist(k_observada,
     breaks = seq(-0.5, max(k_observada) + 0.5, 1),
     plot = F) -> ht_post
lines(
  ht_post$mids,
  ht_post$density,
  type = 'o', pch = 16,
  col = data_color, cex = 1)

# traceplot(bayes)
