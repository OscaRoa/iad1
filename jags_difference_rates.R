# Infiriendo la diferencia entre tasas de éxito
# de dos procesos binomiales (p.ej., una moneda contra una tachuela)
# Lee, MD and Wagenmakers, E-J (2013) Bayesian Cognitive Modeling, a practical course.
# Ejercio 3.2, página 39

rm(list=ls())
library('R2jags')

# La idea es que observamos 5 éxitos en 10 ensayos con la moneda...
k1 <- 5 
n1 <- 10

# ...y observamos 10 éxitos en 12 ensayos con la tachuela.
k2 <- 10
n2 <- 12

# Variables observables
var_observables <- list('k1', 'k2', 'n1', 'n2')

# Variables no observables
var_no_observables <- c('theta_1_prior', 'theta_2_prior',
                        'delta_prior', 'delta_post',
                        'theta_1_post', 'theta_2_post',
                        'k1_post', 'k2_post',
                        'k1_prior', 'k2_prior'
                        )

write("
model{
  theta_1_post ~ dbeta(0.1, 0.1)
  theta_1_prior ~ dbeta(0.1, 0.1)
  
  theta_2_post ~ dbeta(0.1, 0.1)
  theta_2_prior ~ dbeta(0.1, 0.1)

  k1 ~ dbin(theta_1_post, n1)
  k2 ~ dbin(theta_2_post, n2)
  
  # Difference Between Rates
  delta_post <- theta_1_post - theta_2_post
  delta_prior <- theta_1_prior - theta_2_prior
  
  # Predictive posteriors
  k1_post ~ dbin(theta_1_post, n1)
  k2_post ~ dbin(theta_2_post, n2)
  
  # Predictive priors
  k1_prior ~ dbin(theta_1_prior, n1)
  k2_prior ~ dbin(theta_2_prior, n2)
}
", "model_name.bug")

bayes <- jags(data = var_observables,
              parameters.to.save = var_no_observables,
              n.chains = 3,
              n.iter = 35000,
              model.file = "model_name.bug")

nodos <- bayes$BUGSoutput$sims.list

# Graficas
prior_color = rgb(0, 0.85, 0)
post_color = rgb(0.85, 0, 0)

# Delta post & prior
layout(1:2, 2:3)
hist(nodos$delta_prior, freq = F, col = prior_color, xlim = c(-1, 1), main = "Delta prior")
hist(nodos$delta_post, freq = F, col = post_color, xlim = c(-1, 1), main = "Delta post")

# Theta 1, theta 2 post & prior
plot_mat <- rbind(1:2, 3:4)
layout(plot_mat)
hist(nodos$theta_1_prior, freq = F, xlim = c(0,1), main = "Theta 1 prior", col = prior_color)
hist(nodos$theta_1_post, freq = F, xlim = c(0,1), main = "Theta 1 post", col = post_color)

hist(nodos$theta_2_prior, freq = F, xlim = c(0,1), main = "Theta 2 prior", col = prior_color)
hist(nodos$theta_2_post, freq = F, xlim = c(0,1), main = "Theta 2 post", col = post_color)

# K1, K2 post & prior
hist(nodos$k1_prior, freq = F, main = "K1 prior", col = prior_color)
hist(nodos$k1_post, freq = F, main = "K1 post", col = post_color)

hist(nodos$k2_prior, freq = F, main = "K2 prior", col = prior_color)
hist(nodos$k2_post, freq = F, main = "K2 post", col = post_color)

# ¿Deberíamos de creer que la probabilidad de éxito de la moneda
# es *diferente* a la probabilidad de éxito de la tachuela?
# O, más específico: ¿cuáles son los valores más probables de esa 
# diferencia?

# Hay que calcularlo con JAGS, de acuerdo con el modelo gráfico
# en el libro. Recuerden calcular las conclusiones posteriores
# y también los supuestos a priori de todo el modelo.