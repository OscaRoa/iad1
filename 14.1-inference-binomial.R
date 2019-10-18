############################
# Inferencia estadistica 2 #
#    Modelo binomial       #
#  ensayos durante clase   #
############################

rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Funciones
source("distributions/pmf.R") # Funciones de masa de probabilidad

# Variable observable
n <- 5
k <- 0:n

# Datos/observaciones en ensayos de la clase
k_observada <- c(1, 1, 4, 3, 2, 4, 5, 3, 4, 3, 3, 3, 3, 5, 1, 2, 1, 5, 4, 3, 2, 3)
# k_observada <- c(0, 1, 5, 3, 2, 4)

# Variables para color del plot de verosimilitud
k_observada_ordenada <- sort(k_observada)
verosim_colors <- rainbow(length(unique(k_observada_ordenada)), alpha = 0.3)

for (indice in 1:(length(k_observada))) {
  
  # Condicional para asignar la carga inicial de la distribucion a priori
  if(indice == 1) {
    gamma <- seq(0, 1, 0.1)
    P_gamma <- c(0,0,1,1,1,5,1,1,1,0,0)
    P_gamma <- P_gamma/sum(P_gamma)
    plot(gamma, P_gamma, type = 'b', pch=16, ylim=c(0, 1), col = "darkgreen")
  }
  # Condicional para las cargas posteriores con base en los datos observados
  else if (indice > 1) {
    P_gamma <- P_gamma_d_k
  }
  
  # Matrices a llenar por cada nueva distribucion a priori
  P_k_d_gamma <- array(dim = c(length(P_gamma), length(k)))
  P_k_gamma <- array(dim = dim(P_k_d_gamma))
  P_k <- array(dim = length(k))
  
  for (gamma_hyp in gamma) {
    pos = which(gamma == gamma_hyp)
    
    P_k_d_gamma[pos, ] <- binomial_dist(n, gamma_hyp, k)
    P_k_gamma[pos, ] <- P_k_d_gamma[pos, ]*P_gamma[pos]
  }
  
  for (k_hyp in k) {
    pos = which(k == k_hyp)
    P_k[pos] <- sum(P_k_gamma[, pos], na.rm = T)
  }
  
  # posterior
  k_obs <- k_observada[indice]
  pos = which(k==k_obs)
  P_gamma_d_k <- P_k_gamma[, pos]/P_k[pos]

  if (indice == length(k_observada)) {
    alpha = 1
  }
  else {
    alpha = indice*.009
  }
  points(gamma, P_gamma_d_k, type='o', pch=16, col = rgb(0.45, 0, 0, alpha = alpha))
  
  # Verosimilitud
  points(gamma, P_k_d_gamma[, pos], type = 'l', pch = 16, col = verosim_colors[indice])
}
title("Distribuciones posteriores sobre gamma")

# P_gamma_d_k contiene la posterior final sobre gamma
P_k_d_gamma <- array(dim = c(length(P_gamma), length(k)))
P_k_gamma <- array(dim = dim(P_k_d_gamma))
P_k <- array(dim = length(k))

for (gamma_hyp in gamma) {
  pos = which(gamma == gamma_hyp)
  
  P_k_d_gamma[pos, ] <- binomial_dist(n, gamma_hyp, k)
  # Se utiliza la posterior final de gamma en lugar de la marginal apriori de gamma
  P_k_gamma[pos, ] <- P_k_d_gamma[pos, ]*P_gamma_d_k[pos]
}

for (k_hyp in k) {
  pos = which(k == k_hyp)
  P_k[pos] <- sum(P_k_gamma[, pos], na.rm = T)
}

plot(k, P_k, type = "h", lwd = 3, col = "#ee0088", ylim = c(0, 0.8))
ht <- hist(k_observada,
           breaks = seq(-.5, max(k) + .5, 1),
           freq = F, plot = F)
points(ht$mids, ht$density, pch = 16, type = "o", col = "#5500dd")
legend(4, .6,
       legend = c("Posterior predictiva", "Observada"),
       lwd = c(5, 1), pch = c(NA, 16), col = c("#ee0088", "#5500dd"),
       box.lty = "blank", cex = 0.8, pt.cex = c(NA, 1.5))
title("Distribucion posterior predictiva sobre K", line = 2)
