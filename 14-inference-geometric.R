############################
# Inferencia estadistica 2 #
#    Modelo geometrico     #
#  ensayos durante clase   #
#  posterior predictiva    #
############################

rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Funciones
source("distributions/pmf.R") # Funciones de masa de probabilidad

# Variable observable
x <- 0:10

# Datos/observaciones en ensayos de la clase
x_observada <- c(0,0,0,5,0,0,0,0,3,0,0,1,1,3,0,0,1,7,2,0,0,0,1,0)

for (indice in 1:(length(x_observada))) {
  
  # Condicional para asignar la carga inicial de la distribucion a priori
  if(indice == 1) {
    theta <- seq(0, 1, 0.1)
    P_theta <- c(9,8,7,3,2,2,2,3,7,8,9)
    P_theta <- P_theta/sum(P_theta)
    plot(theta, P_theta, type = 'b', pch=16, ylim=c(0, 1), col = "darkgreen")
  }
  # Condicional para las cargas posteriores con base en los datos observados
  else if (indice > 1) {
    P_theta <- P_theta_d_x
  }
  
  # Matrices a llenar por cada nueva distribucion a priori
  P_x_d_theta <- array(dim = c(length(P_theta), length(x)))
  P_X_theta <- array(dim = dim(P_x_d_theta))
  P_x <- array(dim = length(x))
  
  for (theta_hyp in theta) {
    pos = which(theta == theta_hyp)
    
    P_x_d_theta[pos, ] <- geom_dist(x, theta_hyp)
    P_X_theta[pos, ] <- P_x_d_theta[pos, ]*P_theta[pos]
  }
  
  for (x_hyp in x) {
    pos = which(x == x_hyp)
    P_x[pos] <- sum(P_X_theta[, pos], na.rm = T)
  }
  
  # posterior
  x_obs <- x_observada[indice]
  pos = which(x==x_obs)
  P_theta_d_x <- P_X_theta[, pos]/P_x[pos]
  if (indice < 11) {
    color <- paste0("#", indice-1, "f0000", indice-1, "f")
  }
  else {
    color <- paste0("#", indice, "0000", "ff")
  }
  points(theta, P_theta_d_x, type='o', pch=indice-1, col = color, bg = "gray")
  
  # Verosimilitud
  points(theta, P_x_d_theta[, pos], type = 'l', pch = 16, col = "#0000ff44")
}

# Posterior predictiva: "dstribucion sobre variable observable X".
# Tomando en cuenta la incertidumbre final o posterior sobre la
# variable no observable.

# P_theta_d_x contiene la posterior final sobre theta
P_x_d_theta <- array(dim = c(length(P_theta), length(x)))
P_X_theta <- array(dim = dim(P_x_d_theta))
P_x <- array(dim = length(x))

for (theta_hyp in theta) {
  pos = which(theta == theta_hyp)
  
  P_x_d_theta[pos, ] <- geom_dist(x, theta_hyp)
  # Se utiliza la posterior final de theta en lugar de la marginal apriori de theta
  P_X_theta[pos, ] <- P_x_d_theta[pos, ]*P_theta_d_x[pos]
}

for (x_hyp in x) {
  pos = which(x == x_hyp)
  P_x[pos] <- sum(P_X_theta[, pos], na.rm = T)
}

plot(x, P_x, type = "h", lwd = 3, col = "#ee0088", ylim = c(0, 0.8))
ht <- hist(x_observada,
     breaks = seq(-.5, max(x) + .5, 1),
     freq = F, plot = F)
points(ht$mids, ht$density, pch = 16, type = "o", col = "#5500dd")
legend(7, .6,
       legend = c("Posterior predictiva", "Observada"),
       lwd = c(5, 1), pch = c(NA, 16), col = c("#ee0088", "#5500dd"),
       box.lty = "blank", cex = 0.8, pt.cex = c(NA, 1.5))
