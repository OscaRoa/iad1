###############################
#   Inferencia estadistica    #
###############################
rm(list = ls())
# Establece directorio de trabajo
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Funciones
source("distributions/pmf.R") # Funciones de masa de probabilidad

# Parametro desconocido de la distribucion hipergeometrica
m <- c(1,3,6,6,7,7,6,5,3,3,2,2,1,1,1,1,1,2,2,2,1)
P_m <- P_m/sum(P_m) # Proporciones de las cargas probabilisticas a priori (distribucion a priori)

# Parametros conocidos de la distribucion hipergeometrica
n <- 4 # Fracasos
k <- 5 # Tamaño de las extracciones

# Variable observable
x <- 0:5

# Matriz de la probabilidad condicional P(x | m)
P_x_d_m <- array(dim = c(length(m), length(x)))
# Matriz probabilidad conjunta P(m, x)
P_X_m <- array(dim = dim(P_x_d_m))
# Array de la distribucion de X
P_x <- array(dim = length(x))

# Iteramos sobre el posible número de éxitos en la poblacion (m)
for (m_hyp in m) {
  # Ya que estamos trabajando con una distribucion hipergeometrica
  # tenemos que calcular la distribucion para cada posible valor y asignarlo
  # a una posicion en la matriz de la probabilidad conjunta que corresponda a
  # los valores de m
  pos = which(m == m_hyp)
  
  P_x_d_m[pos, ] <- hipergeom_dist(x, m_hyp, n, k)
  P_X_m[pos, ] <- P_x_d_m[pos, ]*P_m[pos]
}

for (x_hyp in x) {
  pos = which(x == x_hyp)
  P_x[pos] <- sum(P_X_m[, pos], na.rm = T)
}

# Datos observados
# Primer dato observado
x_obs <- 1
pos <- which(x==x_obs)
P_m_d_x <- P_X_m[, pos]/P_x[pos]
P_m <- P_m_d_x # Posterior después de la primera observacion

# Matrices a llenar con la nueva distribucion de probabilidad P_m
P_x_d_m <- array(dim = c(length(m), length(x)))
P_X_m <- array(dim = dim(P_x_d_m))
P_x <- array(dim = length(x))

for (m_hyp in m) {
  pos = which(m == m_hyp)
  
  P_x_d_m[pos, ] <- hipergeom_dist(x, m_hyp, n, k)
  P_X_m[pos, ] <- P_x_d_m[pos, ]*P_m[pos]
}

for (x_hyp in x) {
  pos = which(x == x_hyp)
  P_x[pos] <- sum(P_X_m[, pos], na.rm = T)
}

# # Segundo dato observado
x_obs <- 4
pos <- which(x==x_obs)
P_m_d_x <- P_X_m[, pos]/P_x[pos]
P_m <- P_m_d_x # Posterior después de la segunda observacion

# Matrices a llenar con la nueva distribucion de propabilidad P_m
P_x_d_m <- array(dim = c(length(m), length(x)))
P_X_m <- array(dim = dim(P_x_d_m))
P_x <- array(dim = length(x))

for (m_hyp in m) {
  pos = which(m == m_hyp)
  
  P_x_d_m[pos, ] <- hipergeom_dist(x, m_hyp, n, k)
  P_X_m[pos, ] <- P_x_d_m[pos, ]*P_m[pos]
}

for (x_hyp in x) {
  pos = which(x == x_hyp)
  P_x[pos] <- sum(P_X_m[, pos], na.rm = T)
}

# Tercer dato observado
x_obs <- 4
pos <- which(x==x_obs)
P_m_d_x <- P_X_m[, pos]/P_x[pos]
P_m <- P_m_d_x # Posterior después de la tercera observacion

P_x_d_m <- array(dim = c(length(m), length(x)))
P_X_m <- array(dim = dim(P_x_d_m))
P_x <- array(dim = length(x))

for (m_hyp in m) {
  # Ya que estamos trabajando con una distribucion hipergeometrica
  # tenemos que calcular la distribucion para cada posible valor y asignarlo
  # a una posicion en la matriz de la probabilidad conjunta que corresponda a
  # los valores de m
  pos = which(m == m_hyp)
  
  P_x_d_m[pos, ] <- hipergeom_dist(x, m_hyp, n, k)
  P_X_m[pos, ] <- P_x_d_m[pos, ]*P_m[pos]
}

for (x_hyp in x) {
  pos = which(x == x_hyp)
  P_x[pos] <- sum(P_X_m[, pos], na.rm = T)
}

plot(m, P_m, col = "#aaaaaa", lwd = 6, type = "h", ylim = c(0, .5))
points(m, P_m_d_x, col = "#222222", lwd = 6, type = "h")

# Graficas tridimensional de las matrices
mat_plot <- rbind(1:3,
                  c(0, 0, 4))
#layout(matrix(1:2, ncol = 4))
x11(width = 12, height = 8)
layout(mat_plot)

plot(m, P_m,
     type = 'h', lwd = 3, col = "darkgreen")
mtext("Distribucion marginal de M", 3, line = 1)

persp(x = m, y = x, z = P_x_d_m,
      ticktype = 'detailed',
      col = "#0088ff",
      theta = 65)
mtext("Distribucion condicional P(X | M)", 3, line = 1)

persp(x = m, y = x, z = P_X_m,
      ticktype = "detailed",
      col = "#ff88ff",
      theta = 65)
mtext("Distribucion conjunta P(X, M)", 3, line = 1)

plot(x, P_x,
     type = 'h', lwd = 3, col = "darkblue")
mtext("Distribucion marginal de X", 3, line = 1)


# Solucion iterativa
# Valida para n observaciones

rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Funciones
source("distributions/pmf.R") # Funciones de masa de probabilidad

# Parametros conocidos de la distribucion hipergeometrica
n <- 4 # Fracasos
k <- 5 # Tamaño de las extracciones

# Variable observable
x <- 0:5

# Primer Prior (antes de la primera observacion)
m <- 0:20
# Carga probabilistica a priori inicial
# P_m <- c(rep(0, 19), 7, 6)

# Datos/observaciones
x_observada <- c(1, 4, 4, 2, 3, 3, 4, 2, 3)


for (indice in 1:(length(x_observada))) {

  # Condicional para asignar la carga inicial de la distribucion a priori
  if(indice == 1) {
    P_m <- c(1,3,6,6,7,7,6,5,3,3,2,2,1,1,1,1,1,2,2,2,1)
    P_m <- P_m/sum(P_m)
    plot(m, P_m, type = 'b', pch=16, ylim=c(0, 0.5))
  }
  # Condicional para las cargas posteriores con base en los datos observados
  else if (indice > 1) {
    P_m <- P_m_d_x
  }

  # Matrices a llenar por cada nueva distribucion a priori
  P_x_d_m <- array(dim = c(length(m), length(x)))
  P_X_m <- array(dim = dim(P_x_d_m))
  P_x <- array(dim = length(x))

  for (m_hyp in m) {
    pos = which(m == m_hyp)
    
    P_x_d_m[pos, ] <- hipergeom_dist(x, m_hyp, n, k)
    P_X_m[pos, ] <- P_x_d_m[pos, ]*P_m[pos]
  }

  for (x_hyp in x) {
    pos = which(x == x_hyp)
    P_x[pos] <- sum(P_X_m[, pos], na.rm = T)
  }

  # posterior
  x_obs <- x_observada[indice]
  pos = which(x==x_obs)
  P_m_d_x <- P_X_m[, pos]/P_x[pos]

  color <- paste0("#", indice-1, "f0000", indice, "f")
  points(m, P_m_d_x, type='o', pch=14+indice, col = color, bg = "gray")
}

points(m, P_m_d_x, cex=1.5, col='red')
