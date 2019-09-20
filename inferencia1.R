###############################
#   Inferencia estadistica    #
###############################

# Establece directorio de trabajo
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Funciones
source("distributions/pmf.R") # Funciones de masa de probabilidad

# Parametro desconocido de la distribucion hipergeometrica
m <- 0:20
P_m <- c(1,2,3,6,6,7,7,6,5,3,3,2,1,1,1,1,1,1,1,1,1) # Carga probabilistica a priori
P_m <- P_m/sum(P_m) # Proporciones de las cargas probabilisticas a priori

# Parametros conocidos de la distribucion hipergeometrica
n <- 4 # Fracasos
k <- 5 # Tamaño de las extracciones

# Variable observable
x <- 0:5

# Matriz de la probabilidad conjunta P(m, x)
P_x_d_m <- array(dim = c(length(m), length(x)))

# Iteramos sobre el posible número de éxitos en la poblacion (m)
for (i in m) {
    # Ya que estamos trabajando con una distribucion hipergeometrica
    # tenemos que calcular la distribucion para cada posible valor y asignarlo
    # a una posicion en la matriz de la probabilidad conjunta que corresponda a
    # los valores de m

    P_x_d_m[which(m==i), ] <- hipergeom_dist(x, i, n, k)
}

# Grafica tridimensional de la matriz
persp(x = m, y = x, z = P_x_d_m,
      ticktype = 'detailed',
      col = "#0088ff33",
      theta = 65)
