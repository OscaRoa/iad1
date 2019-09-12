# Distribución Hipergeométrica
rm(list=ls())

# Sabemos que utilizaremos el coeficiente binomial 
# para calcular la función de masa hipergeométrica:
coef_bin <- function(a,b){
  result=factorial(a)/(factorial(b)*(factorial(a-b)))
  return(result)
}

# Los parámetros de la distribución:
m <- 4 # Número de éxitos en la población
n <- 3 # Número de fracasos en la población
k <- 5 # Tamaño de la muestra (sin reemplazo)

# Variable aleatoria
x <- max(c(0,k-n)):min(c(k,m)) # Número de éxitos en la muestra (sin reemplazo)

# Función de masa de probabilidad:
pmf_hyper <- coef_bin(m,x)*coef_bin(n,k-x)/coef_bin(m+n,k)
# (después de correr hasta la línea 20, el objeto 'pmf_hyper' contiene
# los valores de probabilidad para cada valor de la variable aleatoria 'x')

# Graficando:
plot(x,pmf_hyper,type='h',lwd=3)