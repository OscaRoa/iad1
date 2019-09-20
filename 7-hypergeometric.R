# Distribucion hipergeometrica

m <- 3 # Total exitos poblacion
n <- 5 # Total fracasos poblacion
k <- 6 # Tamaño de la extraccion (de la muestra) sin remplazo

# Para obtener los límites superiores e inferiores de la variable aleatoria
# se calcula el límite inferior como el máximo entre 0 y k-n, ya que
# esta resta puede dar un negativo; para el límite superior, se toma el mínimo
# entre el tamaño de la muestra o el total de éxitos que hay en población, ya que
# el número de éxitos no puede exceder al tamaño de la muestra
upper_limit <- min(c(k, m))
lower_limit <- max(c(0, k-n))
x <- lower_limit:upper_limit


coef_bin <- function(a,b) {
  # Calcular coeficiente binomial
  # Devuelve ya sea un numero entero o un vector de enteros.
    return(factorial(a)/(factorial(b)*factorial(a-b)))
}
# coef_bin(3,2) Number operation
# coef_bin(n, x) Vector operation

hipergeom_dist <- function(x, m, n, k) {
    # Numero de exitos en muestras sin reemplazo de tamaño k,
    # extraidas de una poblacion con m exitos y n fracasos posibles

    success <- coef_bin(m, x) # Posibles combinaciones de exito
    fail <- coef_bin(n, k - x) # Posibles combinaciones de fracasos
    total <- coef_bin(m + n, k) # Total de combinaciones
    return(success * fail / total)
}

p_dist <- hipergeom_dist(x, m, n, k)

plot(x, p_dist,
     type = "h", lwd = 1.5,
     xlim = c(0, upper_limit), ylim = c(0, max(p_dist))
     )
mtext("Distribución hipergeométrica", 3, line = 2, col = "blue")
mtext(paste("Aciertos m =", m, "; Fracasos n =", n, "; Muestra k =", k), 3, line = .5)
