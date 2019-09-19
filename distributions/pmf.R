# Funciones de masa de probabilidad para diferentes distribuciones

# Coeficiente binomial usado en distintas pmf
coef_bin <- function(a,b) {
    # Calcular coeficiente binomial
    # Devuelve ya sea un numero entero o un vector de enteros.
    return(factorial(a)/(factorial(b)*factorial(a-b)))
}

# Distribucion binomial
binomial_dist <- function(n, theta, x) {
    # Funcion que calcula la distribucion binomial de un vector de datos 'X'
    # con una probabilidad 'theta' de exito en 'n' ensayos.
    # Devuelve un vector con los valores de la distribucion.
    bin_coef <- coef_bin(n, x)
    distribution <- bin_coef*((theta**x)*(1 - theta)**(n-x))
    return(distribution)
}

# Distribucion geometrica
geom_dist <- function(va, prob) {
    # Funcion que calcula la distribucion geometrica
    # con una probabilidad 'prob' de que aparezca el primer exito
    # en una variable aleatoria
    # Devuelve un vector con los valores de la distribucion.
    distribution <- (prob)*(1 - prob)**(va)
    return(distribution)
}

hipergeom_dist <- function(x, m, n, k) {
    # Numero de exitos en muestras sin reemplazo de tamaño k,
    # extraidas de una poblacion con m exitos y n fracasos posibles
    
    success <- coef_bin(m, x) # Posibles combinaciones de exito
    fail <- coef_bin(n, k - x) # Posibles combinaciones de fracasos
    total <- coef_bin(m + n, k) # Total de combinaciones
    return(success * fail / total)
}