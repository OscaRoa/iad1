library(tidyverse)

X <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) # Valores de la variable aleatoria X
p_x <- c(1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1)/36 # Probabilidad de cada valor de X
                                             # 1/36, 2/36, 3/36 ... etc.

# Momentos de la distribucion

E_X <- sum(X*p_x)           # Esperanza
V_X <- sum(p_x*(X-E_X)**2)  # Varianza
SD_X <- V_X**(1/2)          # Desviacion estandar

framed_data <- tibble(X, p_x)
ggplot(data = framed_data) +
    geom_bar(mapping = aes(x = X, y = p_x), stat = "identity", width = .25) +
    scale_x_discrete(limits = X)
