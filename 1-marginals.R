# Declaracion de la matriz con los datos de la distribucion de las variables G y E obtenidos previamente
G_E <- array(dim = c(3,4))
G_E[1, ] <- c(0.05, .03, .01, .01)
G_E[2, ] <- c(0.0, 0.07, 0.09, 0.04)
G_E[3, ] <- c(0.25, .3, .1, .05)

# Marginales de G
p_g1 <- sum(G_E[1,])
p_g2 <- sum(G_E[2,])
p_g3 <- sum(G_E[3,])

# Marginales de E
p_e1 <- sum(G_E[,1])
p_e2 <- sum(G_E[,2])
p_e3 <- sum(G_E[,3])
p_e4 <- sum(G_E[,4])

# Probabilidad condicionada de G dado e2 P(G|e2)
p_G_d_e2 <- G_E[,2]/p_e2

# Probabilidad condicionada de E dado g2 P(E|g2)
p_E_d_g2 <- G_E[2,]/p_g2
