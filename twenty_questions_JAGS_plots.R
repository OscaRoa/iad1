rm(list = ls())
library("R2jags")
setwd("~/Documents/fac/iad1/.")
exam <- read.csv('data/twenty_questions.csv')

np <- nrow(exam)
nq <- ncol(exam)


var_observables <- list('np', 'nq', 'exam')
var_no_observables <- c('p', 'q', 'theta', 'exam_post')

# Evaluacion del modelo con valores nulos
# exam[8, 5] <- NA

write("
  model{
    # Priors personas y preguntas
    for(i in 1:np){
      p[i]~dbeta(1,1)
    }
    for(j in 1:nq){
      q[j]~dbeta(1,1)
    }
    
    # Probabilidad de aciertos para cada persona y pregunta
    for(i in 1:np){
      for(j in 1:nq){
        # Valor de Theta por metodo de producto de probabilidades
        theta[i, j] <- p[i] * q[j]
        # Valor de Theta por el metodo Rash
        # theta[i,j]<-exp(p[i] - q[j])/(1 + exp(p[i] - q[j]))
        
        exam[i,j] ~dbinom(theta[i,j], 1)
        exam_post[i, j] ~dbinom(theta[i,j], 1)
      }
    }
    
  }
      ", "twenty.bug")

bayes <- jags(
  data = var_observables,
  parameters.to.save = var_no_observables,
  n.chains = 3,
  n.iter = 10000,
  model.file = "twenty.bug"
  )

nodos <- bayes$BUGSoutput$sims.list

# Matriz y layout para preguntas
layout(matrix(1:20, nrow = 4))
par(mar = rep(2, 4))

# Sorting
# Contar aciertos por reactivo y devuelve la posicion con los aciertos en orden ascendente
ord_q <- order(apply(nodos$q, FUN = sum, MARGIN = 2, na.rm = T))

for (i in ord_q) {
  hist(nodos$q[,i],
       xlim = c(0,1),
       freq = F,
       main = paste("Reactivo", colnames(exam)[i])
       )
}

# Matriz y layout para personas y para preguntas por persona
layout(matrix(1:10, nrow = 2))
par(mar = rep(2, 4))

# Contar aciertos por persona y devuele la posicion con los aciertos en orden ascendente
ord_p <- order(apply(nodos$p, FUN = mean, MARGIN = 2, na.rm = T))
for (i in ord_p) {
  hist(nodos$p[,i], 
       xlim = c(0,1),
       freq = F,
       main = paste("Persona", rownames(exam)[i]))
}

# Asignacion posterior predictiva
pred_post <- array(dim=dim(exam))
for (i in 1:np) {
  for (j in 1:nq) {
    pred_post[i,j] <- median(nodos$exam_post[,i,j])
  }
}

# Plots de la posterior predictiva

metodo <- "producto probabilidades"

layout(1)
plot(NULL,
     xlim = c(0, nq + 1),
     ylim = c(0, np + 1),
     main = paste("Examen ordenado -", metodo)
     )

ord_p <- order(apply(exam, FUN = mean, MARGIN = 1, na.rm = T))
ord_q <- order(apply(exam, FUN = mean, MARGIN = 2, na.rm = T))

exam_ord <- exam[ord_p, ord_q]
pred_post_ord <- pred_post[ord_p, ord_q]

for (i in ord_p) {
  for (j in ord_q) {
    if (exam_ord[i,j] == 1) {
      color <- 'white'
    }
    else {
      color <- 'black'
    }
    points(j,i, pch = 21, bg = color, cex = 2)
  }
}

layout(1)
plot(NULL,
     xlim = c(0, nq + 1),
     ylim = c(0, np + 1),
     main = paste("Posterior predictiva ordenada de examen -", metodo)
     )
for (i in ord_p) {
  for (j in ord_q) {
    if (pred_post_ord[i,j] == 1) {
      color <- 'white'
    }
    else {
      color <- 'black'
    }
    points(j,i, pch = 21, bg = color, cex = 2)
  }
}
