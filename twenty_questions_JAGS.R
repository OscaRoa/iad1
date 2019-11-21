rm(list = ls())
library("R2jags")
setwd("~/Documents/fac/iad1")
exam <- read.csv('data/twenty_questions.csv')

np <- nrow(exam)
nq <- ncol(exam)


var_observables <- list('np', 'nq', 'exam')
var_no_observables <- c('p', 'q', 'theta')

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
        theta[i,j]<-p[i]*q[j]
        exam[i,j]~dbern(theta[i,j])
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

summary(nodos$p)
summary(nodos$q)

layout(matrix(1:10, nrow = 2))
par(mar = rep(5, 4))

for (i in 1:nq) {
  hist(nodos$q[,i],
       xlim = c(0,1),
       freq = F,
       main = paste("Pregunta", i))
}

for (i in 1:np) {
  hist(nodos$p[,i], 
       xlim = c(0,1),
       freq = F,
       main = paste("Persona", i))
}

for (i in 1:nq) {
  for (j in 1:np) {
    hist(nodos$theta[,j,i],
         xlim = c(0,1),
         freq = F,
         main = paste("Pregunta", i, "-", "Persona", j))
  }
}
