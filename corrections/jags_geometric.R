# Infiriendo la tasa de éxitos de una tachuela
# con observaciones geométricas
# utilizando JAGS.

rm(list=ls())
library('R2jags')
# Datos
x_observados <- c(0,0,0,5,0,0,0,0,3,0,0,1,1,3,0,0,1,7,2,0,0,0,1,0)
num_obs <- length(x_observados)
# Variables no-observables
variables_no_observables <- c('theta_prior','theta_post',
                              'x_prior_pred','x_post_pred')
# Variables observables
variables_observables <- list('x_observados','num_obs')
# Modelo
write('
model{
  theta_prior~dbeta(25,5)
  theta_post~dbeta(25,5)
  for(i in 1:num_obs){
    x_observados[i]~dnegbin(theta_post,1)
  }
  x_prior_pred~dnegbin(theta_prior,1)
  x_post_pred~dnegbin(theta_post,1)
}
','model_name.bug')
# Inferencia
bayes <- jags(data = variables_observables,
              parameters.to.save = variables_no_observables,
              model.file = 'model_name.bug',
              n.chains = 3,
              n.iter = 10000,
              n.burnin = 100,
              n.thin = 1)
# 'bayes' tiene toda la información de JAGS
# (posteriores incluidas!!)

# Extrayendo nodos
nodos <- bayes$BUGSoutput$sims.list


# Graficando
color_prior <- '#ee8800'
color_post <- '#22ee33'
color_data <- '#4400ee'
# Preparando el "multiplot":
layout(matrix(1:4,ncol=2,byrow = T))
# Prior sobre theta
hist(nodos$theta_prior,xlim=c(0,1),breaks=seq(0,1,length.out = 20),freq=F,main='theta prior',col=color_prior)
# Posterior sobre theta
hist(nodos$theta_post,xlim=c(0,1),breaks=seq(0,1,length.out = 20),freq=F,main='theta post',col=color_post)
# Predictivas
max_x <- max(max(nodos$x_post_pred),max(nodos$x_prior_pred))
# Prior predictiva sobre x
hist(nodos$x_prior_pred,xlim=c(0,15),ylim=c(0,1),breaks=seq(-.5,max_x+.5,1),freq=F,main='x prior pred',col=color_prior)
# Posterior predictiva sobre x + observaciones "reales"
hist(nodos$x_post_pred,xlim=c(0,15),ylim=c(0,1),breaks=seq(-.5,max_x+.5,1),freq=F,main='x post pred',col=color_post)
hist(x_observados,breaks=seq(-.5,max(x_observados)+.5,1),plot=F)->ht_post
lines(ht_post$mids,ht_post$density,type='o',pch=16,col=color_data)
