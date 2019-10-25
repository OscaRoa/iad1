rm(list=ls())

color_prior <- '#ee8800'
color_likelihood <- '#00dd00'
color_posterior <- '#ee3300'

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Funciones
source("distributions/pmf.R") # Funciones de masa de probabilidad

# Parámetros conocidos
k <- 5
# Variable observable
x <- 0:k # Posbiles cantidades de vaquitas hembra en la muestra de tamaño k


# Parámetros desconocidos y no-observables
m <- 0:20 # Vaquitas hembra
n <- 0:30 # Vaquitas macho

prior_type <- 1

if (prior_type == 1) {
  # 1. Primer posible PRIOR (uniforme)
  P_m <- rep(1/length(m),length(m))
  P_n <- rep(1/length(n),length(n))
  prior_mn <- array(dim=c(length(m),
                          length(n)))
} else if (prior_type == 2) {
  # 2. Otro posible PRIOR (informado sobre cada variable 'por separado')
  P_m <- coef_bin(20,m)*0.3^m*(1-0.3)^(20-m)
  P_n <- coef_bin(30,n)*0.7^n*(1-0.7)^(30-n)
  prior_mn <- array(dim=c(length(m),
                          length(n)))
} else if (prior_type == 3) {
  prior_mn <- array(dim=c(length(m),
                          length(n)))
  for(hyp_m in m){
    for(hyp_n in n){
      position_m <- which(m==hyp_m)
      position_n <- which(n==hyp_n)
      expected_n <- round(hyp_m*1.5)
      prediction_error <- abs(n[position_n]-expected_n)
      if(prediction_error<=1){
        peso <- 10
      }
      else if(prediction_error<=3){
        peso <- 8
      }
      else if(prediction_error<=6){
        peso <- 4
      }
      else if(prediction_error<=10){
        peso <- 2
      }
      else{
        peso <- 1
      }
      prior_mn[position_m,position_n] <- peso
    }
  }
  # Al terminar la iteración, prior_mn está lleno de enteros:
  prior_mn
  # Reescalando:
  prior_mn <- prior_mn/sum(prior_mn)
  # Extrayendo las distribuciones a priori marginales:
  P_m <- apply(prior_mn,MARGIN = 1,FUN=sum)
  P_n <- apply(prior_mn,MARGIN = 2,FUN=sum)
}

if (prior_type == 1 | prior_type == 2) {
  # suponiendo independencia entre m y n a priori
  for(hyp_m in m){
    for(hyp_n in n){
      position_m <- which(m==hyp_m)
      position_n <- which(n==hyp_n)
      prior_mn[position_m,position_n] <- P_m[position_m]*P_n[position_n]
    }
  }
}

# Examiando la distribución conjunta a priori:
# try(dev.off())
persp(x = m,y = n,z=prior_mn,
      zlim=c(0,.04),
      col=paste(color_prior,'88',sep=''),
      border='#888888',
      ticktype = 'detailed')

# Verosimilitud sobre x dada cada posible combinación entre m y n
P_x_d_mn <- array(dim=c(length(m),
                        length(n),
                        length(x)))
# Conjunta sobre cada posible combinación entre m, n Y x
P_xmn <- array(dim=c(length(m),
                     length(n),
                     length(x)))
for(hyp_m in m){
  for(hyp_n in n){
    position_m <- which(m==hyp_m)
    position_n <- which(n==hyp_n)
    P_x_d_mn[position_m,position_n,] <- hipergeom_dist(x, hyp_m, hyp_n, k)
    P_xmn[position_m,position_n,] <- P_x_d_mn[position_m,position_n,]*prior_mn[position_m,position_n]
  }
}

plot_matrix <- rbind(1:3,
                  4:6)
layout(plot_matrix)

# Podemos examinar el 'cubo' de verosimilitud cortando 'rebanadas' respecto de x:
for (i in x) {
  persp(x = m,y = n,z=P_x_d_mn[,,i+1],
        zlim=c(0,1),col=paste(color_likelihood,'88',sep=''),
        border='#888888',ticktype = 'detailed')
  title(main = paste("Distribucion marginal verosimilitud P(m,n) para X =", i))
}
# O examinando combinaciones entre m y n:
plot(x,P_x_d_mn[8,6,],type='h',lwd=10,col=color_likelihood)

layout(plot_matrix)
# También podemos examinar el 'cubo' conjunto cortando 'rebanadas' respecto de x:
for (i in x) {
  persp(x = m,y = n,z=P_xmn[,,i+1],
        # zlim=c(0,.002),
        zlim=c(0,.008),
        col=paste(color_prior,'88',sep=''),border='#888888',ticktype = 'detailed')
  title(main = paste("Distribucion marginal conjunta P(m,n) para X = ", i))
}

# La distribución conjunta entre las tres variables observables 'contiene'
# la incertidumbre marginal sobre x:
(P_x <- apply(P_xmn, MARGIN = 3, FUN = sum, na.rm = T))
# Examinando la incertidumbre a priori sobre la variable observable (resultado de las
# suposiciones iniciales sobre las no-observables y sobre el modelo que las relaciona
# con la observable):
plot(x,P_x,type='h',lwd=10,col=color_prior)

# Suponiendo que *observamos* X=3, actualizamos la incertidumbre sobre los DOS
# parámetros desconocidos siguiendo la definición de Probabilidad Condicional:
# P(m,n|x)=P(m,n,x)/P(x)
x_obs <- 3
position_x <- which(x==x_obs)
posterior_mn_d_x <- P_xmn[,,position_x]/P_x[position_x] 
# ¡La clave es tener claro qué dimensión del arreglo conjunto corresponde a la variable observable!

# Comparando las distribuciones prior y posterior sobre las variables no-observables:
par(mar=rep(1,4))
layout(1:2)
persp(x = m,y = n,z=prior_mn,
      # zlim=c(0,.01),
      zlim=c(0,.05),
      col=paste(color_prior,'88',sep=''),border='#888888',ticktype = 'detailed')
persp(x = m,y = n,z=posterior_mn_d_x,
      # zlim=c(0,.01),
      zlim=c(0,.05),
      col=paste(color_posterior,'88',sep=''),border='#888888',ticktype = 'detailed')

# En tanto que la distribución posterior ES una distribución conjunta (sobre las posibles combinaciones
# entre m y n), contiene la incertidumbre marginal (posterior) sobre cada una de dichas variables:
(posterior_m_d_x <-  apply(posterior_mn_d_x,MARGIN=1,FUN=sum,na.rm=T))
(posterior_n_d_x <- apply(posterior_mn_d_x,MARGIN=2,FUN=sum,na.rm=T))

# Comparando la incertidumbre marginal posterior sobre cada variable no-observable:
par(mar=c(4,4,1,1))
layout(1:2)
plot(m,P_m,type='o',pch=16,
     ylim=c(0,.3),
     col=color_prior)
points(m,posterior_m_d_x,type='o',pch=16,col=color_posterior)
plot(n,P_n,type='o',pch=16,
     ylim=c(0,.3),
     col=color_prior)
points(n,posterior_n_d_x,type='o',pch=16,col=color_posterior)
