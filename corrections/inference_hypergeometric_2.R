# Inferencia Bayesiana con extracciones hipergeométricas

# 1. Actualizando cada observación por separado:
rm(list=ls())
# Como utilizaremos el modelo Hypergeométrico,
# sabemos que es necesario programar el coeficiente binomial:
coef_bin <- function(a,b){
  result=factorial(a)/(factorial(b)*(factorial(a-b)))
  return(result)
}

# Parámetros conocidos
n <- 4
k <- 5
# Variable observable
x <- 0:5

# Parámetro desconocido y no-observable
m <- 0:20
P_m <- c(1,3,6,6,7,7,6,5,3,3,
         2,2,1,1,1,1,1,2,2,2,1)
P_m <- P_m/sum(P_m) # "Reescala" los valores enteros respecto de su propia suma. También conocida 
# como "distribución a priori" sobre m.

# Matrices vacías para guardar:
# 1. La distribución condicional sobre x dado cada 
# posible valor de m.
P_x_d_m <- array(dim=c(length(m), 
                       length(x)))
# 2. La distribución conjunta entre x y m
P_xm <- array(dim=c(length(m),
                    length(x)))

# 'Para cada valor hipotético de m...
for(m_hyp in m){
  # ...calcula la distribución condicional sobre x...
  pr <- coef_bin(m_hyp,x)*coef_bin(n,k-x)/coef_bin(m_hyp+n,k)
  # ...y guárdala en el renglón correspondiente de la matriz condicional...
  P_x_d_m[which(m==m_hyp),] <- pr
  # ...y después calcula y llena el renglón correspondiente en la conjunta.
  P_xm[which(m==m_hyp),] <- P_m[which(m==m_hyp)]*P_x_d_m[which(m==m_hyp),]
}

# Una vez que tenemos la distribución conjunta entre x y m, podemos
# calcular la distribución marginal sobre x. Esta distribución representa
# nuestras expectativas a priori sobre la variable *observable*, y depende
# de las supociones iniciales sobre la variable *no observable*.
P_x <- array(dim=length(x))
for(x_hyp in x){
  P_x[which(x==x_hyp)] <- sum(P_xm[,which(x==x_hyp)],na.rm=T)
}


## Primer dato OBSERVADO:
x_obs <- 1
P_m_d_x <- P_xm[,which(x==x_obs)]/P_x[which(x==x_obs)] # También conocida 
# como "distribución posterior" sobre m.


P_m <- P_m_d_x # La posterior después de la primera
# observación es la prior antes de la segunda

# Matrices vacías para guardar:
# 1. La distribución condicional sobre x dado cada 
# posible valor de m.
P_x_d_m <- array(dim=c(length(m), 
                       length(x)))
# 2. La distribución conjunta entre x y m
P_xm <- array(dim=c(length(m),
                    length(x)))

# 'Para cada valor hipotético de m...
for(m_hyp in m){
  # ...calcula la distribución condicional sobre x...
  pr <- coef_bin(m_hyp,x)*coef_bin(n,k-x)/coef_bin(m_hyp+n,k)
  # ...y guárdala en el renglón correspondiente de la matriz condicional...
  P_x_d_m[which(m==m_hyp),] <- pr
  # ...y después calcula y llena el renglón correspondiente en la conjunta.
  P_xm[which(m==m_hyp),] <- P_m[which(m==m_hyp)]*P_x_d_m[which(m==m_hyp),]
}

# Una vez que tenemos la distribución conjunta entre x y m, podemos
# calcular la distribución marginal sobre x. Esta distribución representa
# nuestras expectativas a priori sobre la variable *observable*, y depende
# de las supociones iniciales sobre la variable *no observable*.
P_x <- array(dim=length(x))
for(x_hyp in x){
  P_x[which(x==x_hyp)] <- sum(P_xm[,which(x==x_hyp)],na.rm=T)
}


## Segundo dato OBSERVADO:
x_obs <- 4
P_m_d_x <- P_xm[,which(x==x_obs)]/P_x[which(x==x_obs)]



P_m <- P_m_d_x # La posterior después de la primera
# observación es la prior antes de la segunda

# Matrices vacías para guardar:
# 1. La distribución condicional sobre x dado cada 
# posible valor de m.
P_x_d_m <- array(dim=c(length(m), 
                       length(x)))
# 2. La distribución conjunta entre x y m
P_xm <- array(dim=c(length(m),
                    length(x)))

# 'Para cada valor hipotético de m...
for(m_hyp in m){
  # ...calcula la distribución condicional sobre x...
  pr <- coef_bin(m_hyp,x)*coef_bin(n,k-x)/coef_bin(m_hyp+n,k)
  # ...y guárdala en el renglón correspondiente de la matriz condicional...
  P_x_d_m[which(m==m_hyp),] <- pr
  # ...y después calcula y llena el renglón correspondiente en la conjunta.
  P_xm[which(m==m_hyp),] <- P_m[which(m==m_hyp)]*P_x_d_m[which(m==m_hyp),]
}

# Una vez que tenemos la distribución conjunta entre x y m, podemos
# calcular la distribución marginal sobre x. Esta distribución representa
# nuestras expectativas a priori sobre la variable *observable*, y depende
# de las supociones iniciales sobre la variable *no observable*.
P_x <- array(dim=length(x))
for(x_hyp in x){
  P_x[which(x==x_hyp)] <- sum(P_xm[,which(x==x_hyp)],na.rm=T)
}


## Tercer dato OBSERVADO:
x_obs <- 4
P_m_d_x <- P_xm[,which(x==x_obs)]/P_x[which(x==x_obs)]



# Graficando la última dist. a priori y la última dist. posterior:
plot(m,P_m,col='#aaaaaa',lwd=10,type='h',
     ylim=c(0,.5))
points(m,P_m_d_x,col='#222222',lwd=5,type='h')


# Graficando la distribución a priori (P_m), la función de verosimilitud (P_x_d_m),
# la distribución conjunta (P_xm) y la marginal sobre x (P_x).
mat_plot <- rbind(1:3,
                  c(0,0,4))
x11(width = 10,height = 4)
layout(mat_plot)
# La distribución marginal sobre 'm'
plot(m,P_m,
     type='h',lwd=5,
     col='#00aaee',
     ylim=c(0,.7))
mtext('P(m)',3)
# La distribución condicional sobre 'x' dado 'm'
persp(x = m,y = x,z = P_x_d_m,
      ticktype = 'detailed',
      col='#7700ee88',
      theta = 30,phi = 20)
mtext('P(x|m)',3)
# La distribución conjunta entre 'x' y 'm'
persp(x = m,y = x,z = P_xm,
      ticktype = 'detailed',
      col='#ff880066',
      theta = 30,phi = 20)
mtext('P(x,m)',3)
# La distribución marginal sobre 'x'
plot(x,P_x,
     type='h',lwd=5,
     col='#dd3300',
     ylim=c(0,.8))
mtext('P(x)',3)








# Repitiendo el mismo proceso de inferencia, pero dentro de un ciclo
# for para trabajar con todas las observaciones:

rm(list=ls())
# Como utilizaremos el modelo Hypergeométrico,
# sabemos que es necesario programar el coeficiente binomial:
coef_bin <- function(a,b){
  result=factorial(a)/(factorial(b)*(factorial(a-b)))
  return(result)
}
# Parámetros conocidos
n <- 4
k <- 5
# Variable observable
x <- 0:5

# Parámetro desconocido y no-observable
m <- 0:20

## Datos/observaciones
x_observada <- c(1,4,4,2,3,3,4,2,3)

# indice <- 3
for(indice in 1:(length(x_observada))){
  if(indice==1){ # En la primera observación, define el prior sobre m como [líneas 206 a 208] y grafícalo...
    # Primer prior (prior antes de la primera obs)
    P_m <- c(1,3,6,6,7,7,6,5,3,3,
             2,2,1,1,1,1,1,2,2,2,1)
    P_m <- P_m/sum(P_m) # "Reescala" los valores enteros respecto de su propia suma
    plot(m,P_m,type='o',pch=16,ylim=c(0,.5))
  }
  else if(indice>1){ # ...si estamos en la segunda observación, o siguientes, el prior antes de dicha obs. es la posterior después de la anterior:
    # Prior a partir de la segunda obs
    P_m <- P_m_d_x # La posterior después de la primera
    # observación es la prior antes de la segunda
  }
  # Matrices vacías para guardar:
  # 1. La distribución condicional sobre x dado cada 
  # posible valor de m.
  P_x_d_m <- array(dim=c(length(m), 
                         length(x)))
  # 2. La distribución conjunta entre x y m
  P_xm <- array(dim=c(length(m),
                      length(x)))
  
  # 'Para cada valor hipotético de m...
  for(m_hyp in m){
    # ...calcula la distribución condicional sobre x...
    pr <- coef_bin(m_hyp,x)*coef_bin(n,k-x)/coef_bin(m_hyp+n,k)
    # ...y guárdala en el renglón correspondiente de la matriz condicional...
    P_x_d_m[which(m==m_hyp),] <- pr
    # ...y después calcula y llena el renglón correspondiente en la conjunta.
    P_xm[which(m==m_hyp),] <- P_m[which(m==m_hyp)]*P_x_d_m[which(m==m_hyp),]
  }
  
  # Una vez que tenemos la distribución conjunta entre x y m, podemos
  # calcular la distribución marginal sobre x. Esta distribución representa
  # nuestras expectativas a priori sobre la variable *observable*, y depende
  # de las supociones iniciales sobre la variable *no observable*.
  P_x <- array(dim=length(x))
  for(x_hyp in x){
    P_x[which(x==x_hyp)] <- sum(P_xm[,which(x==x_hyp)],na.rm=T)
  }
  
  # Posterior
  x_obs <- x_observada[indice]
  P_m_d_x <- P_xm[,which(x==x_obs)]/P_x[which(x==x_obs)]
  
  # Después de calcular cada posterior, agrega una capa semi-transparente al plot abierto
  # y después agrega la distribución posterior recién computada: 
  polygon(x=c(-1,-1,21,21),
          y=c(-1,1,1,-1),
          border = NA,col='#ffffff33')
  points(m,P_m_d_x,type='o',pch=16)
}

