rm(list=ls())
# Parámetros concidos
n <- 4
k <- 5
# Variable observable
x <- 0:5

# Parámetro desconocido y no-observable
m <- 0:20
P_m <- c(1,3,6,6,7,7,6,5,3,3,
         2,2,1,1,1,1,1,2,2,2,1)
P_m <- P_m/sum(P_m) # "Reescala" los valores enteros respecto de su propia suma


# Como utilizaremos el modelo Hypergeométrico,
# sabemos que es necesario programar el coeficiente binomial:
coef_bin <- function(a,b){
  result=factorial(a)/(factorial(b)*(factorial(a-b)))
  return(result)
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


# Graficando...
mat_plot <- rbind(1:3,
                  c(0,0,4))
x11(width = 10,height = 4)
layout(mat_plot)
# La distribución marginal sobre 'm'
plot(m,P_m,
     type='h',lwd=5,
     col='#00aaee',
     ylim=c(0,.2))
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
     ylim=c(0,.5))
mtext('P(x)',3)










