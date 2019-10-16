rm(list=ls())

x_obsvervados <- c(0,0,0,5,0,0,0,0,3,0)
x <- 0:10

for(indice_obs in 1:length(x_obsvervados)){
  
  if(indice_obs==1){
    th <- seq(0,1,0.1)
    p_th <- c(9,8,7,3,2,2,2,3,7,8,9)
    p_th <- p_th/sum(p_th)
    plot(th,p_th,type='o',pch=16,col='#ee0000',ylim=c(0,.5))
  }
  else if(indice_obs>1){
    polygon(x=c(-1,2,2,-1),
            y=c(-1,-1,2,2),
            border=NA,col='#ffffff99')
    points(th,p_th_d_x,type='o',col='#0000ee77')
    p_th <- p_th_d_x
  }
  
  p_x_d_th <- array(dim=c(length(th),length(x)))
  p_x_th <- array(dim=c(length(th),length(x)))
  for(th_hyp in th){
    renglon <- which(th==th_hyp)
    p_x_d_th[renglon,] <- th_hyp*(1-th_hyp)^x
    p_x_th[renglon,] <- p_x_d_th[renglon,]*p_th[renglon]
  }
  
  p_x <- array(dim=length(x))
  for(x_pos in x){
    posicion <- which(x==x_pos)
    p_x[posicion] <- sum(p_x_th[,posicion]) 
  }
  
  x_obs <- x_obsvervados[indice_obs]
  pos_obs <- which(x==x_obs)
  p_th_d_x <- p_x_th[,pos_obs]/p_x[pos_obs]
  
}




# Graficando la distribución a priori (P_m), la función de verosimilitud (P_x_d_m),
# la distribución conjunta (P_xm) y la marginal sobre x (P_x).
mat_plot <- rbind(1:3,
                  c(0,0,4))
x11(width = 10,height = 4)
layout(mat_plot)
# La distribución marginal sobre 'm'
plot(th,p_th,
     type='h',lwd=5,
     col='#00aaee',
     ylim=c(0,.7))
mtext('P(th)',3)
# La distribución condicional sobre 'x' dado 'm'
persp(x = th,y = x,z = p_x_d_th,
      ticktype = 'detailed',
      col='#7700ee88',
      theta = 115,phi = 20)
mtext('P(x|th)',3)
# La distribución conjunta entre 'x' y 'm'
persp(x = th,y = x,z = p_x_th,
      ticktype = 'detailed',
      col='#ff880066',
      theta = 115,phi = 20)
mtext('P(x,th)',3)
# La distribución marginal sobre 'x'
plot(x,p_x,
     type='h',lwd=5,
     col='#dd3300',
     ylim=c(0,.8))
mtext('P(x)',3)


