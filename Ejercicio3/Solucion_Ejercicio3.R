#Nombre: Diego Yupanqui Huaman 20160544C
#Respuesta3: El codigo muestra la comprobacion empirica de la formula del problema
#y la modificacion del codigo dado para que funcione de manera eficaz en el caso de la 
#distribucion de Poisson.



#Comprobamos la valides de la formula para valores de x hasta n, con parametro t:
validez_formula<-function(n,t){
  cont=0
  for (i in (1:n)){
    m<-(i+1)*dpois(c(i+1),t)/dpois(c(i),t)
    if(signif(m,10)-t!=0) cont=cont+1  #Usamos signif para redondear a 10 decimales
  }
  if(cont==0)
    print(TRUE)
  else
    print(FALSE)
}
#Esta funcion demuestra de manera empirica que la formula es valida
#Por ejemplo, tomamos el valor n=100 y parametro t=0.5
validez_formula(100,0.5)

#Creamos una funcion para calcular p(x) con parametro t:
Calculo_p<-function(x,t){
  if(x==0)
    return(dpois(c(x),t))
  else
    return((t/x)*Calculo_p(x-1,t))
}

#Creamos una funcion para calcular F(x) con parametro t:
Calculo_F<-function(x,t){
  suma=0
  for(i in 0:x){

    suma<-suma+Calculo_p(i,t)
  }
  return(suma)
}

#Introduciendo los cambios a la funcion F.rand tenemos:
F.rand<-function(lambda){
  u<-runif(1)
  x<-0
  p.x<-Calculo_p(x,lambda)
  F.x<-Calculo_F(x,lambda)
  while(F.x<u){
    print(x)
    x<-x+1
    p.x<-Calculo_p(x,lambda)
    F.x<-Calculo_F(x,lambda)
  }
  return(x)
}
#Por ejemplo, generamos valores usando lambda=0.9
F.rand(0.9)
