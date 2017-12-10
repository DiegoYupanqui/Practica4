#Nombre: Diego Yupanqui Huaman 20160544C
#Respuesta7: El codigo muestra una funcion que calcula los cuantiles


#Utilizaremos el metodo de la biseccion para encontrar la raiz de la funcion sugerida

funcion_cuantil<-function(alpha){
  #Primero buscamos valores de z que hagan positiva y negativa la funcion:
  
  z_1<-sample(-10:10, size = 1)
  z_2<-sample(-10:10, size = 1)


  while(pnorm(z_1,mean = 0, sd=1)-alpha<=0){
    z_1<-sample(-10:10, size = 1)
  }
  while(pnorm(z_2,mean = 0, sd=1)-alpha>=0){
    z_2<-sample(-10:10, size = 1)
  }
  
  #Con esto, tenemos que el valor buscado se encuentra en el intervalo <z_2,z_1>
  #Ahora acortamos el intervalo hasta que quede un intervalo de longitud menor a 10^-12
  
  z_3<-runif(1, min = z_2, max = z_1)

  while(abs(pnorm(z_3,mean = 0, sd=1)-alpha)>0.0000000000001){
    if(pnorm(z_3,mean = 0, sd=1)-alpha>0) z_1<-z_3
    if(pnorm(z_3,mean = 0, sd=1)-alpha<0) z_2<-z_3
    z_3<-runif(1, min = z_2, max = z_1)
  }
  #El punto que queda es la respuesta que buscamos
  return(z_3)
}

#R cuenta con la funcion qnorm() que calcula los cuantiles
#comparamos resultados:
qnorm(0.6, mean = 0, sd=1)
funcion_cuantil(0.6)

qnorm(0.3, mean = 0, sd=1)
funcion_cuantil(0.3)

qnorm(0.8, mean = 0, sd=1)
funcion_cuantil(0.8)
