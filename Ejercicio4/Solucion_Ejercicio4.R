#Nombre: Diego Yupanqui Huaman 20160544C
#Respuesta4: El codigo muestra la solucion al problema de Claudio planteado en la pregunta

Datos<-scan("C:\\Users\\DCX\\Desktop\\Practica_4\\Ejercicio4\\Entrada_problema_2.txt")

#Creamos una funcion para generar una posible respuesta para Claudio
generar_respuesta<-function(M,N){
  resp<-rep(NA,M)
  for(i in 1:M){
    if(i==1) resp[i]<-sample(1:N,1)
    else {
      if(resp[i-1]==N) resp[i]<-N
      else resp[i]<-sample(resp[i-1]:N,1)
      }
  }
  return(resp)
}

#Creamos una funcion para calcular la maxima moda
f<-function(X,N){
  moda<-rep(0,N)
  for(i in 1:length(X)){
    moda[X[i]]<-moda[X[i]]+1
  }
  return(max(moda))
}

#Creamos una funcion para calcular el valor esperado de manera empirica para cierto n
Calculo_valor_esperado<-function(M,N,n){
  vector<-rep(NA,n)
  for(i in 1:n){
    A<-generar_respuesta(M,N)
    vector[i]<-f(A,N)
  }
  return(mean(vector))
}

#Calculamos los valores esperados segun los datos obtenidos del archivo de texto:

for(i in 1:Datos[1]){
  M<-Datos[2+2*(i-1)]
  N<-Datos[3+2*(i-1)]
  respuesta<-Calculo_valor_esperado(M,N,1000000)
  print(respuesta)
}



