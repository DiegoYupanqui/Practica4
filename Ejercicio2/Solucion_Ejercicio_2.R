#Nombre: Diego Yupanqui Huaman 20160544C
#Respuesta2: El codigo contiene funciones para calcular las probabilidades de los eventos 
#dados en el problema


#A es el evento: S_i y S_j son ambos 1
prob_A<-function(cadena,N){
  cant_1<-sum(cadena)
  return((cant_1/N)*((cant_1-1)/(N-1)))
}

#B es el evento: |i-j| <= k
prob_B<-function(N,K){
  
  prob_1<-0
  prob_2<-0
  prob_3<-0
  cont<-0
  
  if(N==K || N==K+1) return(1)
  
  if(N-2*K>0){
    #Usndo la ley de la probabilidad total:
    #una vez que i toma un valor entre 1 y k
    
    for(i in 0:(K-1))
    {
      if(K+i<=N-1)
        prob_1 <- ((K+i)/(N-1))*(1/N) + prob_1
    }
    
    #una vez que i toma un valor entre k+1 y N-k
    
    prob_2 <- (((N-(2*K))*2*K)/(N-1))*(1/N)
    
    #una vez que i toma un valor entre N-k+1 y N tiene las mismas probabilidades que al
    #tomar un valor entre 1 y k
    
    return(prob_1 + prob_2 + prob_1)
  }
  if(2*K>N){
    
    for(i in 0:K-1)
    {
      if(K+i<=N-1){
        prob_3 <- ((K+i)/(N-1))*(1/N) + prob_3
        cont<-cont+1
      }
    }
    
    return(2*prob_3 + (N-2*cont)/N)
    
  }
}

Datos<-scan("C:\\Users\\DCX\\Desktop\\Practica_4\\Ejercicio2\\Entrada_problema_1.txt")
n<-Datos[1]
for(j in 1:n)
{
  N<-Datos[2+(3*(j-1))]
  K<-Datos[3+(3*(j-1))]
  cadena_numero<-Datos[4+3*(j-1)]
  cadena_0<-rep(NA,N)
  cadena<-rep(NA,N)
  
  for(i in 1:N)
  {
    cadena_0[i]<-as.integer(cadena_numero/10^(N-i))
  }
  for(i in 1:N)
  {
    if(i!=1) 
      cadena[i]<-cadena_0[i]-10*cadena_0[i-1]
    else
      cadena[i]=cadena_0[i]
  }
  
  print(prob_A(cadena,N))
  print(prob_B(N,K))
}





