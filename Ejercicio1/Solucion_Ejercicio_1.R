#Nombre: Diego Yupanqui Huaman 20160544C
#Respuesta1: El codigo contiene la simulacion de generar alturas en 9 generaciones
#a partir de una inicial, y el grafico de histogramas para cada generacion


popular <- data.frame(m=rnorm(100,160,20), f=rnorm(100,160,20))
popular
prox.gen<-function(popular){
  popular$m<-sample(popular$m)
  popular$m<-apply(popular, 1, mean)
  popular$f<-popular$m
  return(popular)
}
popular_generacion1<-prox.gen(popular)
hist(popular_generacion1$m)

popular_generacion2<-prox.gen(popular_generacion1)
hist(popular_generacion2$m)

popular_generacion3<-prox.gen(popular_generacion2)
hist(popular_generacion3$m)

popular_generacion4<-prox.gen(popular_generacion3)
hist(popular_generacion4$m)

popular_generacion5<-prox.gen(popular_generacion4)
hist(popular_generacion5$m)

popular_generacion6<-prox.gen(popular_generacion5)
hist(popular_generacion6$m)

popular_generacion7<-prox.gen(popular_generacion6)
hist(popular_generacion7$m)

popular_generacion8<-prox.gen(popular_generacion7)
hist(popular_generacion8$m)

popular_generacion9<-prox.gen(popular_generacion8)
hist(popular_generacion9$m)

