#Nombre: Diego Yupanqui Huaman 20160544C
#Respuesta5: El codigo muestra la explicacion de como funciona cada linea de codigo
#dada en la pregunta, y el calculo pedido con los valores del dataframe iris

#ITEM A:

plot.new()
#Se inicia un nuevo grafico en blanco

plot.window(range(pressure$temperature), range(pressure$pressure))
#Definimos el sistema coordinado sobre el cual trabajaremos, en el eje x se tendran los valores
#de la columna temperature del dataframe pressure y en el eje y los valores de la columna
#pressure del dataframe pressure.
help("mtext")
box()
#Incluimos una caja en el grafico en blanco

axis(1)
#Agregamos un eje al grafico, el parametro 1 indica que agregamos en eje x

axis(2)
#Agregamos un eje al grafico, el parametro 2 indica que agregamos en eje y

points(pressure$temperature, pressure$pressure)
#Agregamos los puntos del grafco, los parametros nos indican que cada punto corresponde a 
#una fila del dataframe pressure, con el valor de x igual a su valor en la columna temperature
#y con el valor de y igual a su valor en la columna pressure.

mtext("temperatura", side = 1, line = 3)
#Agregamos el texto "temperatura", el parametro side=1 indica que el texto va debajo del eje
#x y el parametro line=3 indica que va 3 lineas por debajo del eje.

mtext("presion", side = 2, line = 3)
#Agregamos el texto "presion", el parametro side=2 indica que el texto va a la izquierda del eje
#y y el parametro line=3 indica que va 3 lineas a la izquierda del eje.

mtext("Presion de vapor \n como una funcion de la Temperatura ", side = 3, line = 1, font = 3)
#Agregamos el texto "Presion de vapor \n como una funcion de la temperatura", el parametro 
#side=3 indica que el texto va sobre la caja, line=1 indica que esta una linea separado de
#la caja y el parametro font=2 nos da la intensidad del color de las letras


#ITEM B:

curve(gamma(x),type = "l", from = 0.1, to= 1)
curve(gamma(x),type = "p", from = 0.1, to= 1, add = T)

#ITEM C:

sepal_length_setosa<-0
petal_length_setosa<-0
sepal_width_setosa<-0
petal_width_setosa<-0

sepal_length_versicolor<-0
petal_length_versicolor<-0
sepal_width_versicolor<-0
petal_width_versicolor<-0

sepal_length_virginica<-0
petal_length_virginica<-0
sepal_width_virginica<-0
petal_width_virginica<-0


for(i in 1:50)
{
  sepal_length_setosa<-sepal_length_setosa + iris[i,1]
  sepal_width_setosa<-sepal_width_setosa + iris[i,2]
  petal_length_setosa<-petal_length_setosa + iris[i,3]
  petal_width_setosa<-petal_width_setosa + iris[i,4]
}
for(i in 51:100)
{
  sepal_length_versicolor<-sepal_length_versicolor + iris[i,1]
  sepal_width_versicolor<-sepal_width_versicolor + iris[i,2]
  petal_length_versicolor<-petal_length_versicolor + iris[i,3]
  petal_width_versicolor<-petal_width_versicolor + iris[i,4]
}
for(i in 101:150)
{
  sepal_length_virginica<-sepal_length_virginica + iris[i,1]
  sepal_width_virginica<-sepal_width_virginica + iris[i,2]
  petal_length_virginica<-petal_length_virginica + iris[i,3]
  petal_width_virginica<-petal_width_virginica + iris[i,4]
}

sepal_length<-c(sepal_length_setosa/50, sepal_length_versicolor/50, sepal_length_virginica/50)
sepal_width<-c(sepal_width_setosa/50, sepal_width_versicolor/50, sepal_width_virginica/50)
petal_lenth<-c(petal_length_setosa/50, petal_length_versicolor/50, petal_length_virginica/50)
petal_width<-c(petal_width_setosa/50, petal_width_versicolor/50, petal_width_virginica/50)

TABLA_DE_MEDIAS<-data.frame(sepal_length, sepal_width, petal_lenth, petal_width, row.names = c("Setosa", "Versicolor", "Virginica"))

TABLA_DE_MEDIAS


