df1 = read.csv('orangeec.csv')
df2 = read.csv('mtcars.csv')

options(scipen = 999)


#Función str(estructura de dataset)
str(mtcars)
class(mtcars$vs)
class(mtcars$am)

#Convertir variable numérica a booleana
mtcars$vs = as.logical(mtcars$vs)
mtcars$am = as.logical(mtcars$am)

#Practica
str(df1)
str(df2)
df2$vs = as.logical(df2$vs)
df2$am = as.logical(df2$am)

#Resumen de los datos
summary(df1)
summary(df2)

#Conversion de libras a kilos de la variable peso
wt <- (df2$wt*1000)/2
wt

#Transformacion en el dataset trabajado
df2.new <- transform(df2,wt=wt*1000/2)
df2.new
summary(df2.new)


