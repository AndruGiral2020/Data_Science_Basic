####1. Carga de datos####
df1 = read.csv('orangeec.csv')
df2 = read.csv('mtcars.csv')

options(scipen = 999)


####2. Función str(estructura de dataset)####
str(mtcars)
class(mtcars$vs)
class(mtcars$am)

####3. Convertir variable numérica a booleana####
mtcars$vs = as.logical(mtcars$vs)
mtcars$am = as.logical(mtcars$am)

#Practica
str(df1)
str(df2)
df2$vs = as.logical(df2$vs)
df2$am = as.logical(df2$am)

####4. Resumen de los datos####
summary(df1)
summary(df2)

####5. Conversion de libras a kilos de la variable peso####
wt <- (df2$wt*1000)/2
wt

####6. Transformacion en el dataset trabajado####
df2.new <- transform(df2,wt=wt*1000/2)
df2.new
summary(df2.new)

####7. Manejo de vectores####
t_platzi <- c(25,5,10,15,10)
t_lecturas <- c(30,10,5,10,15)
t_aprendizaje <- t_platzi + t_lecturas

d_aprendizaje <- c("Lunes","Martes","Miercoles","Jueves","Viernes")
d_20add <- c(TRUE, FALSE, FALSE, TRUE, TRUE)

t_tiempoplatzi <- sum(t_platzi)
t_tiempolecturas <- sum(t_lecturas)
t_tadicional <- t_tiempoplatzi + t_tiempolecturas

####8. Manejo de matrices####
t_matriz <- matrix(c(t_platzi,t_lecturas), nrow = 2,byrow = TRUE)
dias <- c("Lunes","Martes","Miercoles","Jueves","Viernes")
tiempo <- c("Tiempo platzi", "Tiempo lecturas")
colnames(t_matriz) <- dias
rownames(t_matriz) <- tiempo
colSums(t_matriz)

####8.1 Desafio añadiendo columnas y filas####
f_matriz <- rbind(t_matriz, universidad = c(10,15,30,5,0))
colSums(f_matriz)
f_matriz <- cbind(f_matriz, sabado = c(40,20,50))
colSums(f_matriz)

#Ubicando elementos
f_matriz[1,5]

####9. Manejo de operadores####
mtcars[mtcars$cyl<6,] #La , es para buscar en todas las obs.
df1[df1$GDP.PC>=15000,]
df1[df1$Creat.Ind...GDP<=2,]

####10. Utilizando la funcion subset####
df.new <- subset(df1, Internet.penetration...population > 80
                 & Education.invest...GDP >= 4.5)

df.new <- subset(df1, Internet.penetration...population > 80
                 & Education.invest...GDP >= 4.5, 
                 select = Creat.Ind...GDP)

####11. Renombrar columna de dataset####
rename(df1, c("Creat.Ind...GDP" = "AporteEcNja"))

####12. Trabajando con factores variables categoricas####
n_curso <- c("Básico","Intermedio","Avanzado")
head(df2)
head(df1)
tail(df1)
tail(df2)

#13. Utilizando la libreria dplyr y estructuras de datos####
glimpse(df1)
my_vector <- 1:8
my_matrix <- matrix(1:9, ncol = 3)
my_df <- df2[1:4,]
my_list <- list(my_vector,my_matrix,my_df)
my_list[[2]]



