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


####13. Utilizando la libreria dplyr y estructuras de datos####
glimpse(df1)
my_vector <- 1:8
my_matrix <- matrix(1:9, ncol = 3)
my_df <- df2[1:4,]
my_list <- list(my_vector,my_matrix,my_df)
my_list[[2]]


####EDA Scatter plot mtcars####
plot(mtcars$mpg ~ mtcars$hp, xlab = "Caballos de fuerza",
     ylab = "Millas por galon", main = "Relacion cilindros y millas por galon")

plot(df2$mpg ~ df2$cyl, xlab = "Caballos de fuerza",
     ylab = "Millas por galon", main = "Relacion cilindros y caballos de fuerza")


####EDA Scatter plot economia naranja####
plot(df1$Unemployment ~ df1$Education.invest...GDP, 
     xlab = "Inversión educacion % PIB",
     ylab = "Desempleo",
     main = "Relacion educacion y desempleo")

plot(df1$GDP.PC ~ df1$Creat.Ind...GDP, 
     xlab = "Aporte EN al PIB %",
     ylab = "PIB percápita",
     main = "Relacion EN y PIB percapita")


####Histogramas mtcars qplot####
qplot(mtcars$hp, geom = "histogram",
      xlab = "Caballos de fuerza",
      main = "Vehiculos segun caballos de fuerza")

ggplot(mtcars, aes(x=hp))+ geom_histogram()+
  labs(x = "Caballos de fuerza", y = "Cantidad de vehiculos",
       title = "Caballos de fuerza en vehiculos")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), panel.grid = element_blank(),
        panel.grid.minor = element_blank())

ggplot(mtcars, aes(x=hp))+ geom_histogram(binwidth = 30)+
  labs(x = "Caballos de fuerza", y = "Cantidad de vehiculos",
       title = "Caballos de fuerza en vehiculos")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), panel.grid = element_blank(),
        panel.grid.minor = element_blank())

ggplot()+geom_histogram(data = mtcars,
                        aes(x=hp), fill="darkblue", color="black",
                        binwidth = 20)+
  labs(x = "Caballos de fuerza", y = "Cantidad de vehiculos",
       title = "Caballos de fuerza en vehiculos")+
  xlim(c(80,280))+ theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.minor = element_blank())


####Histograma con ggplot EN####
ggplot()+geom_histogram(data = df1,
                        aes(x=GDP.PC), fill="darkgreen", color="black",
                        binwidth = 2000)+
  labs(x = "PIB percápita", y = "Cantidad de países",
       title = "PIB percápita en paises latam")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.minor = element_blank())

ggplot()+geom_histogram(data = df1,
                        aes(x=Creat.Ind...GDP), fill="darkgreen", color="black",
                        binwidth = 1)+
  labs(x = "Aporte EN al PIB (%)", y = "Cantidad de países",
       title = "Contribución EN al PIB en países LATAM")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.minor = element_blank())

ggplot()+geom_histogram(data = df1,
                        aes(x=Internet.penetration...population), 
                        fill="darkgreen", color="black",
                        binwidth = 5)+
  labs(x = "Penetración internet como (%) población", 
       y = "Cantidad de países",
       title = "Penetración de internet en países LATAM")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = seq(40,100,5))


####Box plot mtcars####
boxplot(mtcars$hp, ylab= "Caballos de fuerza",
        main="Caballos de fuerza en vehículos")

ggplot(mtcars, aes(x=as.factor(cyl), y=hp, fill=cyl))+
  geom_boxplot(alpha=0.5)+
  labs(x="Cilindros", y="Caballos de fuerza",
       title = "Caballos de fuerza según cilindros")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.minor = element_blank())

ggplot(mtcars,aes(x=am, y=mpg, fill=am))+
  geom_boxplot()+
  labs(x="Tipo de caja", y="Millas por galon",
       title= "Millas por galon según tipo de caja" )+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.minor = element_blank())

mtcars$am <- factor(mtcars$am, levels =c(TRUE, FALSE),
                    labels = c("Manual", "Automático"))


####Boxplot EN PIB percapita####
install.packages("dyplr")
library(dplyr)
economy <- mean(df1$GDP.PC)
df1 <- df1 %>% mutate(strong_economy = ifelse(GDP.PC<economy,
                                              "Por debajo del promedio PIB percapita",
                                              "Sobre el promedio PIB percapita"))

ggplot(df1, aes(x=strong_economy, y=Creat.Ind...GDP,
                fill=strong_economy))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de pais", y="Aporte economía naranja al PIB",
       title="Aporte economia naranja en PIB paises LATAM con alto y bajo PIB percapita")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.minor = element_blank())


####Boxplto EN internet####
ggplot(df1, aes(x=strong_economy, y=Internet.penetration...population,
                fill=strong_economy))+
  geom_boxplot(alpha=0.4)+
  labs(x="Tipo de pais", y="Penetración de internet %",
       title="Penetración de internet en paises con alto y bajo PIB percapita")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.minor = element_blank())


####Scatterplot con dos variables####
ggplot(mtcars, aes(hp,mpg))+
  geom_point()+
  labs(x="Caballos de fuerza", y="Millas por galon",
       title="Relacion caballos de fuerza y millas por galon")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.minor = element_blank())

ggplot(mtcars, aes(wt,hp))+
  geom_point()+
  labs(x="Peso", y="Potencia",
       title="Relacion peso y potencia")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.grid.minor = element_blank())

ggplot(mtcars, aes(hp,qsec))+
  geom_point(aes(color=am, size=cyl))+
  labs(x="Caballos de fuerza", y="Tiempo cuarto de milla",
       title="Caballos velocidad segun cilindraje y tipo de caja")
  
  






