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
#### EDA con gglplot, plotly, scarttplot####

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

ggplot(df1, aes(Internet.penetration...population,Creat.Ind...GDP))+
  geom_point(aes(color=factor(strong_economy), size=GDP.Growth..))+
  labs(x="Penetración Internet", y="Aporte economia naranja al PIB",
       title="Internet y aporte EN segun economia y crecimiento PIB")

ggplot(df1, aes(Education.invest...GDP,Unemployment))+
  geom_point(aes(color=factor(strong_economy), size=X..pop.below.poverty.line))+
  labs(x="Inversión en educación % PIB", y="Desempleo",
       title="Inversion en Educación y Desempleo según factor de Economía y población por debajo de la linea de pobreza")

install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")
library(plotly)

my_graph <- ggplot(df1, aes(Internet.penetration...population,
                            Creat.Ind...GDP,rownames(df1)))+
  geom_point()+
  labs(x="Penetración Internet", y="Aporte economia naranja al PIB",
       title="Internet y aporte EN segun economia y crecimiento PIB")
p = ggplotly(my_graph)
p

####Las estadistica de los datos####

#### EDA con libreria dplyr y medidas de tendencia central####
#Utilizandao librery dplyr and pairs#

pairs(mtcars[,2:6])

newdata <- subset(mtcars, select = c(2,7:8,11,12))
pairs(newdata)

pairs(mtcars[,-c(1,3,4,5,6,9,10)])

#Utilizando librery dplyr and filtter#

install.packages("dplyr")
library(dplyr)
attach(mtcars)
Eficient <- filter(mtcars, mpg >= 30)
Eficient
pairs(Eficient[,2:6])

#utilizando librery stringr#
install.packages("stringr")
library(stringr)

merc <- mtcars %>%
  filter(str_detect(model,"Merc"))
merc

cor(mtcars[,2:6])
cor(newdata)

View(df1)
pairs(df1[,2:6])
pairs(df1[,5:10])

newdata1 <- subset(df1, select = c(5,6,10,11,12,13))
newdata1
pairs(newdata1)


#Correlaciones Economia naranja#

cor(df1[,2:6])
cor(df1[,2:6], use = "complete.obs")
cor(df1[,5:10], use = "complete.obs")
cor(newdata1, use = "complete.obs")


#Medidad de tendencia central#

summary(mtcars)
sd(mtcars$mpg)
mean(mtcars$mpg)
prom <- mean(mtcars$mpg)
desv <- sd(mtcars$mpg)
cv <- (desv/prom)*100
cv

desvdf1 <- sd(df1$Internet.penetration...population)
promdf1 <- mean(df1$Internet.penetration...population)
cvdf1 <- (desvdf1/promdf1)*100
cvdf1
summary(df1)

mean(df1$Creat.Ind...GDP)
mean(df1$Creat.Ind...GDP, na.rm = TRUE)
sd(df1$Creat.Ind...GDP)
sd(df1$Creat.Ind...GDP, na.rm = TRUE)
prom1 <- mean(df1$Creat.Ind...GDP, na.rm = TRUE)
sd1 <- sd(df1$Creat.Ind...GDP, na.rm = TRUE)
cv1 <- (sd1/prom1)*100


####Ajustando datos para mejorar las visualizaciones####

eficientes <- mean(mtcars$mpg)
eficientes
mtcars <- mtcars %>%
  mutate(mas_eficientes = ifelse(mpg < eficientes,
                               "Bajo promedio", "en o Sobre promedio"))

mas_veloces <- mtcars[mtcars$qsec<16,]
mas_veloces

mtcars <- mtcars  %>%
  mutate(vel_qmilla = ifelse(qsec < 16,
                               "Menos de 16 seg", "Mas de 16 seg"))

mtcars <- mtcars  %>%
  mutate(peso_kilos = (wt/2)*1000)

mtcars <- mtcars  %>%
  mutate(peso=ifelse(peso_kilos <= 1500,
                     "Livianos", "Pesados"))

df1 <- df1 %>%
  mutate(creci_GDP = ifelse(GDP.Growth.. >= 2.5,
                            "2.5% o más", "Menos de 2.5%"))
df1 <- df1 %>%
  mutate(mas_naranja = ifelse(Creat.Ind...GDP >= 2.5,
                            "Más naranjas", "Menos naranjas"))

#Ranking#

df1 %>%
  arrange(desc(Creat.Ind...GDP))

top_naranjas <- df1 %>%
  filter(Country %in% c("Mexico", "Panama", "Paraguay", "Argentina",
                        "Colombia", "Brazil")) 

top_naranjas %>%
  arrange(desc(Creat.Ind...GDP))

mtcars %>%
  arrange(desc(peso_kilos))

mas_pesados <- mtcars %>%
  filter(model %in% c("Lincoln Continental","Chrysler Imperial",
                     "Cadillac Fleetwood","Merc 450SE"))
mas_pesados

ggplot(mas_pesados, aes(x=hp,y=mpg))+
  geom_point()+
  facet_wrap(~model)


ggplot(mtcars, aes(x=cyl, y=mpg, size=peso))+
  geom_point()+
  facet_wrap(~ am)

##Reto##
ggplot(mtcars, aes(x=cyl, y=mpg, size=peso_kilos))+
  geom_point()+
  facet_wrap(~ am)


ggplot(top_naranjas, aes(x = Internet.penetration...population,
                        y = Services...GDP, size = GDP.PC))+
  geom_point()+
  facet_wrap(~Country)


ggplot(top_naranjas, aes(x=Education.invest...GDP,
                        y=Creat.Ind...GDP, size=Unemployment))+
  geom_point()+
  facet_wrap(~Country)

myColors <- brewer.pal(9,"Reds")

ggplot(top_naranjas, aes(x=Internet.penetration...population,
                        y=GDP.PC, fill=Creat.Ind...GDP))+
  geom_tile()+
  facet_wrap(~Country)+
  scale_fill_gradientn(colors=myColors)


####Cierre####

cajas <- c(1,2,3,4,5,6,7,8)
tiempo <- c(10,9,8,5,8,6,3,1,8,1)
plot(tiempo-cajas)
plot(df1$Services...GDP ~ df1$Education.invest...GDP)
plot(mtcars$mpg~mtcars$am)





