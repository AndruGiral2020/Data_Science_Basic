library('ggplot2')
library('tidyverse')
library('caret')
library('reshape2')
library('randomForest')
library('Hmisc')

setwd('/home/david/Documents/crehana/R/ejemplo')

# Importamos los datos

df_train = read.csv('lluvia_train.csv')
df_test = read.csv('lluvia_test.csv')



#--------------------------------------#
#-----  Análisis exploratorio  --------#
#--------------------------------------#

# Número de datos
dim(df_train)

# Nombres de las columnas
names(df_train)
names(df_test)

# Primeras filas del data.frame
head(df_train)

#Practica con metodos de filas y columnas
df_train$evaporacion[5]
df_train[3,5]
df_train[3,]
df_train[,5]
df_train$porcentaje = 0.1*df_train$max_temp
df_test$porcentaje = 0.1*df_test$max_temp

#Opcion filter
df_train %>% filter(max_temp>5)
df_train %>% filter(max_temp>5, lluvia_hoy==1)
df_train %>% arrange(horas_sol)
df_train %>% mutate(porcentaje1 = 0.1*max_temp)
df_test %>% mutate(porcentaje1 = 0.1*max_temp)

# Función describe() del paquete Hmisc
describe(df_train)

# Ordenar data.frame de acuerdo a una variable
df_train %>% arrange(temperatura)

# Agrupamos datos de acuerdo a variable lluvia_hoy y sacamos promedio de horas_sol y máximo de evaporación
df_train %>% group_by(lluvia_hoy) %>% summarise(media_horas_sol = mean(horas_sol), max_evaporacion = max(evaporacion))
df_train %>% group_by(lluvia_hoy) %>% summarise(promedio = mean(max_temp), minimo = min(horas_sol))

#--------------------------------------#
#-----------   Gráficos   -------------#
#--------------------------------------#

# Gráfico 1: dispersión sin discriminar por variable lluvia_manana
ggplot(data = df_train) + 
  geom_point(aes(x = horas_sol, y = temperatura)) + 
  theme_bw() +
  ggtitle("Horas de sol vs temperatura") + 
  xlab("Horas de sol") + ylab("Temperatura (°C)") 

# Gráfico 1: dispersión discriminando con colores por variable lluvia_manana
ggplot(data = df_train) + 
  geom_point(aes(x = horas_sol, y = temperatura, color = factor(lluvia_manana))) + 
  theme_bw() +
  ggtitle("Horas de sol vs temperatura") + 
  xlab("Horas de sol") + ylab("Temperatura (°C)") +
  labs(color='Lluvia mañana') 

#Practica ggplot2 con las demas variables
ggplot(data = df_train) + 
  geom_point(aes(x = lluvia_hoy, y = nubes, color = factor(lluvia_manana))) + 
  theme_bw() +
  ggtitle("Lluvia hoy vs nubes") + 
  xlab("Lluvia hoy") + ylab("nubes") +
  labs(color='Lluvia mañana') 


# Gráfico 2: heat map
cor(df_train)
ggplot(data = melt(cor(df_train))) + 
  geom_tile(aes(x = Var1, y = Var2, fill = value)) + 
  theme_bw() +
  ggtitle("Gráfico de correlaciones") + 
  xlab("Variables") + ylab("Variables") +
  labs(fill='Correlación') 


# Gráfico 3: box plot 1
ggplot(data = df_train) + 
  geom_boxplot(aes(x=lluvia_manana, y = horas_sol, fill = factor(lluvia_manana))) + 
  theme_bw() +
  ggtitle("Horas de sol vs lluvia mañana") + 
  xlab("Lluvia mañana") + ylab("Horas de sol") +
  labs(fill='Lluvia mañana') 


# Gráfico 3: box plot 2
ggplot(data = df_train) + 
  geom_boxplot(aes(x=lluvia_manana, y = nubes, fill = factor(lluvia_manana))) + 
  theme_bw() +
  ggtitle("Nubes vs lluvia mañana") + 
  xlab("Lluvia mañana") + ylab("Nubes") +
  labs(fill='Lluvia mañana') 


# Gráfico 4: histograma
ggplot() + 
  geom_histogram(data = df_train, aes(x = horas_sol)) + 
  theme_bw() +
  ggtitle("Histograma horas sol") + 
  xlab("Horas sol") + ylab("# datos")


#--------------------------------------#
#----   Modelos de clasificación  -----#
#--------------------------------------#

#-----------------------------------------------------#
#---- Regresión lineal univariada - entrenamiento ----#
#-----------------------------------------------------#

model = lm(data = df_train, lluvia_manana ~ horas_sol)
summary(model)

# Predecir valores en conjunto de validación
predicted_values = predict(object = model, newdata = df_test)
predicted_values = ifelse(predicted_values > 0.5, 1, 0)
df_test$predicted = predicted_values

# Métrica #1: Matriz de confusión
confusionMatrix(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted))

# Métrica #2: Recall
recall(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted), relevant = '1')

# Métrica #3: Precisión
precision(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted), relevant = '1')

# Métrica #4: F1-score
F_meas(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted), relevant = '1')



#-------------------------------------------------------#
#---- Regresión lineal multivariada - entrenamiento ----#
#-------------------------------------------------------#

model = lm(data = df_train, lluvia_manana ~ min_temp + max_temp + evaporacion + lluvia_total + horas_sol + nubes + temperatura + lluvia_hoy)
summary(model)

# Predecir valores en conjunto de validación
predicted_values = predict(object = model, newdata = df_test)
predicted_values = ifelse(predicted_values > 0.5, 1, 0)
df_test$predicted = predicted_values

# Métrica #1: Matriz de confusión
confusionMatrix(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted))

# Métrica #2: Recall
recall(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted), relevant = '1')

# Métrica #3: Precisión
precision(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted), relevant = '1')

# Métrica #4: F1-score
F_meas(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted), relevant = '1')




#-------------------------------------------------------#
#--------- Regresión logística - entrenamiento ---------#
#-------------------------------------------------------#

mod = glm(data = df_train, lluvia_manana ~ min_temp + max_temp + evaporacion + lluvia_total + horas_sol + nubes + temperatura + lluvia_hoy, family = binomial)
summary(mod)

# Predecir valores en conjunto de validación
df_test$predicted = predict(mod, newdata = df_test,type='response')
df_test$predicted = ifelse(df_test$predicted > 0.5, 1, 0)

# Métrica #1: Matriz de confusión
confusionMatrix(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted))

# Métrica #2: Recall
recall(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted), relevant = '1')

# Métrica #3: Precisión
precision(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted), relevant = '1')

# Métrica #4: F1-score
F_meas(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted), relevant = '1')




#--------------------------------------------------#
#--------- Random forests - entrenamiento ---------#
#--------------------------------------------------#

mod = randomForest(data = df_train, as.factor(lluvia_manana) ~ .)
summary(mod)

# Predecir valores en conjunto de validación
df_test$predicted = predict(mod, newdata = df_test)

# Métrica #1: Matriz de confusión
confusionMatrix(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted))

# Métrica #2: Recall
recall(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted), relevant = '1')

# Métrica #3: Precisión
precision(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted), relevant = '1')

# Métrica #4: F1-score
F_meas(as.factor(df_test$lluvia_manana), as.factor(df_test$predicted), relevant = '1')

# Importancia de las variables
importance(mod)

