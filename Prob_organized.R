#Limpiar ambiente
rm(list = ls())
#Limpiar consola
cat("\014")

#Importar paquetes y cargar librerías
require(pacman)
if (!require(pacman)) install.packages('pacman', repos = 'https://cran.rstudio.com')
pacman::p_load(tidyverse, rvest, data.table, dplyr, skimr, caret, rio, 
       vtable, stargazer, ggplot2, boot, MLmetrics, lfe,
       tidyverse, fabricatr, stargazer, Hmisc, writexl, viridis, here,
       modelsummary, gamlr, ROCR, class, readxl, writexl,
       glmnet, janitor, doParallel, rattle, fastDummies, tidymodels,
       themis, AER, randomForest,xgboost, ranger,
       foreign, nnet, reshape2)


#Establecer directorio para cargar los datos
setwd("D:/Uniandes/OneDrive - Universidad de los andes/Mis tablas de Excel")

#Se cargan los inputs
datos <- read_xlsx("Probabilidades_victoria.xlsx")
datos <- datos%>%drop_na() #Eliminar observaciones con NA para entrenar el modelo

#En "forecast" se conservan todos los datos para guardar aquí las predicciones
forecast <- read_xlsx("Probabilidades_victoria.xlsx")
#Solo se guardan columnas seleccionadas
forecast <- forecast %>% select(Partido, Dif_pts, Diff_valor, Fav_localia, Equipo_Mayor, Equipo_Menor, Dif_rend)

#Se crean modelos de logit multinomial
#El primer modelo unicamente utiliza la diferencia de puntos de la nómina titular
test <- multinom(Resultado ~ Dif_pts , data = datos)
#El segundo modelo añande la diferencia de rendimiento medida en los puntos obtenidos en los últimos 5 partidos
test2 <- multinom(Resultado ~ Dif_pts + Dif_rend , data = datos)
#El tercer modelo tiene el puntaje promedio de la titular y el valor promedio de las nóminas de ambos equipos
test3 <- multinom(Resultado ~ Dif_pts + Diff_valor , data = datos)

list_tests <- list(test$fitted.values, test2$fitted.values, test3$fitted.values)

for (objeto in list_tests) {
  #Anális de los modelos (se hace solo para el primero, pero se puede cambiar)
  pp <- objeto
  #Se unen los datos y las predicciones de los resultados de partidos ya jugados
  predictions <- bind_cols(datos, pp)
  
  #Se establece cuál es el resultado con mayor probabilidad 
  #Luego se revisa si el resultado con mayor probabilidad es el que ocurrió
  predictions <- predictions %>% mutate(Prediccion = case_when(Gana_fav...17 > Empate...16 & Gana_fav...17 > predictions$Pierde_fav...18 ~ "Gana_fav",
                                                               Gana_fav...17 < Empate...16 & Empate...16 > predictions$Pierde_fav...18 ~ "Empate",
                                                               TRUE ~ "Pierde_fav"),
                                        pasa_fav = Gana_fav...17 + 0.5*Empate...16,
                                        Correcta = ifelse(Prediccion == Resultado, 1, 0))
  
  #Se hace una tabla con los resultados que ocurrieron vs los predichos
  print(table(predictions$Resultado, predictions$Prediccion))
  #Se calcula el porcentaje de predicciones totalmente correctas
  print(predictions%>%count(Correcta)%>%mutate(perc = 100*n/sum(n)))
}


#Ahora se hacen las predicciones para los 3 modelos sobre los partidos que aún no se han jugado
#Solo muestro los últimos 4 partidos de la lista para facilitar la visualización
#Modelo 1
bind_cols(forecast[(nrow(forecast)-3):nrow(forecast),], predict(test, newdata = forecast[(nrow(forecast)-3):nrow(forecast),], "probs"))
#Modelo 2
bind_cols(forecast[(nrow(forecast)-3):nrow(forecast),], predict(test2, newdata = forecast[(nrow(forecast)-3):nrow(forecast),], "probs"))
#Modelo 3
bind_cols(forecast[(nrow(forecast)-3):nrow(forecast),], predict(test3, newdata = forecast[(nrow(forecast)-3):nrow(forecast),], "probs"))