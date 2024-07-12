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
               modelsummary, gamlr, ROCR,class, readxl, writexl,
               glmnet, janitor, doParallel, rattle, fastDummies, 
               tidymodels, themis, AER, parsnip, randomForest,xgboost,
               ranger, finetune)

#####1. Cargar datos#####

#Se establece el directorio de datos
setwd("D:/Uniandes/OneDrive - Universidad de los andes/Mis tablas de Excel")
#En "datos" se guardan los inputs con los que se entranará el modelo
datos <- read_xlsx("jugadores SOFA.xlsx")
#En "predictions" se carga la base sobre la cual se espera hacer las predicciones
predictions <- read_xlsx("CopaAmerica2.xlsx")

#La liga y la posición se trabajan como factores (variables categóricas)
datos$Liga <- as.factor(datos$Liga)
datos$Posición <- as.factor(datos$Posición)
predictions$Liga <- as.factor(predictions$Liga)
predictions$Posición <- as.factor(predictions$Posición)

#Aquellas observaciones con inputs faltantes son eliminadas
train <- datos%>%drop_na()
predictions <- predictions%>%drop_na()


#####2. Se entrena el modelo de Random Forest con el paquete Ranger#####

#Se grea una grilla utilizada para el tunning de los hiperparámetros
hyper_grid <- expand.grid(mtry       = c(3,4),
                          max_depht  = c(2,3,5,7),
                          sampe_size = c(0.6, 0.8, 1),
                          OOB_RMSE   = 0,
                          num_trees  = c(300, 500, 1000))

#A partir del ciclo se itera en las distintas combinaciones de la grilla para entrenar el modelo
for(i in 1:nrow(hyper_grid)) {
  
  #Se entrena el modelo utilizando los distintos valores agregados en la grilla
  model <- ranger(formula         = Puntaje_daca ~ . - Jugador, #Se utilizan todas las variables para predecir el puntaje del jugador, excepto el nombre de este.
                  data            = train, 
                  num.trees       = hyper_grid$num_trees[i],
                  mtry            = hyper_grid$mtry[i],
                  sample.fraction = hyper_grid$sampe_size[i],
                  max.depth       = hyper_grid$max_depht[i],
                  seed            = 123)
  
  # Se agrega el error de predicción a la grilla antes creada
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}



#####3. Se entrena el modelo definitivo de la seccion 2. y se guardan las predicciones#####

#Primero se corre el modelo de la sección 2.
#Este modelo utiliza todas las variables como predictores y consiste en un Random Forest con el paquete Ranger

#Seleccionar el mejor modelo para predecir los resultados según el error cuadrático medio
#Para esto se ordenan los datos del menor al mayor y se sobreescribe la grilla
hyper_grid <- hyper_grid %>% 
  dplyr::arrange(OOB_RMSE)

#Se visualizan los primeros 5
hyper_grid %>% head(5)

#Con los datos del mejor modelo se entrena el modelo definitivo
model_opt <- ranger(formula         = Puntaje_daca ~ . - Jugador, 
                    data            = train, 
                    num.trees       = hyper_grid$num_trees[1],
                    mtry            = hyper_grid$mtry[1],
                    sample.fraction = hyper_grid$sampe_size[1],
                    max.depth       = hyper_grid$max_depht[1],
                    seed            = 123,
                    importance      = "impurity")

#Se predicen los resultados obtenidos a partir de este modelo y se incorporan al data frame de predicciones
predictions$predicciones_puntaje_rf1 <- round(predict(model_opt, predictions)$predictions,2)


#Se construye una gráfica que revele la importancia de los predictores
#Se guarda el data.frame
importancia <- as.data.frame(importance(model_opt))
#Se crea una columna que tenga el nombre de las variables
importancia$variable <- rownames(importancia)
#Se nombran las columnas
colnames(importancia) <- c("importancia", "variable")

#Se crea la gráfica que representa el feature importance
ggplot(importancia, aes(x=reorder(variable,importancia), y=importancia,fill=importancia))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Importancia de las variables")+
  xlab("")+
  ggtitle("Importancia de las variables Random Forest de clasificación")+
  scale_fill_gradient(low="red", high="blue")+
  theme_classic()+
  theme(text = element_text(size = 12), plot.title = element_text(hjust = 0.5, size = 14))


#####4. Se entrena el modelo de Random Forest con el paquete Ranger, para una segunda versión#####

#En este caso, se omite el precio como predictor, suponiendo que puede estar inflando algunos jugadores

#Se grea una grilla utilizada para el tunning de los hiperparámetros
hyper_grid <- expand.grid(mtry       = c(3,4),
                          max_depht  = c(2,3,5,7),
                          sampe_size = c(0.6, 0.8, 1),
                          OOB_RMSE   = 0,
                          num_trees  = c(300, 500, 1000))

#A partir del ciclo se itera en las distintas combinaciones de la grilla para entrenar el modelo
for(i in 1:nrow(hyper_grid)) {
  
  #Se entrena el modelo utilizando los distintos valores agregados en la grilla
  model <- ranger(formula         = Puntaje_daca ~ . - Jugador - Precio, 
                  data            = train, 
                  num.trees       = hyper_grid$num_trees[i],
                  mtry            = hyper_grid$mtry[i],
                  sample.fraction = hyper_grid$sampe_size[i],
                  max.depth       = hyper_grid$max_depht[i],
                  seed            = 123)
  
  # Se agrega el error de predicción a la grilla antes creada
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}


#####5. Se entrena el modelo definitivo de la seccion 4. y se guardan las predicciones#####

#Primero se corre el modelo de la sección 4.
#Este modelo utiliza todas las variables como predictores, menos el precio y consiste en un Random Forest con el paquete Ranger

#Seleccionar el mejor modelo para predecir los resultados según el error cuadrático medio
#Para esto se ordenan los datos del menor al mayor y se sobreescribe la grilla
hyper_grid <- hyper_grid %>% 
  dplyr::arrange(OOB_RMSE)

#Se visualizan los primeros 5
hyper_grid %>% head(5)

#Con los datos del mejor modelo se entrena el modelo definitivo
model_opt <- ranger(formula         = Puntaje_daca ~ . - Jugador - Precio, 
                    data            = train, 
                    num.trees       = hyper_grid$num_trees[1],
                    mtry            = hyper_grid$mtry[1],
                    sample.fraction = hyper_grid$sampe_size[1],
                    max.depth       = hyper_grid$max_depht[1],
                    seed            = 123,
                    importance      = "impurity")

#Se predicen los resultados obtenidos a partir de este modelo y se incorporan al data frame de predicciones
predictions$predicciones_puntaje_rf2 <- round(predict(model_opt, predictions)$predictions,2)


#Se construye una gráfica que revele la importancia de los predictores
#Se guarda el data.frame
importancia <- as.data.frame(importance(model_opt))
#Se crea una columna que tenga el nombre de las variables
importancia$variable <- rownames(importancia)
#Se nombran las columnas
colnames(importancia) <- c("importancia", "variable")

#Se crea la gráfica que representa el feature importance
ggplot(importancia, aes(x=reorder(variable,importancia), y=importancia,fill=importancia))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Importancia de las variables")+
  xlab("")+
  ggtitle("Importancia de las variables Random Forest de clasificación")+
  scale_fill_gradient(low="red", high="blue")+
  theme_classic()+
  theme(text = element_text(size = 12), plot.title = element_text(hjust = 0.5, size = 14))


#####6. Se entrena el modelo XGBoost que a priori supone ser el más robusto#####

#Se define la grilla de hiperparámetros
grid_default <- expand.grid(nrounds = seq(600, 1000, 50),
                            max_depth = c(5, 7),
                            eta = seq(0.03, 0.1, 0.005),
                            gamma = c(0,1),
                            min_child_weight = c(50, 100),
                            colsample_bytree = c(0.7, 1),
                            subsample = c(0.8, 1))

#Se define una semilla
set.seed(1998)

#Se define 5 folds para el Cv
ctrl <- trainControl(method = "cv", number = 5)

#Se utilizan n-1 núcleos del computador debido a la exigencia de entrenar este modelo
n_cores <- detectCores()
cl <- makePSOCKcluster(n_cores-1)
registerDoParallel(cl)


# Se crea una barra de progreso para hacer seguimiento del avance
n_iter <- 10 # Number of iterations of the loop

pb <- txtProgressBar(min = 0,      # Valor mínimo barra de progreso
                     max = n_iter, # Valor máximo barra de progreso
                     style = 3,    # Estilo de la barra de progreso (1, 2 o 3)
                     width = 50,   # Ancho de la barra de progreso
                     char = "=")   # Caracter utilizado para el diseño de la bara

for(i in 1:n_iter) {
  
  #---------------------
  # Acá se inserta el código que se corre para conocer su progreso con la barra
  #---------------------
  
  xgboost <- train(Puntaje_daca ~ .,
                  data = train%>%select(-Jugador),
                  method = "xgbTree",
                  trControl = ctrl,
                  metric = "RMSE",
                  tuneGrid = grid_default,
                  preProcess = c("center", "scale"),
                  verbose = FALSE,
                  verbosity = 0)
  
  #---------------------
  
  # Configurar la barra de porbreso al nuevo estado actual
  setTxtProgressBar(pb, i)
}

close(pb) # Cerrar la conección con la barra de porgreso


#Frenar uso de los núcleos del computador
stopCluster(cl)

#Ver los resultados del modelo
xgboost

#Se guarda el modelo para ser utilizado en futuras ocasiones
write_rds(xgboost, "modelo_xgboost.rds")


##### 7. Se corren las predicciones del modelo XG_BOOST#####
#Se carga el modelo
mod_xgboost <- read_rds("modelo_xgboost.rds")

#Se guardan las predicciones de este modelo
predictions$predicciones_puntaje_XG <- round(predict(mod_xgboost, predictions),2)

#Se crea la matriz de importancia de las variables
variableimp <- varImp(mod_xgboost)
variableimp <- as.data.frame(variableimp[1])
variableimp <- rownames_to_column(variableimp, "Variable")

#Se grafica la importancia de las variables en el modelo XGBoost
ggplot(variableimp, aes(x=reorder(Variable,Overall), y=Overall,fill=Overall))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Importancia de las variables")+
  xlab("")+
  ggtitle("Importancia de las variables Random Forest de clasificación")+
  scale_fill_gradient(low="red", high="blue")+
  theme_classic()+
  theme(text = element_text(size = 12), plot.title = element_text(hjust = 0.5, size = 14))



#####8. Se juntan todas las predicciones en un solo puntaje final con el promedio#####

#Se crea el puntaje promedio
predictions <- predictions%>%mutate(pred_final = round((predicciones_puntaje_rf1 +
                                                          predicciones_puntaje_rf2+
                                                          predicciones_puntaje_XG)/3,2))

#Ver la tabla de predicciones unicamente con el nombre del jugador y los 4 puntajes creados
View(predictions%>%dplyr::select(Jugador, predicciones_puntaje_rf1, predicciones_puntaje_rf2,
                                 predicciones_puntaje_XG, pred_final, Equipo))


#Salvar la tabla de resultados de los puntajes predichos
write_xlsx(predictions, "Predicciones.xlsx")