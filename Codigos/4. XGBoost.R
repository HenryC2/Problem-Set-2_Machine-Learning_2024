# --------------------
# XGBoost ------------

setwd("C:/Users/Steven Ramirez/Downloads/Problem-Set-2_Machine-Learning_2024/Problem-Set-2_Machine-Learning_2024")
getwd()

source("Codigos/0. Script_Base.R")

Train <- import(file = "Base/Train_hogares_final.rds")
Test <- import(file = "Base/Test_hogares_final.rds")

setwd("C:/Users/Steven Ramirez/Downloads/Problem-Set-2_Machine-Learning_2024/Problem-Set-2_Machine-Learning_2024")
getwd()

# 1. Filtro de variables -------------------------------------------------------

Filtro_Variables_Train =  function(data,...){
  data <- data %>% 
    
    #modificar variables
    select(-id,-Li,-Lp,-Fex_c,-Fex_dpto,-Valor_Cuota,-Ln_Cuota,-Ln_Pago_arrien,-Departamento)
}

Filtro_Variables_Test =  function(data,...){
  data <- data %>% 
    
    #modificar variables
    select(-Li,-Lp,-Fex_c,-Fex_dpto,-Valor_Cuota,-Ln_Cuota,-Ln_Pago_arrien,-Departamento)
}

Train = Filtro_Variables_Train(Train)
Test = Filtro_Variables_Test(Test)

# 2. Re-balanceo ---------------------------------------------------------------

table(Train$Pobre)
cat("La tasa de pobrza es", Train%>%select(Pobre)%>% filter(Pobre=="Yes")%>% nrow()/ nrow(Train)*100,"%")

# Debido a que la tasa de pobreza es 20,01%, se debe re balancear la muestra 
# para mejorar el proceso de estimaci?n. Para ello, se utilizar? el m?todo 
# "synthetic minority over-sampling technique (SMOTE)" que hacia un muestro hacia 
# arriba de la clase minoritaria (pobres) y hacia abajo de la clase mayoritaria (no pobres)

# 2.1 Arreglos de las variales categoricas en base train -----------------------

Dummys <- subset(Train, select = c(Dominio,tipo_vivienda,maxEducLevel,Head_EducLevel,
                                   Head_Oficio,Head_Ocupacion))

# Convertir las variables categ?ricas a factores
Dummys$Dominio <- as.factor(Dummys$Dominio)
Dummys$tipo_vivienda <- as.factor(Dummys$tipo_vivienda)
Dummys$maxEducLevel <- as.factor(Dummys$maxEducLevel)
Dummys$Head_EducLevel <- as.factor(Dummys$Head_EducLevel)
Dummys$Head_Oficio <- as.factor(Dummys$Head_Oficio)
Dummys$Head_Ocupacion <- as.factor(Dummys$Head_Ocupacion)

Dummys <- model.matrix(~ . - 1, data = Dummys)  # Eliminar el intercepto

# Union con la base original
Train <- cbind(subset(Train, select = -c(Dominio,tipo_vivienda,maxEducLevel,Head_EducLevel,
                                         Head_Oficio,Head_Ocupacion)),Dummys)

# 2.2 Arreglos de las variales categoricas en base test -----------------------

Dummys <- subset(Test, select = c(Dominio,tipo_vivienda,maxEducLevel,Head_EducLevel,
                                  Head_Oficio,Head_Ocupacion))

# Convertir las variables categ?ricas a factores
Dummys$Dominio <- as.factor(Dummys$Dominio)
Dummys$tipo_vivienda <- as.factor(Dummys$tipo_vivienda)
Dummys$maxEducLevel <- as.factor(Dummys$maxEducLevel)
Dummys$Head_EducLevel <- as.factor(Dummys$Head_EducLevel)
Dummys$Head_Oficio <- as.factor(Dummys$Head_Oficio)
Dummys$Head_Ocupacion <- as.factor(Dummys$Head_Ocupacion)

Dummys <- model.matrix(~ . - 1, data = Dummys)  # Eliminar el intercepto

# Union con la base original
Test <- cbind(subset(Test, select = -c(Dominio,tipo_vivienda,maxEducLevel,Head_EducLevel,
                                       Head_Oficio,Head_Ocupacion)),Dummys)

# 2.3 Particion de muestra Train en Train_2 y Test_2. 
# Esto se hace para poder evaluar mejores modelos fueras de muestra analizando
# directamente el F1-score y llevar el mejor de todos a la plataforma. 

set.seed(1536) # Para reproducibilidad

Train_2_indices <- as.integer(createDataPartition(Train$Pobre, p = 0.85, list = FALSE))
Train_2 <- Train[Train_2_indices, ]
Test_2 <- Train[-Train_2_indices, ]
prop.table(table(Train_2$Pobre))
prop.table(table(Test_2$Pobre))

# 2.4  Re balanceo con SMOTE  --------------------------------------------------

# 2.4.1 Re balanceo usando el Train original 
Predictores <- Train %>%
  select(-Pobre) %>%
  as.data.frame()  # Asegurate de que sea un dataframe

SMOTE <- SMOTE(X = Predictores,target = Train$Pobre,K=5)
Train_SMOTE <- SMOTE$data

setwd("C:/Users/Steven Ramirez/Downloads/Problem-Set-2_Machine-Learning_2024/Problem-Set-2_Machine-Learning_2024/Base/Base_Elastic_Net")
# export(Train_SMOTE, "Train_SMOTE.rds")
Train_SMOTE = import(file = "Train_SMOTE.rds")

# 2.4.2 Re balanceo usando el Train_2 
Predictores_2 <- Train_2 %>%
  select(-Pobre) %>%
  as.data.frame()  # Aseg?rate de que sea un dataframe

SMOTE_2 <- SMOTE(X = Predictores_2,target = Train_2$Pobre,K=10)
Train_2_SMOTE <- SMOTE_2$data

#export(Train_2_SMOTE, "Train_2_SMOTE.rds")
Train_2_SMOTE = import(file = "Train_2_SMOTE.rds")

# P2 ----------------------

setwd("C:/Users/Steven Ramirez/Downloads/Problem-Set-2_Machine-Learning_2024/Problem-Set-2_Machine-Learning_2024")
Test <- import(file = "Base/Test_hogares_final.rds")

# Cargar test_2
set.seed(1536) # Para reproducibilidad
Train_2_indices <- as.integer(createDataPartition(Train$Pobre, p = 0.85, list = FALSE))
Train_2 <- Train[Train_2_indices, ]
Test_2 <- Train[-Train_2_indices, ]
prop.table(table(Train_2$Pobre))
prop.table(table(Test_2$Pobre))

# Convertir las variables categ?ricas a factores en Test
Dummys <- subset(Test, select = c(Dominio,tipo_vivienda,maxEducLevel,Head_EducLevel,
                                  Head_Oficio,Head_Ocupacion))

Dummys$Dominio <- as.factor(Dummys$Dominio)
Dummys$tipo_vivienda <- as.factor(Dummys$tipo_vivienda)
Dummys$maxEducLevel <- as.factor(Dummys$maxEducLevel)
Dummys$Head_EducLevel <- as.factor(Dummys$Head_EducLevel)
Dummys$Head_Oficio <- as.factor(Dummys$Head_Oficio)
Dummys$Head_Ocupacion <- as.factor(Dummys$Head_Ocupacion)

Dummys <- model.matrix(~ . - 1, data = Dummys)  # Eliminar el intercepto

# Union con la base original
Test <- cbind(subset(Test, select = -c(Dominio,tipo_vivienda,maxEducLevel,Head_EducLevel,
                                       Head_Oficio,Head_Ocupacion)),Dummys)

# Convertir las variables categ?ricas a factores en test_2
Dummys <- subset(Test_2, select = c(Dominio,tipo_vivienda,maxEducLevel,Head_EducLevel,
                                    Head_Oficio,Head_Ocupacion))

Dummys$Dominio <- as.factor(Dummys$Dominio)
Dummys$tipo_vivienda <- as.factor(Dummys$tipo_vivienda)
Dummys$maxEducLevel <- as.factor(Dummys$maxEducLevel)
Dummys$Head_EducLevel <- as.factor(Dummys$Head_EducLevel)
Dummys$Head_Oficio <- as.factor(Dummys$Head_Oficio)
Dummys$Head_Ocupacion <- as.factor(Dummys$Head_Ocupacion)

Dummys <- model.matrix(~ . - 1, data = Dummys)  # Eliminar el intercepto

# Union con la base original
Test_2 <- cbind(subset(Test_2, select = -c(Dominio,tipo_vivienda,maxEducLevel,Head_EducLevel,
                                           Head_Oficio,Head_Ocupacion)),Dummys)


Train_2_SMOTE = import(file = "Base/Base_Elastic_Net/Train_2_SMOTE.rds")

Regresores_1 = c("hacinamiento","nocupados","ndesempleados","ntrabajo_menores","Head_Mujer","Pago_Arriendo","n_cuartos_duermen",
                 "n_cuartos","Head_ocupado","Head_Afiliado_SS","Head_Cot_pension",grep("maxEducLevel",colnames(Train_2_SMOTE),value = T))


# 3. Se paraleliza -------------------------------------------------------------
library(doParallel)
num_cores <- parallel::detectCores()
print(num_cores)
cl <- makeCluster(num_cores - 4)
registerDoParallel(cl)

# stopCluster(cl)    # Para liberar los nucleos 



p_load('xgboost')

# Definir el control para el entrenamiento
fitControl <- trainControl(
  method = "cv",              # Validación cruzada
  number = 10,                # 10 pliegues
  classProbs = TRUE,          # Probabilidades de clase
  summaryFunction = twoClassSummary,  # Para clasificación binaria
  verboseIter = TRUE          # Mostrar progreso
)

grid_xbgoost

grid_xbgoost <- expand.grid(nrounds = c(100,250),
                            max_depth = c(2,4), 
                            eta = c(0.01,0.05), 
                            gamma = c(0,1), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4,0.8))

grid_xbgoost


set.seed(1011)
Xgboost_tree <- train(class ~ .,
                      data=Train_2_SMOTE,
                      method = "xgbTree", 
                      trControl = fitControl,
                      tuneGrid=grid_xbgoost
)        

Xgboost_tree

stopCluster(cl)


Xgboost_tree$bestTune

Prediccion_1_XGB <- predict(Xgboost_tree, newdata = Test_2) ## class for class prediction
# F1-Score 
f1_score_modelo_RF_1 <- F1_Score(y_true = as.factor(Test_2$Pobre), y_pred = as.factor(Prediccion_1_XGB), positive = "Yes")
# 0.4895

# Prediccion con test 
Prediccion_1_RF <- predict(Xgboost_tree, newdata = Test)

# Se deja en el formato requerido
Prediccion_1_xbg <- as.data.frame(Prediccion_1_XGB) %>% cbind(Test["id"]) %>%
  mutate(pobre=ifelse(Prediccion_1_RF=="Yes",1,0)) %>% 
  select(id,pobre)

# ------------------------------
# Prueba 2 de XGBoost
# Tunning

grid_xbgoost <- expand.grid(nrounds = c(500,1000),
                            max_depth = c(4,6), 
                            eta = c(0.01,0.05), 
                            gamma = c(0,1), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4,0.8))


fitControl <- trainControl(
  method = "cv",              
  number = 10,                
  classProbs = TRUE,          
  summaryFunction = twoClassSummary,
  verboseIter = TRUE,
  sampling = "smote",  # Añadir SMOTE en la validación cruzada
  returnResamp = "all",
  allowParallel = TRUE,
  savePredictions = "final"
)

set.seed(1011)
Xgboost_tree <- train(class ~ .,
                      data=Train_2_SMOTE,
                      method = "xgbTree", 
                      trControl = fitControl,
                      tuneLength = 10  # Prueba más combinaciones de parámetros
)       




