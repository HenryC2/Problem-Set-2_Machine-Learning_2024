#------------------------------------------------------------------------------#
#----------------------------- Pruebas - XG Boosting  -------------------------#
#------------------------------------------------------------------------------#

# 0. Carga de informacion  -----------------------------------------------------
setwd(paste0(wd,"/Base"))
Train <- import(file = "Train_hogares_final.rds")
Test <- import(file = "Test_hogares_final.rds")

# 1. Carga de información balanceada (SMOTE) -----------------------------------
# Este procedimiento se realizo en el código 2.Pruebas_Elastic_Net
setwd(paste0(wd,"\\Base\\Base_Elastic_Net"))
Train_3_SMOTE = import(file = "Train_3_SMOTE.rds")

# Cargar test_2
set.seed(1536) # Para reproducibilidad
Train_3_indices <- as.integer(createDataPartition(Train$Pobre, p = 0.85, list = FALSE))
Train_3 <- Train[Train_3_indices, ]
Test_3 <- Train[-Train_3_indices, ]
prop.table(table(Train_3$Pobre))
prop.table(table(Test_3$Pobre))

# Convertir las variables categóricas a factores en Test
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

# Convertir las variables categóricas a factores en test_3
Dummys <- subset(Test_3, select = c(Dominio,tipo_vivienda,maxEducLevel,Head_EducLevel,
                                    Head_Oficio,Head_Ocupacion))

Dummys$Dominio <- as.factor(Dummys$Dominio)
Dummys$tipo_vivienda <- as.factor(Dummys$tipo_vivienda)
Dummys$maxEducLevel <- as.factor(Dummys$maxEducLevel)
Dummys$Head_EducLevel <- as.factor(Dummys$Head_EducLevel)
Dummys$Head_Oficio <- as.factor(Dummys$Head_Oficio)
Dummys$Head_Ocupacion <- as.factor(Dummys$Head_Ocupacion)

Dummys <- model.matrix(~ . - 1, data = Dummys)  # Eliminar el intercepto

# Union con la base original
Test_3 <- cbind(subset(Test_3, select = -c(Dominio,tipo_vivienda,maxEducLevel,Head_EducLevel,
                                           Head_Oficio,Head_Ocupacion)),Dummys)


# 2. Se paraleliza -------------------------------------------------------------
library(doParallel)
num_cores <- parallel::detectCores()
print(num_cores)
cl <- makeCluster(3)  # Usar todos menos 3
registerDoParallel(cl)
#stopCluster(cl)    # Para liberar los nucleos
# closeAllConnections()
#gc() # librar memoria 


#3. Estimaciones ---------------------------------------------------------------
Regresores_1 = c("hacinamiento","nocupados","ndesempleados","ntrabajo_menores","Head_Mujer","Pago_Arriendo","n_cuartos_duermen",
                 "n_cuartos","Head_ocupado","Head_Afiliado_SS","Head_Cot_pension",grep("maxEducLevel",colnames(Train_2_SMOTE),value = T))


grid_xbgoost <- expand.grid(nrounds = c(100,500,1000),
                            max_depth = c(2,4,6), 
                            eta = c(0.01,0.05,0.1), 
                            gamma = c(0,0.5,1), 
                            min_child_weight = c(10,20,30),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4,0.8))

grid_xbgoost

#3.1 Modelo 1 ------------------------------------------------------------------
Regresores_1 = c("hacinamiento","nocupados","ndesempleados","ntrabajo_menores","Head_Mujer","Pago_Arriendo","n_cuartos_duermen",
                 "n_cuartos","Head_ocupado","Head_Afiliado_SS","Head_Cot_pension",grep("maxEducLevel",colnames(Train_2_SMOTE),value = T))


set.seed(1536)
Xgboost_tree_1 <- train(formula(paste0("class ~", paste0(Regresores_1, collapse = " + "))),
                      data=ames,
                      method = "xgbTree", 
                      trControl = fitControl,
                      tuneGrid=grid_xbgoost,
                      metric="F"
)        

# Mejor modelo 
Xgboost_tree_1
Xgboost_tree_1$bestTune

# Prediccion fuera de muestra
Prediccion_1_Xgboost <- predict(Xgboost_tree_1, 
                           newdata = Test_2)

# F1-Score 
f1_score_modelo_xgboost_1 <- F1_Score(y_true = as.factor(Test_2$Pobre), y_pred = as.factor(Prediccion_1_Xgboost), positive = "Yes")
# 0.4895