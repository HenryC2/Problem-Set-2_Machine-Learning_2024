#------------------------------------------------------------------------------#
#----------------------------- Pruebas - Boosting  ----------------------------#
#------------------------------------------------------------------------------#

library(doParallel)
num_cores <- parallel::detectCores()
print(num_cores)
cl <- makeCluster(num_cores - 2)  # Usar todos menos 3
print(cl)
registerDoParallel(cl)

#stopCluster(cl)    # Para liberar los nucleos 


# 0. Carga de informacion  -----------------------------------------------------
setwd(paste0(wd,"/Base"))
Train <- import(file = "Train_hogares_final.rds")
Test <- import(file = "Test_hogares_final.rds")

# Observamos la estructura de los datos

str(Train) # Confirmamos que la variable pobre es factor


# 1. Filtro de variables -------------------------------------------------------

# Filtramos la base Train con las varaibles de interes
train_filtro <- Train %>%
  select(maxEducLevel, Dominio, n_cuartos, Nper, nocupados, Head_Ocupacion, Cabecera, Head_edad, Pobre)

# En Test, omitimos la variable Pobre
test_filtro <- Test %>%
  select(id,maxEducLevel, Dominio, n_cuartos, Nper, nocupados, Cabecera,Head_Ocupacion, Head_edad)




# 2. Division de la muestra de train para probar el modelo ----------------

set.seed(12345)
train_indices <- as.integer(createDataPartition(train_filtro$Pobre, p = 0.85, list = FALSE))
train <- train_filtro[train_indices, ]
test <- train_filtro[-train_indices, ]
prop.table(table(train$Pobre))
prop.table(table(test$Pobre))


# Modelo 1 ------------------------------------------------------------

p_load('gbm')
p_load(Metrics)
p_load(MLmetrics)

#Adecuacion del Grid
grid_gbm<-expand.grid(n.trees=c(200,300,500),
                      interaction.depth=c(4,6),
                      shrinkage=c(0.001,0.01),
                      n.minobsinnode = c(10,30))



set.seed(098063)
fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...))  ## Para 

fitControl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

#Estimación del modelo
set.seed(1011)
gbm_tree <- train( Pobre~.,
                   data = train,
                   method = "gbm", 
                   trControl = fitControl,
                   tuneGrid = grid_gbm,
                   verbose = FALSE
)  


gbm_tree
gbm_tree$resample
gbm_tree_F<-gbm_tree$results$F
min(gbm_tree$results$F)


#Test

test_gbm <- test %>%
  mutate(Pobre_hat= predict(gbm_tree, newdata = test, type = "raw")) 

confusionMatrix(data = test_gbm$Pobre_hat, 
                reference = test_gbm$Pobre, positive="Yes", mode = "prec_recall")

# Se deja en el formato requerido

# Predicción fuera de muestra
Prediccion_gbm <- test_filtro  %>% 
  mutate(Pobre = predict(gbm_tree,newdata = test_filtro,type = "raw"),
         pobre = ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,pobre)



# Modelo 2 ------------------------------------------------------------
#Ajustado por F

set.seed(098065)
fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...))  ## Para 

fitControl2<- trainControl(method = "cv",
                          number = 10,
                          classProbs = TRUE,
                          summaryFunction = fiveStats,
                          savePredictions = T)

# Modelo entrenado por metrica F
set.seed(1015)
gbm_tree <- train( Pobre~.,
                   data = train,
                   method = "gbm",
                   metric = "F",
                   trControl = fitControl2,
                   tuneGrid = grid_gbm,
                   verbose = FALSE
)  


gbm_tree
gbm_tree$resample
gbm_tree_F<-gbm_tree$results$F
min(gbm_tree$results$F)


#Test

test <- test %>%
  mutate(hat_modelo_logit_F = predict(gbm_tree, newdata = test, type = "raw")) 

confusionMatrix(data = test$hat_modelo_logit_F, 
                reference = test$Pobre, positive="Yes", mode = "prec_recall")

# Prediccion fuera de muestra
Prediccion <- test_filtro  %>% 
  mutate(Pobre = predict(gbm_tree,newdata = test_filtro,type = "raw"),
         pobre = ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,pobre)




# Modelo 3 ------------------------------------------------------------

grid_gbm2<-expand.grid(n.trees=c(200,300,500,700),
                      interaction.depth=c(4,6),
                      shrinkage=c(0.001,0.01),
                      n.minobsinnode = c(10,30,40))


# Filtramos la base Train con las varaibles de interes
train_filtro_2 <- Train %>%
  select(maxEducLevel, Dominio, n_cuartos, Nper, nocupados, ndesempleados, ntrabajo_menores,Pago_Arriendo,
         n_cuartos_duermen,Cabecera,
         Head_Ocupacion, Head_edad, Head_Mujer, Head_ocupado,hacinamiento, Pobre)

# En Test, omitimos la variable Pobre
test_filtro_2 <- Test %>%
  select(id, maxEducLevel, Dominio, n_cuartos, Nper, nocupados, ndesempleados, ntrabajo_menores,Pago_Arriendo,
         n_cuartos_duermen,Cabecera,
         Head_Ocupacion, Head_edad, Head_Mujer, Head_ocupado,hacinamiento)



# 2. Division de la muestra de train para probar el modelo ----------------

set.seed(12345)
train_indices <- as.integer(createDataPartition(train_filtro_2$Pobre, p = 0.85, list = FALSE))
train_2 <- train_filtro_2[train_indices, ]
test_2 <- train_filtro_2[-train_indices, ]
prop.table(table(train_2$Pobre))



set.seed(1020)
gbm_tree <- train( Pobre~.,
                   data = train_2,
                   method = "gbm",
                   metric = "F",
                   trControl = fitControl2,
                   tuneGrid = grid_gbm2,
                   verbose = FALSE
)  


gbm_tree
gbm_tree$resample
gbm_tree_F<-gbm_tree$results$F
min(gbm_tree$results$F)


#Test

test_gbm_2 <- test_2 %>%
  mutate(pobre_hat = predict(gbm_tree, newdata = test_2, type = "raw")) 

confusionMatrix(data = test_gbm_2$pobre_hat, 
                reference = test_gbm_2$Pobre, positive="Yes", mode = "prec_recall")

# Predicción fuera de muestra
Prediccion_3 <- test_filtro_2  %>% 
  mutate(Pobre = predict(gbm_tree,newdata = test_filtro_2,type = "raw"),
         pobre = ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,pobre)


Nombre_3 <- paste0("gbm","_n.trees_" , "700","_interaction_" ,
                   "6","_shrinkage_" , "0.01","_minobsinnode_" , "30", ".csv") 
setwd(paste0(wd,"\\Output\\Decision tree"))
write.csv(Prediccion_3,Nombre_3, row.names = FALSE)





# Modelo 4 XgBoost ------------------------------------------------------------
#Ajustado por F


p_load('xgboost')



grid_xbgoost <- expand.grid(nrounds = c(100,300,600),
                            max_depth = c(2,4,6), 
                            eta = c(0.01,0.05), 
                            gamma = c(0,1,5), 
                            min_child_weight = c(10, 25,50),
                            colsample_bytree = c(0.33,0.66),
                            subsample = c(0.4,0.8))


# Filtramos la base Train con las varaibles de interes
train_filtro_2 <- Train %>%
  select(maxEducLevel, Dominio, n_cuartos, Nper, nocupados, ndesempleados, ntrabajo_menores,Pago_Arriendo,
         n_cuartos_duermen,Cabecera,
         Head_Ocupacion, Head_edad, Head_Mujer, Head_ocupado,hacinamiento, Pobre)

# En Test, omitimos la variable Pobre
test_filtro_2 <- Test %>%
  select(id, maxEducLevel, Dominio, n_cuartos, Nper, nocupados, ndesempleados, ntrabajo_menores,Pago_Arriendo,
         n_cuartos_duermen,Cabecera,
         Head_Ocupacion, Head_edad, Head_Mujer, Head_ocupado,hacinamiento)



# 2. Division de la muestra de train para probar el modelo ----------------

set.seed(12345)
train_indices <- as.integer(createDataPartition(train_filtro_2$Pobre, p = 0.85, list = FALSE))
train_2 <- train_filtro_2[train_indices, ]
test_2 <- train_filtro_2[-train_indices, ]
prop.table(table(train_2$Pobre))


set.seed(1011)
Xgboost_tree <- train(Pobre~.,
                      data=train_2,
                      method = "xgbTree", 
                      metric = "F",
                      trControl = fitControl2,
                      tuneGrid=grid_xbgoost
)        


Xgboost_tree
Xgboost_tree$results$F
Xgboost_tree

#Test

test_xgboost <- test_2 %>%
  mutate(pobre_hat = predict(Xgboost_tree, newdata = test_2, type = "raw")) 

confusionMatrix(data = test_2$pobre_hat, 
                reference = test_2$Pobre, positive="Yes", mode = "prec_recall")

# Prediccion fuera de muestra
Prediccion_3 <- test_filtro_2  %>% 
  mutate(Pobre = predict(gbm_tree,newdata = test_filtro_2,type = "raw"),
         pobre = ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,pobre)


Nombre_3 <- paste0("gbm","_n.trees_" , "700","_interaction_" ,
                   "6","_shrinkage_" , "0.01","_minobsinnode_" , "30", ".csv") 
setwd(paste0(wd,"\\Output\\Decision tree"))
write.csv(Prediccion_3,Nombre_3, row.names = FALSE)





