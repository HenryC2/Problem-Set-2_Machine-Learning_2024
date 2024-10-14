#------------------------------------------------------------------------------#
#----------------------------- Pruebas - Reg.Logit  ---------------------------#
#------------------------------------------------------------------------------#

library(doParallel)
num_cores <- parallel::detectCores()
print(num_cores)
cl <- makeCluster(num_cores - 4)  # Usar todos menos 3
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
  select(maxEducLevel, Dominio, n_cuartos, Nper, nocupados, Cabecera,Head_Ocupacion, Head_edad, Pobre)

# En Test, omitimos la variable Pobre
test_filtro <- Test %>%
  select(id,maxEducLevel, Dominio, n_cuartos, Nper, nocupados, Cabecera,Head_Ocupacion, Head_edad)



# 2. Division de la muestra de train para probar el modelo ----------------

set.seed(12345)
train_indices <- as.integer(createDataPartition(train_filtro$Pobre, p = 0.85, list = FALSE))
train <- train_filtro[train_indices, ]
test <- train_filtro[-train_indices, ]
prop.table(table(train$Pobre))


# Modelo 1 Arbol ------------------------------------------------------------

fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))  ## Para usar ROC) (u otras más) para tuning


ctrl<- trainControl(method = "cv",
                    number = 10,
                    summaryFunction = fiveStats,
                    classProbs = TRUE, 
                    verbose=FALSE,
                    savePredictions = T)


grid <- expand.grid(cp = seq(0, 0.1, 0.001))

set.seed(9151)  
cv_tree_1 <- train(Pobre~.,
                   data = train,
                   method = "rpart", 
                   trControl = ctrl, 
                   tuneGrid = grid, 
                   metric= "F"
)


cv_tree_1
cv_tree_1$bestTune$cp

#Test interno
Tree_test_1 <- test  %>% 
  mutate(Pobre_hat =predict(cv_tree_1,newdata = test,type = "raw"))

confusionMatrix(data = Tree_test_1$Pobre_hat, 
                reference = Tree_test_1$Pobre, positive="Yes", mode = "prec_recall")


# Prediccion fuera de muestra
Prediccion_Tree_1 <- test_filtro  %>% 
  mutate(Pobre =predict(cv_tree_1,newdata = test_filtro,type = "raw"),
         pobre=ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,pobre)



Nombre_1 <- paste0("Tree","_cp_" , "0", ".csv") 
setwd(paste0(wd,"\\Output\\Decision tree"))
write.csv(Prediccion_Tree_1,Nombre_1, row.names = FALSE)


# Modelo 2 Arbol(SMOTE) ------------------------------------------------------------

# 2.4.1 Re balanceo usando el Train original 

train_filtro_smote <- Train %>%
  select(n_cuartos, Nper, nocupados, Cabecera,adultos,ntrabajo_menores,Head_edad, Pobre)

# En Test, omitimos la variable Pobre
test_filtro_smote <- Test %>%
  select(id ,n_cuartos, Nper, nocupados, Cabecera,adultos,ntrabajo_menores,Head_edad)

set.seed(123)
train_indices_2 <- as.integer(createDataPartition(train_filtro_smote$Pobre, p = 0.85, list = FALSE))
train_SMOTE <- train_filtro_smote[train_indices_2, ]
test_SMOTE <- train_filtro_smote[-train_indices_2, ]
prop.table(table(train_filtro_smote$Pobre))



Predictores <- train_SMOTE %>%
  select(-Pobre) %>%
  as.data.frame()

SMOTE <- SMOTE(X = Predictores,target = train_SMOTE$Pobre,K=5)
Train_SMOTE <- SMOTE$data



set.seed(6392)
cv_tree_2 <- train(class~.,
                 data = Train_SMOTE,
                 metric = "F",
                 method = "rpart",
                 trControl = ctrl,
                 preProcess = c("center", "scale"))

cv_tree_2
cv_tree_2$bestTune$cp


#Test interno
Tree_test_2 <- test_SMOTE  %>% 
  mutate(Pobre_hat =predict(cv_tree_2,newdata = test_SMOTE,type = "raw"))

confusionMatrix(data = Tree_test_2$Pobre_hat, 
                reference = Tree_test_2$Pobre, positive="Yes", mode = "prec_recall")


# Prediccion fuera de muestra
Prediccion_Tree_2 <- test_filtro_smote  %>% 
  mutate(Pobre =predict(cv_tree_2,newdata = test_filtro_smote,type = "raw"),
         pobre=ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,pobre)


Nombre_2 <- paste0("Tree","_cp_" , "0.042", ".csv") 
setwd(paste0(wd,"\\Output\\Decision tree"))
write.csv(Prediccion_Tree_2,Nombre_2, row.names = FALSE)







# Modelo 3 Arbol ------------------------------------------------------------



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


grid <- expand.grid(cp = seq(0, 0.3, 0.001))

set.seed(9153)  
cv_tree_3 <- train(Pobre~.,
                   data = train_2,
                   method = "rpart", 
                   trControl = ctrl, 
                   tuneGrid = grid, 
                   metric= "F"
)


cv_tree_3
cv_tree_3$bestTune$cp

#Test interno
Tree_test_3 <- test_2  %>% 
  mutate(Pobre_hat =predict(cv_tree_3,newdata = test_2,type = "raw"))

confusionMatrix(data = Tree_test_3$Pobre_hat, 
                reference = Tree_test_3$Pobre, positive="Yes", mode = "prec_recall")


# Prediccion fuera de muestra
Prediccion_Tree_3 <- test_filtro_2  %>% 
  mutate(Pobre =predict(cv_tree_3,newdata = test_filtro_2,type = "raw"),
         pobre=ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,pobre)



Nombre_3 <- paste0("Tree","_cp_" , "0", ".csv") 
setwd(paste0(wd,"\\Output\\Decision tree"))
write.csv(Prediccion_Tree_3,Nombre_3, row.names = FALSE)



