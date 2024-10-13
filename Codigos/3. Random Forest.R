#------------------------------------------------------------------------------#
#----------------------------- Pruebas - Random Forest ------------------------#
#------------------------------------------------------------------------------#

# 0. Carga de informacion  -----------------------------------------------------
setwd(paste0(wd,"/Base"))
Train <- import(file = "Train_hogares_final.rds")
Test <- import(file = "Test_hogares_final.rds")

# 1. Carga de información balanceada (SMOTE) -----------------------------------
# Este procedimiento se realizo en el código 2.Pruebas_Elastic_Net
setwd(paste0(wd,"\\Base\\Base_Elastic_Net"))
Train_2_SMOTE = import(file = "Train_2_SMOTE.rds")

# Cargar test_2
set.seed(1536) # Para reproducibilidad
Train_2_indices <- as.integer(createDataPartition(Train$Pobre, p = 0.85, list = FALSE))
Train_2 <- Train[Train_2_indices, ]
Test_2 <- Train[-Train_2_indices, ]
prop.table(table(Train_2$Pobre))
prop.table(table(Test_2$Pobre))

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

# Convertir las variables categóricas a factores en test_2
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

set.seed(1536)
fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...)) 

fitControl<- trainControl(method = "cv",
                          number = 5,
                          classProbs = TRUE,
                          summaryFunction = fiveStats,
                          savePredictions = "final")

mtry_grid<-expand.grid(mtry =c(2,4,6,8,19),
                       min.node.size= c(1,2,5,10), 
                       splitrule= 'gini') #splitrule constante

#3.1 Modelo 1 ------------------------------------------------------------------
Regresores_1 = c("hacinamiento","nocupados","ndesempleados","ntrabajo_menores","Head_Mujer","Pago_Arriendo","n_cuartos_duermen",
                 "n_cuartos","Head_ocupado","Head_Afiliado_SS","Head_Cot_pension",grep("maxEducLevel",colnames(Train_2_SMOTE),value = T))

RF_1<- train(formula(paste0("class ~", paste0(Regresores_1, collapse = " + "))), 
            data = Train_2_SMOTE,
            method = "ranger",
            trControl = fitControl,
            metric="F",
            tuneGrid = mtry_grid,
            ntree=100,
            importance="impurity")

# Mejor modelo 
RF_1$finalModel

# Prediccion fuera de muestra
Prediccion_1_RF <- predict(RF_1, 
                   newdata = Test_2 
                   ) ## class for class prediction
# F1-Score 
f1_score_modelo_RF_1 <- F1_Score(y_true = as.factor(Test_2$Pobre), y_pred = as.factor(Prediccion_1_RF), positive = "Yes")
# 0.4895

# Prediccion con test 
Prediccion_1_RF <- predict(RF_1, 
                           newdata = Test)

# Se deja en el formato requerido
Prediccion_1_RF <- as.data.frame(Prediccion_1_RF) %>% cbind(Test["id"]) %>%
  mutate(pobre=ifelse(Prediccion_1_RF=="Yes",1,0)) %>% 
  select(id,pobre)


Nombre <- paste0("RF_mtry_", "4", "_splitrule_" , "gini","_min.node.size_","10",".csv") 
setwd(paste0(wd,"\\Output\\Random_Forest"))
write.csv(Prediccion_1_RF,Nombre, row.names = FALSE)
          
          