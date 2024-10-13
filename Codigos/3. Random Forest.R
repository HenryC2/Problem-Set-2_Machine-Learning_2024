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
Train_SMOTE = import(file = "Train_2_SMOTE.rds")

# 2. Se paraleliza -------------------------------------------------------------
library(doParallel)
num_cores <- parallel::detectCores()
print(num_cores)
cl <- makeCluster(num_cores - 3)  # Usar todos menos 3
registerDoParallel(cl)
# stopCluster(cl)    # Para liberar los nucleos

#3. Estimaciones ---------------------------------------------------------------

set.seed(1536)
fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...)) 

fitControl<- trainControl(method = "cv",
                          number = 10,
                          classProbs = TRUE,
                          summaryFunction = fiveStats,
                          savePredictions = T)

mtry_grid<-expand.grid(mtry =c(2,4,6,8),
                       min.node.size= c(1, 5, 10, 30), 
                       splitrule= 'gini') #splitrule constante

Train_2_SMOTE$class <- as.factor(Train_2_SMOTE$class)
Test_2$class <- as.factor(Test_2$class)

#3.1 Modelo 1 ------------------------------------------------------------------
Regresores_1 = c("hacinamiento","nocupados","ndesempleados","ntrabajo_menores","Head_Mujer","Pago_Arriendo","n_cuartos_duermen",
                 "n_cuartos","Head_ocupado","Head_Afiliado_SS","Head_Cot_pension",grep("maxEducLevel",colnames(Train_2_SMOTE),value = T))

RF_1<- ranger(formula(paste0("class ~", paste0(Regresores_1, collapse = " + "))), 
            data = Train_2_SMOTE,
            method = "ranger",
            trControl = fitControl,
            metric="F1",
            tuneGrid = mtry_grid,
            ntree=100)

# Prediccion fuera de muestra
Prediccion_1_RF <- Test_2   %>% 
  mutate(Pobre = predict(RF_1, newdata = Test_2, type = "raw")    ## predicted class labels
  )  %>% select(Pobre)                 


# F1-Score 
f1_score_modelo_1 <- F1_Score(y_true = as.factor(Test_2$Pobre), y_pred = as.factor(Prediccion_1_RF$Pobre), positive = "Yes")
print(f1_score_modelo_2)
# 0.4738