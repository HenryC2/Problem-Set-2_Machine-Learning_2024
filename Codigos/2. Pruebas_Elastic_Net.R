#------------------------------------------------------------------------------#
#----------------------------- Pruebas - Elastic Net  -------------------------#
#------------------------------------------------------------------------------#

# 0. Carga de informacion  -----------------------------------------------------
setwd(paste0(wd,"/Base"))
Train <- import(file = "Train_hogares_final.rds")
Test <- import(file = "Test_hogares_final.rds")

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
# para mejorar el proceso de estimación. Para ello, se utilizará el método 
# "synthetic minority over-sampling technique (SMOTE)" que hacia un muestro hacia 
# arriba de la clase minoritaria (pobres) y hacia abajo de la clase mayoritaria (no pobres)

# 2.1 Arreglos de las variales categoricas en base train -----------------------

Dummys <- subset(Train, select = c(Dominio,tipo_vivienda,maxEducLevel,Head_EducLevel,
                                   Head_Oficio,Head_Ocupacion))

# Convertir las variables categóricas a factores
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

# Convertir las variables categóricas a factores
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
  as.data.frame()  # Asegúrate de que sea un dataframe

SMOTE <- SMOTE(X = Predictores,target = Train$Pobre,K=5)
Train_SMOTE <- SMOTE$data

setwd(paste0(wd,"\\Base\\Base_Elastic_Net"))
# export(Train_SMOTE, "Train_SMOTE.rds")
Train_SMOTE = import(file = "Train_SMOTE.rds")

# 2.4.2 Re balanceo usando el Train_2 
Predictores_2 <- Train_2 %>%
  select(-Pobre) %>%
  as.data.frame()  # Asegúrate de que sea un dataframe

SMOTE_2 <- SMOTE(X = Predictores_2,target = Train_2$Pobre,K=10)
Train_2_SMOTE <- SMOTE_2$data

setwd(paste0(wd,"\\Base\\Base_Elastic_Net"))
#export(Train_2_SMOTE, "Train_2_SMOTE.rds")
Train_SMOTE = import(file = "Train_2_SMOTE.rds")

# 3. Se paraleliza -------------------------------------------------------------
library(doParallel)
num_cores <- parallel::detectCores()
print(num_cores)
cl <- makeCluster(num_cores - 3)  # Usar todos menos 3
registerDoParallel(cl)

# stopCluster(cl)    # Para liberar los nucleos 

#4. Estimaciones ---------------------------------------------------------------

set.seed(098063)
fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...)) 

fitControl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)

#4.1 Estimacion 1 -------------------------------------------------------------
Regresores = c("hacinamiento","nocupados","ndesempleados","ntrabajo_menores","Head_Mujer","Pago_Arriendo")

# Definicion del modelo 
Modelo_1 <- train(formula(paste0("class ~", paste0(Regresores, collapse = " + "))),
                  data=Train_SMOTE,
                  metric = "F",
                  method = "glmnet",
                  trControl = fitControl,
                  family="binomial",
                  tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.10),
                  lambda =10^seq(-1, -3, length = 10)
                  )
)

# Prediccion fuera de muestra
Prediccion_1 <- Test   %>% 
  mutate(Pobre = predict(Modelo_1, newdata = Test, type = "raw")    ## predicted class labels
  )  %>% select(id,Pobre)

# Se deja en el formato requerido
Prediccion_1 <- Prediccion_1 %>% 
  mutate(pobre=ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,pobre)

Nombre <- paste0("EN_lambda_", "0.001", "_alpha_" , "0.8", ".csv") 
setwd(paste0(wd,"\\Output\\Elastic_Net"))
write.csv(Prediccion_1,Nombre, row.names = FALSE)

#4.2 Estimaciones contrastando directamente el F1-Score-------------------------

# 4.2.1 Modelo 2
Regresores_2 = c("hacinamiento","nocupados","ndesempleados","ntrabajo_menores","Head_Mujer","Pago_Arriendo")

# Definicion del modelo 
Modelo_2 <- train(formula(paste0("class ~", paste0(Regresores_2, collapse = " + "))),
                  data=Train_2_SMOTE,
                  metric = "F",
                  method = "glmnet",
                  trControl = fitControl,
                  family="binomial",
                  tuneGrid=expand.grid(
                    alpha = seq(0,1,by=.10),
                    lambda =10^seq(-1, -3, length = 10)
                 ))

# Prediccion fuera de muestra
Prediccion_2 <- Test_2   %>% 
  mutate(Pobre = predict(Modelo_2, newdata = Test_2, type = "raw")    ## predicted class labels
  )  %>% select(Pobre)                 

# F1-Score 
f1_score_modelo_2 <- F1_Score(y_true = as.factor(Test_2$Pobre), y_pred = as.factor(Prediccion_2$Pobre), positive = "Yes")
print(f1_score_modelo_2)
# 0.4738

#4.2.2 Modelo 3 ----------------------------------------------------------------
Regresores_3 = c("hacinamiento","nocupados","ndesempleados","ntrabajo_menores","Head_Mujer","Pago_Arriendo","n_cuartos_duermen",
                 "n_cuartos","Head_ocupado","Head_Afiliado_SS","Head_Cot_pension")

# Definicion del modelo 
Modelo_3 <- train(formula(paste0("class ~", paste0(Regresores_3, collapse = " + "))),
                  data=Train_2_SMOTE,
                  metric = "F",
                  method = "glmnet",
                  trControl = fitControl,
                  family="binomial",
                  tuneGrid=expand.grid(
                    alpha = seq(0,1,by=.10),
                    lambda =10^seq(-1, -3, length = 10)
                  ))

# Prediccion fuera de muestra
Prediccion_3 <- Test_2   %>% 
  mutate(Pobre = predict(Modelo_3, newdata = Test_2, type = "raw")    ## predicted class labels
  )  %>% select(Pobre)                 

# F1-Score 
f1_score_modelo_3 <- F1_Score(y_true = as.factor(Test_2$Pobre), y_pred = as.factor(Prediccion_3$Pobre), positive = "Yes")
print(f1_score_modelo_3)
# 0.5364376

# Prediccion fuera de muestra
Prediccion_3_1 <- Test   %>% 
  mutate(Pobre = predict(Modelo_3, newdata = Test, type = "raw")    ## predicted class labels
  )  %>% select(id,Pobre)

# Se deja en el formato requerido
Prediccion_3_1 <- Prediccion_3_1 %>% 
  mutate(pobre=ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,pobre)

Nombre <- paste0("EN_lambda_", "0.001", "_alpha_" , "1", ".csv") 
setwd(paste0(wd,"\\Output\\Elastic_Net"))
write.csv(Prediccion_3_1,Nombre, row.names = FALSE)

#4.2.3 Modelo 4 ----------------------------------------------------------------
Regresores_4 = c("hacinamiento","nocupados","ndesempleados","ntrabajo_menores","Head_Mujer","Pago_Arriendo","n_cuartos_duermen",
                 "n_cuartos","Head_ocupado","Head_Afiliado_SS","Head_Cot_pension",grep("maxEducLevel",colnames(Train_2_SMOTE),value = T))

# Definicion del modelo 
Modelo_4 <- train(formula(paste0("class ~", paste0(Regresores_4, collapse = " + "))),
                  data=Train_2_SMOTE,
                  metric = "F",
                  method = "glmnet",
                  trControl = fitControl,
                  family="binomial",
                  tuneGrid=expand.grid(
                    alpha = seq(0,1,by=.10),
                    lambda =10^seq(-1, -3, length = 10)
                  ))

# Prediccion fuera de muestra
Prediccion_4 <- Test_2   %>% 
  mutate(Pobre = predict(Modelo_4, newdata = Test_2, type = "raw")    ## predicted class labels
  )  %>% select(Pobre)                 

# F1-Score 
f1_score_modelo_4 <- F1_Score(y_true = as.factor(Test_2$Pobre), y_pred = as.factor(Prediccion_4$Pobre), positive = "Yes")
print(f1_score_modelo_4)
# 0.5364376

# Prediccion fuera de muestra
Prediccion_3_1 <- Test   %>% 
  mutate(Pobre = predict(Modelo_3, newdata = Test, type = "raw")    ## predicted class labels
  )  %>% select(id,Pobre)

# Se deja en el formato requerido
Prediccion_3_1 <- Prediccion_3_1 %>% 
  mutate(pobre=ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,pobre)

Nombre <- paste0("EN_lambda_", "0.001", "_alpha_" , "1", ".csv") 
setwd(paste0(wd,"\\Output\\Elastic_Net"))
write.csv(Prediccion_3_1,Nombre, row.names = FALSE)                  