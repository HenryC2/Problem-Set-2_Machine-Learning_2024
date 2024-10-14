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


# Modelo Logit ------------------------------------------------------------

ctrl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    savePredictions = TRUE,
                    verbose=FALSE
)

set.seed(1409)
modelo_logit <- train(Pobre~. ,
                       data = train, 
                       method = "glm",
                       family = "binomial",
                       trControl = ctrl
)

modelo_logit
modelo_logit$resample
logit_accuracy<-modelo_logit$results$Accuracy
logit_accuracy

#Test interno
test_1 <- test  %>% 
  mutate(Pobre_hat =predict(modelo_logit,newdata = test,type = "raw"))

confusionMatrix(data = test_1$Pobre_hat, 
                reference = test_1$Pobre, positive="Yes", mode = "prec_recall")

# Prediccion fuera de muestra
Prediccion <- test_filtro  %>% 
  mutate(Pobre =predict(modelo_logit,newdata = test_filtro,type = "raw"),
         pobre=ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,pobre)


Nombre <- paste0("Logit_Simple", ".csv") 
setwd(paste0(wd,"\\Output\\Logit"))
write.csv(Prediccion,Nombre, row.names = FALSE)




# Modelo Logit ajustado por F ---------------------------------------------


p_load(Metrics)
set.seed(098063)
fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...))  ## Para 


ctrl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)


set.seed(1410)
modelo_logit_F  <- train(Pobre~.,
                       data = train, 
                       metric = "F",
                       method = "glm",
                       trControl = ctrl,
                       family = "binomial")



modelo_logit_F
modelo_logit_F$resample
logit_F<-modelo_logit_F$results$F
logit_F


#Test interno
test_2 <- test  %>% 
  mutate(Pobre_hat =predict(modelo_logit_F,newdata = test,type = "raw"))

confusionMatrix(data = test_2$Pobre_hat, 
                reference = test_2$Pobre, positive="Yes", mode = "prec_recall")

# Prediccion fuera de muestra
Prediccion_2 <- test_filtro  %>% 
  mutate(Pobre =predict(modelo_logit_F,newdata = test_filtro,type = "raw"),
         pobre=ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,pobre)


Nombre_2 <- paste0("Logit_F", ".csv") 
setwd(paste0(wd,"\\Output\\Logit"))
write.csv(Prediccion_2,Nombre_2, row.names = FALSE)



# Modelo rebalanceado ------------------------------------------------------------
# Re balanceo con SMOTE  --------------------------------------------------

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

#setwd(paste0(wd,"\\Base\\Base_Smote_Logit"))



set.seed(6392)
logit_smote <- train(class~.,
                     data = Train_SMOTE,
                     metric = "F",
                     method = "glm",
                     trControl = ctrl,
                     family = "binomial",
                     preProcess = c("center", "scale"))


logit_smote
logit_smote$resample
logit_SMOTE_F<-logit_smote$results$F
logit_SMOTE_F

#Test interno

test_3 <- test_SMOTE  %>% 
  mutate(Pobre_hat =predict(logit_smote,newdata = test_SMOTE,type = "raw"))

confusionMatrix(data = test_3$Pobre_hat, 
                reference = test_3$Pobre, positive="Yes", mode = "prec_recall")

# Prediccion fuera de muestra

Prediccion_3 <- test_filtro_smote %>% 
  mutate(Pobre =predict(logit_smote,newdata = test_filtro_smote,type = "raw"),
         pobre=ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,pobre)


Nombre_3 <- paste0("Logit", ".csv") 
setwd(paste0(wd,"\\Output\\Logit"))
write.csv(Prediccion_3,Nombre_3, row.names = FALSE)


