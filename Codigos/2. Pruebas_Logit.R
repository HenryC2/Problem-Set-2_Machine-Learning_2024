#------------------------------------------------------------------------------#
#----------------------------- Pruebas - Reg.Logit  ---------------------------#
#------------------------------------------------------------------------------#

library(doParallel)
num_cores <- parallel::detectCores()
print(num_cores)
cl <- makeCluster(num_cores - 3)  # Usar todos menos 3
registerDoParallel(cl)

stopCluster(cl)    # Para liberar los nucleos 



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
  select(maxEducLevel, Dominio, n_cuartos, Nper, nocupados, Head_Ocupacion, Cabecera, Head_edad)



# 2. Division de la muestra de train para probar el modelo ----------------

set.seed(12345)
train_indices <- as.integer(createDataPartition(train_filtro$Pobre, p = 0.85, list = FALSE))
train <- Train[train_indices, ]
test <- Train[-train_indices, ]
prop.table(table(train$Pobre))
prop.table(table(test$Pobre))


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

#Test
test <- test  %>% 
  mutate(Pobre =predict(modelo_logit,newdata = test,type = "raw")) %>%
  select(id,Pobre)

confusionMatrix(data = test$hat_modelo_logit, 
                reference = test$Pobre, positive="Yes", mode = "prec_recall")

# Se deja en el formato requerido
Prediccion <- test %>% 
  mutate(pobre=ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,pobre)

Nombre <- paste0("Logit_", ".csv") 
setwd(paste0(wd,"\\Output\\Logit"))
write.csv(Prediccion,Nombre, row.names = FALSE)




# Modelo Logit ajustado por F ---------------------------------------------


p_load(Metrics)
set.seed(098063)
fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...))  ## Para 


ctrl<- trainControl(method = "cv",
                    number = 5,
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

#Test
test <- test  %>% mutate(hat_modelo_logit_F=predict(modelo_logit_F,newdata = test,
                                                      type = "raw"))
confusionMatrix(data = test$hat_modelo_logit_F, 
                reference = test$Pobre, positive="Yes", mode = "prec_recall")






# Modelo rebalanceado ------------------------------------------------------------
# Re balanceo con SMOTE  --------------------------------------------------

predictors<-colnames(train_filtro  %>% select(-Pobre))

#Convertir a dummies
Predictores_num <- Predictores %>%
  mutate_if(is.factor, as.numeric) 

# Ajuste SMOTE
smote_output <- SMOTE(X = Predictores_num,
                      target = train_filtro$Pobre, K = 5)
Train_SMOTE <- smote_output$data



set.seed(6392)
logit_smote <- train(class~.,
                     data = Train_SMOTE,
                     metric = "F",
                     method = "glm",
                     trControl = ctrl,
                     family = "binomial")


logit_smote
logit_smote$resample
logit_SMOTE_F<-logit_smote$results$F
logit_SMOTE_F

#Test
test <- test  %>% mutate(hat_modelo_logit_smote_F=predict(logit_SMOTE_F,newdata = test,
                                                    type = "raw"))
confusionMatrix(data = test$hat_modelo_logit_smote_F, 
                reference = test$Pobre, positive="Yes", mode = "prec_recall")




