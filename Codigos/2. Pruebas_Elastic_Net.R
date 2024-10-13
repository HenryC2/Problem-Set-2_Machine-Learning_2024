#------------------------------------------------------------------------------#
#----------------------------- Pruebas - Elastic Net  -------------------------#
#------------------------------------------------------------------------------#

# 0. Carga de informacion  -----------------------------------------------------
setwd(paste0(wd,"/Base"))
Train <- import(file = "Train_hogares_final.rds")
Test <- import(file = "Test_hogares_final.rds")

# 1. Filtro de variables -------------------------------------------------------

Filtro_Variables =  function(data,...){
  data <- data %>% 
    
  #modificar variables
  select(-id,-Li,-Lp,-Fex_c,-Fex_dpto,-Valor_Cuota,-Ln_Cuota,-Ln_Pago_arrien,-Departamento)
}

Train = Filtro_Variables(Train)
Test = Filtro_Variables(Test)

# 2. Re-balanceo ---------------------------------------------------------------

table(Train$Pobre)
cat("La tasa de pobrza es", Train%>%select(Pobre)%>% filter(Pobre==1)%>% nrow()/ nrow(Train)*100)

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

# 2.1 Arreglos de las variales categoricas en base test -----------------------

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

# 2.2  Re balanceo con SMOTE  --------------------------------------------------

Predictores <- Train %>%
  select(-Pobre) %>%
  as.data.frame()  # Asegúrate de que sea un dataframe

SMOTE <- SMOTE(X = Predictores,target = Train$Pobre,K=5)
Train_SMOTE <- SMOTE$data

setwd(paste0(wd,"\\Base\\Base_Elastic_Net"))
# export(Train_SMOTE, "Train_SMOTE.rds")
Train_SMOTE = import(file = "Train_SMOTE.rds")

# 3. Division de la muestra de train: Entre train 2.0 y test  2.0 --------------

set.seed(12345) # Para reproducibilidad
train_indices <- as.integer(createDataPartition(Train_SMOTE$Pobre, p = 0.85, list = FALSE))
train <- train_hogares[train_indices, ]
test <- train_hogares[-train_indices, ]
prop.table(table(train$Pobre))
prop.table(table(test$Pobre))




data=train,
metric = "F",
method = "glmnet",
trControl = ctrl,
family="binomial",
tuneGrid=expand.grid(
  alpha = seq(0,1,by=.5),
  lambda =10^seq(-1, -3, length = 10)
)

)


predictSample <- test   %>% 
  mutate(pobre_lab = predict(model1, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample)

predictSample<- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre)
head(predictSample)   



name<- paste0("EN_lambda_", "00046", "_alpha_" , "05", ".csv") 

write.csv(predictSample,name, row.names = FALSE)


