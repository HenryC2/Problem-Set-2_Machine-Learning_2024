#------------------------------------------------------------------------------#
#------------------------------------ RIDGE -----------------------------------#
#------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------
# 1. Cargar base de datos ------------------------------------------------------
#-------------------------------------------------------------------------------

# Julieth 
wd =  "C:/Users/Usuario/OneDrive - Universidad de los andes/Escritorio/Taller 2 Big data" #DirectorioS  # Julieth 
wd2 = "C:/Users/Usuario/OneDrive - Universidad de los andes/Documentos/GitHub/Problem-Set-2_Machine-Learning_2024" #Modelo git hub


#Cargar base de datos
setwd(paste0(wd,"/Datos/Out"))
Train <- import(file = "Train_hogares_final.rds")
str(Train)
Test <- import(file = "Test_hogares_final.rds")
str(Test)


#--------------------------CONSERVAR VARIABLES --------------------------------#

#Posibles variables dependientes
Train_n <- Train %>% 
  select(id, Pobre,
         tipo_vivienda,
         n_cuartos,
         Cabecera,
         Departamento,
         Nper,
         Dominio,
         maxEducLevel,
         rsubsidiado,
         Head_Mujer,
         Head_EducLevel,
         Head_Ocupacion,
         Head_Cot_pension,
         Head_ocupado,
         Head_edad,
         Head_Afiliado_SS,
         nmenores_6,
         nsubsidios,
         ndesempleados,
         nincapacitados) 
names(Train_n)


Test_n <- Test %>% 
  select(id,
         tipo_vivienda,
         n_cuartos,
         Cabecera,
         Departamento,
         Nper,
         Dominio,
         maxEducLevel,
         rsubsidiado,
         Head_Mujer,
         Head_EducLevel,
         Head_Ocupacion,
         Head_Cot_pension,
         Head_ocupado,
         Head_edad,
         Head_Afiliado_SS,
         nmenores_6,
         nsubsidios,
         ndesempleados,
         nincapacitados) 


des_vars <- c("tipo_vivienda", "Cabecera", "maxEducLevel", "Head_Mujer",
              "Head_EducLevel", "Head_Ocupacion", "Head_Cot_pension", "Head_ocupado", 
              "Head_Afiliado_SS", "Departamento", "Dominio")
stargazer:: stargazer(as.data.frame(Train_n[,des_vars]), type="text")
Train_n <- Train_n %>% mutate_at(des_vars, as.factor)
Test_n <- Test_n %>% mutate_at(des_vars, as.factor)

# Dividir aleatoriamente los datos en entrenamiento y un conjunto de prueba
set.seed(54832)
train_indices <- as.integer(createDataPartition(Train_n$Pobre, p = 0.85, list = FALSE))
Train1 <- Train_n[train_indices, ]
Test1 <- Train_n[-train_indices, ]
prop.table(table(Train_n$Pobre))



fitControl <- trainControl( 
  method = "cv",
  number = 10) ##  10 fold CV

model_form<-  Pobre ~ tipo_vivienda + 
  Cabecera +
  Nper +
  Dominio +
  Departamento +
  n_cuartos +
  maxEducLevel +
  Head_Ocupacion +
  Head_Mujer +
  rsubsidiado +
  nsubsidios +
  Head_EducLevel + 
  Head_Cot_pension +
  Head_ocupado +
  Head_edad +
  Head_Afiliado_SS +
  nmenores_6 +
  nincapacitados


set.seed(308873)  
lambda_seq <- 100*seq(1,.505,-.005)^14 
set.seed(308873)  
ridge_seq<-train(model_form,
                 data=Train1,
                 method = 'glmnet', 
                 trControl = fitControl,
                 tuneGrid = expand.grid(alpha = 0,
                                        lambda=lambda_seq)
) 
ridge_seq
ridge_RMSE<- min(ridge_seq$results$RMSE)
ridge_RMSE

# Lamda optimo
ridge_opt<-train(model_form,
                 data=Train1,
                 method = 'glmnet', 
                 trControl = fitControl,
                 tuneGrid = expand.grid(alpha = 0,
                                        lambda=0.01379946)
) 
ridge_opt
ridge_accuracy0<-ridge_opt$results$Accuracy
ridge_accuracy0

#Test interno - LDA
test_int_ridge <- Test1  %>% 
  mutate(Pobre_hat =predict(ridge_opt,newdata = Test1,type = "raw"))

confusionMatrix(data = test_int_ridge$Pobre_hat, 
                reference = test_int_ridge$Pobre, positive="Yes", mode = "prec_recall")

# Prediccion fuera de muestra - RIDGE-------------------------------------------#
Prediccion_rigde0 <- Test_n  %>% 
  mutate(pobre_lab = predict(ridge_opt, newdata = Test_n,type = "raw"),
         pobre_lab=ifelse(pobre_lab=="Yes",1,0)) %>% 
  select(id,pobre_lab)
head(Prediccion_rigde0)

#Exportar prediccion
NombreRIDGE <- paste0("R_", "lambda_", "0.013_", "alpha_", "0", ".csv") 
setwd(paste0(wd2,"/Output/RIGDE"))
write.csv(Prediccion_rigde0,NombreRIDGE, row.names = FALSE)
  