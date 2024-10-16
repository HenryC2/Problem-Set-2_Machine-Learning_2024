#------------------------------------------------------------------------------#
#------------------ MODELOS DE CLASIFICACION - KNN ----------------------------#
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

#----------------------------------------------------------------------------#
#------------------------------ K-NN VECINOS --------------------------------#
#----------------------------------------------------------------------------#


#Validazcion cruzada 10 veces
ctrl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    savePredictions = TRUE,
                    verbose=FALSE
)

set.seed(54832)
default_knn0 <- train(Pobre ~ tipo_vivienda + 
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
                        nincapacitados,
                      data = Train1, 
                      method = "knn",  
                      tuneGrid = expand.grid(k=seq(13,50,by=5)),
                      trControl = ctrl)

default_knn0
#K-mayor precision lograda
max(default_knn0$results$Accuracy)
k = default_knn0$bestTune$k  # k


#Modelo con el k definido
set.seed(54832)
default_knn1 <- train(Pobre ~ tipo_vivienda + 
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
                        nincapacitados,
                      data = Train1,  
                      method = "knn",  
                      tuneGrid = expand.grid(k= k),
                      trControl = ctrl)


#Test interno - k vecinos
test_int_knn <- Test1  %>% 
  mutate(Pobre_hat_knn =predict(default_knn0,newdata = Test1,type = "raw"))

confusionMatrix(data = test_int_knn$Pobre_hat_knn, 
                reference = test_int_knn$Pobre, positive="Yes", mode = "prec_recall")


# Prediccion fuera de muestra - KNN-------------------------------------------#
Prediccion_knn <- Test_n  %>% 
  mutate(Pobre_knn =predict(default_knn1, newdata = Test_n,type = "raw"),
         Pobre_knn=ifelse(Pobre=="Yes",1,0)) %>% 
  select(id,pobre_lab)

#Exportar ejercicio ---------------------------------------------------------#

Nombre <- paste0("KNN_K2", ".csv") 
setwd(paste0(wd2,"/Output/KNN"))
write.csv(Prediccion,Nombre, row.names = FALSE)