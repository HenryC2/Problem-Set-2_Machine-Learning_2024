#------------------------------------------------------------------------------#
#--------------------------------- MODELOS DE LDA ----------------------------#
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
         nocupados,
         nsubsidios,
         rsubsidiado,
         ndesempleados,
         nincapacitados) %>%
  ungroup()
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
         nocupados,
         nsubsidios,
         rsubsidiado,
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


#Validazcion cruzada 10 veces
ctrl<- trainControl(method = "cv",
                    number = 10,
                    classProbs = TRUE,
                    savePredictions = TRUE,
                    verbose=FALSE
)

#Correr modelo

  set.seed(54832)
  modelo_lda0 = train(Pobre ~ tipo_vivienda + 
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
                      method="lda",
                      trControl = ctrl)
  modelo_lda0
  lda_accuracy0<-modelo_lda0$results$Accuracy
  lda_accuracy0

  
  #Test interno - LDA
  test_int_lda <- Test1  %>% 
    mutate(Pobre_hat =predict(modelo_lda0,newdata = Test1,type = "raw"))
  
  confusionMatrix(data = test_int_lda$Pobre_hat, 
                  reference = test_int_lda$Pobre, positive="Yes", mode = "prec_recall")
  
  
  # Prediccion fuera de muestra - LDA-------------------------------------------#
  Prediccion_lda0 <- Test_n  %>% 
    mutate(pobre_lab = predict(modelo_lda0, newdata = Test_n,type = "raw"),
           pobre_lab=ifelse(pobre_lab=="Yes",1,0)) %>% 
    select(id,pobre_lab)
  head(Prediccion_lda0)

  #Exportar prediccion
  NombreLDA <- paste0("LDA", ".csv") 
  setwd(paste0(wd2,"/Output/LDA"))
  write.csv(Prediccion_lda0,NombreLDA, row.names = FALSE)
  
#------------------------------------------------------------------------------#
#                         AJUSTAR MODELO POR F
#------------------------------------------------------------------------------#

  set.seed(54832)
  fiveStats <- function(...)  c(defaultSummary(...),  prSummary(...))  ## Para 
  
  
  ctrl<- trainControl(method = "cv",
                      number = 10,
                      classProbs = TRUE,
                      summaryFunction = fiveStats,
                      savePredictions = T)
  
  
  set.seed(1410)
  modelo_lda_f  <- train(Pobre ~ tipo_vivienda + 
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
                           metric = "F",
                           method = "lda",
                           trControl = ctrl,
                           family = "binomial")
  
  modelo_lda_f
  modelo_lda_f$resample
  lda_accuracy_f<-modelo_lda_f$results$Accuracy
  lda_accuracy_f
  lda_f<-modelo_lda_f$results$F
  lda_f
  
  
  #Test interno
  test_lda_f <- Test1  %>% 
    mutate(Pobre_hat_lda =predict(modelo_lda_f,newdata = Test1,type = "raw"))
  
  confusionMatrix(data = test_lda_f$Pobre_hat_lda, 
                  reference = test_lda_f$Pobre, positive="Yes", mode = "prec_recall")
  
  # Prediccion fuera de muestra
  Prediccion_lda_f <- Test_n  %>% 
    mutate(pobre_lab =predict(modelo_lda_f,newdata = Test_n,type = "raw"),
           pobre_lab =ifelse(pobre_lab=="Yes",1,0)) %>% 
    select(id,pobre_lab)
  
  #Exportar resultados
  Nombre_2 <- paste0("Lda_F", ".csv") 
  setwd(paste0(wd2,"/Output/LDA"))
  write.csv(Prediccion_lda_f,Nombre_2, row.names = FALSE)

 