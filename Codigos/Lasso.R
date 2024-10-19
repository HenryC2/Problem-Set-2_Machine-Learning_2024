#------------------------------------------------------------------------------#
#------------------------------------ LASSO -----------------------------------#
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

#-------------------------------------------------------------------------------
# 1.LASSO --------------- ------------------------------------------------------
#-------------------------------------------------------------------------------

#Definir modelo

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


#Validacion cruzada
fitControl <- trainControl( 
  method = "cv",
  number = 10) ##  10 fold CV

y <- Train1$Pobre
y <- as.numeric(as.character(y))

# Predictors in matrix form
X<- model.matrix(model_form, ## formula
                 data = Train1)
X<-X[,-1] #remove constant

#Modelo lasso
lasso0 <- glmnet(
  x = X,
  y = y,
  alpha = 1 # lasso penalty
)
lasso0
