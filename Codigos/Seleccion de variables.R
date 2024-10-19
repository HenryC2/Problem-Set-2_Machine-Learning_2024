#------------------------------------------------------------------------------#
#---------------- Script . SELECCION DE VARIABLES PARA MODELOS-----------------#
#------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------
# 1. Cargar base de datos de entrenamiento y testeo ----------------------------
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

#-------------------------------------------------------------------------------
# 2. Seleccion de variables a evaluar ------------------------------------------
#-------------------------------------------------------------------------------

Train_n <- Train %>% 
  select( Pobre ,tipo_vivienda , 
            Cabecera,
            Nper,
            n_cuartos,
            maxEducLevel,
            Head_Ocupacion ,
            Head_Mujer,
            rsubsidiado,
            nsubsidios,
            Head_EducLevel, 
            Head_Cot_pension ,
            Head_ocupado,
            Head_edad,
            Head_Afiliado_SS,
            nmenores_6,
            nincapacitados) 
names(Train_n)
str(Train_n)
#  Head_edad,



#-------------------------------------------------------------------------------
# 3. ------------------------------------------
#-------------------------------------------------------------------------------

model_form <- Pobre ~ tipo_vivienda + 
  Cabecera +
  Nper +
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


bestsub_model <- regsubsets(model_form,
                            data = Train_n, ## data frame Note we are using the training sample.
                            nvmax = 38) ## show all the model groups lets define all the variables. 

summary(bestsub_model)
max_nvars= bestsub_model[["np"]]-1  ## minus one because it counts the intercept.
max_nvars
coef(bestsub_model, id=5)


predict.regsubsets<- function (object , newdata , id, ...) {
  
  form<- model_form
  mat <- model.matrix(form , newdata) ## model matrix in the test data
  coefi <- coef(object , id = id) ## coefficient for the best model with id vars
  xvars <- names(coefi)  ## select only variables in the model
  mat[, xvars] %*% coefi  ## prediction 
  
}

#Validacion cruzada
k <- 10
n <- nrow(Train_n)
## important!!!  set seed
set.seed(308873)

folds <- sample (rep (1:k, length = n))
cv.errors <- matrix (NA, k, max_nvars,
                     dimnames = list (NULL , paste (1:max_nvars)))

#Algortmo de validacion cruzada
for (j in 1:k) {
  best_fit <- regsubsets(model_form,
                         data = Train_n[folds != j, ],
                         nvmax = max_nvars)
  for (i in 1:max_nvars) {
    pred <- predict(best_fit , Train_n[folds == j, ], id = i)
    cv.errors[j, i] <- RMSE(pred,Train_n$Pobre[folds == j])
  }
}

mean.cv.errors <- apply (cv.errors , 2, mean)
mean.cv.errors
which.min (mean.cv.errors)
plot (mean.cv.errors , type = "b")