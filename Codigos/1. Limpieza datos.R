#------------------------------------------------------------------------------#
#---------------- Script 1. LIMPIEZA DE LAS BASES DE DATOS  -------------------#
#------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------
# 1. Cargar base de datos de entrenamiento y testeo ----------------------------
#-------------------------------------------------------------------------------

# Julieth 
setwd("C:/Users/Usuario/OneDrive - Universidad de los andes/Escritorio/Taller 2 Big data/Datos/Originales") #DirectorioS  # Julieth 

# Diego 
setwd("C:\\Users\\HP\\OneDrive - Universidad Nacional de Colombia\\Documentos\\Diego\\PEG\\2024-2\\Machine learning\\Problem Sets\\Problem_Set_2")  # Diego 
getwd()

  ### Bases de datos de entrenamiento
    
    #Hogares
    train_hogares<- import(file = "train_hogares.csv") 
    length(train_hogares) # 23 variables 
    nrow(train_hogares) # 164.960 observaciones   
    
    #Personas
    train_personas<-import(file = "train_personas.csv")
    length(train_personas) # 135 variables 
    nrow(train_personas) # 543.109 observaciones  
  
  ### Bases de datos de testeo
    
    # Hogares
    test_hogares <- import(file =  "test_hogares.csv")
    length(test_hogares) # 16 variables 
    nrow(test_hogares) # 66.168 observaciones
    
    # Personas
    test_personas <- import(file = "test_personas.csv")
    length(test_personas) # 63 variables 
    nrow(test_personas) # 219.644 observaciones
   
#-------------------------------------------------------------------------------
# 2. Mirar las variables que contiene cada base  -------------------------------
#-------------------------------------------------------------------------------
    
   ### Bases de hogares
      
      #Base de entrenamiento
      names(train_hogares)  
      str(train_hogares)  
    
      #Base de test
      names(test_hogares)  # No tiene variable de ingresos
      str(test_hogares)  
      
      # Calcular y mostrar los porcentajes directamente
      prop.table(table(train_hogares$Pobre)) * 100 # 79.98 de los hogares no son pobres
      
  ### Bases de personas
      
      #Base de entrenamiento
      names(train_personas)  
      str(train_personas)  
      
      #Base de test
      names(test_personas) 
      str(test_personas)    
      
#-------------------------------------------------------------------------------
# 3. Verificar la variables de pobreza  -----------------------------------
#-------------------------------------------------------------------------------
      
      #Forma 1
      train_hogares<- train_hogares %>% mutate(Pobre_hand=ifelse(Ingpcug<Lp,1,0))
      table(train_hogares$Pobre,train_hogares$Pobre_hand) # Variable bien definida
      
      #Forma 2
      train_hogares<- train_hogares %>% mutate(Pobre_hand_2=ifelse(Ingtotugarr<Lp*Npersug,1,0))
      table(train_hogares$Pobre,train_hogares$Pobre_hand_2) #Variable bien definida
      
 
#------------------------------------------------------------------------------
# 4. Tramiento de la base de personas -----------------------------------------
#------------------------------------------------------------------------------
     
#4.1  Dejar las variables que comparta la base de train y test ---------------
  train_personas <- train_personas[,c(colnames(test_personas))]
  
#4.2 . Missings variables de interes antes de agrupar por hogar --------------
  
  data_table_missing <- train_personas %>% 
    dplyr::select(P6020, #Sexo (Dummy)
                  P6040, #Edad (Continua)
                  P6050, #parentesco (Categorica)
                  P6090, #Si es cotizante (Categorica)
                  P6100, #Regimen de salud (Categorica)
                  P6210, #Maximo nivel educativo alcanzado (Categorica)
                  P6240, #A que dedico la mayor parte del tiempo. Ej: trabajando, buscando empleo, etc (Categorica)
                  P6426, #Tiempo trabajando en la empresa
                  P6430, #Posicion ocupacional
                  P6610, #Utiliza transporte para desplazarse (categorica)
                  P6800, #Horas trabajas normalmente a la semana (Continua)
                  P6870, #Numero de personas que tien empresa, negocio (categorica)
                  P6920, #Cotizado actualmente a fondo de pensiones (categorica)
                  P7505, #(categorica)
                  Pet,   #(categorica)  - convertir los missings en cero
                  Oc,    #(categorica)  - convertir los missings en cero
                  Des,   #(categorica)  - convertir los missings en cero
                  Ina,   #(categorica)  - convertir los missings en cero
                  Depto)            

    # Tabla parar mirar el porcentaje de missings
    db_miss <- skim(data_table_missing) %>% dplyr::select(skim_variable, n_missing)
    Nobs= nrow(train_personas) 
    db_miss<- db_miss %>% filter(n_missing!= 0)
    db_miss<- db_miss %>% mutate(p_missing= n_missing/Nobs) %>% arrange(-n_missing)
    db_miss  
  
  #4.3. Crear las potenciales variables de interes ----------------------------
      
    #Funcion parar crear las variables al mismo tiempo en la base de entrenamiento y de test
    
      pre_process_personas <- function(data,...){
        data <- data %>% 
          
          #modificar variables
          mutate(
            
          #Variables de mercado laboral
            Oc =  ifelse(is.na(Oc),0,Oc), #Ocupados
            Des = ifelse(is.na(Des),0,Des), #Desocupados
            Ina = ifelse(is.na(Ina),0,Ina), #Inactivos
            
          #Variables de caracteristicas socieconomicas de las personas/hogar
            #Dummy de mujeres
            Mujer = ifelse(P6020==2,1,0), 
            #Dummy de jefe de hogar
            H_Head = ifelse(P6050== 1, 1, 0),#Household head
            #Dumy de mujeres jefa de hogar
            H_Head_mujer = ifelse(P6050== 1&P6020==2, 1, 0), #Household head women
            #Dummy < 6 anios
            Menor_6 = ifelse(P6040<=6,1,0), # Menores o igual a 6 anios
            #Dummy de adultos
            adultos = ifelse(P6040>=60,1,0),
            #Dummy trabajo menores
            Trabajo_menores = ifelse(P6040<18 & Oc==1,1,0), # Menores que trabajan 
            #Dummy incapacidad
            incapacitado = ifelse(P6240==5,1,0),
            #Reemplazar 9 con ceros (no sabe, no responde)
            EducLevel = ifelse(P6210==9,0,P6210), #Replace 9 with 0
            EducLevel = ifelse(is.na(EducLevel),0,EducLevel),
          
            #Experiencia laboral
            exper_ult_trab = ifelse(is.na(P6426),0,P6426),
        
          # Variables de cotizacion y regimen
          
            #Dummy afiliacion al SS
            Afiliado_SS = ifelse(P6090== 1, 1, 0), #Afiliado a seg social en salud
            Afiliado_SS = ifelse(is.na(Afiliado_SS),0,Afiliado_SS), 
            #Dummy si pertenece al regimen subsidiado
            Reg_subs_salud = ifelse(P6100== 3, 1, 0), 
            Reg_subs_salud = ifelse(is.na(Reg_subs_salud),0,Reg_subs_salud), 
            #Cotizacion de pension
            Cot_pension = ifelse(P6920==1|P6920==3,1,0),
            Cot_pension = ifelse(is.na(Cot_pension),0,Cot_pension),
          
        #Variables ocupacion e ingresos 
        
            #Otras formas de ingresos
            Rec_alimento = ifelse(P6590==1,1,0), #Recibio alimentos como parte de pago del salario
            Rec_alimento = ifelse(is.na(Rec_alimento),0,Rec_alimento),
            Rec_vivienda = ifelse(P6600==1,1,0), #Recibio vivienda como parte de pago del salario
            Rec_vivienda = ifelse(is.na(Rec_vivienda),0,Rec_vivienda), 
            Rec_otros = ifelse(P6620==1,1,0), #Recibio otris ingresos es especie por su trabajo
            Rec_otros = ifelse(is.na(Rec_otros),0,Rec_otros), 
            Primas = ifelse(P6545==1,1,0), #dummy de primas
            Primas = ifelse(is.na(Primas),0,Primas), #pone 0 en NA para primas
            Bonificaciones = ifelse(P6580==1,1,0), #dummy de bonificaciones
            Bonificaciones = ifelse(is.na(Bonificaciones),0,Bonificaciones), #pone 0 en NA para bonificaciones
            rec_gub = ifelse(P7505==1,1,0), #Recibir dinero de otros hogares o entidades gubernamentales
            rec_gub = ifelse(is.na(rec_gub),0,rec_gub),
            rec_hp_res = ifelse(P7510s1==1,1,0), #Recibir dinero de otros hogares o personas residentes en el pais
            rec_hp_res = ifelse(is.na(rec_hp_res),0,rec_hp_res),
            rec_hp_nores = ifelse(P7510s2==1,1,0), #Recibir dinero de otros hogares o personas no residentes en el pais
            rec_hp_nores = ifelse(is.na(rec_hp_nores),0,rec_hp_nores),
        
        #Auxilios o subsidios
        
            #Ayuda del gobierno
            Rec_subsidio_pais = ifelse(P7510s3==1,1,0),
            Rec_subsidio_pais = ifelse(is.na(Rec_subsidio_pais),0,Rec_subsidio_pais),
            #Subsidios laborales
            sub_alim = ifelse(P6585s1==1,1,0), #Recibio auxilio o subsidio por alimentacion
            sub_alim = ifelse(is.na(sub_alim),0,sub_alim),
            sub_transp = ifelse(P6585s2==1,1,0), #Recibio subsidio de transporte
            sub_transp = ifelse(is.na(sub_transp),0,sub_transp),
            sub_famil = ifelse(P6585s3==1,1,0), #Recibio subsidio familiar
            sub_famil = ifelse(is.na(sub_famil),0,sub_famil),
            sub_educ = ifelse(P6585s4==1,1,0), #Recibio subsidio educacion
            sub_educ = ifelse(is.na(sub_educ),0,sub_educ),
            
            #Horas trabajadas
            P6800 = ifelse(is.na(P6800),0,P6800), #Horas laborales
        
        #Variables de ocupacion
        
            #Ocupacion
            Ocupacion = ifelse(is.na(P6430),P7350,P6430),#pone la ocupacion de su ultimo trabajo si tiene NA
            Ocupacion = ifelse(is.na(P6430),0,P6430),
            Oficio = ifelse(is.na(Oficio),0,Oficio), 
            #Segunda actividad economica
            Segundo_trabajo = ifelse(P7040==1,1,0), #dummy si tiene segundo trabajo
            Segundo_trabajo = ifelse(is.na(Segundo_trabajo),0,Segundo_trabajo),
            Nivel_formalidad = ifelse(is.na(P6870),0,P6870),
            #utiliza transporte parar ir al trabajo
            uti_transport = ifelse(P6610==1,1,0), 
            uti_transport = ifelse(is.na(uti_transport),0,uti_transport))
      }  
  
      #Hacer el procedimiento con ambas bases
      train_personas <- pre_process_personas(train_personas)
        names(train_personas)
      test_personas <- pre_process_personas(test_personas)
        names(test_personas)

    
  #4.4 Agregar base a nivel de hogar -----------------------------------------

      personas_nivel_hogar <- function(data,...){
          data <- data %>% 
            group_by(id)%>%
            summarize(nmujeres=sum(Mujer,na.rm=TRUE),
                      nmenores_6=sum(Menor_6,na.rm=TRUE),
                      adultos=sum(adultos,na.rm=TRUE),
                      ntrabajo_menores=sum(Trabajo_menores,na.rm = T),
                      maxEducLevel=max(EducLevel,na.rm=TRUE),
                      nocupados=sum(Oc,na.rm=TRUE),
                      ndesempleados= sum(Des,na.rm=TRUE),
                      ninac= sum(Ina,na.rm=TRUE),
                      npet= sum(Pet,na.rm=TRUE),
                      nincapacitados=sum(incapacitado,na.rm = T),
                      nsubsidios = sum(Rec_subsidio_pais,na.rm = T),
                      rsubsidiado = sum(Reg_subs_salud,na.rm = T))
        }
        
        train_personas_hogar <- personas_nivel_hogar(train_personas)
          names(train_personas_hogar)
        test_personas_hogar <- personas_nivel_hogar(test_personas)  
          names(test_personas_hogar)
        
      
  #4.5. Crear base de variables a nivel jefe de hogar ------------------------
          
        personas_jefe_hogar <- function(data,...){
          data <- data %>% 
            filter(H_Head==1) %>% 
            select(id,Mujer,EducLevel,Oc, #caracteristicas socioeconomicas
                   Afiliado_SS,Reg_subs_salud, Cot_pension, #afilicacion y pension
                   exper_ult_trab, #Experiencia laboral
                   Rec_alimento, Rec_vivienda, Rec_otros, Primas, Bonificaciones, #Otros ingresos
                   rec_gub, rec_hp_res,rec_hp_nores, 
                   sub_alim,sub_transp,sub_famil,sub_educ,#Subsidios laborales
                   Rec_subsidio_pais, #Subsidios del pais
                   Nivel_formalidad,Oficio,Ocupacion,Segundo_trabajo)%>% 
            rename(Head_Mujer=Mujer,
                   Head_EducLevel=EducLevel,
                   Head_ocupado=Oc,
                   Head_Afiliado_SS=Afiliado_SS,
                   Head_Reg_subs_salud=Reg_subs_salud,
                   Head_Cot_pension=Cot_pension,
                   Head_exper_ult_trab=exper_ult_trab,
                   Head_Rec_alimento=Rec_alimento,
                   Head_Rec_vivienda=Rec_vivienda,
                   Head_Rec_otros=Rec_otros,
                   Head_Primas = Primas,
                   Head_Bonificaciones = Bonificaciones,
                   Head_rec_gub = rec_gub,
                   Head_rec_hp_res = rec_hp_res,
                   Head_rec_hp_nores = rec_hp_nores,
                   Head_sub_alim = sub_alim,
                   Head_sub_transp = sub_transp,
                   Head_sub_famil = sub_famil,
                   Head_sub_educ = sub_educ,
                   Head_Rec_subsidio_pais=Rec_subsidio_pais,
                   Head_Nivel_formalidad=Nivel_formalidad,
                   Head_Oficio = Oficio,
                   Head_Ocupacion = Ocupacion,
                   Head_Segundo_trabajo = Segundo_trabajo)
        }
        
        
        train_personas_hogar <- left_join(train_personas_hogar,personas_jefe_hogar(train_personas),by = "id")
          names(train_personas_hogar)
        test_personas_hogar <- left_join(test_personas_hogar,personas_jefe_hogar(test_personas),by = "id")
          names(train_personas_hogar)
          
     
#------------------------------------------------------------------------------#
# 5.                     PROCESAR BASES DE HOGARES
#------------------------------------------------------------------------------#
    
          
  #5.1. Dejar las variables que se encuentran en ambas bases---------------------
          
    train_hogares <- train_hogares[,c(colnames(test_hogares),"Pobre")]
    names(train_hogares) #Mirar las variables que quedaron
    
  #5.2. Renombras y crear nuevas variables -------------------------------------
    
    pre_limp_hogares <- function(data,...){
      data <- data %>% 
        rename(n_cuartos = P5000,             #Cuartos en total disponible en el hogar
               n_cuartos_duermen = P5010,     #Cuarto donde duermen las personas del hogar
               tipo_vivienda = P5090,         #Si la vivienda en propia, arredandada, entre otras.
               pagm_armort = P5100,           #Pago mensual por cuota de amortizacion
               pagm_arriendo_est = P5130,     #Estimacion de pago mensual de arriendo
               pagm_arriendo = P5140) %>%        #Pago mensual de arriendo
        mutate(per_dor = Nper/n_cuartos_duermen, #Personas por cuarto
               hacinamiento =  ifelse(per_dor > 3, 1, 0)) #Si duermen mas de tres personas en el mismo cuarto estan en hacinamiento
    }
    
    train_hogares <- pre_limp_hogares(train_hogares)
      names(train_hogares)
    test_hogares <- pre_limp_hogares(test_hogares)
      names(test_hogares)
    
#------------------------------------------------------------------------------#
#  6.             PEGAR BASES DE PERSONAS A NIVEL HOGAR 
#------------------------------------------------------------------------------#
  
  #6.1. Pegar bases de personas a nivel de hogar y solo informacion de jefe de hogar
      
     n_train_hogares <- left_join(train_hogares,train_personas_hogar,by="id")
     n_test_hogares <- left_join(test_hogares,test_personas_hogar,by="id")
      
#------------------------------------------------------------------------------#
#  7.             PEGAR BASES DE PERSONAS A NIVEL HOGAR 
#------------------------------------------------------------------------------#     
     
#7.1. Pegar las bases creadas de nivel persona-hogar con la base de hogar
     
     pre_process_hogares <- function(data,...){
      data <- data %>% 
        
        #modificar variables
        mutate(
          Dominio=factor(Dominio),
          Head_Oficio=factor(Head_Oficio),
          Head_Ocupacion=factor(Head_Ocupacion),
          tipo_vivienda=factor(tipo_vivienda), #factor de ocupacion de vivienda
          Head_EducLevel = factor(Head_EducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
          maxEducLevel=factor(maxEducLevel,levels=c(0:6), labels=c("Ns",'Ninguno', 'Preescolar','Primaria', 'Secundaria','Media', 'Universitaria')),
          Departamento = factor(Depto),
          Cabecera = ifelse(Clase==1,1,0), 
          Pago_Arriendo = ifelse(is.na(pagm_arriendo),pagm_arriendo_est,pagm_arriendo), #si no paga arriendo pone el valor que pagaria
          Ln_Cuota = log(pagm_armort),#Log de pago de cuota
          Ln_Cuota = ifelse(is.na(Ln_Cuota),0,Ln_Cuota), #pone 0 en NA
          Valor_Cuota = ifelse(is.na(pagm_armort),0,pagm_armort), #pone 0 en NA (valor cuota)
          Ln_Pago_arrien = log(Pago_Arriendo)) #Log de pago arriendo 
    }
    
    n_train_hogares <- pre_process_hogares(n_train_hogares)
    n_test_hogares <- pre_process_hogares(n_test_hogares)
    
#7.2. Mirar la variable de pobre despues de armado la muestra
    table(n_train_hogares$Pobre)
    table(n_train_hogares$Pobre)[2]/nrow(n_train_hogares) 
    
  
# Se eliminan de la base las variables pagm_arriendo, pagm_arriendo_est  y pagm_armort.
# Para las dos primeras, la información ya esta considerando en la variable "Pago_Arriendo".
# Para la ultima, ya se esta considerando la informacion en "Valor_Cuota"
 
    Data_Final <- function(data,...){
      data <- data %>% 
        
        #modificar variables
        select(-pagm_arriendo,-pagm_arriendo_est,-pagm_armort)
    }
    
    n_train_hogares <- Data_Final(n_train_hogares)
    n_test_hogares <- Data_Final(n_test_hogares)
    
#7.3 Ultima verificacion de NAs
    
# Tabla parar mirar el porcentaje de missings
db_miss <- skim(n_train_hogares)
Nobs= nrow(n_train_hogares) 
db_miss<- db_miss %>% filter(n_missing!= 0)
db_miss<- db_miss %>% mutate(p_missing= n_missing/Nobs) %>% arrange(-n_missing)
db_miss 
   
#7.4. Exportar bases
    # setwd(paste0(wd,"/Datos/Out")) #Directorios
    setwd(paste0(wd,"\\Base"))
    
    #Hogares
    export(n_train_hogares, "Train_hogares_final.rds") #train hogares
    export(n_test_hogares, "Test_hogares_final.rds") #test hogares
    

    