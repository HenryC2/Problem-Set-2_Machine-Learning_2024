#------------------------------------------------------------------------------#
#------------------------ ESTADISTICAS DESCRIPLTIVAS---------------------------#
#------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------
# 1. Cargar base de datos ------------------------------------------------------
#-------------------------------------------------------------------------------


#Cargar base de datos
setwd(paste0(wd,"/Base"))
Train <- import(file = "Train_hogares_final.rds")
str(Train)
Test <- import(file = "Test_hogares_final.rds")
str(Test)

#Mantener variables de interes
Train_n <- Train %>% 
  select(id, Pobre,
         tipo_vivienda,
         n_cuartos,
         Cabecera,
         hacinamiento,
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
         adultos,
         nsubsidios,
         ndesempleados,
         nincapacitados) %>%
  mutate(Pobre = ifelse(Pobre == "Yes", 1, 0),
         Arriendo = ifelse(tipo_vivienda == 3, 1 ,0) 
  )
names(Train_n)



#Correlacion de variables ----------------------------------------------------*

  
  #Mantener variables
des_vars <- c("Pobre","tipo_vivienda", "Cabecera", "maxEducLevel", "Head_Mujer",
              "Head_EducLevel", "Head_Ocupacion", "Head_Cot_pension", "Head_ocupado", 
              "Head_Afiliado_SS", "Departamento", "Dominio")
Train_n <- Train_n %>% mutate_at(des_vars, as.factor)
  
subset_data <- Train_n[, c("Pobre", "Cabecera", "Nper","Dominio", "n_cuartos", 
                           "hacinamiento","maxEducLevel", "nmenores_6", 
                           "nincapacitados", "Head_Mujer", "rsubsidiado",
                           "Head_Cot_pension", "ndesempleados", "Head_Afiliado_SS", 
                           "Head_ocupado","Head_EducLevel", "adultos")]



#3. Calcular la matriz de correlación para esas variables
setwd(paste0(wd2,"/Graficas"))

png("graf_corr_var.png") # Formato grafica
subset_data_num <- subset_data[sapply(subset_data, is.numeric)]

# Calcular la matriz de correlación
cor_matrix <- cor(subset_data_num, use = "complete.obs")
print(cor_matrix)

# Visualizar la matriz de correlación
corrplot(cor_matrix,
         tl.cex = 0.8,               # Tamaño de los labels
         tl.col = "black",           # Color de los labels (negro en este caso)
         tl.srt = 90,                # Rotar las etiquetas 90 grados
         addCoef.col = "black",      # Color de los coeficientes numéricos
         number.cex = 0.7)           # Tamaño de los números que muestran los coeficientes

# Finalizar la gráfica y cerrar el dispositivo gráfico
dev.off()




#Tipo de vivienda -------------------------------------------------------------#

# Calcular porcentajes por tipo de vivienda y condición de pobreza
setwd(paste0(wd2,"/Graficas"))
png("tipo_vivienda.png") # Formato grafica
datos_porcentaje <- Train_n %>%
  group_by(Pobre, tipo_vivienda) %>%
  summarise(count = n()) %>%
  mutate(porcentaje = count / sum(count) * 100)

# Crear la gráfica
a <- ggplot(datos_porcentaje, aes(x = tipo_vivienda, y = porcentaje, fill = Pobre)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
            position = position_dodge(width = 1), 
            vjust = 0.5, color = "black", size = 3, angle = 0) +
  scale_x_discrete(labels = c("1" = "Propia,\ntotalmente \npagada", 
                              "2" = "Propia, la\nestán \npagando", 
                              "3" = "En arriendo\no subarriendo",
                              "4" = "En usufructo",
                              "5" = "Posesión\nsin título", 
                              "6" = "Otra")) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 20)) + # Ajustar escala de Y
  scale_fill_manual(values = c("1" = "#87CEFA", "0" = "#CFCFCF"), 
                    labels = c("No Pobre", "Pobre")) + # Colores personalizados
  labs(title = "",
       x = "Tipo de Vivienda",
       y = "Porcentaje",
       fill = "Condición de Pobreza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() # Hacer la gráfica horizontal
a
dev.off()



# Por clase -------------------------------------------------------------------#

setwd(paste0(wd2,"/Graficas"))
png("Clase.png") # Formato grafica
datos_porcentaje2 <- Train_n %>%
  group_by(Pobre, Cabecera) %>%
  summarise(count = n()) %>%
  mutate(porcentaje2 = count / sum(count) * 100)

# Crear la gráfica
b <- ggplot(datos_porcentaje2, aes(x = Cabecera, y = porcentaje2, fill = Pobre)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(porcentaje2, 1), "%")), 
            position = position_dodge(width = 0.6), 
            vjust = -0.5, color = "black", size = 3, angle = 0) +
  scale_x_discrete(labels = c("1" = "Cabecera", 
              "0" = "Rural")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) + # Ajustar escala de Y
  scale_fill_manual(values = c("1" = "#87CEFA", "0" = "#CFCFCF"), 
                    labels = c("No Pobre", "Pobre")) + # Colores personalizados
  labs(title = "",
       x = "Zona",
       y = "Porcentaje",
       fill = "Condición de Pobreza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) 
b
dev.off()

# Educacion ------------------------------------------------------------------#


setwd(paste0(wd2,"/Graficas"))
png("Educacion.png") # Formato grafica
datos_porcentaje3 <- Train_n %>%
  group_by(Pobre, maxEducLevel) %>%
  summarise(count = n()) %>%
  mutate(porcentaje3 = count / sum(count) * 100)

# Crear la gráfica
c <- ggplot(datos_porcentaje3, aes(x = maxEducLevel, y = porcentaje3, fill = Pobre)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(porcentaje3, 1), "%")), 
            position = position_dodge(width = 0.6), 
            vjust = 0.5, color = "black", size = 3, angle = 0) +
  scale_x_discrete(labels = c("1" = "Cabecera", 
                              "0" = "Rural")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) + # Ajustar escala de Y
  scale_fill_manual(values = c("1" = "#87CEFA", "0" = "#CFCFCF"), 
                    labels = c("No Pobre", "Pobre")) + # Colores personalizados
  labs(title = "",
       x = "Máximo nivel educativo",
       y = "Porcentaje",
       fill = "Condición de Pobreza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  coord_flip() # Hacer la gráfica horizontal
c
dev.off()


# Jefatura de hogar -----------------------------------------------------------#


setwd(paste0(wd2,"/Graficas"))
png("Jefatura.png") # Formato grafica
datos_porcentaje4 <- Train_n %>%
  group_by(Pobre, Head_Mujer) %>%
  summarise(count = n()) %>%
  mutate(porcentaje4 = count / sum(count) * 100)

# Crear la gráfica
d <- ggplot(datos_porcentaje4, aes(x = Head_Mujer, y = porcentaje4, fill = Pobre)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(porcentaje4, 1), "%")), 
            position = position_dodge(width = 0.6), 
            vjust = -0.5, color = "black", size = 3, angle = 0) +
  scale_x_discrete(labels = c("1" = "Jefe mujer", 
                              "0" = "Jefe hombre")) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 20)) + # Ajustar escala de Y
  scale_fill_manual(values = c("1" = "#87CEFA", "0" = "#CFCFCF"), 
                    labels = c("No Pobre", "Pobre")) + # Colores personalizados
  labs(title = "",
       x = "Jefatura",
       y = "Porcentaje",
       fill = "Condición de Pobreza") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) 
d
dev.off()



# Estadísticas datos de la base SMOTE -------------------------------------

setwd(paste0(wd,"\\Base"))
Train_estadisticas <- import(file = "Train_hogares_final.rds") %>% 
  select(-Li,-Lp,-Fex_c,-Fex_dpto,-Valor_Cuota,-Ln_Cuota,-Ln_Pago_arrien,
         -Depto, -hacinamiento, -Cabecera)

# Seleccionamos solo columnas que no sean factores
base_origen <- Train_estadisticas[, sapply(Train_estadisticas, is.numeric) & 
                         !(grepl("^Head", names(Train_estadisticas)) & names(Train_estadisticas) != "Head_edad")]


columnas_numeric <- names(base_origen)



# En la base SMOTE
setwd(paste0(wd,"\\Base\\Base_Elastic_Net"))
Train_3_SMOTE = import(file = "Train_3_SMOTE.rds")

base_modelo <- Train_3_SMOTE[, columnas_numeric]


# Renombramos las variable
base_origen <- base_origen %>%
  rename(
    `Clase del hogar` = Clase,
    `Número de cuartos` = n_cuartos,
    `Número de cuartos donde se duerme` = n_cuartos_duermen,
    `Número de personas en el hogar` = Nper,
    `Número de personas en la unidad de gasto` = Npersug,
    `Personas por dormitorio` = per_dor,
    `Número de mujeres` = nmujeres,
    `Número de menores de 6 años` = nmenores_6,
    `Número de adultos` = adultos,
    `Número de menores trabajando` = ntrabajo_menores,
    `Número de ocupados` = nocupados,
    `Número de desempleados` = ndesempleados,
    `Número de inactivos` = ninac,
    `Número de personas en edad de trabajar` = npet,
    `Número de incapacitados` = nincapacitados,
    `Número de subsidios` = nsubsidios,
    `Régimen subsidiado` = rsubsidiado,
    `Edad del jefe de hogar` = Head_edad,
    `Pago de arriendo` = Pago_Arriendo
  )


base_modelo <- base_modelo %>%
  rename(
    `Clase del hogar` = Clase,
    `Número de cuartos` = n_cuartos,
    `Número de cuartos donde se duerme` = n_cuartos_duermen,
    `Número de personas en el hogar` = Nper,
    `Número de personas en la unidad de gasto` = Npersug,
    `Personas por dormitorio` = per_dor,
    `Número de mujeres` = nmujeres,
    `Número de menores de 6 años` = nmenores_6,
    `Número de adultos` = adultos,
    `Número de menores trabajando` = ntrabajo_menores,
    `Número de ocupados` = nocupados,
    `Número de desempleados` = ndesempleados,
    `Número de inactivos` = ninac,
    `Número de personas en edad de trabajar` = npet,
    `Número de incapacitados` = nincapacitados,
    `Número de subsidios` = nsubsidios,
    `Régimen subsidiado` = rsubsidiado,
    `Edad del jefe de hogar` = Head_edad,
    `Pago de arriendo` = Pago_Arriendo
  )


setwd(paste0(wd,"\\Base"))
#Tabla de estadisticas
stargazer(base_origen, type = "text", title = "Estadisticas basicas de las variables en la base original")
stargazer(base_modelo, type = "text", title = "Estadisticas basicas de las variables en la base modelo")

# Genera el código LaTeX con los nombres modificados


