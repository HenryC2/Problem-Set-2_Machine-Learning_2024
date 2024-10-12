#------------------------------------------------------------------------------#
#------------------------ CODIGO BASE - PROBLEM SET 2 -------------------------#
#------------------------------------------------------------------------------#

# El presente codigo permite:
# 1: Cambiar el directorio entre cada uno de los colaboradores del proyecto
# 2: Correr cada uno de los scripts utilizados en la resolucion del Problem set 2.

# 0. Se borra la memoria y se cargan los paquetes ------------------------------
rm(list = ls())   # Borra la memoria

# Se cargan los paquetes de interes
library(pacman)
p_load(rio,       # importacion/exportacion de datos
       tidyverse, # datos ordenados (ggplot y Tidyverse)
       skimr,     # datos de resumen
       visdat,    # visualizacion de datos faltantes
       corrplot,  # graficos de correlacion
       stargazer, # tablas/salida a TEX.
       rvest,     # web-scraping
       readr,     # importar CSV
       writexl,   # exportar Excel
       boot,      # bootstrapping
       ggpubr,    # extensiones de ggplot2
       WVPlots,   # graficos de variables ponderadas
       patchwork, # para combinar graficos
       gridExtra, # para combinar graficos
       ggplot2,   # graficos
       caret,     # para evaluacion de modelos predictivos
       data.table,# para manipulacion de datos
       glmnet,
       smotefamily, #remuestreo SMOTE
       dplyr,
       dummy,    # crear dummys
       Metrics,  # evaluation metrics for ML
       MLeval,   # Machine Learning Model Evaluation
       pROC,
       ROSE,     #remuestreo ROSE
       ranger,   #random forest
       xgboost)  #xgboosting


# 1. Definicion del directorio -------------------------------------------------

ifelse(grepl("HP", getwd()), # Diego
       wd <- "C:\\Users\\HP\\OneDrive - Universidad Nacional de Colombia\\Documentos\\Diego\\PEG\\2024-2\\Machine learning\\Repositorios\\Problem Set 2\\Problem-Set-2_Machine-Learning_2024",
       ifelse(grepl("Usuario", getwd()), # Julieth1
              wd <- "C:\\Users\\Usuario\\OneDrive - Universidad de los andes\\Escritorio\\Taller 2 Big data",
              ifelse(grepl("Usuario", getwd()), # Julieth2
                     wd <- "C:\\Users\\hncar\\Documents\\GitHub\\Problem-Set-1_Machine-Learning_2024",
                     ifelse(grepl("C:\\Users\\User", getwd()),  # Henry
                            wd <- "C:\\Users\\User\\OneDrive - Universidad de los Andes\\Big Data y Machine Learning\\Problem_set_1\\Problem_set_1",
                            ifelse(grepl("\\Users\\aleja\\", getwd()), 
                                   wd <- "Directorio",  # Jorge
                                   ifelse(grepl("Steven Ramirez", getwd()), 
                                          wd <- "",
                                          wd <- "otro_directorio"))))))



