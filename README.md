# Problem-Set-2_Machine-Learning_2024

**Integrantes**
* Julieth Molano
* Diego Cuesta
* Henry Carvajal
* Jorge Ramírez

Este repositorio contiene todos los recursos necesarios para resolver el segundo Problem Set del curso de Big Data y Machine Learning del segundo semestre de 2024 en la Universidad de los Andes. La información está organizada en varias carpetas, cada una con un propósito claro, para facilitar tanto la comprensión del análisis como la replicación de los resultados obtenidos.

En primer lugar, fuera de las carpetas principales, se encuentra el archivo PDF con el enunciado del ejercicio y las consideraciones para su resolución. Además, en cada carpeta se incluyen los archivos resultantes del proceso realizado para resolver el problema de clasificación de la pobreza.

Respecto a las carpetas, contienen la siguiente información:

* Base: Esta carpeta almacena las bases de datos utilizadas durante el taller. Se encuentran tanto las bases de entrenamiento como las de testeo, resultado del proceso de limpieza y transformación de los datos, las cuales son el soporte principal para los modelos. En las subcarpetas están las bases de datos originales dispuestas en la plataforma Kaggle, las cuales fueron procesadas posteriormente. También se incluye la base rebalanceada mediante la metodología SMOTE, utilizada para la estimación del modelo.

* Códigos: En esta carpeta están los scripts utilizados para generar los resultados. Dado que para solucionar el problema de clasificación fue necesario probar diferentes metodologías, cada archivo refleja el enfoque utilizado. Es importante destacar que antes de ejecutar cualquier otro código, se deben revisar los archivos "0. Script_Base.R", "1. Limpieza_datos.R", y "Estadísticas_descriptivas.R", ya que estos scripts realizan el proceso de limpieza y selección de los datos, siendo esenciales para los demás análisis.

* Gráficas: Aquí se almacenan las visualizaciones que ayudaron en el análisis de la información y permitieron evaluar visualmente los resultados obtenidos durante el taller.

* Output: Esta carpeta contiene los archivos .csv que fueron cargados a la plataforma de Kaggle para el cálculo del F1-Score. Cada archivo se almacena en una carpeta con el nombre del algoritmo que fue utilizado. 
