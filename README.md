# TFG_Codigo
Código en R para el Trabajo de Fin de Grado.

# Requisitos
Este proyecto está desarrollado en el lenguaje de programación **R**. Para poder ejecutar correctamente el script de modelado, es necesario tener instaladas las siguientes librerías:
- car
- caret
- corrplot
- dplyr
- fastshap
- ggdist
- ggplot2
- kernlab
- lsr
- mice
- MLmetrics
- moments
- nnet
- nortest
- patchwork
- pROC
- ranger
- recipes
- rpart
- shapviz
- themis
- tidymodels
- tidyr
- tidyverse
- VIM

Para instalar todas las dependencias necesarias:
```R
install.packages(c("caret", "corrplot", "dplyr", "fastshap", "ggdist", "ggplot2", "kernlab", "lsr", "mice", "MLmetrics", "moments", "nnet", "nortest", "patchwork", "pROC", "ranger", "recipes", "rpart", "shapviz", "themis", "tidymodels", "tidyr", "tidyverse", "VIM"))
```
# Ejecución
1. Descargar la base de datos: healthcare-dataset-stroke-data.xlsx .
2. Abrir R/RStudio e importar los datos descargados y guardados previamente.
3. Ejecutar el código TFG.

# Notas
Inicialmente la base de datos contaba con valores faltantes por lo que es posible que al inicio de la ejecución aparezca un Warning avisando de ello.

El código cuenta con unos tiempos de ejecución de aproximadamente 40 minutos.
