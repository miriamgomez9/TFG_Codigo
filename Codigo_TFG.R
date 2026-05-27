datos<- healthcare_dataset_stroke_data
str(datos)
library(dplyr)

datos_limpios <- datos %>% 
  

  mutate(across(c(gender, ever_married, work_type, Residence_type, smoking_status),
                as.factor)) %>% 
  

  mutate(across(c(hypertension, heart_disease, stroke),
                ~ factor(.x, levels = c(0, 1), labels = c("No", "Yes")))) %>% 
  

  mutate(
    age = as.numeric(age),
    avg_glucose_level = as.numeric(avg_glucose_level),
    bmi = as.numeric(na_if(bmi, "N/A"))
  )

# Revisar estructura
str(datos_limpios)
sum(duplicated(datos_limpios))

#GRÁFICOS
#VARIABLES NUMÉRICAS
library(ggplot2)
library(ggdist)
library(patchwork)

crear_univariante <- function(data, var, titulo, color) {
  media <- mean(data[[var]], na.rm = TRUE)
  mediana <- median(data[[var]], na.rm = TRUE)
  
  ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = color, alpha = 0.2) +
    geom_density(color = color, size = 1) +
    geom_vline(xintercept = media, color = "red", linetype = "dashed", size = 0.8) +
    geom_vline(xintercept = mediana, color = "darkgreen", linetype = "dotted", size = 0.8) +
    labs(title = titulo, x = "", y = "Densidad") +
    theme_minimal() +
    theme(plot.title = element_text(size = 10, face = "bold"))
}


p1 <- crear_univariante(datos_limpios, "age", "Distribución de Edad", "#2c3e50")
p2 <- crear_univariante(datos_limpios, "avg_glucose_level", "Distribución de Glucosa", "#2980b9")
p3 <- crear_univariante(datos_limpios, "bmi", "Distribución de BMI", "#8e44ad")
p4 <- crear_univariante(datos_limpios, "id", "Distribución de Id", "#E74C3C")

(p1+p2) / (p3 + p4) + 
  plot_annotation(title = "Análisis de Distribución: Variables Numéricas",
                  subtitle = "Línea roja: Media | Línea verde: Mediana")


#VARIABLES CATEGÓRICAS
library(ggplot2)

resumen_stroke <- datos_limpios %>%
  count(stroke) %>%
  mutate(prop = n / sum(n) * 100)

ggplot(resumen_stroke, aes(x = 2, y = n, fill = stroke)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y") + 
  xlim(0.5, 2.5) + 
  theme_void() +   
  scale_fill_manual(values = c("#BDC3C7", "#E74C3C")) + 
  labs(title = "Distribución variable dependiente",
       fill = "¿Sufrió Ictus?") +
  annotate("text", x = 0.5, y = 0, label = paste0("Total\n", sum(resumen_stroke$n)), 
           size = 5, fontface = "bold")




library(tidyverse)

datos_cat <- datos_limpios %>%
  select(hypertension, heart_disease, ever_married, work_type, Residence_type, gender, smoking_status) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Categoria")

ggplot(datos_cat, aes(x = Categoria, fill = Variable)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = 'count', 
            aes(label = after_stat(count)), 
            vjust = -0.5, 
            size = 3.5,
            fontface = "bold") + 
  facet_wrap(~Variable, scales = "free", ncol = 2) + 
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Distribución de Frecuencias: Factores Categóricos",
       subtitle = "Recuento exacto por cada nivel de variable",
       x = "", y = "Número de Casos") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"),
        panel.spacing = unit(1, "lines")) 



#TABLAS
calc_moda <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

tabla_numericas_final <- datos_limpios %>%
  select(age, avg_glucose_level, bmi, id) %>%
  summarise(across(everything(), list(
    Media    = ~mean(.x, na.rm = TRUE),
    Mediana  = ~median(.x, na.rm = TRUE),
    Moda     = ~calc_moda(.x),
    SD       = ~sd(.x, na.rm = TRUE),
    Varianza = ~var(.x, na.rm = TRUE)
  ), .names = "{.col}###{.fn}")) %>% 
  pivot_longer(cols = everything(), names_sep = "###", names_to = c("Variable", "Estadistico")) %>%
  pivot_wider(names_from = Estadistico, values_from = value)

print(tabla_numericas_final)

library(dplyr)
library(tidyr)


calc_moda_cat <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

tabla_cat_final <- datos_limpios %>%
  select(gender, hypertension, heart_disease, ever_married, 
         work_type, Residence_type, smoking_status, stroke) %>%
  summarise(across(everything(), list(
    Nivel_Moda      = ~as.character(calc_moda_cat(.x)),
    Frecuencia      = ~as.character(sum(.x == calc_moda_cat(.x), na.rm = TRUE)),
    Porcentaje      = ~as.character(round((sum(.x == calc_moda_cat(.x), na.rm = TRUE) / n()) * 100, 2)),
    Datos_Faltantes = ~as.character(sum(is.na(.x)))
  ), .names = "{.col}###{.fn}")) %>%
  pivot_longer(cols = everything(), names_sep = "###", names_to = c("Variable", "Estadistico")) %>%
  pivot_wider(names_from = Estadistico, values_from = value)

print(tabla_cat_final)




tabla_diagnostico_num <- datos_limpios %>%
  select(age, avg_glucose_level, bmi, id) %>%
  summarise(across(everything(), list(
    Varianza = ~var(.x, na.rm = TRUE),
    Desv_Std = ~sd(.x, na.rm = TRUE),
    
    Relacion_Ruido = ~(sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100,
    Faltantes_NAs = ~sum(is.na(.x))
  ), .names = "{.col}###{.fn}")) %>%
  pivot_longer(cols = everything(), names_sep = "###", names_to = c("Variable", "Metrica")) %>%
  pivot_wider(names_from = Metrica, values_from = value)

print(tabla_diagnostico_num)




#DETECCION DE ATIPICOS

library(dplyr)
library(tidyr)

contar_outliers <- function(x) {
  x <- x[!is.na(x)]
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lim_inf <- q1 - 1.5 * iqr
  lim_sup <- q3 + 1.5 * iqr
  return(sum(x < lim_inf | x > lim_sup))
}

tabla_outliers <- datos_limpios %>%
  select(age, avg_glucose_level, bmi, id) %>%
  summarise(across(everything(), list(
    Casos_Atipicos = ~contar_outliers(.x),
    Porcentaje = ~round((contar_outliers(.x) / n()) * 100, 2)
  ), .names = "{.col}###{.fn}")) %>%
  pivot_longer(cols = everything(), names_sep = "###", names_to = c("Variable", "Metrica")) %>%
  pivot_wider(names_from = Metrica, values_from = value)

print("--- Detección de Outliers (Criterio de Tukey) ---")
print(tabla_outliers)


#IMPUTACIÓN
library(dplyr)
library(caret)
library(VIM)
library(mice)
library(ggplot2)

set.seed(123)
datos_limpios <- datos %>%
  select(-id) %>%                 
  filter(gender != "Other")      

indice_train <- createDataPartition(datos_limpios$stroke, p = 0.8, list = FALSE)

train <- datos_limpios[indice_train, ]
test  <- datos_limpios[-indice_train, ]

prop.table(table(train$stroke))
prop.table(table(test$stroke))

set.seed(456)
train_prep_imp <- train %>% 
  mutate(bmi = as.numeric(as.character(bmi)))

train_completo <- train_prep_imp %>% filter(!is.na(bmi))

idx_eval <- createDataPartition(train_completo$stroke, p = 0.1, list = FALSE)

entrenamiento_imp <- train_completo[-idx_eval, ] 
evaluacion_imp    <- train_completo[idx_eval, ]   

valor_real_bmi <- evaluacion_imp$bmi

evaluacion_con_na <- evaluacion_imp %>% mutate(bmi = NA)

data_experimento <- rbind(entrenamiento_imp, evaluacion_con_na)
n_entrenamiento <- nrow(entrenamiento_imp)
#MEDIA
bmi_media <- mean(entrenamiento_imp$bmi, na.rm = TRUE)
imp_media <- rep(bmi_media, length(valor_real_bmi))

#MEDIANA
bmi_mediana <- median(entrenamiento_imp$bmi, na.rm = TRUE)
imp_mediana <- rep(bmi_mediana, length(valor_real_bmi))

#K-NN
data_experimento_pro <- data_experimento %>%
  mutate(
    age = as.numeric(as.character(age)),
    avg_glucose_level = as.numeric(as.character(avg_glucose_level)),
    gender = as.factor(gender),
    smoking_status = as.factor(smoking_status),
    hypertension = as.factor(hypertension),
    heart_disease = as.factor(heart_disease)
  )

temp_knn_final <- kNN(data_experimento_pro, 
                      variable = "bmi", 
                      dist_var = c("age", "avg_glucose_level", "gender", 
                                   "smoking_status", "hypertension", "heart_disease"), 
                      k = 25, 
                      imp_var = FALSE)

imp_knn_final <- temp_knn_final$bmi[(n_entrenamiento + 1):nrow(temp_knn_final)]

#MICE
data_mice_pro <- data_experimento %>%
  mutate(
    across(c(gender, hypertension, heart_disease, ever_married, work_type, 
             Residence_type, smoking_status, stroke), as.factor),
    age = as.numeric(age),
    avg_glucose_level = as.numeric(avg_glucose_level),
    bmi = as.numeric(bmi)
  )

temp_mice_pro <- mice(data_mice_pro, 
                      m = 5,           
                      method = 'pmm',  
                      maxit = 10,      
                      printFlag = FALSE)

imp_mice_list <- list()
for(i in 1:5) {
  df_temp <- complete(temp_mice_pro, i)
  imp_mice_list[[i]] <- df_temp$bmi[(n_entrenamiento + 1):nrow(df_temp)]
}
imp_mice <- rowMeans(do.call(cbind, imp_mice_list))


rmse_calc <- function(real, estimado) sqrt(mean((real - estimado)^2))
mae_calc  <- function(real, estimado) mean(abs(real - estimado))

tabla_comparativa <- data.frame(
  Metodo = c("Media", "Mediana", "K-NN", "MICE"),
  
  RMSE = c(rmse_calc(valor_real_bmi, imp_media),
           rmse_calc(valor_real_bmi, imp_mediana),
           rmse_calc(valor_real_bmi, imp_knn_final),
           rmse_calc(valor_real_bmi, imp_mice)),
  
  MAE  = c(mae_calc(valor_real_bmi, imp_media),
           mae_calc(valor_real_bmi, imp_mediana),
           mae_calc(valor_real_bmi, imp_knn_final),
           mae_calc(valor_real_bmi, imp_mice))
)

print("--- Comparativa del Desempeño de Imputación ---")
print(tabla_comparativa %>% arrange(MAE))


#GRÁFICO COMPARATIVO
resultados_grafico <- data.frame(
  Real = rep(valor_real_bmi, 4),
  Imputado = c(imp_media, imp_mediana, imp_knn_final, imp_mice),
  Metodo = rep(c("Media", "Mediana", "K-NN", "MICE (PMM)"), each = length(valor_real_bmi))
)


ggplot(resultados_grafico, aes(x = Real, y = Imputado, color = Metodo)) +
  geom_point(alpha = 0.4, size = 1.5) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) + 
  facet_wrap(~Metodo) + 
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Comparación de Métodos de Imputación (BMI)",
    subtitle = "La línea discontinua representa la predicción perfecta (Valor Real = Imputado)",
    x = "Valor Real de BMI (Observado)",
    y = "Valor Imputado (Predicho)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )



#APLICAR IMPUTACIÓN
train_pre_knn <- train %>%
  mutate(
    
    age = as.numeric(as.character(age)),
    avg_glucose_level = as.numeric(as.character(avg_glucose_level)),
    bmi = as.numeric(as.character(bmi)),
    
    across(c(gender, smoking_status, hypertension, heart_disease, 
             ever_married, work_type, Residence_type, stroke), as.factor)
  )


train_final_imputado <- kNN(train_pre_knn, 
                            variable = "bmi", 
                            dist_var = c("age", "avg_glucose_level", "gender", 
                                         "smoking_status", "hypertension", "heart_disease"), 
                            k = 25, 
                            imp_var = FALSE)

#vereficar
cat("¿Hay NAs en BMI?:", sum(is.na(train_final_imputado$bmi)), "\n")
cat("Casos de Stroke (deben ser los originales):", sum(train_final_imputado$stroke == "1"), "\n")
cat("Número total de filas (debe ser el mismo que train):", nrow(train_final_imputado), "\n")



test_ready <- test %>%
  mutate(
    age = as.numeric(as.character(age)),
    avg_glucose_level = as.numeric(as.character(avg_glucose_level)),
    bmi = as.numeric(as.character(bmi)),
    across(c(gender, smoking_status, hypertension, heart_disease, 
             ever_married, work_type, Residence_type, stroke), as.factor)
  )


temp_test_imp <- rbind(train_final_imputado, test_ready)

n_train <- nrow(train_final_imputado)

#Imputación KNN
imp_test_full <- kNN(temp_test_imp, 
                     variable = "bmi", 
                     dist_var = c("age", "avg_glucose_level", "gender", 
                                  "smoking_status", "hypertension", "heart_disease"), 
                     k = 25, 
                     imp_var = FALSE)


test_final <- imp_test_full[(n_train + 1):nrow(imp_test_full), ]

#comprobación final de la imputación
cat("Casos de Stroke en Train Final:", sum(train_final_imputado$stroke == "1"), "\n")
cat("Casos de Stroke en Test Final:", sum(test_final$stroke == "1"), "\n")
cat("¿Quedan NAs en Test?:", sum(is.na(test_final$bmi)), "\n")




#PREPROCESAMIENTO
library(tidymodels)

train_final_imputado <- train_final_imputado %>%
  mutate(across(c(age, avg_glucose_level, bmi), as.numeric),
         stroke = as.factor(stroke))

test_final <- test_final %>%
  mutate(across(c(age, avg_glucose_level, bmi), as.numeric),
         stroke = as.factor(stroke))

receta_stroke <- recipe(stroke ~ ., data = train_final_imputado) %>%
  step_rm(any_of("id")) %>%

  step_relevel(work_type, ref_level = "Private") %>%
  

  step_BoxCox(avg_glucose_level, bmi) %>%

  step_normalize(age, avg_glucose_level, bmi) %>%

  step_dummy(all_nominal_predictors(), -stroke)


pipa_preparada <- prep(receta_stroke, training = train_final_imputado)


train_preprocesado <- bake(pipa_preparada, new_data = NULL)
test_preprocesado  <- bake(pipa_preparada, new_data = test_final)


train_preprocesado <- train_preprocesado %>% select(-any_of("gender_Other"))
test_preprocesado  <- test_preprocesado %>% select(-any_of("gender_Other"))


cat("--- COMPARATIVA DE RECUENTOS (STROKE) ---\n")
cat("Antes de prep (Imputado):", table(train_final_imputado$stroke)[2], "ictus\n")
cat("Después de bake (Preprocesado):", table(train_preprocesado$stroke)[2], "ictus\n")

if(nrow(train_final_imputado) == nrow(train_preprocesado)) {
  print("ÉXITO: La cantidad de registros es idéntica.")
} else {
  print("EROR: Ha habido una pérdida de registros.")
}

print(dim(train_preprocesado))



#ANÁLISIS TRAS EL PROCESAMIENTO
library(ggplot2)
library(patchwork)
library(dplyr)
library(moments)
library(nortest)
library(tidyr)

theme_tfg <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, color = "grey30", hjust = 0.5),
      axis.title = element_text(size = 9, face = "italic"),
      axis.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text = element_text(size = 10, face = "bold"),
      legend.position = "none"
    )
}


df_proc <- as.data.frame(train_preprocesado)

plot_comparativo <- function(data_orig, data_proc, var_name, titulo_label) {
  
  
  p1 <- ggplot(data_orig, aes(x = .data[[var_name]])) +
    geom_density(fill = "#4682B4", alpha = 0.2, color = "#4682B4", linewidth = 0.8) +
    geom_vline(aes(xintercept = mean(.data[[var_name]], na.rm = TRUE)), 
               color = "#4682B4", linetype = "dashed") +
    labs(subtitle = "Distribución Original", x = "Unidades Reales", y = "Densidad") +
    theme_tfg()
  

  p2 <- ggplot(data_proc, aes(x = .data[[var_name]])) +
    geom_density(fill = "#2E8B57", alpha = 0.2, color = "#2E8B57", linewidth = 0.8) +
    geom_vline(aes(xintercept = 0), color = "#2E8B57", linetype = "dashed") +
    labs(subtitle = "Post-Preprocesamiento (Box-Cox + Scaled)", 
         x = "Z-Score (Standardized)", y = "Densidad") +
    theme_tfg()

  (p1 / p2) + plot_annotation(title = titulo_label)
}


panel_age <- plot_comparativo(train_final_imputado, df_proc, "age", "VALIDACIÓN") + 
  ggtitle("Age") +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold", color = "black"))

panel_glucosa <- plot_comparativo(train_final_imputado, df_proc, "avg_glucose_level", "VALIDACIÓN") + 
  ggtitle("Avg_glucose") +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold", color = "black"))

panel_bmi <- plot_comparativo(train_final_imputado, df_proc, "bmi", "VALIDACIÓN") + 
  ggtitle("BMI") +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold", color = "black"))


(panel_age | panel_glucosa | panel_bmi) + 
  plot_annotation(
    title = "Comparación de las Distribuciones: Original vs. Preprocesado",
    caption = "Nota: La línea discontinua representa la media (Z=0 en datos procesados).",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 15)),
      plot.caption = element_text(size = 9, face = "italic", hjust = 1)
    )
  )


lambdas_calculados <- tidy(pipa_preparada, number = 1) 

print("--- VALORES DE LAMBDA ESTIMADOS (BOX-COX) ---")
print(lambdas_calculados)


skew_orig_glu <- skewness(train_final_imputado$avg_glucose_level, na.rm = TRUE)
skew_orig_bmi <- skewness(train_final_imputado$bmi, na.rm = TRUE)
skew_orig_age <- skewness(train_final_imputado$age, na.rm = TRUE)


skew_proc_glu <- skewness(train_preprocesado$avg_glucose_level)
skew_proc_bmi <- skewness(train_preprocesado$bmi)
skew_proc_age <- skewness(train_preprocesado$age)


tabla_validacion_math <- data.frame(
  Variable = c("Glucosa", "BMI", "Edad"),
  Skewness_Original = c(skew_orig_glu, skew_orig_bmi, skew_orig_age),
  Skewness_Post_BoxCox = c(skew_proc_glu, skew_proc_bmi, skew_proc_age)
)

print(tabla_validacion_math)





vars_numericas_post <- train_preprocesado %>% 
  select(age, avg_glucose_level, bmi)

resultados_lilliefors_post <- data.frame(
  Variable = names(vars_numericas_post),
  Estadistico_D = sapply(vars_numericas_post, function(x) lillie.test(x)$statistic),
  P_Valor = sapply(vars_numericas_post, function(x) lillie.test(x)$p.value)
)
options(scipen = 999)

print("TEST DE LILLIEFORS (POST-PROCESAMIENTO)")
print(resultados_lilliefors_post)


vars_numericas_pre <- train_final_imputado %>% 
  select(age, avg_glucose_level, bmi)

resultados_lilliefors_pre <- data.frame(
  Variable = names(vars_numericas_pre),
  Estadistico_D = sapply(vars_numericas_pre, function(x) lillie.test(x)$statistic),
  P_Valor = sapply(vars_numericas_pre, function(x) lillie.test(x)$p.value)
)

print("TEST DE LILLIEFORS (DATOS ORIGINALES IMPUTADOS)")
print(resultados_lilliefors_pre)


#ESTDANDARIZACIÓN
validar_zscore <- function(data, vars) {
  resultados <- data.frame()
  
  for(v in vars) {
    vec <- data[[v]]
    

    t_test <- t.test(vec, mu = 0)
    

    n <- length(vec)
    est_chi <- (n - 1) * var(vec) / 1
    p_var <- 2 * min(pchisq(est_chi, df = n - 1), 1 - pchisq(est_chi, df = n - 1))
    
    resultados <- rbind(resultados, data.frame(
      Variable = v,
      Media = mean(vec),
      p_valor_Media = t_test$p.value,
      SD = sd(vec),
      p_valor_SD = p_var
    ))
  }
  return(resultados)
}

tabla_audit <- validar_zscore(train_preprocesado, c("age", "avg_glucose_level", "bmi"))

print("AUDITORÍA ESTADÍSTICA DE ESTANDARIZACIÓN")
print(tabla_audit)


auditar_distribucion <- function(data_orig, data_proc, var_name) {
  

  v_orig <- na.omit(data_orig[[var_name]])
  v_proc <- data_proc[[var_name]]
  

  sk_o <- skewness(v_orig)
  sk_p <- skewness(v_proc)
  

  kt_o <- kurtosis(v_orig)
  kt_p <- kurtosis(v_proc)
  

  set.seed(123) 
  sh_o <- shapiro.test(sample(v_orig, min(length(v_orig), 5000)))$statistic
  sh_p <- shapiro.test(sample(v_proc, min(length(v_proc), 5000)))$statistic
  
  data.frame(
    Variable = var_name,
    Métrica = c("Skewness (Simetría)", "Curtosis (Aplastamiento)", "Shapiro-W (Proximidad)"),
    Objetivo = c("0", "3", "1"),
    Original = c(sk_o, kt_o, sh_o),
    Procesado = c(sk_p, kt_p, sh_p)
  )
}

auditoria_final <- bind_rows(
  auditar_distribucion(train_final_imputado, train_preprocesado, "avg_glucose_level"),
  auditar_distribucion(train_final_imputado, train_preprocesado, "bmi"),
  auditar_distribucion(train_final_imputado, train_preprocesado, "age")
)

print("--- TABLA DE AUDITORÍA DE NORMALIZACIÓN ---")
print(auditoria_final)


atipicos_final <- train_preprocesado %>%
  summarise(across(c(age, avg_glucose_level, bmi), 
                   ~sum(abs(.x) > 3)))

print("Atípicos restantes (Valores fuera de 3 desviaciones típicas)")
print(atipicos_final)

test_estandarizacion <- train_preprocesado %>%
  select(age, avg_glucose_level, bmi) %>%
  summarise(across(everything(), list(
    Media_Post = ~round(mean(.x), 4),
    SD_Post    = ~round(sd(.x), 4),
    Min        = ~round(min(.x), 2),
    Max        = ~round(max(.x), 2)
  ), .names = "{.col}###{.fn}")) %>%
  pivot_longer(cols = everything(), names_sep = "###", names_to = c("Variable", "Estadistico")) %>%
  pivot_wider(names_from = Estadistico, values_from = value)

print("TEST DE VALIDACIÓN DE ESTANDARIZACIÓN (Z-SCORE)")
print(test_estandarizacion)



library(nortest)



variables_num <- c("age", "avg_glucose_level", "bmi")

for (var in variables_num) {
  cat("ANÁLISIS DE NORMALIDAD PARA:", toupper(var), "\n")

  columna_datos <- train_preprocesado[[var]]

  if (length(columna_datos) > 5000) {
    set.seed(123)
    datos_shapiro <- sample(columna_datos, 5000)
    cat("Nota: Muestra > 5000, usando submuestra para Shapiro.\n")
  } else {
    datos_shapiro <- columna_datos
  }
  
  sw <- shapiro.test(datos_shapiro)
  cat("\n[Test de Shapiro-Wilk]\n")
  cat("Estadístico W:", round(sw$statistic, 4), " | p-valor:", format.pval(sw$p.value), "\n")
  
  if (sw$p.value > 0.05) {
    cat("CONCLUSIÓN: No se rechaza H0. Los datos parecen seguir una distribución normal.\n")
  } else {
    cat("CONCLUSIÓN: Se rechaza H0. Los datos NO siguen una distribución normal.\n")
  }
  
  # --- 2. TEST DE LILLIEFORS ---
  lf <- lillie.test(columna_datos)
  cat("\n[Test de Lilliefors]\n")
  cat("Estadístico D:", round(lf$statistic, 4), " | p-valor:", format.pval(lf$p.value), "\n")
  
  if (lf$p.value > 0.05) {
    cat("CONCLUSIÓN: No se rechaza H0. Los datos parecen seguir una distribución normal.\n")
  } else {
    cat("CONCLUSIÓN: Se rechaza H0. Los datos NO siguen una distribución normal.\n")
  }

}



#Análisis bivariante con la variable objetivo
library(epitools)
library(dplyr)


vars_cualitativas <- c(
  "hypertension_X1", 
  "heart_disease_X1", 
  "gender_Male", 
  "ever_married_Yes",
  "work_type_Private", 
  "work_type_Self.employed", 
  "work_type_children",      
  "work_type_Govt_job",  
  "work_type_Never_worked",
  "Residence_type_Urban", 
  "smoking_status_never.smoked", 
  "smoking_status_smokes",    
  "smoking_status_formerly.smoked", 
  "smoking_status_Unknown"
)


test_chi_cuadrado <- function(var, df) {
  

  if(!var %in% colnames(df)) return(NULL)
  

  tabla <- table(as.factor(df[[var]]), as.factor(df$stroke))
  

  chi_test <- chisq.test(tabla, simulate.p.value = TRUE, B = 2000)
  

  return(data.frame(
    Variable = var,
    Chi_Estadistico = round(chi_test$statistic, 2),
    P_Value = chi_test$p.value,
    Resultado = ifelse(chi_test$p.value < 0.05, "Asociación Significativa", "No Significativa")
  ))
}


resumen_chi_list <- lapply(vars_cualitativas, test_chi_cuadrado, df = train_preprocesado)
resumen_chi <- do.call(rbind, resumen_chi_list)


resumen_chi <- resumen_chi[order(resumen_chi$P_Value), ]


print("TEST DE CHI-CUADRADO: ASOCIACIÓN CON STROKE")
print(resumen_chi)


library(tidyr)
library(purrr)


obtener_tabla_contingencia <- function(var, df) {
  if(!var %in% colnames(df)) return(NULL)
  

  tab <- table(Factor = df[[var]], Stroke = df$stroke)
  

  data.frame(
    Variable = var,
    Categoria = c("No (0)", "Sí (1)"),
    No_Ictus = c(tab[1,1], tab[2,1]), 
    Si_Ictus = c(tab[1,2], tab[2,2]) 
  ) %>%
    mutate(
      Total = No_Ictus + Si_Ictus,
      Prevalencia_Ictus = round((Si_Ictus / Total) * 100, 2)
    )
}


tablas_detalladas <- map_df(vars_cualitativas, obtener_tabla_contingencia, df = train_preprocesado)

print("TABLAS DE CONTINGENCIA (RECUENTOS REALES)")
print(tablas_detalladas)



library(lsr) 
library(dplyr)


analisis_asociacion_completo <- function(var, df) {
  
  if(!var %in% colnames(df)) return(NULL)

  tabla <- table(as.factor(df[[var]]), as.factor(df$stroke))
  

  chi_test <- chisq.test(tabla, simulate.p.value = TRUE, B = 2000)
  

  v_cramer <- cramersV(tabla)
  

  n_no_ictus <- tabla[2, 1] 
  n_ictus    <- tabla[2, 2] 
  
  return(data.frame(
    Variable = var,
    Casos_Con_Condicion = sum(df[[var]] == 1),
    Ictus_Si = n_ictus,
    Ictus_No = n_no_ictus,
    Chi_Estadistico = round(chi_test$statistic, 2),
    P_Value = chi_test$p.value,
    V_Cramer = round(v_cramer, 4),
    Interpretacion_V = cut(v_cramer, 
                           breaks = c(-Inf, 0.1, 0.3, 0.5, Inf), 
                           labels = c("Insignificante", "Baja", "Moderada", "Alta"))
  ))
}


resumen_asociacion <- do.call(rbind, lapply(vars_cualitativas, analisis_asociacion_completo, df = train_preprocesado))

resumen_asociacion <- resumen_asociacion[order(-resumen_asociacion$V_Cramer), ]


print("--- TABLA DE CONTINGENCIA Y FUERZA DE ASOCIACIÓN (V DE CRAMÉR) ---")
print(resumen_asociacion)


#SPEARMAN
library(dplyr)
library(corrplot)

df_ml_numeric <- train_preprocesado %>% 
  mutate(stroke = as.numeric(as.character(stroke))) %>% 
  select_if(is.numeric)

matriz_corr <- cor(df_ml_numeric, method = "spearman", use = "complete.obs")

options(scipen = 999)

par(mar = c(4, 1, 1, 1)) 

corrplot(matriz_corr, 
         method = "color", 
         type = "lower",        
         order = "original",
         
         col = colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))(200),
         
         tl.pos = "ld",         
         tl.col = "black", 
         tl.cex = 0.8,          
         tl.srt = 45,           
         
         addCoef.col = "black",
         number.cex = 0.7,      
         
         cl.pos = "b",          
         cl.ratio = 0.1,
         diag = TRUE,           
         mar = c(0, 0, 0, 0)) 

title(main = "Matriz de correlación de Spearman", 
      line = -0.5, 
      cex.main = 1.2)



#VIF
library(car)

vars_cualitativas_vif <- c(
  "hypertension_X1", 
  "heart_disease_X1", 
  "gender_Male", 
  "ever_married_Yes",
  "work_type_Self.employed", 
  "work_type_children", 
  "work_type_Govt_job",
  "work_type_Never_worked", 
  "Residence_type_Urban", 
  "smoking_status_never.smoked", 
  "smoking_status_smokes", 
  "smoking_status_Unknown" 
)

formula_vif_cual <- as.formula(paste("as.numeric(as.character(stroke)) ~", paste(vars_cualitativas_vif, collapse = " + ")))

modelo_vif_cual <- lm(formula_vif_cual, data = train_preprocesado)

valores_vif_cual <- vif(modelo_vif_cual)

df_vif_cual <- data.frame(
  Variable = names(valores_vif_cual),
  VIF = valores_vif_cual
)

df_vif_cual <- df_vif_cual[order(-df_vif_cual$VIF), ]
print("FACTOR DE INFLACIÓN DE LA VARIANZA (VIF) - SOLO CUALITATIVAS")
print(df_vif_cual)



#Bivariante numéricas

library(patchwork)
library(ggplot2)
library(dplyr)

vars_numericas <- c("age", "avg_glucose_level", "bmi")
lista_graficos <- list() 

for (var in vars_numericas) {
  

  columna_datos <- train_preprocesado[[var]]
  stroke_datos <- train_preprocesado$stroke
  
  datos_temp <- data.frame(
    valor_num = as.numeric(as.character(columna_datos)),
    stroke = stroke_datos
  ) %>%
    mutate(stroke_fac = factor(stroke, labels = c("No Ictus", "Ictus"))) %>%
    filter(!is.na(valor_num))
  
  p <- ggplot(datos_temp, aes(x = stroke_fac, y = valor_num, fill = stroke_fac)) +
    geom_boxplot(alpha = 0.7, outlier.color = "red") +
    labs(title = toupper(var), 
         x = "", 
         y = "Valor") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  lista_graficos[[var]] <- p 
  
  cat("ANÁLISIS DE LA VARIABLE:", toupper(var), "\n")

  cat("\nTABLA DE ESTADÍSTICOS POR GRUPO:\n")
  
  tabla_descriptivos <- datos_temp %>%
    group_by(stroke_fac) %>%
    summarise(
      N = n(),
      Media = mean(valor_num),
      Mediana = median(valor_num),
      Desviacion_Std = sd(valor_num)
    )
  
  print(as.data.frame(tabla_descriptivos))
  
  cat("\nCONTRASTE DE HIPÓTESIS:\n")
  
  test_res <- wilcox.test(valor_num ~ stroke_fac, data = datos_temp)
  p_val <- test_res$p.value
  
  cat("P-Valor obtenido:", format.pval(p_val, digits = 4), "\n")
  
  if (p_val < 0.05) {
    cat("SIGNIFICADO: El p-valor es inferior a 0.05. Existen diferencias \n")
    cat("estadísticamente significativas en", toupper(var), "según el ictus.\n")
    cat("Esta variable es un predictor relevante.\n")
  } else {
    cat("SIGNIFICADO: El p-valor es superior a 0.05. No hay evidencia estadística \n")
    cat("suficiente para la variable", toupper(var), ".\n")
  }
  
} 

cat("\nGenerando panel visual conjunto...\n")

panel_final <- lista_graficos[[1]] + lista_graficos[[2]] + lista_graficos[[3]]

panel_final + plot_annotation(
  title = 'Análisis Bivariante: Variables Numéricas vs Stroke',
  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
)




#DESBALANCEO

frecuencias <- table(train_preprocesado$stroke)

n_mayoritaria <- frecuencias["0"]
n_minoritaria <- frecuencias["1"]

IR <- n_mayoritaria / n_minoritaria

print(paste("Frecuencia clase mayoritaria:", n_mayoritaria))
print(paste("Frecuencia clase minoritaria:", n_minoritaria))
print(paste("Imbalance Ratio (IR):", round(IR, 4)))



#ESCENARIOS
library(recipes)
library(themis)
library(dplyr)

train_preprocesado$stroke <- as.factor(train_preprocesado$stroke)
set.seed(123)
#BASE
train_base <- train_preprocesado

#ROS
set.seed(123)
ros_recipe <- recipe(stroke ~ ., data = train_preprocesado) %>%
  step_upsample(stroke, over_ratio = 1) %>% 
  prep()

train_ros <- bake(ros_recipe, new_data = NULL)

#SMOTE
set.seed(123)
smote_recipe <- recipe(stroke ~ ., data = train_preprocesado) %>%
  step_smote(stroke, over_ratio = 1) %>% 
  prep()

train_smote <- bake(smote_recipe, new_data = NULL)


#HIBRIDO
set.seed(123)

hibrido_recipe <- recipe(stroke ~ ., data = train_preprocesado) %>%
  step_downsample(stroke, under_ratio = 2) %>% 
  step_upsample(stroke, over_ratio = 1) %>%    
  prep()

train_hibrido <- bake(hibrido_recipe, new_data = NULL)

print("Distribución Original (Base):")
print(table(train_base$stroke))

print("Distribución ROS:")
print(table(train_ros$stroke))

print("Distribución SMOTE:")
print(table(train_smote$stroke))

print("Distribución Híbrida:")
print(table(train_hibrido$stroke))





library(ggplot2)
library(tidyr)

conteo_base <- as.data.frame(table(train_base$stroke)) %>% mutate(Metodo = "Base (Original)")
conteo_ros  <- as.data.frame(table(train_ros$stroke)) %>% mutate(Metodo = "ROS")
conteo_smote <- as.data.frame(table(train_smote$stroke)) %>% mutate(Metodo = "SMOTE")
conteo_hibrido <- as.data.frame(table(train_hibrido$stroke)) %>% mutate(Metodo = "Híbrido")

df_grafico <- rbind(conteo_base, conteo_ros, conteo_smote, conteo_hibrido)
colnames(df_grafico) <- c("Clase", "Frecuencia", "Metodo")

ggplot(df_grafico, aes(x = Metodo, y = Frecuencia, fill = Clase)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparación de la distribución de clases por método de balanceo",
       subtitle = "Dataset de entrenamiento",
       x = "Método aplicado",
       y = "Número de instancias",
       fill = "Ictus (0: No, 1: Sí)") +
  scale_fill_manual(values = c("#5DADE2",  "#FF7F50")) + 
  geom_text(aes(label = Frecuencia), position = position_dodge(width = 0.9), vjust = -0.5)




#MODELOS
#REGRESIÓN LOGÍSTICA
library(caret)
library(dplyr)
library(MLmetrics)


escenarios_log <- list(
  "Base"    = train_base,    
  "ROS"     = train_ros,     
  "SMOTE"   = train_smote,   
  "Hibrido" = train_hibrido  
)


ctrl_f1 <- trainControl(
  method = "cv",           
  number = 10,            
  classProbs = TRUE,       
  summaryFunction = prSummary, 
  savePredictions = "final"    
)

resultados_logistica <- list()
umbrales_logistica <- list()

for (nombre_esc in names(escenarios_log)) {
  message(paste(">>> Optimizando Logística para:", nombre_esc))
  

  datos_act <- escenarios_log[[nombre_esc]]
  datos_act$stroke <- factor(datos_act$stroke, levels = c(0, 1), labels = c("No", "Si"))
  
  inicio_t <- Sys.time() 
  set.seed(123)         
  
  modelo_glm <- train(
    stroke ~ .,           
    data = datos_act, 
    method = "glm",       
    family = "binomial",  
    metric = "F",         
    trControl = ctrl_f1
  )
  
  preds_cv <- modelo_glm$pred
  umbrales_seq <- seq(0.01, 0.9, length = 100) 
  
  f1_vec <- sapply(umbrales_seq, function(u) {
    clases <- factor(ifelse(preds_cv$Si >= u, "Si", "No"), levels = c("No", "Si"))
    F1_Score(y_true = preds_cv$obs, y_pred = clases, positive = "Si")
  })
  
  u_opt <- umbrales_seq[which.max(f1_vec)]
  f1_max <- max(f1_vec, na.rm = TRUE)
  fin_t <- Sys.time()
  
  id_mod <- paste0("mod_", tolower(nombre_esc), "_logistica")
  assign(id_mod, modelo_glm)
  
  umbrales_logistica[[id_mod]] <- u_opt
  
  resultados_logistica[[id_mod]] <- data.frame(
    Modelo_Escenario = id_mod,
    HP_Optimizado = "Ninguno (GLM)", 
    Umbral_F1 = round(u_opt, 4),
    F1_Alcanzado = round(f1_max, 4),
    Tiempo_Seg = round(as.numeric(difftime(fin_t, inicio_t, units = "secs")), 2)
  )
}

message(">>> Optimizando Logística para: Cost-Sensitive")

frec_cs <- table(train_base$stroke)
pesos_cs <- ifelse(train_base$stroke == 1, frec_cs[1]/frec_cs[2], 1)

df_cs <- train_base
df_cs$stroke <- factor(df_cs$stroke, levels = c(0, 1), labels = c("No", "Si"))

inicio_t_cs <- Sys.time()
set.seed(123)

mod_cs_log <- train(
  stroke ~ ., data = df_cs, method = "glm", family = "binomial",
  metric = "F", trControl = ctrl_f1, weights = pesos_cs
)

preds_cv_cs <- mod_cs_log$pred
f1_vec_cs <- sapply(umbrales_seq, function(u) {
  clases <- factor(ifelse(preds_cv_cs$Si >= u, "Si", "No"), levels = c("No", "Si"))
  F1_Score(y_true = preds_cv_cs$obs, y_pred = clases, positive = "Si")
})

u_opt_cs <- umbrales_seq[which.max(f1_vec_cs)]
id_cs <- "mod_costsensitive_logistica"
assign(id_cs, mod_cs_log)
umbrales_logistica[[id_cs]] <- u_opt_cs


resultados_logistica[[id_cs]] <- data.frame(
  Modelo_Escenario = id_cs,
  HP_Optimizado = "Pesos (Cost-Sensitive)",
  Umbral_F1 = round(u_opt_cs, 4),
  F1_Alcanzado = round(max(f1_vec_cs, na.rm = TRUE), 4),
  Tiempo_Seg = round(as.numeric(difftime(Sys.time(), inicio_t_cs, units = "secs")), 2)
)


tabla_logistica <- bind_rows(resultados_logistica)
print(as.data.frame(tabla_logistica))



#REGRESIONES REGULARIZADAS (LASSO, RIDGE, ENet)

tecnicas_reg <- list(
  "lasso"      = expand.grid(alpha = 1, lambda = seq(0.0001, 0.1, length = 20)),
  "ridge"      = expand.grid(alpha = 0, lambda = seq(0.0001, 0.1, length = 20)),
  "elasticnet" = expand.grid(alpha = seq(0, 1, by = 0.2), lambda = seq(0.0001, 0.1, length = 20))
)

resultados_regularizada <- list()

if(!exists("umbrales_maestros")) umbrales_maestros <- list()

for (nombre_esc in names(escenarios_log)) {
  
  datos_act <- escenarios_log[[nombre_esc]]
  datos_act$stroke <- factor(datos_act$stroke, levels = c(0, 1), labels = c("No", "Si"))
  
  for (nombre_tec in names(tecnicas_reg)) {
    message(paste(">>> Optimizando", nombre_tec, "para:", nombre_esc))
    
    inicio_t <- Sys.time()
    set.seed(123)
    
    modelo_fit <- train(
      stroke ~ ., 
      data = datos_act,
      method = "glmnet",
      metric = "F",
      trControl = ctrl_f1,
      tuneGrid = tecnicas_reg[[nombre_tec]]
    )
    
    preds_cv <- modelo_fit$pred
    umbrales_seq <- seq(0.01, 0.9, length = 100)
    
    f1_vec <- sapply(umbrales_seq, function(u) {
      clases <- factor(ifelse(preds_cv$Si >= u, "Si", "No"), levels = c("No", "Si"))
      F1_Score(y_true = preds_cv$obs, y_pred = clases, positive = "Si")
    })
    
    u_opt <- umbrales_seq[which.max(f1_vec)]
    f1_max <- max(f1_vec, na.rm = TRUE)
    fin_t <- Sys.time()
    
    id_mod <- paste0("mod_", tolower(nombre_esc), "_", nombre_tec)
    assign(id_mod, modelo_fit)
    umbrales_maestros[[id_mod]] <- u_opt
    
    resultados_regularizada[[id_mod]] <- data.frame(
      Modelo_Escenario = id_mod,
      Parametros_Opt = paste0("alpha:", modelo_fit$bestTune$alpha, 
                              ", lambda:", round(modelo_fit$bestTune$lambda, 5)),
      Umbral_F1 = round(u_opt, 4),
      F1_Alcanzado = round(f1_max, 4),
      Tiempo_Seg = round(as.numeric(difftime(fin_t, inicio_t, units = "secs")), 2)
    )
  }
}

message(">>> Optimizando Regularizadas para: Cost-Sensitive")

for (nombre_tec in names(tecnicas_reg)) {
  inicio_t <- Sys.time()
  set.seed(123)
  
  mod_cs <- train(
    stroke ~ ., data = df_cs, method = "glmnet", metric = "F",
    trControl = ctrl_f1, tuneGrid = tecnicas_reg[[nombre_tec]], weights = pesos_cs
  )
  
  preds_cv_cs <- mod_cs$pred
  f1_vec_cs <- sapply(umbrales_seq, function(u) {
    clases <- factor(ifelse(preds_cv_cs$Si >= u, "Si", "No"), levels = c("No", "Si"))
    F1_Score(y_true = preds_cv_cs$obs, y_pred = clases, positive = "Si")
  })
  
  u_opt_cs <- umbrales_seq[which.max(f1_vec_cs)]
  id_cs <- paste0("mod_costsensitive_", nombre_tec)
  assign(id_cs, mod_cs)
  umbrales_maestros[[id_cs]] <- u_opt_cs
  
  resultados_regularizada[[id_cs]] <- data.frame(
    Modelo_Escenario = id_cs,
    Parametros_Opt = paste0("alpha:", mod_cs$bestTune$alpha, 
                            ", lambda:", round(mod_cs$bestTune$lambda, 5), " + weights"),
    Umbral_F1 = round(u_opt_cs, 4),
    F1_Alcanzado = round(max(f1_vec_cs, na.rm = TRUE), 4),
    Tiempo_Seg = round(as.numeric(difftime(Sys.time(), inicio_t, units = "secs")), 2)
  )
}

tabla_regularizada <- bind_rows(resultados_regularizada)
print(as.data.frame(tabla_regularizada))



# ÁRBOL DE DECISIÓN

library(rpart)
library(caret)
library(dplyr)
library(MLmetrics)

if(!exists("umbrales_maestros")) umbrales_maestros <- list()

escenarios_dt <- list(
  "Base"    = train_base,
  "ROS"     = train_ros,
  "SMOTE"   = train_smote,
  "Hibrido" = train_hibrido
)

rango_cp <- seq(0.001, 0.05, length = 20)
rango_depth <- c(3, 5, 7, 10)

ctrl_f1 <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = prSummary, 
  savePredictions = "final"
)

resultados_dt <- list()

for (nombre_esc in names(escenarios_dt)) {
  message(paste(">>> Optimizando DT para:", nombre_esc))
  
  datos_actuales <- escenarios_dt[[nombre_esc]]
  datos_actuales$stroke <- factor(datos_actuales$stroke, levels = c(0, 1), labels = c("No", "Si"))
  
  inicio_t <- Sys.time()
  
  mejor_f1_global <- -1
  mejor_modelo_global <- NULL
  mejor_depth <- NA
  mejor_umbral <- NA
  
  for (d in rango_depth) {
    set.seed(123)
    modelo_temp <- train(
      stroke ~ ., 
      data = datos_actuales, 
      method = "rpart", 
      metric = "F", 
      trControl = ctrl_f1,
      tuneGrid = expand.grid(cp = rango_cp),
      control = rpart.control(maxdepth = d, minsplit = 20)
    )
    
    p_cv <- modelo_temp$pred
    u_seq <- seq(0.01, 0.9, length = 100)
    f1_vec <- sapply(u_seq, function(u) {
      clases <- factor(ifelse(p_cv$Si >= u, "Si", "No"), levels = c("No", "Si"))
      F1_Score(y_true = p_cv$obs, y_pred = clases, positive = "Si")
    })
    
    f1_max_local <- max(f1_vec, na.rm = TRUE)
    u_opt_local <- u_seq[which.max(f1_vec)]
    
    if (f1_max_local > mejor_f1_global) {
      mejor_f1_global <- f1_max_local
      mejor_modelo_global <- modelo_temp
      mejor_depth <- d
      mejor_umbral <- u_opt_local
    }
  }
  
  fin_t <- Sys.time()
  id_mod <- paste0("mod_", tolower(nombre_esc), "_dt")
  
  assign(id_mod, mejor_modelo_global, envir = .GlobalEnv)
  umbrales_maestros[[id_mod]] <- mejor_umbral
  
  resultados_dt[[id_mod]] <- data.frame(
    Modelo_Escenario = id_mod,
    HP_Optimizados = paste0("cp:", round(mejor_modelo_global$bestTune$cp, 5), " depth:", mejor_depth),
    Umbral_F1 = round(mejor_umbral, 4),
    F1_Alcanzado = round(mejor_f1_global, 4),
    Tiempo_Seg = round(as.numeric(difftime(fin_t, inicio_t, units = "secs")), 2)
  )
}


message(">>> Optimizando DT para: Cost-Sensitive")
frec_cs <- table(train_base$stroke)
matrix_loss <- matrix(c(0, 1, frec_cs[1]/frec_cs[2], 0), byrow = TRUE, nrow = 2)

df_cs <- train_base
df_cs$stroke <- factor(df_cs$stroke, levels = c(0, 1), labels = c("No", "Si"))

inicio_t_cs <- Sys.time()
mejor_f1_cs <- -1
mejor_mod_cs <- NULL
mejor_u_cs <- 0.5
mejor_d_cs <- NA

for (d in rango_depth) {
  set.seed(123)
  mod_temp_cs <- train(
    stroke ~ ., data = df_cs, method = "rpart", metric = "F",
    trControl = ctrl_f1, tuneGrid = expand.grid(cp = rango_cp),
    parms = list(loss = matrix_loss),
    control = rpart.control(maxdepth = d, minsplit = 20)
  )
  
  p_cv_cs <- mod_temp_cs$pred
  f1_vec_cs <- sapply(seq(0.01, 0.9, length = 100), function(u) {
    clases <- factor(ifelse(p_cv_cs$Si >= u, "Si", "No"), levels = c("No", "Si"))
    F1_Score(y_true = p_cv_cs$obs, y_pred = clases, positive = "Si")
  })
  
  if (max(f1_vec_cs, na.rm=T) > mejor_f1_cs) {
    mejor_f1_cs <- max(f1_vec_cs, na.rm=T)
    mejor_mod_cs <- mod_temp_cs
    mejor_u_cs <- seq(0.01, 0.9, length = 100)[which.max(f1_vec_cs)]
    mejor_d_cs <- d
  }
}

id_cs <- "mod_costsensitive_dt"

assign(id_cs, mejor_mod_cs, envir = .GlobalEnv)
umbrales_maestros[[id_cs]] <- mejor_u_cs

resultados_dt[[id_cs]] <- data.frame(
  Modelo_Escenario = id_cs,
  HP_Optimizados = paste0("cp:", round(mejor_mod_cs$bestTune$cp, 5), " depth:", mejor_d_cs, " + LossMat"),
  Umbral_F1 = round(mejor_u_cs, 4),
  F1_Alcanzado = round(mejor_f1_cs, 4),
  Tiempo_Seg = round(as.numeric(difftime(Sys.time(), inicio_t_cs, units = "secs")), 2)
)

tabla_final_dt <- bind_rows(resultados_dt)
print(as.data.frame(tabla_final_dt))




# RANDOM FOREST

library(caret)
library(ranger)
library(dplyr)
library(MLmetrics)

n_cores <- parallel::detectCores() - 1

ctrl_f1 <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = prSummary, 
  savePredictions = "final",
  allowParallel = TRUE
)

rf_grid <- expand.grid(
  mtry = floor(sqrt(ncol(train_base) - 1) * c(0.5, 1, 1.5, 2)),
  splitrule = "gini",
  min.node.size = c(1, 5, 10)
)

resultados_rf <- list()
umbrales_seq <- seq(0.01, 0.9, length = 100)

datasets_lista <- list(
  Base = train_base,
  ROS = train_ros,
  SMOTE = train_smote,
  Hibrido = train_hibrido
)

for (nombre in names(datasets_lista)) {
  message(paste(">>> Optimizando RF para:", nombre))
  
  df_act <- datasets_lista[[nombre]]
  df_act$stroke <- factor(df_act$stroke, levels = c(0, 1), labels = c("No", "Si"))
  
  inicio_t <- Sys.time()
  set.seed(123)
  
  modelo_rf <- train(
    stroke ~ ., data = df_act, method = "ranger",
    trControl = ctrl_f1, tuneGrid = rf_grid,
    metric = "F", num.trees = 150, num.threads = n_cores,
    importance = "impurity"
  )
  
  preds_cv <- modelo_rf$pred
  f1_vec <- sapply(umbrales_seq, function(u) {
    clases <- factor(ifelse(preds_cv$Si >= u, "Si", "No"), levels = c("No", "Si"))
    F1_Score(y_true = preds_cv$obs, y_pred = clases, positive = "Si")
  })
  
  u_opt <- umbrales_seq[which.max(f1_vec)]
  f1_max <- max(f1_vec, na.rm = TRUE)
  fin_t <- Sys.time()
  
  id_mod <- paste0("mod_", tolower(nombre), "_rf")
  assign(id_mod, modelo_rf)
  umbrales_maestros[[id_mod]] <- u_opt 
  
  resultados_rf[[id_mod]] <- data.frame(
    Modelo_Escenario = id_mod,
    HP_Optimizados = paste0("mtry:", modelo_rf$bestTune$mtry, 
                            " node:", modelo_rf$bestTune$min.node.size),
    Umbral_F1 = round(u_opt, 4),
    F1_Alcanzado = round(f1_max, 4),
    Tiempo_Seg = round(as.numeric(difftime(fin_t, inicio_t, units = "secs")), 2)
  )
}

message(">>> Optimizando RF para: Cost-Sensitive")
frecuencias <- table(train_base$stroke)

pesos_clase <- c("No" = 1, "Si" = as.numeric(frecuencias[1] / frecuencias[2]))

df_cs <- train_base
df_cs$stroke <- factor(df_cs$stroke, levels = c(0, 1), labels = c("No", "Si"))

inicio_t_cs <- Sys.time()
set.seed(123)

mod_cs_rf <- train(
  stroke ~ ., data = df_cs, method = "ranger",
  trControl = ctrl_f1, tuneGrid = rf_grid,
  metric = "F", num.trees = 150, num.threads = n_cores,
  class.weights = pesos_clase, replace = FALSE,
  importance = "impurity"
)

p_cv_cs <- mod_cs_rf$pred
f1_vec_cs <- sapply(umbrales_seq, function(u) {
  clases <- factor(ifelse(p_cv_cs$Si >= u, "Si", "No"), levels = c("No", "Si"))
  F1_Score(y_true = p_cv_cs$obs, y_pred = clases, positive = "Si")
})

u_opt_cs <- umbrales_seq[which.max(f1_vec_cs)]
id_cs <- "mod_costsensitive_rf"
assign(id_cs, mod_cs_rf)
umbrales_maestros[[id_cs]] <- u_opt_cs

resultados_rf[[id_cs]] <- data.frame(
  Modelo_Escenario = id_cs,

  HP_Optimizados = paste0("mtry:", mod_cs_rf$bestTune$mtry, 
                          " node:", mod_cs_rf$bestTune$min.node.size, " + weights"),
  Umbral_F1 = round(u_opt_cs, 4),
  F1_Alcanzado = round(max(f1_vec_cs, na.rm = TRUE), 4),
  Tiempo_Seg = round(as.numeric(difftime(Sys.time(), inicio_t_cs, units = "secs")), 2)
)

tabla_final_rf <- bind_rows(resultados_rf)
print(as.data.frame(tabla_final_rf))





# SVM RADIAL
library(caret)
library(kernlab)
library(dplyr)
library(MLmetrics)

ctrl_f1_svm <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE,
  summaryFunction = prSummary, 
  savePredictions = "final",
  allowParallel = TRUE
)

svm_grid <- expand.grid(sigma = c(0.01, 0.05), C = c(1, 2, 4,8))
umbrales_seq <- seq(0.01, 0.9, length = 100)
resultados_svm <- list()

datasets_directos <- list(
  "Base"    = train_base,
  "ROS"     = train_ros,
  "SMOTE"   = train_smote,
  "Hibrido" = train_hibrido
)

for (nombre in names(datasets_directos)) {
  message(">>> Optimizando SVM Radial: ", nombre)
  
  datos_entrenamiento <- datasets_directos[[nombre]]
  datos_entrenamiento$stroke <- factor(datos_entrenamiento$stroke, 
                                       levels = c(0, 1), 
                                       labels = c("No", "Si"))
  
  inicio_t <- Sys.time()
  set.seed(123)
  
  mod_svm <- train(
    stroke ~ ., 
    data = datos_entrenamiento, 
    method = "svmRadial", 
    metric = "F", 
    trControl = ctrl_f1_svm, 
    tuneGrid = svm_grid
  )
  
  p_cv <- mod_svm$pred
  f1_v <- sapply(umbrales_seq, function(u) {
    clases <- factor(ifelse(p_cv$Si >= u, "Si", "No"), levels = c("No", "Si"))
    F1_Score(y_true = p_cv$obs, y_pred = clases, positive = "Si")
  })
  
  u_opt <- umbrales_seq[which.max(f1_v)]
  id_mod <- paste0("mod_", tolower(nombre), "_svm_radial")
  
  assign(id_mod, mod_svm)
  umbrales_maestros[[id_mod]] <- u_opt
  
  resultados_svm[[id_mod]] <- data.frame(
    Modelo_Escenario = id_mod,
    HP = paste0("sigma:", mod_svm$bestTune$sigma, " C:", mod_svm$bestTune$C),
    Umbral = round(u_opt, 4), 
    F1 = round(max(f1_v, na.rm=T), 4),
    Tiempo_Seg = round(as.numeric(difftime(Sys.time(), inicio_t, units="secs")), 2)
  )
}

message(">>> Optimizando SVM Radial: Cost-Sensitive")

df_cs_final <- train_base
df_cs_final$stroke <- factor(df_cs_final$stroke, levels = c(0, 1), labels = c("No", "Si"))

frecuencias <- table(train_base$stroke) 

pesos_cs_vector <- c("No" = 1, "Si" = as.numeric(frecuencias["0"]/frecuencias["1"]))

inicio_t_cs <- Sys.time()
set.seed(123)
mod_cs_svm <- train(
  stroke ~ ., 
  data = df_cs_final, 
  method = "svmRadial", 
  metric = "F", 
  trControl = ctrl_f1_svm, 
  tuneGrid = svm_grid,

  class.weights = pesos_cs_vector
)

p_cv_cs <- mod_cs_svm$pred
f1_v_cs <- sapply(umbrales_seq, function(u) {
  clases <- factor(ifelse(p_cv_cs$Si >= u, "Si", "No"), levels = c("No", "Si"))
  F1_Score(y_true = p_cv_cs$obs, y_pred = clases, positive = "Si")
})

u_opt_cs <- umbrales_seq[which.max(f1_v_cs)]
id_cs <- "mod_costsensitive_svm_radial"
assign(id_cs, mod_cs_svm)
umbrales_maestros[[id_cs]] <- u_opt_cs

resultados_svm[[id_cs]] <- data.frame(
  Modelo_Escenario = id_cs, 
  HP = paste0("sigma:", mod_cs_svm$bestTune$sigma, " C:", mod_cs_svm$bestTune$C, " + Weights"),
  Umbral = round(u_opt_cs, 4), 
  F1 = round(max(f1_v_cs, na.rm=T), 4),
  Tiempo_Seg = round(as.numeric(difftime(Sys.time(), inicio_t_cs, units="secs")), 2)
)


print(bind_rows(resultados_svm))


# SVM LINEAL
library(caret)
library(kernlab)
library(dplyr)
library(MLmetrics)

ctrl_f1_svmlineal <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE,
  summaryFunction = prSummary, 
  savePredictions = "final",
  allowParallel = TRUE
)


svmlineal_grid <- expand.grid(C = c(1, 2, 4, 8))
umbrales_seq <- seq(0.01, 0.9, length = 100)
resultados_svmlineal <- list()

datasets_directos <- list(
  "Base"    = train_base,
  "ROS"     = train_ros,
  "SMOTE"   = train_smote,
  "Hibrido" = train_hibrido
)

for (nombre in names(datasets_directos)) {
  message(">>> Optimizando SVM Lineal: ", nombre)
  
  datos_entrenamiento <- datasets_directos[[nombre]]
  datos_entrenamiento$stroke <- factor(datos_entrenamiento$stroke, 
                                       levels = c(0, 1), 
                                       labels = c("No", "Si"))
  
  inicio_t <- Sys.time()
  set.seed(123)
  
  mod_svm_lin <- train(
    stroke ~ ., 
    data = datos_entrenamiento, 
    method = "svmLinear", 
    metric = "F", 
    trControl = ctrl_f1_svmlineal, 
    tuneGrid = svmlineal_grid
  )
  
  p_cv <- mod_svm_lin$pred
  f1_v <- sapply(umbrales_seq, function(u) {
    clases <- factor(ifelse(p_cv$Si >= u, "Si", "No"), levels = c("No", "Si"))
    F1_Score(y_true = p_cv$obs, y_pred = clases, positive = "Si")
  })
  
  u_opt <- umbrales_seq[which.max(f1_v)]
  f1_max <- max(f1_v, na.rm = TRUE)
  fin_t <- Sys.time()
  
  id_mod <- paste0("mod_", tolower(nombre), "_svm_lineal")
  assign(id_mod, mod_svm_lin)
  umbrales_maestros[[id_mod]] <- u_opt
  
  resultados_svmlineal[[id_mod]] <- data.frame(
    Modelo_Escenario = id_mod,
    HP_Optimizados = paste0("C:", mod_svm_lin$bestTune$C),
    Umbral_F1 = round(u_opt, 4), 
    F1_Alcanzado = round(f1_max, 4),
    Tiempo_Seg = round(as.numeric(difftime(fin_t, inicio_t, units = "secs")), 2)
  )
}

message(">>> Optimizando SVM Lineal: Cost-Sensitive")

df_cs_lin <- train_base
df_cs_lin$stroke <- factor(df_cs_lin$stroke, levels = c(0, 1), labels = c("No", "Si"))

frecuencias <- table(train_base$stroke)

pesos_cs_lin_vector <- c("No" = 1, "Si" = as.numeric(frecuencias["0"]/frecuencias["1"]))

inicio_t_cs <- Sys.time()
set.seed(123)

mod_cs_svm_lin <- train(
  stroke ~ ., 
  data = df_cs_lin, 
  method = "svmLinear", 
  metric = "F", 
  trControl = ctrl_f1_svmlineal, 
  tuneGrid = svmlineal_grid,

  class.weights = pesos_cs_lin_vector
)

p_cv_cs <- mod_cs_svm_lin$pred
f1_v_cs <- sapply(umbrales_seq, function(u) {
  clases <- factor(ifelse(p_cv_cs$Si >= u, "Si", "No"), levels = c("No", "Si"))
  F1_Score(y_true = p_cv_cs$obs, y_pred = clases, positive = "Si")
})

u_opt_cs <- umbrales_seq[which.max(f1_v_cs)]
id_cs <- "mod_costsensitive_svm_lineal"
assign(id_cs, mod_cs_svm_lin)
umbrales_maestros[[id_cs]] <- u_opt_cs

resultados_svmlineal[[id_cs]] <- data.frame(
  Modelo_Escenario = id_cs, 
  HP_Optimizados = paste0("C:", mod_cs_svm_lin$bestTune$C, " + Weights"),
  Umbral_F1 = round(u_opt_cs, 4), 
  F1_Alcanzado = round(max(f1_v_cs, na.rm = TRUE), 4),
  Tiempo_Seg = round(as.numeric(difftime(Sys.time(), inicio_t_cs, units = "secs")), 2)
)

tabla_final_svm_lineal <- bind_rows(resultados_svmlineal)
print(as.data.frame(tabla_final_svm_lineal))




# REDES NEURONALES
library(caret)
library(nnet)
library(dplyr)
library(MLmetrics)

ctrl_f1_ann <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = prSummary, 
  savePredictions = "final",
  allowParallel = TRUE
)

ann_grid <- expand.grid(
  size = c(1, 3, 5, 7),
  decay = c(0.1, 0.5, 1)
)

umbrales_seq <- seq(0.01, 0.9, length = 100)
resultados_ann <- list()

datasets_lista <- list(
  Base = train_base,
  ROS = train_ros,
  SMOTE = train_smote,
  Hibrido = train_hibrido
)

for (nombre in names(datasets_lista)) {
  message(paste(">>> Optimizando ANN escenario:", nombre))
  
  datos_entreno <- datasets_lista[[nombre]]
  datos_entreno$stroke <- factor(datos_entreno$stroke, levels = c(0, 1), labels = c("No", "Si"))
  
  inicio_t <- Sys.time()
  set.seed(123)
  
  modelo_ann <- train(
    stroke ~ ., 
    data = datos_entreno,
    method = "nnet",
    metric = "F", 
    trControl = ctrl_f1_ann,
    tuneGrid = ann_grid,

    trace = FALSE,
    MaxNWts = 2000,
    maxit = 500
  )
  

  p_cv <- modelo_ann$pred
  f1_vec <- sapply(umbrales_seq, function(u) {
    clases <- factor(ifelse(p_cv$Si >= u, "Si", "No"), levels = c("No", "Si"))
    F1_Score(y_true = p_cv$obs, y_pred = clases, positive = "Si")
  })
  
  u_opt <- umbrales_seq[which.max(f1_vec)]
  f1_max <- max(f1_vec, na.rm = TRUE)
  fin_t <- Sys.time()
  

  id_mod <- paste0("mod_", tolower(nombre), "_red_neuronal")
  assign(id_mod, modelo_ann)
  umbrales_maestros[[id_mod]] <- u_opt
  
  resultados_ann[[id_mod]] <- data.frame(
    Modelo_Escenario = id_mod,
    HP_Optimizados = paste0("size:", modelo_ann$bestTune$size, " decay:", modelo_ann$bestTune$decay),
    Umbral_F1 = round(u_opt, 4),
    F1_Alcanzado = round(f1_max, 4),
    Tiempo_Seg = round(as.numeric(difftime(fin_t, inicio_t, units = "secs")), 2)
  )
}

message(">>> Optimizando ANN escenario: Cost-Sensitive")

datos_cs <- train_base
datos_cs$stroke <- factor(datos_cs$stroke, levels = c(0, 1), labels = c("No", "Si"))

frecuencias <- table(train_base$stroke)
pesos_vector <- ifelse(datos_cs$stroke == "Si", as.numeric(frecuencias["0"] / frecuencias["1"]), 1)

inicio_t_cs <- Sys.time()
set.seed(123)

mod_costsensitive_red_neuronal <- train(
  stroke ~ ., 
  data = datos_cs,
  method = "nnet",
  weights = pesos_vector,
  metric = "F",
  trControl = ctrl_f1_ann,
  tuneGrid = ann_grid,
  trace = FALSE,
  MaxNWts = 2000,
  maxit = 1000
)

p_cv_cs <- mod_costsensitive_red_neuronal$pred
f1_vec_cs <- sapply(umbrales_seq, function(u) {
  clases <- factor(ifelse(p_cv_cs$Si >= u, "Si", "No"), levels = c("No", "Si"))
  F1_Score(y_true = p_cv_cs$obs, y_pred = clases, positive = "Si")
})

u_opt_cs <- umbrales_seq[which.max(f1_vec_cs)]
umbrales_maestros[["mod_costsensitive_red_neuronal"]] <- u_opt_cs

resultados_ann[["mod_costsensitive_red_neuronal"]] <- data.frame(
  Modelo_Escenario = "mod_costsensitive_red_neuronal",
  HP_Optimizados = paste0("size:", mod_costsensitive_red_neuronal$bestTune$size, " decay:", mod_costsensitive_red_neuronal$bestTune$decay, " + Weights"),
  Umbral_F1 = round(u_opt_cs, 4),
  F1_Alcanzado = round(max(f1_vec_cs, na.rm = TRUE), 4),
  Tiempo_Seg = round(as.numeric(difftime(Sys.time(), inicio_t_cs, units = "secs")), 2)
)

tabla_final_ann <- bind_rows(resultados_ann)
print(as.data.frame(tabla_final_ann))















# EVALUACIÓN FINAL DE TODOS LOS MODELOS EN TEST

library(caret)
library(dplyr)
library(MLmetrics)
library(pROC)


nombre_modelos <- c(
  # Logística
  "mod_base_logistica", "mod_ros_logistica", "mod_smote_logistica", "mod_hibrido_logistica", "mod_costsensitive_logistica",
  # Regularizadas (Lasso, Ridge, ElasticNet)
  "mod_base_lasso", "mod_ros_lasso", "mod_smote_lasso", "mod_hibrido_lasso", "mod_costsensitive_lasso",
  "mod_base_ridge", "mod_ros_ridge", "mod_smote_ridge", "mod_hibrido_ridge", "mod_costsensitive_ridge",
  "mod_base_elasticnet", "mod_ros_elasticnet", "mod_smote_elasticnet", "mod_hibrido_elasticnet", "mod_costsensitive_elasticnet",
  # Árboles de Decisión
  "mod_base_dt", "mod_ros_dt", "mod_smote_dt", "mod_hibrido_dt", "mod_costsensitive_dt",
  # Random Forest
  "mod_base_rf", "mod_ros_rf", "mod_smote_rf", "mod_hibrido_rf", "mod_costsensitive_rf",
  # SVM Radial
  "mod_base_svm_radial", "mod_ros_svm_radial", "mod_smote_svm_radial", "mod_hibrido_svm_radial", "mod_costsensitive_svm_radial",
  # SVM Lineal
  "mod_base_svm_lineal", "mod_ros_svm_lineal", "mod_smote_svm_lineal", "mod_hibrido_svm_lineal", "mod_costsensitive_svm_lineal",
  # Redes Neuronales
  "mod_base_red_neuronal", "mod_ros_red_neuronal", "mod_smote_red_neuronal", "mod_hibrido_red_neuronal", "mod_costsensitive_red_neuronal"
)


test_eval <- test_preprocesado
test_eval$stroke <- factor(test_eval$stroke, levels = c(0, 1), labels = c("No", "Si"))

todos_los_umbrales <- c(umbrales_logistica, umbrales_maestros)

lista_maestra_resultados <- list()

for (m in nombre_modelos) {
  
  if (!exists(m)) {
    message(paste("Saltando:", m, "(No encontrado)"))
    next
  }
  
  obj <- get(m)
  
  probs <- predict(obj, newdata = test_eval, type = "prob")$Si
  
  u_opt <- ifelse(!is.null(todos_los_umbrales[[m]]), todos_los_umbrales[[m]], 0.5)
  
  preds_clase <- factor(ifelse(probs >= u_opt, "Si", "No"), levels = c("No", "Si"))
  
  cm <- confusionMatrix(preds_clase, test_eval$stroke, positive = "Si")
  
  roc_obj <- roc(test_eval$stroke, probs, quiet = TRUE)
  auc_val <- as.numeric(auc(roc_obj))
  
  recall <- as.numeric(cm$byClass["Sensitivity"])
  spec   <- as.numeric(cm$byClass["Specificity"])
  prec   <- as.numeric(cm$byClass["Precision"])
  acc    <- as.numeric(cm$overall["Accuracy"])
  kappa  <- as.numeric(cm$overall["Kappa"])
  
  f1 <- if(is.na(prec) || (prec + recall) == 0) 0 else 2 * ((prec * recall) / (prec + recall))
  
  lista_maestra_resultados[[m]] <- data.frame(
    Modelo   = m,
    Recall   = round(recall, 4),
    AUC      = round(auc_val, 4),
    F1       = round(f1, 4),
    Accuracy = round(acc, 4),
    Kappa    = round(kappa, 4),
    Especificidad = round(spec, 4),
    Umbral_Usado = round(u_opt, 4)
  )
}

if (length(lista_maestra_resultados) > 0) {
  tabla_tfg_final <- bind_rows(lista_maestra_resultados) %>%
    mutate(Escenario = case_when(
      grepl("base", Modelo) ~ "Base",
      grepl("ros", Modelo) ~ "ROS",
      grepl("smote", Modelo) ~ "SMOTE",
      grepl("hibrido", Modelo) ~ "Híbrido",
      grepl("costsensitive", Modelo) ~ "Cost-Sensitive",
      TRUE ~ "Otro"
    )) %>%

    mutate(Algoritmo = case_when(
      grepl("logistica", Modelo) ~ "Reg. Logística",
      grepl("lasso", Modelo) ~ "Lasso",
      grepl("ridge", Modelo) ~ "Ridge",
      grepl("elasticnet", Modelo) ~ "Elastic Net",
      grepl("dt", Modelo) ~ "Árbol Decisión",
      grepl("rf", Modelo) ~ "Random Forest",
      grepl("svm_radial", Modelo) ~ "SVM Radial",
      grepl("svm_lineal", Modelo) ~ "SVM Lineal",
      grepl("red_neuronal", Modelo) ~ "Red Neuronal",
      TRUE ~ "Otro"
    )) %>%

    arrange(desc(Recall)) %>%
    select(Algoritmo, Escenario, Recall, AUC, F1, Accuracy, Kappa, Especificidad, Umbral_Usado)
  

  cat("TABLA COMPARATIVA FINAL (ORDENADA POR RECALL)\n")

  print(as.data.frame(tabla_tfg_final))
  
  
} else {
  stop("Error: No se han podido evaluar los modelos. Verifica que los nombres coincidan.")
}



#GRAFICO AGRUPADO POR ESCENARIOS
library(ggplot2)
library(dplyr)
library(tidyr)


datos_escenarios <- tabla_tfg_final %>%
  group_by(Escenario) %>%
  summarise(
    Recall = mean(Recall, na.rm = TRUE),
    F1_Score = mean(F1, na.rm = TRUE),
    Especificidad = mean(Especificidad, na.rm = TRUE)
  )

datos_long <- datos_escenarios %>%
  pivot_longer(
    cols = c("Recall", "F1_Score", "Especificidad"),
    names_to = "Metrica",
    values_to = "Valor"
  ) %>%
  
  mutate(Metrica = case_match(Metrica,
                              "F1_Score"      ~ "F1-Score",
                              "Especificidad" ~ "Especificidad",
                              "Recall"        ~ "Recall"
  ))

datos_long$Escenario <- factor(
  datos_long$Escenario, 
  levels = c("Base", "Cost-Sensitive", "ROS", "SMOTE", "Híbrido")
)

ggplot(datos_long, aes(x = Escenario, y = Valor, fill = Metrica)) +
  geom_bar(
    stat = "identity", 
    position = position_dodge(width = 0.7), 
    width = 0.6,          
    color = "white", 
    linewidth = 0.4, 
    alpha = 0.9
  ) +
  geom_text(
    aes(label = round(Valor, 2)), 
    position = position_dodge(width = 0.7), 
    vjust = -0.6, 
    size = 3.6,
    fontface = "bold",
    color = "#2C3E50"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("#1F4E79", "#E74C3C", "#27AE60")) +
  labs(
    title = "Comparativa del Rendimiento Promedio por Escenario",
    subtitle = "Evaluación del impacto de las técnicas de tratamiento del desequilibrio de clases",
    x = "Escenario de Tratamiento",
    y = "Valor Promedio de la Métrica",
    fill = "Métrica de Evaluación"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5, color = "#1A252F"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#566573", margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 11, color = "#1A252F"),
    axis.text = element_text(size = 10, color = "#34495E"),
    axis.text.x = element_text(face = "bold"),
    
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.background = element_rect(fill = "#FDFDFD", color = "#EAECEF"),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#EAECEF", linewidth = 0.5) 
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1.05))












#grupo1

library(caret)
library(pROC)
library(ggplot2)

preds_lasso <- factor(
  ifelse(predict(mod_hibrido_lasso, newdata = test_eval, type = "prob")$Si >= 0.4235, "Si", "No"),
  levels = c("No", "Si")
)

preds_logistica <- factor(
  ifelse(predict(mod_hibrido_logistica, newdata = test_eval, type = "prob")$Si >= 0.3516, "Si", "No"),
  levels = c("No", "Si")
)

cm_lasso <- confusionMatrix(preds_lasso, test_eval$stroke, positive = "Si")
cm_logistica <- confusionMatrix(preds_logistica, test_eval$stroke, positive = "Si")

crear_df_cm <- function(cm_obj, titulo) {

  t <- as.data.frame(cm_obj$table)

  colnames(t) <- c("Predicho", "Real", "Frecuencia")
  t$Modelo <- titulo
  return(t)
}

df_cm <- rbind(
  crear_df_cm(cm_lasso, "Lasso Híbrido"),
  crear_df_cm(cm_logistica, "Reg. Logística Híbrida")
)

df_cm$Real <- factor(df_cm$Real, levels = rev(levels(df_cm$Real)))

p_cm <- ggplot(df_cm, aes(x = Predicho, y = Real, fill = Frecuencia)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Frecuencia), size = 4.5, color = "black") +
  scale_fill_gradient(low = "#C8E6C9", high = "#2E7D32") + 
  facet_wrap(~ Modelo) +
  labs(
    title = "Matrices de Confusión",
    x = "Valor Predicho",
    y = "Valor Real",
    fill = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.text = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10)
  )

print(p_cm)


prob_lasso <- predict(mod_hibrido_lasso, newdata = test_eval, type = "prob")$Si
prob_logistica <- predict(mod_hibrido_logistica, newdata = test_eval, type = "prob")$Si

roc_lasso <- roc(test_eval$stroke, prob_lasso)
roc_logistica <- roc(test_eval$stroke, prob_logistica)

plot(roc_lasso, col = "blue", lwd = 2, main = "Comparativa de Curvas ROC", print.auc = FALSE)
plot(roc_logistica, col = "red", lwd = 2, add = TRUE, print.auc = FALSE)

legend("bottomright", 
       legend = c(
         paste("Lasso Híbrido (AUC =", round(auc(roc_lasso), 4), ")"), 
         paste("Reg. Logística Híbrida (AUC =", round(auc(roc_logistica), 4), ")")
       ),
       col = c("blue", "red"), lty = 1, lwd = 2, cex = 0.8, bg = "white")






#grupo 2

library(caret)
library(pROC)
library(ggplot2)

preds_svm_ros <- factor(
  ifelse(predict(mod_ros_svm_lineal, newdata = test_eval, type = "prob")$Si >= 0.3966, "Si", "No"),
  levels = c("No", "Si")
)

cm_svm <- confusionMatrix(preds_svm_ros, test_eval$stroke, positive = "Si")

df_cm <- as.data.frame(cm_svm$table)
colnames(df_cm) <- c("Predicho", "Real", "Frecuencia")
df_cm$Real <- factor(df_cm$Real, levels = rev(levels(df_cm$Real)))

p_cm <- ggplot(df_cm, aes(x = Predicho, y = Real, fill = Frecuencia)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Frecuencia), size = 4.5, color = "black") +
  scale_fill_gradient(low = "#C8E6C9", high = "#2E7D32") +
  labs(
    title = "Matriz de Confusión: SVM Lineal ROS",
    x = "Valor Predicho",
    y = "Valor Real",
    fill = "Frecuencia"
  ) +
  theme_minimal()

print(p_cm)

#GRUPO 3
library(caret)
library(pROC)
library(ggplot2)

preds_svm_hibrido <- factor(
  ifelse(predict(mod_hibrido_svm_lineal, newdata = test_eval, type = "prob")$Si >= 0.4056, "Si", "No"),
  levels = c("No", "Si")
)

cm_svm_hibrido <- confusionMatrix(preds_svm_hibrido, test_eval$stroke, positive = "Si")

df_cm_hibrido <- as.data.frame(cm_svm_hibrido$table)
colnames(df_cm_hibrido) <- c("Predicho", "Real", "Frecuencia")

df_cm_hibrido$Real <- factor(df_cm_hibrido$Real, levels = rev(levels(df_cm_hibrido$Real)))

p_cm_hibrido <- ggplot(df_cm_hibrido, aes(x = Predicho, y = Real, fill = Frecuencia)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Frecuencia), size = 4.5, color = "black") +
  scale_fill_gradient(low = "#C8E6C9", high = "#2E7D32") + 
  labs(
    title = "Matriz de Confusión: SVM Lineal Híbrido",
    x = "Valor Predicho",
    y = "Valor Real",
    fill = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text = element_text(size = 10)
  )

print(p_cm_hibrido)



#AGRUPADO
library(car)

datos_modelo <- mod_hibrido_logistica$trainingData

datos_modelo$work_type_never <- pmax(
  datos_modelo$work_type_Never_worked, 
  datos_modelo$work_type_children
)


formula_mod_final <- .outcome ~ hypertension_X1 + heart_disease_X1 + gender_Male + 
  ever_married_Yes  + 
  work_type_Self.employed + work_type_Govt_job + 
  work_type_never + Residence_type_Urban + 
  smoking_status_never.smoked + smoking_status_smokes + 
  smoking_status_Unknown + 
  age + avg_glucose_level + bmi

mod_final_agrupado <- glm(formula_mod_final, data = datos_modelo, family = binomial)


print(summary(mod_final_agrupado))


odds_ratio <- exp(coef(mod_final_agrupado))

intervalo_confianza <- exp(confint(mod_final_agrupado))

tabla_or <- data.frame(
  "Odds Ratio" = odds_ratio,
  "IC 2.5%" = intervalo_confianza[, 1],
  "IC 97.5%" = intervalo_confianza[, 2]
)

print("ODDS RATIO E INTERVALOS DE CONFIANZA")
print(tabla_or)




library(ggplot2)

abs_z_values <- abs(summary(mod_final_agrupado)$coefficients[, "z value"])

abs_z_values <- abs_z_values[names(abs_z_values) != "(Intercept)"]

importancia <- data.frame(
  Variable = names(abs_z_values),
  Importancia = abs_z_values
)

importancia <- importancia[order(-importancia$Importancia), ]
top_10 <- head(importancia, 10)

top_10$Variable <- factor(top_10$Variable, levels = rev(top_10$Variable))

ggplot(top_10, aes(x = Importancia, y = Variable)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(
    title = "Las 10 variables más importantes del modelo final",
    x = "Importancia (|z-value|)",
    y = "Variables"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10, face = "bold")
  )



#VIF AGRUPADO
library(car)

valores_vif <- vif(mod_final_agrupado)

df_vif_completo <- data.frame(
  Variable = names(valores_vif),
  VIF = valores_vif
)

df_vif_completo <- df_vif_completo[order(-df_vif_completo$VIF), ]
print(df_vif_completo)






#SHAP

library(shapviz)
library(fastshap)

test_shap <- test_preprocesado
test_shap$work_type_never <- pmax(
  test_shap$work_type_Never_worked, 
  test_shap$work_type_children
)

vars_modelo <- c("hypertension_X1", "heart_disease_X1", "gender_Male", 
                 "ever_married_Yes", "work_type_Self.employed", 
                 "work_type_Govt_job", "work_type_never", "Residence_type_Urban", 
                 "smoking_status_never.smoked", "smoking_status_smokes", 
                 "smoking_status_Unknown", "age", "avg_glucose_level", "bmi")

X_shap <- test_shap[, vars_modelo]
set.seed(789)
shap_values <- explain(
  mod_final_agrupado, 
  X = X_shap,                    
  nsim = 50,                      
  pred_wrapper = function(object, newdata) predict(object, newdata = newdata, type = "response")
)

sv <- shapviz(shap_values, X = X_shap)

sv_importance(sv, kind = "beeswarm")

sv_waterfall(sv, row_id = 1)





