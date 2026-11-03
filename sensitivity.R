workingD <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(workingD))
rm(list = ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(VIM)
library(writexl)
library(patchwork) #para organizar gráficos
library(rstatix) #para polaridad predominante

##############
#CARGAR DATOS
##############

df_final <- read_excel("datos_tras_imputacion.xlsx")

####################################
#Distribucion INES vs disease years
####################################

p_years_vs_ines <- ggplot(df_final, aes(x = new_INES, y = `Disease years`)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 2) +
  #Añade una curva de tendencia suave (LOESS) para ver el patrón general
  geom_smooth(method = "loess", color = "darkblue", fill = "lightblue", alpha = 0.3, se = TRUE) +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(
    title = "INES Score vs. Disease Duration",
    x = "INES Score (0 to 10)",
    y = "Disease Duration (Years)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    panel.grid = element_blank() # Elimina la cuadrícula
  )

ggsave("histograms/scatterplot_DiseaseYears_vs_INES.png", plot = p_years_vs_ines, width = 7, height = 5, dpi = 300)
print(p_years_vs_ines)

#Grafico invertido --> es el bueno

p_ines_vs_years <- ggplot(df_final,
                          aes(x = `Disease years`,
                              y = new_INES)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "loess",
              se = TRUE,
              color = "red",
              linewidth = 1) +
  labs(
    title = "INES across disease duration",
    x = "Disease duration (years)",
    y = "INES"
  ) +
  theme_minimal()

ggsave("histograms/scatterplot_INES_vs_DiseaseYears.png", plot = p_ines_vs_years, width = 7, height = 5, dpi = 300)
print(p_ines_vs_years)

####################################
#Correlación INES vs disease years
####################################

cor.test(
  df_final$new_INES,
  df_final$`Disease years`,
  method = "spearman"
)
#S = 4296519, p = 0.05633, rho = 0.1090414 

cor.test(
  df_final$Sum_INES,
  df_final$`Disease years`,
  method = "spearman"
)
#S = 3595678, p = 6.384e-06, rho = 0.254373

###########################################################
#Correlación de INES con n_hosp y n_sui (medidas gravedad)
###########################################################

cor.test(df_final$new_INES, df_final$Number_hosp, method = "spearman")
#S = 2112447, p-value < 2.2e-16; rho = 0.561947 
cor.test(df_final$new_INES, df_final$Number_suicide, method = "spearman")
#S = 2211523, p-value < 2.2e-16; rho = 0.5414019

###################################################
#Correlaciones de INES y raw sum con edades y años
###################################################

cat("DISTRIBUCIÓN DE DURACIÓN DE ENFERMEDAD\n")
print(table(cut(df_final$`Disease years`, breaks = c(-Inf, 1, 3, 6, Inf), right = FALSE)))
#[-Inf,1)    [1,3)    [3,6) [6, Inf) 
#0        6        9      292 

#TABLA COMPARATIVA DE CORRELACIONES (INES vs. Raw_Sum)

# Lista de variables sociodemográficas a probar
vars_comparar <- c("Disease years", "Edad", "Age of onset")

# Función corregida para calcular Spearman rho y p-valor
calcular_correlaciones <- function(var_interes) {
  res <- lapply(vars_comparar, function(v) {
    if (v %in% names(df_final) && var_interes %in% names(df_final)) {
      # Usamos exact = FALSE para evitar avisos por empates (ties)
      test <- cor.test(df_final[[var_interes]], df_final[[v]], method = "spearman", exact = FALSE)
      data.frame(
        Variable = v,
        Rho = round(test$estimate, 3),
        p_val = format.pval(test$p.value, digits = 3)
      )
    } else {
      NULL
    }
  }) %>% bind_rows()
  return(res)
}

# Pasar el nombre de las columnas como strings ("new_INES" y "Raw_Sum")
cor_INES <- calcular_correlaciones("new_INES") %>% rename(Rho_INES = Rho, p_INES = p_val)
cor_RAW  <- calcular_correlaciones("Sum_INES")  %>% rename(Rho_RawSum = Rho, p_RawSum = p_val)

tabla_comparativa <- left_join(cor_INES, cor_RAW, by = "Variable")

cat("CORRELACIONES: INES y RAW SUM\n")
print(tabla_comparativa)
#Variable Rho_INES   p_INES Rho_RawSum p_RawSum
#1 Disease years    0.109   0.0563      0.254 6.38e-06
#2          Edad   -0.131   0.0219     -0.048    0.399
#3  Age of onset   -0.231 4.23e-05     -0.288  2.7e-07


#Representar distribución de Raw Sum

p_hist_density_raw <- ggplot(df_final, aes(x = Sum_INES)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, fill = "steelblue", color = "white", alpha = 0.6) +
  geom_density(color = "darkblue", fill = "lightblue", alpha = 0.3, linewidth = 1) +
  labs(
    title = "Raw Sum distribution",
    x = "Raw Sum",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    panel.grid = element_blank() # Elimina la cuadrícula para un look más limpio
  )
# Guardar la imagen en alta resolución
ggsave("analisis sensibilidad/density_plot_RawSum.png", plot = p_hist_density_raw, width = 7, height = 5, dpi = 300)
print(p_hist_density_raw)

##############################
#Análisis con distintos pesos
##############################

#Definición de la función generadora de INES según peso w
calcular_ines_peso <- function(df, w) {
  num <- w * (df$Sum_INES / log2(df$`Disease years`)) + (1 - w) * df$Sum_INES + 1
  ines <- (log2(num) / log2(19)) * 10
  return(ines)
}

#Creación de las variables alternativas en df_final
df_final <- df_final %>%
  mutate(
    INES_w0.5_orig = new_INES, # Valor original ya guardado
    INES_w0.6      = calcular_ines_peso(., 0.6),
    INES_w0.4      = calcular_ines_peso(., 0.4),
    INES_w0.7      = calcular_ines_peso(., 0.7),
    INES_w0.3      = calcular_ines_peso(., 0.3),
    INES_w0.8      = calcular_ines_peso(., 0.8),
    INES_w0.2      = calcular_ines_peso(., 0.2),
    INES_w0.9      = calcular_ines_peso(., 0.9),
    INES_w0.1      = calcular_ines_peso(., 0.1),
    INES_w1.0_only = calcular_ines_peso(., 1.0),
    INES_w0.0_raw  = calcular_ines_peso(., 0.0)
  )

colnames(df_final) #se han calculado los INES con todos los pesos

#Lista de columnas de INES a evaluar
ines_vars <- c("INES_w1.0_only", "INES_w0.9", "INES_w0.8", "INES_w0.7", "INES_w0.6", 
               "INES_w0.5_orig", 
               "INES_w0.4", "INES_w0.3", "INES_w0.2", "INES_w0.1", "INES_w0.0_raw")


#ESTADÍSTICOS DESCRIPTIVOS Y DISTRIBUCIÓN

cat("\nDISTRIBUCIÓN Y DESCRIPTIVOS POR VARIANTE DE PESO\n")

tabla_descriptivos <- lapply(ines_vars, function(v) {
  data.frame(
    Variante = v,
    Media = round(mean(df_final[[v]], na.rm = TRUE), 2),
    SD = round(sd(df_final[[v]], na.rm = TRUE), 2),
    Mediana = round(median(df_final[[v]], na.rm = TRUE), 2),
    Min = round(min(df_final[[v]], na.rm = TRUE), 2),
    Max = round(max(df_final[[v]], na.rm = TRUE), 2)
  )
}) %>% bind_rows()

print(tabla_descriptivos)


#TABLA DE CORRELACIONES CON VARIABLES DE EDAD

variables_target <- c("Disease years", "Edad", "Age of onset")

tabla_sensibilidad_cor <- lapply(ines_vars, function(ines_col) {
  res_row <- data.frame(Variante = ines_col)
  
  for (target in variables_target) {
    test <- cor.test(df_final[[ines_col]], df_final[[target]], method = "spearman", exact = FALSE)
    rho_val <- round(test$estimate, 3)
    p_val   <- format.pval(test$p.value, digits = 3)
    
    res_row[[paste0("Rho_", target)]] <- rho_val
    res_row[[paste0("p_", target)]]   <- p_val
  }
  return(res_row)
}) %>% bind_rows()

cat("\nCORRELACIONES (SPEARMAN) POR VARIANTE DE PESO\n")
print(tabla_sensibilidad_cor)

#Guardar resultados en Excel
writexl::write_xlsx(
  list(Descriptivos = tabla_descriptivos, Correlaciones = tabla_sensibilidad_cor),
  "analisis sensibilidad/Analisis_Sensibilidad_diferentes_Pesos_INES.xlsx")

writexl::write_xlsx(df_final, "analisis sensibilidad/df_final_con_distintos_pesos_INES.xlsx")

#GRÁFICOS DE DISPERSIÓN COMPONENTES (VS DISEASE DURATION)

# Transformar datos a formato largo para Facet Plot en ggplot2
df_long_pesos <- df_final %>%
  select(`Disease years`, all_of(ines_vars)) %>%
  pivot_longer(
    cols = -`Disease years`,
    names_to = "Variante_Peso",
    values_to = "Valor_INES"
  )

p_facet_pesos <- ggplot(df_long_pesos, aes(x = `Disease years`, y = Valor_INES)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 1.2) +
  geom_smooth(method = "loess", color = "red", se = TRUE, linewidth = 0.8) +
  facet_wrap(~ Variante_Peso, ncol = 3) +
  labs(
    title = "INES vs. Disease Duration across Weight Configurations",
    x = "Disease Duration (years)",
    y = "INES Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold")
  )

#Guardar figura multifaceta
ggsave("analisis sensibilidad/facet_scatterplot_different_INES_weights.png", plot = p_facet_pesos, width = 10, height = 10, dpi = 300)
print(p_facet_pesos)

#DISTRIBUCIONES DEL INES CON DISTINTOS PESOS

#Preparar datos en formato largo para ggplot
df_long_dist <- df_final %>%
  select(all_of(ines_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variante_Peso",
    values_to = "Valor_INES"
  )

#Generar el Facet Plot de Distribuciones
p_facet_dist <- ggplot(df_long_dist, aes(x = Valor_INES)) +
  geom_histogram(
    aes(y = after_stat(density)), 
    bins = 30, 
    fill = "steelblue", 
    color = "white", 
    alpha = 0.7
  ) +
  geom_density(color = "darkred", linewidth = 0.9) +
  facet_wrap(~ Variante_Peso, ncol = 3, scales = "free_y") +
  scale_x_continuous(limits = c(0, 10.5), breaks = seq(0, 10, by = 2)) +
  labs(
    title = "INES Score Distributions across Weight Configurations",
    x = "INES Score",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    panel.grid = element_blank(), # Mantiene la coherencia sin cuadrículas
    strip.text = element_text(face = "bold")
  )

# 3. Guardar el gráfico en la carpeta histograms/
ggsave(
  "analisis sensibilidad/facet_distributions_different_INES_weights.png", 
  plot = p_facet_dist, 
  width = 10, 
  height = 10, 
  dpi = 300
)

print(p_facet_dist)


################################
#INES vs POLARIDAD PREDOMINANTE
################################

#Usando las tres polaridades (incluida la unknown)
df_final <- read_excel("datos_tras_imputacion.xlsx")
table(df_final$`Predominant polarity`)
#DPP MPP UPP 
#156  94  57


#DESCRIPTIVOS DE INES POR GRUPO DE POLARIDAD

cat("\nDESCRIPTIVOS DE INES POR PREDOMINANT POLARITY\n")

tabla_polaridad_desc <- df_final %>%
  filter(!is.na(`Predominant polarity`)) %>%
  group_by(`Predominant polarity`) %>%
  summarise(
    N = n(),
    Media = round(mean(new_INES, na.rm = TRUE), 2),
    SD = round(sd(new_INES, na.rm = TRUE), 2),
    Mediana = round(median(new_INES, na.rm = TRUE), 2),
    IQR_25 = round(quantile(new_INES, 0.25, na.rm = TRUE), 2),
    IQR_75 = round(quantile(new_INES, 0.75, na.rm = TRUE), 2)
  )

print(tabla_polaridad_desc)

#PRUEBA DE KRUSKAL-WALLIS Y POST-HOC DE DUNN (por las 3 polaridades)

cat("\nKRUSKAL-WALLIS TEST\n")
kw_res <- kruskal.test(new_INES ~ `Predominant polarity`, data = df_final)
print(kw_res)

#Post-hoc de Dunn (comparaciones por pares)
cat("\nPOST-HOC DE DUNN (AJUSTE BONFERRONI)\n")
dunn_res <- df_final %>%
  rename(Predominant_polarity = `Predominant polarity`) %>%
  #filter(!is.na(`Predominant polarity`)) %>%
  dunn_test(new_INES ~ Predominant_polarity, p.adjust.method = "bonferroni")

print(dunn_res)

# Guardar resultados numéricos en Excel
#write_xlsx(
#  list(Descriptivos_UPP_MPP_DPP = tabla_polaridad_desc, PostHoc_Dunn_UPP_MPP_DPP = as.data.frame(dunn_res)),
#  "analisis sensiblidad/Resultados_INES_vs_PredominantPolarity.xlsx"
#)


#Boxplot con las tres polaridades

p_boxplot_polaridad <- ggplot(
  df_final %>% filter(!is.na(`Predominant polarity`)), 
  aes(x = `Predominant polarity`, y = new_INES, fill = `Predominant polarity`)
) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2, width = 0.45) +
  geom_jitter(width = 0.12, alpha = 0.35, size = 1.5, color = "black") + # Muestra los puntos individuales
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(0, 10.5), breaks = seq(0, 10, by = 2)) +
  labs(
    title = "INES Score by Predominant Polarity",
    x = "Predominant Polarity",
    y = "INES Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    panel.grid = element_blank(),
    legend.position = "none"
  )

#Guardar gráfico
ggsave(
  "analisis sensibilidad/boxplot_INES_vs_PredominantPolarity_UPP_MPP_DPP.png", 
  plot = p_boxplot_polaridad, 
  width = 7, 
  height = 5, 
  dpi = 300
)

print(p_boxplot_polaridad)

#Usando solo MPP y DPP


#Filtar los datos para excluir las unknown
df_polaridad_sub <- df_final %>%
  filter(`Predominant polarity` %in% c("DPP", "MPP"))

#DESCRIPTIVOS POR GRUPO (DPP vs MPP)

cat("\nDESCRIPTIVOS DE INES: DPP VS. MPP\n")

tabla_dpp_mpp_desc <- df_polaridad_sub %>%
  group_by(`Predominant polarity`) %>%
  summarise(
    N = n(),
    Media = round(mean(new_INES, na.rm = TRUE), 2),
    SD = round(sd(new_INES, na.rm = TRUE), 2),
    Mediana = round(median(new_INES, na.rm = TRUE), 2),
    IQR_25 = round(quantile(new_INES, 0.25, na.rm = TRUE), 2),
    IQR_75 = round(quantile(new_INES, 0.75, na.rm = TRUE), 2)
  )

print(tabla_dpp_mpp_desc)

#PRUEBA DE WILCOXON / MANN-WHITNEY Y TAMAÑO DEL EFECTO

cat("\nWILCOXON RANK-SUM TEST + EFFECT SIZE (DPP VS. MPP)\n")

#Test de Wilcoxon tabulado (contiene W y p-valor)
wilcox_tabla <- df_polaridad_sub %>%
  rename(Predominant_polarity = `Predominant polarity`) %>%
  wilcox_test(new_INES ~ Predominant_polarity, detailed = TRUE)

#Tamaño del efecto (r de Rosenthal)
eff_size <- df_polaridad_sub %>%
  rename(Predominant_polarity = `Predominant polarity`) %>%
  wilcox_effsize(new_INES ~ Predominant_polarity)

#Combinar ambas salidas en una sola tabla
tabla_wilcoxon_completa <- wilcox_tabla %>%
  select(group1, group2, n1, n2, statistic, p) %>%
  rename(W = statistic, p_val = p) %>%
  left_join(
    eff_size %>% select(group1, group2, effsize, magnitude),
    by = c("group1", "group2")
  ) %>%
  rename(r = effsize)

print(tabla_wilcoxon_completa)

#Guardar resultados en Excel
write_xlsx(
  list(
    Descriptivos_UPP_MPP_DPP = tabla_polaridad_desc, 
    PostHoc_Dunn_UPP_MPP_DPP = as.data.frame(dunn_res),
    Descriptivos_MPP_DPP = tabla_dpp_mpp_desc, 
    Wilcoxon_MPP_DPP = tabla_wilcoxon_completa
  ),
  "analisis sensibilidad/Resultados_INES_vs_PredominantPolarity.xlsx")


#Boxplot con DPP vs. MPP

p_boxplot_dpp_mpp <- ggplot(
  df_polaridad_sub, 
  aes(x = `Predominant polarity`, y = new_INES, fill = `Predominant polarity`)
) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2, width = 0.4) +
  geom_jitter(width = 0.1, alpha = 0.35, size = 1.5, color = "black") +
  scale_fill_manual(values = c("DPP" = "#4C72B0", "MPP" = "#C44E52")) + # Azul para DPP, rojo para MPP
  scale_y_continuous(limits = c(0, 10.5), breaks = seq(0, 10, by = 2)) +
  labs(
    title = "INES Score: DPP vs. MPP",
    x = "Predominant Polarity",
    y = "INES Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
    panel.grid = element_blank(),
    legend.position = "none"
  )

#Guardar figura
ggsave(
  "analisis sensibilidad/boxplot_INES_DPP_vs_MPP.png", 
  plot = p_boxplot_dpp_mpp, 
  width = 6, 
  height = 5, 
  dpi = 300)

print(p_boxplot_dpp_mpp)

