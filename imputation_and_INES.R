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

##############
#Load data
##############

df <- read_excel("Data_for_revision.xlsx")

# See column names
names(df)
head(df)

###################
#Clean data
###################
#Remove unnecessary variables if present
df_clean <- df %>%
  dplyr::select(-c("Rapid cycling YES/NO", "Categorical_mixed", "Number_mixed")) %>%
  dplyr::select(-c("Categorical_suicide_attempts", "Categorical_manic", "Categorical_depressive", "Categorical hipomanic", "Categorical_hospitalizations"))

# KNN imputation of missing data points

df_for_imputation <- df_clean %>% #remove variables not to be considered in imputation (IDs y GAF, CGI, WHODAS)
  select(
    -ID,
    -ID_complete,
    -CGI,
    -GAF,
    -WHODAS
  ) %>%
  mutate( #set as factor
    Sexo = as.factor(Sexo),
    BD_subtype = as.factor(BD_subtype),
    `Predominant polarity` = as.factor(`Predominant polarity`),
    `Psychosis YES/NO` = as.factor(`Psychosis YES/NO`)
  ) %>%
  mutate( #set as numeric
    across(
      c(
        Edad,
        `Age of onset`,
        `Disease years`,
        Number_suicide,
        Number_hosp,
        Number_manic,
        Number_hipomanic,
        Number_depressive
      ),
      as.numeric
    )
  )

complete_data <- df_for_imputation[ #271 complete data
  complete.cases(df_for_imputation),]

#Select K only with variables that are actually going to be imputed
set.seed(1234)
vars_a_imputar <- c(
  "Number_suicide", 
  "Number_hosp", 
  "Number_manic", 
  "Number_hipomanic", 
  "Number_depressive"
)

prop_missing <- 0.20 #Create a mask of 20% of NAs ONLY for those 5 variables
mask <- matrix(FALSE, nrow = nrow(complete_data), ncol = ncol(complete_data))
colnames(mask) <- colnames(complete_data)

mask[, vars_a_imputar] <- matrix( #prodece random NAs 
  runif(nrow(complete_data) * length(vars_a_imputar)) < prop_missing,
  nrow = nrow(complete_data))

data_missing <- complete_data 
data_missing[mask] <- NA

num_cols <- sapply(complete_data, is.numeric)
fact_cols <- sapply(complete_data, is.factor)

#Loop to identify K

ks <- 2:25
rmse_vec <- numeric(length(ks))

for(i in seq_along(ks)) {
  
  imp <- kNN( #try different K
    data_missing,
    k = ks[i],
    imp_var = FALSE
  )
  
  #evaluate RMSE only in the marsked cells 
  true_num <- as.numeric(as.matrix(complete_data)[mask])
  pred_num <- as.numeric(as.matrix(imp)[mask])
  rmse_vec[i] <- sqrt(mean((true_num - pred_num)^2, na.rm = TRUE))
}

results <- data.frame(
  k = ks,
  RMSE = rmse_vec)

best_k <- results$k[which.min(results$RMSE)] #smallest RMSE
print(results)
cat("Optimum K:", best_k, "\n") #K = 3

#Impute with chosen K

df_imputed_vars <- kNN(
  df_for_imputation,
  k = best_k,
  imp_var = FALSE)
df_final <- bind_cols(
  df_clean %>% select(ID, ID_complete, CGI, GAF, WHODAS),
  df_imputed_vars)

#verify NAs
colSums(is.na(df_final))

#write_xlsx(df_final, "datos_tras_imputacion.xlsx")

######################
#Calculate bins
######################

vars_eventos <- c(
  "Number_suicide", 
  "Number_hosp", 
  "Number_manic", 
  "Number_hipomanic", 
  "Number_depressive")

tabla_terciles <- lapply(vars_eventos, function(var) {
  vals <- df_final[[var]]
  vals_pos <- vals[vals > 0]
  
  q <- quantile(vals_pos, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE) 
  
  data.frame(
    Variable = var,
    N_distinto_de_0 = length(vals_pos),
    Min_P0 = q[1],
    Tercil_1_P33 = round(q[2], 2),
    Tercil_2_P66 = round(q[3], 2),
    Max_P100 = q[4]
  )
}) %>% bind_rows()

print(tabla_terciles)

#Apply to selected variables
df_final <- df_final %>%
  mutate(
    #suicide attempts (P33 = 1, P66 = 2)
    Categorica_Number_suicide = factor(case_when(
      Number_suicide == 0 ~ 0,
      Number_suicide == 1 ~ 1,
      Number_suicide == 2 ~ 2,
      Number_suicide >= 3 ~ 3
    ), levels = 0:3),
    
    #Hospitalizations (P33 = 2, P66 = 4)
    Categorica_Number_hosp = factor(case_when(
      Number_hosp == 0 ~ 0,
      Number_hosp %in% 1:2 ~ 1,
      Number_hosp %in% 3:4 ~ 2,
      Number_hosp >= 5 ~ 3
    ), levels = 0:3),
    
    #Manic episodes (P33 = 2, P66 = 5)
    Categorica_Number_manic = factor(case_when(
      Number_manic == 0 ~ 0,
      Number_manic %in% 1:2 ~ 1,
      Number_manic %in% 3:5 ~ 2,
      Number_manic >= 6 ~ 3
    ), levels = 0:3),
    
    #Hipomanic episodes (P33 = 3, P66 = 10)
    Categorica_Number_hipomanic = factor(case_when(
      Number_hipomanic == 0 ~ 0,
      Number_hipomanic %in% 1:3 ~ 1,
      Number_hipomanic %in% 4:10 ~ 2,
      Number_hipomanic >= 11 ~ 3
    ), levels = 0:3),
    
    #Depressive (P33 = 3, P66 = 10)
    Categorica_Number_depressive = factor(case_when(
      Number_depressive == 0 ~ 0,
      Number_depressive %in% 1:3 ~ 1,
      Number_depressive %in% 4:10 ~ 2,
      Number_depressive >= 11 ~ 3
    ), levels = 0:3)
  )
#Check distribution and plot

lapply(df_final %>% select(starts_with("Categorica_")), table, useNA = "ifany")

vars_cat <- c(
  "Categorica_Number_suicide", 
  "Categorica_Number_hosp", 
  "Categorica_Number_manic", 
  "Categorica_Number_hipomanic", 
  "Categorica_Number_depressive"
)

#Titulos
titulos <- c(
  "Suicide Attempts (Bins)",
  "Hospitalizations (Bins)",
  "Manic Episodes (Bins)",
  "Hypomanic Episodes (Bins)",
  "Depressive Episodes (Bins)"
)

for (i in seq_along(vars_cat)) {
  var <- vars_cat[i]
  title <- titulos[i]
  
  p <- ggplot(df_final, aes(x = .data[[var]], fill = .data[[var]])) +
    geom_bar(color = "black", alpha = 0.85) +
    geom_text(
      stat = "count", 
      aes(label = paste0(after_stat(count), " (", round(after_stat(count)/sum(after_stat(count))*100, 1), "%)")),
      vjust = -0.5, 
      size = 3.5
    ) +
    scale_fill_brewer(palette = "Blues") +
    scale_x_discrete(labels = c("0" = "Grade 0\n(0)", "1" = "Grade 1\n(Low)", "2" = "Grade 2\n(Mod)", "3" = "Grade 3\n(High)")) +
    labs(
      title = paste("Distribution of", title),
      x = "Ordinal Category (Grade)",
      y = "Patient Count"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      panel.grid.major.x = element_blank()
    ) +
    ylim(0, max(table(df_final[[var]])) * 1.15) 
  

  ggsave(
    filename = file.path("histograms", paste0(var, "_new_bins.png")),
    plot = p,
    width = 6,
    height = 4,
    dpi = 300
  )
}

#Variable psicosis as 0 or 3

df_final <- df_final %>%
  mutate( #Asigna 0 si es NO (0) y 3 si es SÍ (1)
    Categorical_psychosis = factor(
      ifelse(as.numeric(as.character(`Psychosis YES/NO`)) == 1, 3, 0),
      levels = c(0, 3)))

#write_xlsx(df_final, "datos_tras_imputacion.xlsx")

###############
#Calculate INES 
###############

df_final <- df_final %>%
  mutate(
    num_suicide    = as.numeric(as.character(Categorica_Number_suicide)),
    num_hosp       = as.numeric(as.character(Categorica_Number_hosp)),
    num_manic      = as.numeric(as.character(Categorica_Number_manic)),
    num_hipomanic  = as.numeric(as.character(Categorica_Number_hipomanic)),
    num_depressive = as.numeric(as.character(Categorica_Number_depressive)),
    num_psychosis  = as.numeric(as.character(Categorical_psychosis)),
    
    # cumulative burden score (max = 18)
    Sum_INES = num_suicide + num_hosp + num_manic + num_hipomanic + num_depressive + num_psychosis,
    
    # new_INES
    # log2(x) en R es log2(x)
    new_INES = (log2((Sum_INES / 2) * ((1 / log2(`Disease years`)) + 1) + 1) / log2(19)) * 10
  ) %>%
  select(-num_suicide, -num_hosp, -num_manic, -num_hipomanic, -num_depressive, -num_psychosis)

#write_xlsx(df_final, "datos_tras_imputacion.xlsx")

#############################################
#Statisticsc summary and distribution
#############################################

df_final <- read_excel("datos_tras_imputacion.xlsx")

summary(df_final$new_INES)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.679   4.770   5.659   5.577   6.573   7.846 
mean(df_final$new_INES) #5.576875
sd(df_final$new_INES) #1.260412

#histogram
hist(df_final$new_INES, 
     main = "INES distribution", 
     xlab = "INES", 
     col = "steelblue", 
     breaks = 20)


#density plot
p_ines <- ggplot(df_final, aes(x = new_INES)) +
  geom_density(fill = "steelblue", color = "steelblue", linewidth = 1) +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(
    title = "INES distribution",
    x = "INES",
    y = "Density"
  ) +
  theme_minimal() #+
  #theme(
  #  plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
  #  panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
  #  panel.grid.minor = element_line(color = "grey92", linewidth = 0.2)
  #)

p_ines_hist <- ggplot(df_final, aes(x = new_INES)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white", alpha = 0.85) +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
  labs(
    title = "INES Histogram Distribution",
    x = "INES",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        panel.grid = element_blank())

ggsave("histograms/density_new_INES.png", plot = p_ines, width = 7, height = 4, dpi = 300)
ggsave("histograms/histogram_new_INES.png", plot = p_ines_hist, width = 7, height = 4, dpi = 300)

print(p_ines)
print(p_ines_hist)
