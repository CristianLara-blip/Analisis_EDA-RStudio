# Cargar paquetes necesarios
library(readr)
library(dplyr)
library(ggplot2)

rm(list = ls())

# Cargar los datos
Salaries <- read_csv("Salaries.csv")

# Ver las primeras filas de los datos
head(Salaries)

# Verificar duplicados en una columna específica, por ejemplo, 'salary'
any(duplicated(Salaries))

# Verificar si hay filas con datos faltantes en cualquier columna
any(!complete.cases(Salaries))

# Suponiendo que tu conjunto de datos se llama "data" y la columna de rangos académicos se llama "rank"
unique_ranks <- unique(Salaries$rank)
num_unique_ranks <- length(unique_ranks)
print(num_unique_ranks)

# Análisis descriptivo
summary(Salaries)

# Convertir la variable "sex" a un formato numérico
data <- Salaries %>% mutate(sex_numeric = ifelse(sex == "Male", 0, 1))

# Comparación de salarios por género y rango académico
ggplot(data, aes(x = reorder(rank, salary, FUN = median), y = salary, fill = sex)) +
  geom_boxplot() +
  labs(title = "Distribución de salarios por rango académico y género",
       x = "Rango Académico",
       y = "Salario") +
  theme_minimal()

# Correlación entre género (convertido a numérico) y salario
correlation_matrix <- cor(data[, c("salary", "sex_numeric")], method = "spearman")
print(correlation_matrix)

# Gráfico de dispersión de salario vs. años de servicio
ggplot(data, aes(x = yrs.service, y = salary)) +
  geom_point() +
  labs(title = "Relación entre años de servicio y salario",
       x = "Años de Servicio",
       y = "Salario") +
  theme_minimal()
