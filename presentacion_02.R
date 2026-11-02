# 🛠️ 1. Configuración del Entorno y Carga de Librerías

# Instalar paquetes si no los tienes instalados
# install.packages(c("tidyverse", "lubridate"))

library(tidyverse)
library(lubridate)

# Verificar versiones de las librerías
print(paste("Versión de R:", R.version.string))
print(paste("Versión de dplyr:", packageVersion("dplyr")))
print(paste("Versión de readr:", packageVersion("readr")))

# Carga de datos
peru_df <- read_csv("peru.csv")
print(peru_df)

# 🧼 2. Diagnóstico y Limpieza de Datos Inicial

# Identificar y mostrar metadatos y estructura
glimpse(peru_df)
summary(peru_df)

# Manejo y conteo de valores nulos (faltantes)
valores_nulos <- colSums(is.na(peru_df))
print(valores_nulos)

# Mostrar filas duplicadas
filas_duplicadas <- peru_df[duplicated(peru_df) | duplicated(peru_df, fromLast = TRUE), ]
print(filas_duplicadas)

# Eliminar duplicados manteniendo la primera ocurrencia
peru_df <- peru_df %>% distinct(.keep_all = TRUE)

# Asignar valores de población nulos con datos reales y exactos usando condiciones
peru_df$pob[peru_df$capital == "Huaraz"] <- 118
peru_df$pob[peru_df$capital == "Cusco"] <- 137
peru_df$pob[peru_df$capital == "Iquitos"] <- 103

# Renombrado profesional de columnas
peru_df <- peru_df %>%
  rename(
    CAPITAL = capital,
    `POBLACION (millones)` = pob,
    DEPARTAMENTO = departamento,
    `SUPERFICIE (km²)` = `area km2`,
    PROVINCIAS = provincias,
    REGION = region,
    `ALTITUD (msnm)` = `altitud (m)`,
    PIB = pibPerCapita,
    FECHA = fecha
  )

# Establecer DEPARTAMENTO como nombre de fila (equivalente a set_index en Pandas)
peru_df <- peru_df %>% column_to_rownames("DEPARTAMENTO")

# Convertir la columna FECHA a formato Date
peru_df$FECHA <- mdy(peru_df$FECHA)
print(peru_df)

# 📊 3. Análisis Exploratorio de Datos (EDA) e Ingeniería de Características

# Estadísticas descriptivas generales
summary(peru_df)

# Extraer el nombre del día en español usando lubridate
peru_df <- peru_df %>%
  mutate(
    `DIA SEMANA ENG` = wday(FECHA, label = TRUE, abbr = FALSE, locale = "en_US.UTF-8"),
    `DIA SEMANA` = case_when(
      `DIA SEMANA ENG` == "Monday" ~ "Lunes",
      `DIA SEMANA ENG` == "Tuesday" ~ "Martes",
      `DIA SEMANA ENG` == "Wednesday" ~ "Miércoles",
      `DIA SEMANA ENG` == "Thursday" ~ "Jueves",
      `DIA SEMANA ENG` == "Friday" ~ "Viernes",
      `DIA SEMANA ENG` == "Saturday" ~ "Sábado",
      `DIA SEMANA ENG` == "Sunday" ~ "Domingo"
    )
  )

# Eliminar columna temporal en inglés
peru_df <- peru_df %>% select(-`DIA SEMANA ENG`)

# Ingeniería de Características Personalizada

# Normalización de datos de población (dividir entre 1000)
peru_df <- peru_df %>%
  mutate(`POBLACION (millones)` = `POBLACION (millones)` / 1000)

# Cálculo de la Densidad Poblacional (hab/km²)
peru_df <- peru_df %>%
  mutate(`DENSIDAD (hab/km²)` = (`POBLACION (millones)` * 1000000) / `SUPERFICIE (km²)`)

# Categorización de departamentos basada en su desempeño económico con función personalizada
segmentar_pib <- function(valor) {
  if (valor > 9000) {
    return("Alto")
  } else if (valor >= 5000 & valor <= 9000) {
    return("Medio")
  } else {
    return("Bajo")
  }
}

peru_df <- peru_df %>%
  mutate(`NIVEL ECONOMICO` = sapply(PIB, segmentar_pib))

print(peru_df)

# Selección de Columnas, Indexación y Filtrado

# Selección de atributos específicos
subset_columnas <- peru_df %>% select(CAPITAL, `ALTITUD (msnm)`, REGION)
print(subset_columnas)

# Selección por etiqueta (equivalente a .loc de Pandas)
subset_etiquetas <- peru_df[c("Cusco", "Arequipa", "Lima"), c("POBLACION (millones)", "REGION")]
print(subset_etiquetas)

# Selección por posición (equivalente a .iloc de Pandas)
subset_posicion <- peru_df[c(8, 3, 15), c(2, 5)]
print(subset_posicion)

# Formas de Filtrar

# Filtro 1: Población < 0.5 Millones
filtro_pob_bajo <- peru_df %>% filter(`POBLACION (millones)` < 0.5)
print(filtro_pob_bajo)

# Filtro 2: Región Sierra
filtro_sierra <- peru_df %>% filter(REGION == "Sierra")
print(filtro_sierra)

# Filtro 3: Máscara combinada (.loc con condiciones y columnas específicas)
filtro_mascara <- peru_df %>%
  filter(`POBLACION (millones)` < 0.5) %>%
  select(CAPITAL, REGION)
print(filtro_mascara)

# Filtro 4: Operador %in% (equivalente a .isin())
filtro_isin <- peru_df %>% filter(REGION %in% c("Sierra", "Selva"))
print(filtro_isin)

# Filtro 5: rangos inclusivos

filtro_rango <- peru_df %>%
  filter(`POBLACION (millones)` >= 0.2 & `POBLACION (millones)` <= 0.5)

# Filtro 6: texto parcial

library(dplyr)
library(stringr)

# Usando str_detect con ignore_case = TRUE (equivalente a case=False)
filtro_texto <- peru_df %>%
  filter(str_detect(CAPITAL, regex("chapo", ignore_case = TRUE)))

print(filtro_texto)

# Agregaciones y Operaciones por Grupos (group_by y summarise)

# Agrupar por región y calcular métricas estadísticas clave
resumen_region <- peru_df %>%
  rownames_to_column("DEPARTAMENTO") %>%
  group_by(REGION) %>%
  summarise(
    `Total Departamentos` = n(),
    `PIB Promedio` = mean(PIB, na.rm = TRUE),
    `Población Total (M)` = sum(`POBLACION (millones)`, na.rm = TRUE),
    `Altitud Máxima (msnm)` = max(`ALTITUD (msnm)`, na.rm = TRUE)
  )

print(resumen_region)

# Ordenamiento y Creación de Atributo

library(dplyr)

# 1. Ordenar por población de forma descendente (equivalente a sort_values con ascending=False)
peru_ordenado_pob <- peru_df %>% 
  arrange(desc(`POBLACION (millones)`))

print(peru_ordenado_pob)

# 2. Ordenar por index / nombre de fila (equivalente a sort_index)
peru_ordenado_index <- peru_df %>% 
  arrange(rownames(peru_df))

print(peru_ordenado_index)

# Multi-Agregación

# Estadísticas en lote para Población y PIB
stats_lote <- peru_df %>%
  summarise(
    across(c(`POBLACION (millones)`, PIB), 
           list(
             min  = ~min(., na.rm = TRUE),
             max  = ~max(., na.rm = TRUE),
             mean = ~mean(., na.rm = TRUE)
           ))
  )

print(stats_lote)

# Segmentación avanzada de información por REGION
segmentacion_region <- peru_df %>%
  group_by(REGION) %>%
  summarise(
    # Métricas para Población
    pob_mean = mean(`POBLACION (millones)`, na.rm = TRUE),
    pob_sum  = sum(`POBLACION (millones)`, na.rm = TRUE),
    pob_sd   = sd(`POBLACION (millones)`, na.rm = TRUE),
    
    # Métricas para PIB
    pib_mean = mean(PIB, na.rm = TRUE),
    pib_max  = max(PIB, na.rm = TRUE),
    
    # Métricas para Altitud
    altitud_mean = mean(`ALTITUD (msnm)`, na.rm = TRUE)
  )

print(segmentacion_region)

# Definir la función personalizada con un parámetro por defecto k = 1.2
promedio_con_constante <- function(columna, k = 1.2) {
  mean(columna * k, na.rm = TRUE)
}

# Aplicar la función personalizada a las columnas especificadas con k = 1.5
agg_personalizado <- peru_df %>%
  summarise(
    across(c(`POBLACION (millones)`, PIB), ~ promedio_con_constante(.x, k = 1.5))
  )

print(agg_personalizado)

# Análisis de Correlación

# Definir la lista de columnas estrictamente numéricas seleccionadas
columnas_numericas <- c(
  "POBLACION (millones)",
  "SUPERFICIE (km²)",
  "PROVINCIAS",
  "ALTITUD (msnm)",
  "PIB",
  "DENSIDAD (hab/km²)"
)

# Filtrar el DataFrame (usando corchetes y asegurando nombres con espacios)
peru_numerico_filtrado <- peru_df[, columnas_numericas]

# Calcular la matriz de correlación de Pearson (equivalente a .corr(method="pearson"))
matriz_correlacion <- cor(peru_numerico_filtrado, method = "pearson", use = "complete.obs")

# Mostrar la matriz de correlación
print(matriz_correlacion)

# Pruebas de hipótesis (t-test para dos muestras independientes)

# Hipótesis Nula (H0): El PIB promedio de la Costa y de la Sierra es igual.
# Hipótesis Alternativa (H1): El PIB promedio de la Costa y de la Sierra es diferente.

# Filtrar muestras válidas quitando nulos si existieran (equivalente a Welch's t-test con equal.var = FALSE)
pib_costa <- peru_df$PIB[peru_df$REGION == "Costa"]
pib_sierra <- peru_df$PIB[peru_df$REGION == "Sierra"]

# Realizar el t-test (Welch's t-test por defecto con var.equal = FALSE)
test_resultado <- t.test(pib_costa, pib_sierra, var.equal = FALSE)

t_stat <- test_resultado$statistic
p_val <- test_resultado$p.value

# Imprimir resultados formateados
cat(sprintf("Estadístico t: %.4f\n", t_stat))
cat(sprintf("Valor p (p-value): %.6f\n", p_val))

if (p_val < 0.05) {
  cat("Rechazamos la hipótesis nula (H0). Existe una diferencia estadísticamente significativa entre el PIB de la Costa y la Sierra.\n")
} else {
  cat("No se puede rechazar la hipótesis nula (H0). No hay evidencia suficiente para afirmar que los PIB promedio sean distintos.\n")
}

# Restaurar temporalmente los nombres de filas como columna para trabajar cómodamente
peru_limpio <- peru_df %>% rownames_to_column("DEPARTAMENTO")

# print(resumen_multinivel)

# 💾 7. Exportación de Datos Procesados

# 1. Exportar el DataFrame principal limpio y enriquecido a CSV
write_csv(peru_limpio, "peru_procesado_completo.csv")

# 2. Exportar los dataframes de resumen generados
write_csv(resumen_region, "resumen_por_region.csv")
write_csv(resumen_multinivel, "resumen_multinivel.csv")


library(tidyverse)

# Crear un segundo data frame de ejemplo (con datos coincidentes y no coincidentes)
autoridades_df <- tibble(
  DEPARTAMENTO = c("Arequipa", "Lima", "Cusco", "Piura", "Loreto", "DepartamentoFantasma"),
  ALCALDE = c("Victor Hugo", "Rafael Lopez", "Luis Pantoja", "Gabriel Madrid", "Doris Barry", "Nadie"),
  PARTIDO = c("Alianza", "Renovación", "Cusco Lindo", "Unidad", "Amazonas", "Fantasma")
)

# Asegurar que nuestro data frame principal tenga 'DEPARTAMENTO' como columna normal
peru_base <- peru_df %>% rownames_to_column("DEPARTAMENTO")

# 1. Inner Join (Equivalente a how = 'inner' en Pandas)
inner_res <- peru_base %>%
  inner_join(autoridades_df, by = "DEPARTAMENTO")

print(inner_res %>% select(DEPARTAMENTO, CAPITAL, ALCALDE, PARTIDO))


# 2. Left Join (Equivalente a how = 'left' en Pandas)
left_res <- peru_base %>%
  left_join(autoridades_df, by = "DEPARTAMENTO")

print(left_res %>% select(DEPARTAMENTO, CAPITAL, ALCALDE, PARTIDO) %>% head(10))

# 3. Right Join (Equivalente a how = 'right' en Pandas)
right_res <- peru_base %>%
  right_join(autoridades_df, by = "DEPARTAMENTO")

print(right_res %>% select(DEPARTAMENTO, CAPITAL, ALCALDE, PARTIDO))

# 4. Full Join (Equivalente a how = 'outer' / 'full' en Pandas)
full_res <- peru_base %>%
  full_join(autoridades_df, by = "DEPARTAMENTO")

print(full_res %>% select(DEPARTAMENTO, CAPITAL, ALCALDE, PARTIDO))

# 5. Semi Join (Filtro de existencia)
# Retorna las filas de la tabla izquierda que SÍ tienen coincidencia en la derecha, 
semi_res <- peru_base %>%
  semi_join(autoridades_df, by = "DEPARTAMENTO")

print(semi_res %>% select(DEPARTAMENTO, CAPITAL, REGION))

# 6. Anti Join (Filtro de exclusión)
anti_res <- peru_base %>%
  anti_join(autoridades_df, by = "DEPARTAMENTO")

print(anti_res %>% select(DEPARTAMENTO, CAPITAL, REGION))

# 5. Visualizaciones e Insights

library(ggplot2)
library(reshape2)

# GRÁFICO 1: Top 10 Departamentos por PIB

# 1. Seleccionar el top 10 departamentos por PIB
top_10_pib <- peru_df %>%
  arrange(desc(PIB)) %>%
  head(10)

# 2. Gráfico de barras con ggplot2
ggplot(top_10_pib, aes(x = reorder(DEPARTAMENTO, -PIB), y = PIB)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Top 10 Departamentos por PIB",
    x = "Departamento",
    y = "PIB"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold")
  )

# GRÁFICO 2: Relación entre Altitud y PIB

# 2. Gráfico de dispersión: Altitud vs PIB
ggplot(peru_df, aes(x = `ALTITUD (msnm)`, y = PIB, color = REGION)) +
  geom_point(alpha = 0.6, size = 3) +
  theme_minimal() +
  labs(
    title = "Relación entre Altitud y PIB por Región",
    x = "Altitud (msnm)",
    y = "PIB",
    color = "Región"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold"),
    legend.position = "right"
  )

# GRÁFICO 3: MATRIZ DE CORRELACIÓN

library(ggplot2)
library(reshape2)

# 3. Gráfico de correlaciones (Heatmap)
columnas_numericas <- c(
  "POBLACION (millones)",
  "SUPERFICIE (km²)",
  "PIB",
  "ALTITUD (msnm)",
  "DENSIDAD (hab/km²)"
)

# Calcular la matriz de correlación omitiendo valores faltantes si los hubiera
correlaciones <- cor(peru_df[, columnas_numericas], use = "complete.obs")

# Fundir la matriz en un formato apto para ggplot2
melted_corr <- melt(correlaciones)

# Generar el heatmap con ggplot2
ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 1) +
  scale_fill_gradient2(
    low = "#3B75AF",       # Azul para correlaciones negativas (similar a coolwarm)
    mid = "#F7F7F7", 
    high = "#D95F02",      # Naranja/Rojo para correlaciones positivas
    midpoint = 0, 
    limit = c(-1, 1), 
    name = "Pearson"
  ) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() +
  labs(
    title = "Matriz de Correlación entre Variables Económicas y Demográficas",
    x = "", 
    y = ""
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.major = element_blank()
  ) +
  coord_fixed() # Mantiene las celdas cuadradas (equivalente a square=True en seaborn)

# GRÁFICO 4: PIB por Región Geográfica

library(ggplot2)

# 4. Diagrama de caja: PIB por región (con media en rojo)
ggplot(peru_df, aes(x = REGION, y = PIB, fill = REGION)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  # Añadir los puntos de la media (equivalente a showmeans=True en seaborn)
  stat_summary(
    fun = mean, 
    geom = "point", 
    shape = 21, 
    size = 3, 
    fill = "red", 
    color = "red"
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  labs(
    title = "PIB por Región (con media en rojo)",
    x = "Región",
    y = "PIB"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold"),
    legend.position = "none" # Ocultar leyenda ya que el eje X e Y ya distinguen por color
  )

# GRÁFICO 5: Top 10 Departamentos por Población

library(dplyr)
library(ggplot2)

# 5. Seleccionar el top 10 departamentos por población
top_10_pob <- peru_df %>%
  arrange(desc(`POBLACION (millones)`)) %>%
  head(10)

# Gráfico de barras con ggplot2
ggplot(top_10_pob, aes(x = reorder(DEPARTAMENTO, -`POBLACION (millones)`), y = `POBLACION (millones)`)) +
  geom_bar(stat = "identity", fill = "purple") +
  theme_minimal() +
  labs(
    title = "Top 10 Departamentos por Población",
    x = "Departamento",
    y = "Población (millones)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold")
  )