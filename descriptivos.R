# ============================================================
# FASE 2: Análisis exploratorio de la nota promedio al egreso
# ============================================================

# ---- 0) Preparación ----
# install.packages(c("tidyverse", "moments"))   # solo la primera vez
library(tidyverse)
library(moments)
setwd("~/TESIS-UPEU")
base_modelo <- read_csv("base_modelo.csv")
nrow(base_modelo)    # esperado: 69193

colores_gestion <- c(Privado = "#5B8DEF", Público = "#F4A261")

# ---- 1) Estadística descriptiva (cuantitativas) ----
desc <- base_modelo |>
  pivot_longer(c(NOTA_PROMEDIO, CREDITOS_APROBADOS, EDAD),
               names_to = "variable", values_to = "valor") |>
  group_by(variable) |>
  summarise(media = mean(valor), DE = sd(valor), mediana = median(valor),
            min = min(valor), Q1 = quantile(valor, .25),
            Q3 = quantile(valor, .75), max = max(valor), .groups = "drop")
desc |> mutate(across(where(is.numeric), ~ round(.x, 2)))
write_excel_csv(desc, "descriptivos_cuantitativas.csv")

# ---- 2) Estadística descriptiva (categóricas) ----
base_modelo |> count(SEXO) |> mutate(pct = round(100 * n / sum(n), 2))
base_modelo |> count(TIPO_GESTION) |> mutate(pct = round(100 * n / sum(n), 2))
base_modelo |> count(DEPARTAMENTO_NACIMIENTO, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 2)) |> print(n = 10)

# ---- 3) Nota: tendencia central, dispersión y forma (total y por gestión) ----
moda <- function(x) { t <- table(x); as.numeric(names(t)[which.max(t)]) }

resumen_nota <- function(x) tibble(
  n = length(x),
  media = mean(x), mediana = median(x), moda = moda(round(x, 1)),
  DE = sd(x), varianza = var(x), CV_pct = 100 * sd(x) / mean(x),
  min = min(x), max = max(x), rango = max(x) - min(x),
  Q1 = quantile(x, .25), Q3 = quantile(x, .75), RIC = IQR(x),
  asimetria = skewness(x), curtosis_exceso = kurtosis(x) - 3)

tabla_nota <- bind_rows(
  resumen_nota(base_modelo$NOTA_PROMEDIO) |> mutate(grupo = "Total", .before = 1),
  base_modelo |> group_by(grupo = TIPO_GESTION) |>
    reframe(resumen_nota(NOTA_PROMEDIO)))

print(tabla_nota |> mutate(across(where(is.numeric), ~ round(.x, 2))), width = Inf)
write_excel_csv(tabla_nota, "descriptivos_nota.csv")

# ---- 4) Figuras de la distribución de la nota ----
# 4a) Histograma con densidad
p_hist <- ggplot(base_modelo, aes(NOTA_PROMEDIO)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = .5,
                 fill = "#2A9D8F", colour = "white", alpha = .85) +
  geom_density(colour = "#1B6B61", linewidth = .9) +
  geom_vline(xintercept = c(mean(base_modelo$NOTA_PROMEDIO),
                            median(base_modelo$NOTA_PROMEDIO)),
             linetype = c("dashed", "solid"), colour = "grey25") +
  labs(x = "Nota promedio (escala 0 a 20)", y = "Densidad") +
  theme_minimal(base_size = 12)
ggsave("hist_nota.png", p_hist, width = 7, height = 4, dpi = 600, bg = "white")

# 4b) Densidad por tipo de gestión
p_gest <- ggplot(base_modelo, aes(NOTA_PROMEDIO, fill = TIPO_GESTION)) +
  geom_density(alpha = .55, colour = NA) +
  scale_fill_manual(values = colores_gestion, name = NULL) +
  labs(x = "Nota promedio (escala 0 a 20)", y = "Densidad") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
ggsave("densidad_nota_gestion.png", p_gest, width = 7, height = 4, dpi = 600, bg = "white")

# 4c) Boxplot por tipo de gestión
p_box_gest <- ggplot(base_modelo, aes(NOTA_PROMEDIO, TIPO_GESTION, fill = TIPO_GESTION)) +
  geom_boxplot(outlier.size = .4, outlier.alpha = .2, show.legend = FALSE) +
  scale_fill_manual(values = colores_gestion) +
  labs(x = "Nota promedio (escala 0 a 20)", y = NULL) +
  theme_minimal(base_size = 12)
ggsave("box_nota_gestion.png", p_box_gest, width = 7, height = 3, dpi = 600, bg = "white")

# 4d) Gráfico Q-Q (normalidad descriptiva)
p_qq <- ggplot(base_modelo, aes(sample = NOTA_PROMEDIO)) +
  stat_qq(alpha = .2, colour = "#2A9D8F") + stat_qq_line(colour = "grey25") +
  labs(x = "Cuantiles teóricos", y = "Cuantiles de la nota") +
  theme_minimal(base_size = 12)
ggsave("qq_nota.png", p_qq, width = 5, height = 5, dpi = 600, bg = "white")

# ---- 5) Nota por universidad ----
por_univ <- base_modelo |>
  group_by(NOMBRE_ENTIDAD, TIPO_GESTION) |>
  summarise(n = n(), media = mean(NOTA_PROMEDIO), mediana = median(NOTA_PROMEDIO),
            DE = sd(NOTA_PROMEDIO), RIC = IQR(NOTA_PROMEDIO), .groups = "drop") |>
  arrange(desc(mediana))
print(por_univ |> mutate(across(where(is.numeric), ~ round(.x, 2))), n = 10)
summary(por_univ$mediana)        # esperado: 12,19 a 16,45
sum(por_univ$n < 30)             # esperado: 8
write_excel_csv(por_univ, "nota_por_universidad.csv")

p_univ <- base_modelo |>
  mutate(NOMBRE_ENTIDAD = fct_reorder(NOMBRE_ENTIDAD, NOTA_PROMEDIO, median)) |>
  ggplot(aes(NOTA_PROMEDIO, NOMBRE_ENTIDAD, fill = TIPO_GESTION)) +
  geom_boxplot(outlier.size = .3, outlier.alpha = .2, linewidth = .25) +
  scale_fill_manual(values = colores_gestion, name = NULL) +
  labs(x = "Nota promedio (escala 0 a 20)", y = NULL) +
  theme_minimal(base_size = 8) + theme(legend.position = "top")
ggsave("box_nota_universidad.png", p_univ, width = 7, height = 10, dpi = 600, bg = "white")

# Variación entre universidades (descriptiva, no es el ICC)
por_univ |> summarise(rango_medianas = max(mediana) - min(mediana),
                      DE_medianas = sd(mediana))

# ---- 6) Nota por programa ----
cardinalidad <- tibble(
  universidades = n_distinct(base_modelo$NOMBRE_ENTIDAD),
  programas = n_distinct(base_modelo$NOMBRE_PROGRAMA),
  combinaciones_univ_prog = n_distinct(base_modelo$NOMBRE_ENTIDAD, base_modelo$NOMBRE_PROGRAMA))
cardinalidad     # esperado: 77, 498, y el número de combinaciones

top_prog <- base_modelo |> count(NOMBRE_PROGRAMA, sort = TRUE) |>
  slice_head(n = 15) |> pull(NOMBRE_PROGRAMA)

por_prog <- base_modelo |> group_by(NOMBRE_PROGRAMA) |>
  summarise(n = n(), media = mean(NOTA_PROMEDIO), mediana = median(NOTA_PROMEDIO),
            DE = sd(NOTA_PROMEDIO), RIC = IQR(NOTA_PROMEDIO)) |>
  arrange(desc(n))
print(por_prog |> head(15) |> mutate(across(where(is.numeric), ~ round(.x, 2))))
write_excel_csv(por_prog, "nota_por_programa.csv")

p_prog <- base_modelo |> filter(NOMBRE_PROGRAMA %in% top_prog) |>
  mutate(NOMBRE_PROGRAMA = fct_reorder(NOMBRE_PROGRAMA, NOTA_PROMEDIO, median)) |>
  ggplot(aes(NOTA_PROMEDIO, NOMBRE_PROGRAMA)) +
  geom_boxplot(fill = "#2A9D8F", alpha = .8, outlier.size = .3, outlier.alpha = .2) +
  labs(x = "Nota promedio (escala 0 a 20)", y = NULL) +
  theme_minimal(base_size = 9)
ggsave("box_nota_programa.png", p_prog, width = 7, height = 5, dpi = 600, bg = "white")

# ---- 7) Nota según características demográficas ----
# Sexo
por_sexo <- base_modelo |> group_by(SEXO) |>
  summarise(n = n(), media = mean(NOTA_PROMEDIO), mediana = median(NOTA_PROMEDIO),
            DE = sd(NOTA_PROMEDIO), RIC = IQR(NOTA_PROMEDIO))
por_sexo |> mutate(across(where(is.numeric), ~ round(.x, 2)))

p_sexo <- ggplot(base_modelo, aes(NOTA_PROMEDIO, SEXO, fill = SEXO)) +
  geom_boxplot(outlier.size = .4, outlier.alpha = .2, show.legend = FALSE) +
  scale_fill_manual(values = c(F = "#E76F8B", M = "#5B8DEF")) +
  labs(x = "Nota promedio (escala 0 a 20)", y = NULL) +
  theme_minimal(base_size = 12)
ggsave("box_nota_sexo.png", p_sexo, width = 7, height = 3, dpi = 600, bg = "white")

# Edad (grupos y correlación de Spearman)
por_edad <- base_modelo |>
  mutate(grupo_edad = cut(EDAD, c(19, 24, 29, 39, 100),
                          labels = c("20-24", "25-29", "30-39", "40+"))) |>
  group_by(grupo_edad) |>
  summarise(n = n(), mediana = median(NOTA_PROMEDIO), RIC = IQR(NOTA_PROMEDIO))
por_edad
cor(base_modelo$NOTA_PROMEDIO, base_modelo$EDAD, method = "spearman")   # esperado: -0,15

# Departamento de nacimiento (n >= 100, sin "No registrado")
por_dep <- base_modelo |>
  filter(DEPARTAMENTO_NACIMIENTO != "No registrado") |>
  group_by(DEPARTAMENTO_NACIMIENTO) |>
  summarise(n = n(), mediana = median(NOTA_PROMEDIO)) |>
  filter(n >= 100) |> arrange(desc(mediana))
print(por_dep, n = Inf)
write_excel_csv(por_dep, "nota_por_departamento.csv")

# ---- 8) Lista de archivos generados ----
list.files(pattern = "\\.(csv|png)$")
