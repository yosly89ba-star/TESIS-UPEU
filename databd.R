# ---- PASO 1: Carga, selección de variables y exclusiones iniciales ----
setwd("~/TESIS-UPEU")

# install.packages(c("readr", "dplyr", "stringr"))   # solo la primera vez
library(readr)
library(dplyr)
library(stringr)

ruta <- "egresado_2025.csv"
file.exists(ruta)      # debe dar TRUE

# 1) Cargar (separador "|", latin-1, todo como texto)
raw <- read_delim(ruta, delim = "|",
                  locale = locale(encoding = "latin1"),
                  col_types = cols(.default = col_character()),
                  na = "", progress = FALSE)
raw$fila_origen <- seq_len(nrow(raw)) + 1L   # nº de línea en el CSV (cabecera = 1)
dim(raw)               # esperado: 89487 filas

# 2) Seleccionar las 10 variables de la tesis + 4 auxiliares
vars_tesis <- c("NOTA_PROMEDIO", "TIPO_ENTIDAD", "NOMBRE_ENTIDAD", "TIPO_GESTION",
                "NIVEL_ACADEMICO", "NOMBRE_PROGRAMA", "CREDITOS_APROBADOS",
                "SEXO", "EDAD", "DEPARTAMENTO_NACIMIENTO")
vars_aux   <- c("fila_origen", "IDENTIFICADOR_PERSONA", "FECHA_EGRESO", "NACIONALIDAD")

df <- raw |>
  select(all_of(c(vars_tesis, vars_aux))) |>
  mutate(NOTA_PROMEDIO       = as.numeric(str_replace(NOTA_PROMEDIO, ",", ".")),  # hay notas con coma decimal
         CREDITOS_APROBADOS  = as.numeric(CREDITOS_APROBADOS),
         EDAD                = as.numeric(EDAD) - 1)   # la fuente trae 2026 - año de nacimiento; -1 = edad al egresar en 2025

# 3) Exclusiones, con conteo en cada paso
flujo <- tibble(paso = "Registros originales", n_excluidos = 0L, quedan = nrow(df))
registrar <- function(flujo, paso, antes, despues)
  bind_rows(flujo, tibble(paso = paso, n_excluidos = antes - despues, quedan = despues))

# 3a) Solo pregrado
n0 <- nrow(df)
df <- df |> filter(NIVEL_ACADEMICO == "Carrera Profesional")
flujo <- registrar(flujo, "Nivel distinto de pregrado", n0, nrow(df))

# 3b) Solo universidades (fuera escuelas superiores e institutos)
n0 <- nrow(df)
df <- df |> filter(TIPO_ENTIDAD == "Universidad")
flujo <- registrar(flujo, "Entidad no universitaria", n0, nrow(df))

# 3c) Universidad de Lima: comprobar y excluir (todas sus notas son 0)
df |> filter(NOMBRE_ENTIDAD == "Universidad de Lima") |>
  summarise(n = n(), nota_min = min(NOTA_PROMEDIO), nota_max = max(NOTA_PROMEDIO))
n0 <- nrow(df)
df <- df |> filter(NOMBRE_ENTIDAD != "Universidad de Lima")
flujo <- registrar(flujo, "Universidad de Lima (100% nota = 0)", n0, nrow(df))

# 4) Revisar
flujo
n_distinct(df$NOMBRE_ENTIDAD)
count(df, NIVEL_ACADEMICO)
count(df, TIPO_ENTIDAD)
summary(df$EDAD)
###datos faltantes
# ---- PASO 2: Datos faltantes ----

# 2a) NA y textos tipo "Sin información" por variable
patron <- "^\\s*(sin informaci[oó]n|s/i|n/a|no registrado|-|\\?)\\s*$"
faltantes <- data.frame(
  variable = names(df),
  NA_      = sapply(df, function(x) sum(is.na(x))),
  texto    = sapply(df, function(x) sum(str_detect(as.character(x), regex(patron, ignore_case = TRUE)), na.rm = TRUE))
)
faltantes$pct_NA <- round(100 * faltantes$NA_ / nrow(df), 2)
faltantes[order(-faltantes$NA_), ]

# 2b) Los vacíos de departamento de nacimiento: ¿peruanos o extranjeros?
df |> filter(is.na(DEPARTAMENTO_NACIMIENTO)) |>
  mutate(origen = case_when(is.na(NACIONALIDAD)       ~ "Sin nacionalidad",
                            NACIONALIDAD == "PERUANA" ~ "Peruano sin dato",
                            TRUE                      ~ "Extranjero")) |>
  count(origen)

# 2c) Extranjeros con departamento: ¿tienen dato aunque no sean peruanos?
df |> filter(!is.na(NACIONALIDAD), NACIONALIDAD != "PERUANA") |>
  summarise(n = n(), con_departamento = sum(!is.na(DEPARTAMENTO_NACIMIENTO)))
# datosduplicados
# ---- PASO 3: Datos duplicados ----

# 3a) ¿Cuántas filas extra hay según el identificador de persona?
sum(duplicated(df$IDENTIFICADOR_PERSONA))                      # esperado: 65 (57 personas repetidas)

# 3b) Tipo 1: filas idénticas en todas las variables (sin contar fila_origen)
cols_comp <- setdiff(names(df), "fila_origen")
sum(duplicated(df[, cols_comp]))                                # esperado: 10

# 3c) Tipo 2: misma persona + universidad + programa (reregistro del mismo egreso)
clave <- c("IDENTIFICADOR_PERSONA", "NOMBRE_ENTIDAD", "NOMBRE_PROGRAMA")
sum(duplicated(df[, clave]))                                    # esperado: 29 (incluye los 10 idénticos)

# 3d) Tipo 3: misma persona y universidad, pero programa distinto (posible doble titulación)
sum(duplicated(df[, c("IDENTIFICADOR_PERSONA", "NOMBRE_ENTIDAD")])) - sum(duplicated(df[, clave]))   # esperado: 2

# 3e) Tipo 4: misma persona en universidades distintas
df |> group_by(IDENTIFICADOR_PERSONA) |>
  summarise(n_univ = n_distinct(NOMBRE_ENTIDAD)) |>
  filter(n_univ > 1) |> nrow()                                  # esperado: 34 personas

# 3f) Ver los reregistros (tipo 2) para decidir cuál conservar
reregistros <- df |>
  group_by(across(all_of(clave))) |> filter(n() > 1) |> ungroup() |>
  arrange(IDENTIFICADOR_PERSONA, desc(FECHA_EGRESO)) |>
  select(IDENTIFICADOR_PERSONA, NOMBRE_ENTIDAD, NOMBRE_PROGRAMA, FECHA_EGRESO,
         NOTA_PROMEDIO, CREDITOS_APROBADOS, fila_origen)
head(reregistros, 12)

#eliminar lod datos duplicados
# ---- PASO 4: Eliminar duplicados reales ----

# 4a) Ordenar: fecha de egreso más reciente primero; si empatan, la primera fila del archivo
orden <- df |> arrange(desc(FECHA_EGRESO), fila_origen)

# 4b) Filas a eliminar = repeticiones de persona + universidad + programa
filas_dup <- orden$fila_origen[duplicated(orden[, clave])]
length(filas_dup)                                    # esperado: 29

# 4c) Guardar lo eliminado con su motivo (para el registro de exclusiones)
dup_excluidos <- df |>
  filter(fila_origen %in% filas_dup) |>
  mutate(motivo_exclusion = "Duplicado real: misma persona, universidad y programa; se conserva la fecha de egreso más reciente")

# 4d) Aplicar y registrar en el flujo
n0 <- nrow(df)
df <- df |> filter(!fila_origen %in% filas_dup)
flujo <- registrar(flujo, "Duplicados reales (persona + universidad + programa)", n0, nrow(df))
flujo                                                # esperado: quedan 69,448
sum(duplicated(df[, clave]))                         # esperado: 0

# 4e) Marcar (sin eliminar) a quienes tienen más de un título: doble titulación o programa conjunto
df <- df |>
  group_by(IDENTIFICADOR_PERSONA) |>
  mutate(flag_multiple_titulo = n() > 1) |>
  ungroup()
sum(df$flag_multiple_titulo)                         # esperado: unos 72 registros (36 personas con 2 títulos)
n_distinct(df$IDENTIFICADOR_PERSONA)                 # esperado: 69,412 personas únicas

##datos atipicos 
# ---- PASO 5: Datos atípicos (solo inspección, sin excluir) ----

# Función: resumen + conteo de atípicos por regla de Tukey (1.5 × IQR)
resumen_atipicos <- function(x) {
  q <- quantile(x, c(.25, .5, .75)); iqr <- q[3] - q[1]
  c(min = min(x), Q1 = q[1], mediana = q[2], Q3 = q[3], max = max(x),
    lim_inf = q[1] - 1.5 * iqr, lim_sup = q[3] + 1.5 * iqr,
    n_bajo = sum(x < q[1] - 1.5 * iqr), n_alto = sum(x > q[3] + 1.5 * iqr))
}
round(rbind(NOTA     = resumen_atipicos(df$NOTA_PROMEDIO),
            CREDITOS = resumen_atipicos(df$CREDITOS_APROBADOS),
            EDAD     = resumen_atipicos(df$EDAD)), 2)

# ---- 5a) NOTA ----
table(cut(df$NOTA_PROMEDIO, c(-0.1, 0, 7, 10.5, 12, 14, 16, 18, 20), right = TRUE))
sum(df$NOTA_PROMEDIO == 0)                                   # notas exactamente 0
sum(df$NOTA_PROMEDIO < 7)                                    # candidatas a exclusión
df |> filter(NOTA_PROMEDIO < 7) |> count(NOMBRE_ENTIDAD, sort = TRUE) |> head(8)
boxplot(df$NOTA_PROMEDIO, horizontal = TRUE, main = "Nota promedio")

# ---- 5b) CRÉDITOS ----
table(cut(df$CREDITOS_APROBADOS, c(0, 50, 99, 149, 199, 300, 400, 1000)))
es_med <- str_detect(df$NOMBRE_PROGRAMA, regex("Medicina", ignore_case = TRUE))
sum(df$CREDITOS_APROBADOS < 150)                             # créditos bajos
df |> filter(CREDITOS_APROBADOS < 150) |> count(NOMBRE_ENTIDAD, sort = TRUE) |> head(5)
sum(df$CREDITOS_APROBADOS > 300 &  es_med)                   # Medicina: esperable
sum(df$CREDITOS_APROBADOS > 300 & !es_med)                   # fuera de Medicina: revisar
df |> filter(CREDITOS_APROBADOS > 300, !es_med) |> count(NOMBRE_ENTIDAD, sort = TRUE) |> head(5)
sum(df$CREDITOS_APROBADOS > 400)
hist(df$CREDITOS_APROBADOS, breaks = 60, main = "Créditos aprobados")

# ---- 5c) EDAD (edad al egresar) ----
table(cut(df$EDAD, c(0, 22, 30, 40, 50, 60, 100)))
sum(df$EDAD >= 50); sum(df$EDAD >= 60)
df |> filter(EDAD >= 60) |> count(NOMBRE_ENTIDAD, sort = TRUE) |> head(5)
boxplot(df$EDAD, horizontal = TRUE, main = "Edad al egreso")

## # ---- PASO 6: Atípicos por plausibilidad (sin reemplazar valores) ----
NOTA_MIN <- 7

# 6a) NOTA: excluir < 7 (guardar el motivo)
nota_excluidos <- df |>
  filter(NOTA_PROMEDIO < NOTA_MIN) |>
  mutate(motivo_exclusion = paste0("Nota promedio < ", NOTA_MIN, ": valor no plausible en un egresado"))
n0 <- nrow(df)
df <- df |> filter(NOTA_PROMEDIO >= NOTA_MIN)
flujo <- registrar(flujo, paste0("Nota promedio < ", NOTA_MIN), n0, nrow(df))
flujo                                          # esperado: quedan 69,193
n_distinct(df$NOMBRE_ENTIDAD)                  # esperado: 77

# 6b) Bandera de nota baja (7 a 10.5), sin excluir
df <- df |> mutate(flag_nota_baja = NOTA_PROMEDIO < 10.5)
sum(df$flag_nota_baja)                         # esperado: cerca de 282

# 6c) CRÉDITOS: solo banderas, el valor original no se toca
es_med <- str_detect(df$NOMBRE_PROGRAMA, regex("Medicina", ignore_case = TRUE))
df <- df |>
  group_by(NOMBRE_ENTIDAD, NOMBRE_PROGRAMA) |>
  mutate(.n_cred = n_distinct(CREDITOS_APROBADOS)) |>
  ungroup() |>
  mutate(flag_creditos_invalidos =
           (str_detect(NOMBRE_ENTIDAD, "Peruana Unión") & CREDITOS_APROBADOS < 150) |
           (CREDITOS_APROBADOS > 400 & !es_med & .n_cred > 1),
         flag_creditos_revisar = CREDITOS_APROBADOS < 200 & !flag_creditos_invalidos) |>
  select(-.n_cred)
sum(df$flag_creditos_invalidos)                # esperado: 878
sum(df$flag_creditos_revisar)                  # esperado: 820
sum(is.na(df$CREDITOS_APROBADOS))              # esperado: 0 (no hay NA)

# 6d) EDAD: bandera de edad avanzada, sin excluir
df <- df |> mutate(flag_edad_extrema = EDAD >= 60)
sum(df$flag_edad_extrema)                      # esperado: cerca de 116
##paso 7

# ---- PASO 7: Cierre de la limpieza ----

# 7a) Departamento de nacimiento vacío -> categoría "No registrado" (texto, no NA)
df <- df |>
  mutate(DEPARTAMENTO_NACIMIENTO = coalesce(DEPARTAMENTO_NACIMIENTO, "No registrado"))
sum(df$DEPARTAMENTO_NACIMIENTO == "No registrado")   # esperado: cerca de 453
sum(is.na(df$DEPARTAMENTO_NACIMIENTO))               # esperado: 0

# 7b) Registro de exclusiones: nota < 7 y duplicados (los otros motivos están en el flujo)
registro_exclusiones <- bind_rows(
  nota_excluidos |> select(fila_origen, IDENTIFICADOR_PERSONA, NOMBRE_ENTIDAD, NOMBRE_PROGRAMA,
                           NOTA_PROMEDIO, CREDITOS_APROBADOS, motivo_exclusion),
  dup_excluidos  |> select(fila_origen, IDENTIFICADOR_PERSONA, NOMBRE_ENTIDAD, NOMBRE_PROGRAMA,
                           NOTA_PROMEDIO, CREDITOS_APROBADOS, motivo_exclusion))
nrow(registro_exclusiones)                           # esperado: 284 (255 + 29)

# 7c) Base de modelamiento: tus 10 variables + banderas
base_modelo <- df |>
  select(all_of(vars_tesis), flag_multiple_titulo, flag_nota_baja,
         flag_creditos_invalidos, flag_creditos_revisar, flag_edad_extrema)
dim(base_modelo)                                     # esperado: 69193 filas, 15 columnas

# 7d) Guardar
write_excel_csv(base_modelo, "base_modelo.csv")
write_excel_csv(df, "base_depurada_completa.csv")    # con identificador, fecha y nacionalidad
write_excel_csv(registro_exclusiones, "registro_exclusiones.csv")
write_excel_csv(flujo, "flujo_conteos.csv")

# 7e) Control final: el flujo debe sumar
flujo
89487 - sum(flujo$n_excluidos) == nrow(base_modelo)  # debe dar TRUE
