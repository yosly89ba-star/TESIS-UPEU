# ---- PASO 8: Diccionario de datos ----
rng <- function(x) paste0(min(x), " a ", max(x))

diccionario <- tibble(
  variable = c("NOTA_PROMEDIO", "TIPO_ENTIDAD", "NOMBRE_ENTIDAD", "TIPO_GESTION", "NIVEL_ACADEMICO",
               "NOMBRE_PROGRAMA", "CREDITOS_APROBADOS", "SEXO", "EDAD", "DEPARTAMENTO_NACIMIENTO"),
  definicion = c(
    "Promedio de calificaciones del egresado, en escala vigesimal (0 a 20). Se usó coma decimal en parte del archivo y se normalizó a punto.",
    "Tipo de institución. Solo se consideraron universidades.",
    "Nombre de la universidad.",
    "Régimen de gestión de la universidad: pública o privada.",
    "Nivel de formación. Solo pregrado (Carrera Profesional).",
    "Nombre del programa académico o carrera profesional.",
    "Número de créditos académicos aprobados durante la trayectoria universitaria. Valor original de la fuente, sin modificar.",
    "Sexo del egresado: femenino o masculino.",
    "Edad al egreso, en años cumplidos en 2025. La fuente la calcula como 2026 menos el año de nacimiento; aquí se resta 1.",
    "Departamento de nacimiento del egresado."),
  tipo = c("Cuantitativa continua", "Categórica", "Categórica", "Categórica (binaria)", "Categórica",
           "Categórica", "Cuantitativa discreta", "Categórica (binaria)", "Cuantitativa discreta", "Categórica"),
  valores_posibles = c(
    rng(base_modelo$NOTA_PROMEDIO),
    paste(unique(base_modelo$TIPO_ENTIDAD), collapse = ", "),
    paste0(n_distinct(base_modelo$NOMBRE_ENTIDAD), " universidades"),
    paste(sort(unique(base_modelo$TIPO_GESTION)), collapse = ", "),
    paste(unique(base_modelo$NIVEL_ACADEMICO), collapse = ", "),
    paste0(n_distinct(base_modelo$NOMBRE_PROGRAMA), " programas"),
    rng(base_modelo$CREDITOS_APROBADOS),
    paste(sort(unique(base_modelo$SEXO)), collapse = ", "),
    rng(base_modelo$EDAD),
    paste0(n_distinct(base_modelo$DEPARTAMENTO_NACIMIENTO) - 1, " departamentos + No registrado")),
  faltantes = c(rep("Ninguno", 8), "Ninguno",
                paste0("\"No registrado\" (", sum(base_modelo$DEPARTAMENTO_NACIMIENTO == "No registrado"), " registros)")),
  observacion = c(
    "Se excluyeron las notas < 7 (error de reporte). Las de 7 a 10.5 se conservan con bandera flag_nota_baja.",
    "Constante en la base; no aporta variación.",
    "Unidad de agrupamiento para la validación agrupada.",
    "",
    "Constante en la base; no aporta variación.",
    "",
    "Los de Peruana Unión (< 150) y los > 400 fuera de Medicina se marcan con flag_creditos_invalidos; el valor no se alteró.",
    "",
    "Referencia: 2025 (año de egreso). Los de 60 años o más se marcan con flag_edad_extrema.",
    "Vacíos de la fuente recodificados como texto, no imputados."))

diccionario
write_excel_csv(diccionario, "diccionario_datos.csv")

diccionario$tipo[diccionario$variable == "CREDITOS_APROBADOS"] <- "Cuantitativa"
write_excel_csv(diccionario, "diccionario_datos.csv")
####
banderas <- tibble(
  bandera = c("flag_multiple_titulo", "flag_nota_baja", "flag_creditos_invalidos",
              "flag_creditos_revisar", "flag_edad_extrema"),
  definicion = c("La persona aparece en más de un registro (doble titulación o programa conjunto).",
                 "Nota promedio entre 7 y 10.5.",
                 "Peruana Unión con menos de 150 créditos, o más de 400 créditos fuera de Medicina con valor no uniforme en el programa.",
                 "Créditos menores a 200 que no están marcados como inválidos.",
                 "Edad al egreso de 60 años o más."),
  n = colSums(base_modelo[, c("flag_multiple_titulo", "flag_nota_baja", "flag_creditos_invalidos",
                              "flag_creditos_revisar", "flag_edad_extrema")]))
banderas                      # esperado: 72, 282, 878, 821, 116
write_excel_csv(banderas, "diccionario_banderas.csv")
###########
# ---- PASO 8 (cierre): DICCIONARIO.md para el repositorio ----

# 0) Corrección del tipo de créditos y tabla de banderas (si aún no los ejecutaste)
diccionario$tipo[diccionario$variable == "CREDITOS_APROBADOS"] <- "Cuantitativa"
banderas <- tibble(
  bandera = c("flag_multiple_titulo", "flag_nota_baja", "flag_creditos_invalidos",
              "flag_creditos_revisar", "flag_edad_extrema"),
  definicion = c("La persona aparece en más de un registro (doble titulación o programa conjunto).",
                 "Nota promedio entre 7 y 10.5.",
                 "Peruana Unión con menos de 150 créditos, o más de 400 créditos fuera de Medicina con valor no uniforme en el programa.",
                 "Créditos menores a 200 que no están marcados como inválidos.",
                 "Edad al egreso de 60 años o más."),
  n = colSums(base_modelo[, c("flag_multiple_titulo", "flag_nota_baja", "flag_creditos_invalidos",
                              "flag_creditos_revisar", "flag_edad_extrema")]))

# 1) Función que convierte un data.frame en tabla Markdown
tabla_md <- function(df) {
  df[] <- lapply(df, function(x) gsub("\\|", "/", as.character(x)))
  cab   <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep   <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  filas <- apply(df, 1, function(f) paste0("| ", paste(ifelse(is.na(f) | f == "", "-", f), collapse = " | "), " |"))
  c(cab, sep, filas)
}

# 2) Tabla de flujo con conteos
flujo_md <- flujo |>
  transmute(Paso = paso,
            Excluidos = format(n_excluidos, big.mark = ","),
            Quedan = format(quedan, big.mark = ","))

# 3) Escribir el archivo
md <- c(
  "# Diccionario de datos: egresados de pregrado 2025",
  "",
  "Fuente: `egresado_2025.csv` (separador `|`, codificación latin-1).",
  paste0("Base analítica: **", format(nrow(base_modelo), big.mark = ","), " registros**, ",
         n_distinct(base_modelo$NOMBRE_ENTIDAD), " universidades, ",
         format(n_distinct(df$IDENTIFICADOR_PERSONA), big.mark = ","), " personas únicas."),
  "",
  "## 1. Variables",
  "",
  tabla_md(diccionario),
  "",
  "**Fecha de referencia de la edad:** la fuente calcula `EDAD` como 2026 menos el año de nacimiento. En este estudio se resta 1, de modo que `EDAD` es la edad al egreso en 2025.",
  "",
  "## 2. Banderas (no excluyen registros)",
  "",
  tabla_md(banderas),
  "",
  "Los valores originales de las variables no se modificaron; las banderas permiten pruebas de sensibilidad.",
  "",
  "## 3. Criterios de depuración",
  "",
  "1. Solo pregrado (`Carrera Profesional`).",
  "2. Solo universidades (se excluyen escuelas superiores e institutos).",
  "3. Se excluye la Universidad de Lima: el 100% de sus notas es 0.",
  "4. Duplicados reales (misma persona, universidad y programa): se conserva la fecha de egreso más reciente. Los egresados con dos títulos se conservan con `flag_multiple_titulo`.",
  "5. Se excluyen las notas menores a 7 (valor no plausible en un egresado).",
  "6. Departamento de nacimiento vacío: se recodifica como texto `No registrado`, sin imputar.",
  "",
  "## 4. Flujo de depuración",
  "",
  tabla_md(flujo_md),
  "",
  "Notas: en el archivo fuente, 1,365 notas usan coma decimal y se normalizaron a punto."
)
writeLines(md, "DICCIONARIO.md", useBytes = FALSE)
readLines("DICCIONARIO.md", n = 15)     # revisar las primeras líneas

