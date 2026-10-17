# Diccionario de datos: egresados de pregrado 2025

Fuente: `egresado_2025.csv` (separador `|`, codificación latin-1).
Base analítica: **69,193 registros**, 77 universidades, 69,157 personas únicas.

## 1. Variables

| variable | definicion | tipo | valores_posibles | faltantes | observacion |
|---|---|---|---|---|---|
| NOTA_PROMEDIO | Promedio de calificaciones del egresado, en escala vigesimal (0 a 20). Se usó coma decimal en parte del archivo y se normalizó a punto. | Cuantitativa continua | 7 a 20 | Ninguno | Se excluyeron las notas < 7 (error de reporte). Las de 7 a 10.5 se conservan con bandera flag_nota_baja. |
| TIPO_ENTIDAD | Tipo de institución. Solo se consideraron universidades. | Categórica | Universidad | Ninguno | Constante en la base; no aporta variación. |
| NOMBRE_ENTIDAD | Nombre de la universidad. | Categórica | 77 universidades | Ninguno | Unidad de agrupamiento para la validación agrupada. |
| TIPO_GESTION | Régimen de gestión de la universidad: pública o privada. | Categórica (binaria) | Privado, Público | Ninguno | - |
| NIVEL_ACADEMICO | Nivel de formación. Solo pregrado (Carrera Profesional). | Categórica | Carrera Profesional | Ninguno | Constante en la base; no aporta variación. |
| NOMBRE_PROGRAMA | Nombre del programa académico o carrera profesional. | Categórica | 498 programas | Ninguno | - |
| CREDITOS_APROBADOS | Número de créditos académicos aprobados durante la trayectoria universitaria. Valor original de la fuente, sin modificar. | Cuantitativa | 15 a 546 | Ninguno | Los de Peruana Unión (< 150) y los > 400 fuera de Medicina se marcan con flag_creditos_invalidos; el valor no se alteró. |
| SEXO | Sexo del egresado: femenino o masculino. | Categórica (binaria) | F, M | Ninguno | - |
| EDAD | Edad al egreso, en años cumplidos en 2025. La fuente la calcula como 2026 menos el año de nacimiento; aquí se resta 1. | Cuantitativa discreta | 20 a 79 | Ninguno | Referencia: 2025 (año de egreso). Los de 60 años o más se marcan con flag_edad_extrema. |
| DEPARTAMENTO_NACIMIENTO | Departamento de nacimiento del egresado. | Categórica | 25 departamentos + No registrado | "No registrado" (458 registros) | Vacíos de la fuente recodificados como texto, no imputados. |

**Fecha de referencia de la edad:** la fuente calcula `EDAD` como 2026 menos el año de nacimiento. En este estudio se resta 1, de modo que `EDAD` es la edad al egreso en 2025.

## 2. Banderas (no excluyen registros)

| bandera | definicion | n |
|---|---|---|
| flag_multiple_titulo | La persona aparece en más de un registro (doble titulación o programa conjunto). | 72 |
| flag_nota_baja | Nota promedio entre 7 y 10.5. | 282 |
| flag_creditos_invalidos | Peruana Unión con menos de 150 créditos, o más de 400 créditos fuera de Medicina con valor no uniforme en el programa. | 878 |
| flag_creditos_revisar | Créditos menores a 200 que no están marcados como inválidos. | 821 |
| flag_edad_extrema | Edad al egreso de 60 años o más. | 116 |

Los valores originales de las variables no se modificaron; las banderas permiten pruebas de sensibilidad.

## 3. Criterios de depuración

1. Solo pregrado (`Carrera Profesional`).
2. Solo universidades (se excluyen escuelas superiores e institutos).
3. Se excluye la Universidad de Lima: el 100% de sus notas es 0.
4. Duplicados reales (misma persona, universidad y programa): se conserva la fecha de egreso más reciente. Los egresados con dos títulos se conservan con `flag_multiple_titulo`.
5. Se excluyen las notas menores a 7 (valor no plausible en un egresado).
6. Departamento de nacimiento vacío: se recodifica como texto `No registrado`, sin imputar.

## 4. Flujo de depuración

| Paso | Excluidos | Quedan |
|---|---|---|
| Registros originales |      0 | 89,487 |
| Nivel distinto de pregrado | 16,820 | 72,667 |
| Entidad no universitaria |    329 | 72,338 |
| Universidad de Lima (100% nota = 0) |  2,861 | 69,477 |
| Duplicados reales (persona + universidad + programa) |     29 | 69,448 |
| Nota promedio < 7 |    255 | 69,193 |

Notas: en el archivo fuente, 1,365 notas usan coma decimal y se normalizaron a punto.
