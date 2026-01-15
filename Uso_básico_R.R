
# PROYECTO: MICRO-PIPELINE DE ANÁLISIS DE DATOS (R BASE)
# AUTOR: Oswaldo Fentanes Gasca 



# 1. CARGA DE DATOS (Entrada)

calificaciones <- c(4, 5, 3, 5, 4, 2, 1, 5, 4, 5, 3, 4, 4, 5, 1, 2, 5, 5)

comentarios <- c(
  "Muy buen curso, volvería a tomarlo",
  "Excelente contenido y profesor",
  "Regular, esperaba más ejercicios",
  "Muy completo, realmente excelente",
  "Aprendí bastante, buen curso",
  "Demasiado básico para mi nivel",
  "Muy malo, no lo recomiendo",
  "Excelente, aprendí cosas nuevas",
  "Muy útil para mi trabajo",
  "Genial enfoque, excelente!",
  "Regular, temas poco profundizados",
  "Curso bueno, aunque mejorable",
  "Muy buen curso, buenos ejemplos",
  "Excelente de verdad, recomendable",
  "Malo, esperaba otra cosa",
  "Podría mejorar bastante",
  "Excelente, superó mis expectativas",
  "Excelente, volvería a tomarlo"
)


# 2. DEFINICIÓN DE FUNCIONES PURAS (Lógica)


# Funciones Estadísticas
mi_media <- function(x) {
  return(sum(x) / length(x))
}

mi_mediana <- function(x) {
  x_ord <- sort(x)
  n <- length(x)
  if (n %% 2 == 1) {
    return(x_ord[(n + 1) / 2])
  } else {
    return((x_ord[n / 2] + x_ord[(n / 2) + 1]) / 2)
  }
}

mi_moda <- function(x) {
  
  tabla <- table(x)
  moda <- as.numeric(names(tabla)[which.max(tabla)])
  return(moda)
}

mi_rango <- function(x) {
  return(max(x) - min(x))
}

mi_varianza_pob <- function(x) {
  # Varianza poblacional (dividiendo por N, no n-1)
  media <- mi_media(x)
  sq_diff <- (x - media)^2
  return(sum(sq_diff) / length(x))
}

# Funciones de Texto 
limpiar_tokenizar <- function(texto_arr) {
 
  txt <- tolower(texto_arr)
 
  txt <- chartr("áéíóú", "aeiou", txt)

  txt <- gsub("[[:punct:]]", "", txt)

  lista_palabras <- strsplit(txt, " ")
 
  return(unlist(lista_palabras))
}

# 3. EJECUCIÓN DEL ANÁLISIS


# A) Cálculos Estadísticos
resultado_estadistico <- list(
  Media    = mi_media(calificaciones),
  Mediana  = mi_mediana(calificaciones),
  Moda     = mi_moda(calificaciones),
  Rango    = mi_rango(calificaciones),
  Varianza = mi_varianza_pob(calificaciones)
)

# B) Distribución de Frecuencias
tabla_frecuencias <- table(calificaciones)

# C) Análisis de Texto
todos_los_tokens <- limpiar_tokenizar(comentarios)
tokens_validos <- todos_los_tokens[todos_los_tokens != ""] # Quitar espacios vacíos

# Top 5 palabras
top_palabras <- head(sort(table(tokens_validos), decreasing = TRUE), 5)

# Porcentaje Positivo/Negativo (Diccionario simple)
dicc_pos <- c("buen", "bueno", "excelente", "genial", "util", "recomiendo", "supero", "mejor", "completo")
dicc_neg <- c("malo", "regular", "basico", "peor", "demasiado", "no")

count_pos <- sum(tokens_validos %in% dicc_pos)
count_neg <- sum(tokens_validos %in% dicc_neg)
total_tok <- length(tokens_validos)

pct_pos <- (count_pos / total_tok) * 100
pct_neg <- (count_neg / total_tok) * 100

# D) Clasificación Automática (Regla: >=4 Pos, ==3 Neu, <=2 Neg)
clasificar <- function(nota) {
  if (nota >= 4) return("Positivo")
  if (nota == 3) return("Neutro")
  return("Negativo")
}
etiquetas <- sapply(calificaciones, clasificar)

# Crear Tabla Final
df_final <- data.frame(
  Nota = calificaciones,
  Comentario = comentarios,
  Clasificacion = etiquetas
)


# 4. IMPRESIÓN DE RESULTADOS


cat("RESULTADOS DEL ANÁLISIS EXPLORATORIO\n")


cat(" 1. Métricas Centrales y Dispersión \n")
print(unlist(resultado_estadistico))

cat("\n2. Distribución de Calificaciones \n")
print(tabla_frecuencias)

cat("\n 3. Top 5 Palabras Frecuentes \n")
print(top_palabras)

cat("\n 4. Análisis de Sentimiento (Léxico) \n")
cat("Porcentaje palabras Positivas:", round(pct_pos, 2), "%\n")
cat("Porcentaje palabras Negativas:", round(pct_neg, 2), "%\n")

cat("\n 5. Muestra de Clasificación Automática (Primeros 5) ---\n")
print(head(df_final, 5))

cat("\n 6. Recomendación Ejecutiva \n")
cat("El análisis revela una alta satisfacción general (Moda 5, Media 3.72), respaldada por el léxico predominante ('excelente', 'buen'). Sin embargo, la varianza de 1.75 señala polarización: un segmento minoritario percibe el curso como 'básico' o 'regular'. Para el negocio, esto implica que el contenido actual es un éxito para el público objetivo general, pero insuficiente para perfiles técnicos avanzados. RECOMENDACIÓN: Mantener la estructura actual para preservar la alta satisfacción base, pero desarrollar un módulo 'Anexo Avanzado' opcional con ejercicios complejos. Esto neutralizará las críticas negativas sobre profundidad sin aumentar la barrera de entrada para la mayoría, maximizando así la retención y el prestigio del curso.\n")


cat("FIN DEL PROCESO\n")