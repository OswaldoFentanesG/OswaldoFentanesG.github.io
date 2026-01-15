# ==============================================================================
# PROYECTO INTEGRAL: DATA SCIENCE CON R
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. INSTALACIÓN Y CARGA DE LIBRERÍAS
# ------------------------------------------------------------------------------
# Instalamos librerías si no las tienes (solo se ejecuta si faltan)
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(janitor)) install.packages("janitor")
if(!require(corrplot)) install.packages("corrplot")

library(tidyverse)
library(janitor)
library(corrplot)

# ------------------------------------------------------------------------------
# 2. CARGA DE DATASETS (Rutas ajustadas a tu usuario 'oswal')
# ------------------------------------------------------------------------------
# NOTA: read_csv puede tirar advertencias naranjas (warnings), es normal.
# Significa que hay datos sucios que vamos a limpiar más abajo.

path_airbnb  <- "C:/Users/oswal/Downloads/Airbnb_Open_Data (1).csv"
path_melb    <- "C:/Users/oswal/Downloads/melb_data (1).csv"
path_spotify <- "C:/Users/oswal/Downloads/spotify_data clean (1).csv"

airbnb  <- read_csv(path_airbnb, show_col_types = FALSE)
melb    <- read_csv(path_melb, show_col_types = FALSE)
spotify <- read_csv(path_spotify, show_col_types = FALSE)

print(">>> ¡Archivos cargados! Pasando a inspección...")

# ------------------------------------------------------------------------------
# 3. INSPECCIÓN Y DETECCIÓN (Valores nulos y duplicados)
# ------------------------------------------------------------------------------

audit_data <- function(df, name) {
  cat("\n--- REPORTE:", name, "---\n")
  cat("Filas:", nrow(df), "| Columnas:", ncol(df), "\n")
  cat("Duplicados:", sum(duplicated(df)), "\n")
  cat("Nulos (NA):", sum(is.na(df)), "\n")
}

audit_data(airbnb, "Airbnb")
audit_data(melb, "Melbourne")
audit_data(spotify, "Spotify")

# ------------------------------------------------------------------------------
# 4. LIMPIEZA Y TRANSFORMACIÓN 
# ------------------------------------------------------------------------------

# --- LIMPIEZA AIRBNB ---
airbnb_clean <- airbnb %>%
  clean_names() %>%
  distinct() %>%
  mutate(
    
    price = as.numeric(gsub("[\\$,]", "", price)),
    service_fee = as.numeric(gsub("[\\$,]", "", service_fee)),
  
    number_of_reviews = replace_na(number_of_reviews, 0)
  ) %>%
  drop_na(price)

# --- LIMPIEZA MELBOURNE ---
melb_clean <- melb %>%
  clean_names() %>%
  distinct() %>%
  mutate(
  
    date = as.Date(date, format = "%d/%m/%Y"), 
    
    building_area = ifelse(is.na(building_area), median(building_area, na.rm=TRUE), building_area)
  )

# --- LIMPIEZA SPOTIFY ---
spotify_clean <- spotify %>%
  clean_names() %>%
  distinct() %>%
  drop_na() 

print(">>> ¡Limpieza terminada sin errores!")

# ------------------------------------------------------------------------------
# 5. VISUALIZACIONES
# ------------------------------------------------------------------------------

# Gráfico 1: Boxplot Airbnb (Precio vs Tipo de Habitación)
g1 <- ggplot(airbnb_clean %>% filter(price < 1000), aes(x = room_type, y = price)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Precios Airbnb por Tipo", subtitle = "Filtrado < $1000")
print(g1)

# Gráfico 2: Scatterplot Melbourne (Precio vs Distancia)
g2 <- ggplot(melb_clean, aes(x = distance, y = price)) +
  geom_point(alpha = 0.3, color = "darkred") +
  theme_minimal() +
  labs(title = "Melbourne: Precio vs Distancia")
print(g2)

# Gráfico 3: Matriz de Correlación Spotify
# Tomamos solo columnas numéricas
spotify_nums <- spotify_clean %>% select(where(is.numeric))
M <- cor(spotify_nums)
corrplot(M, method="color", type="upper", tl.cex=0.6, title="Correlación Spotify", mar=c(0,0,2,0))

# ------------------------------------------------------------------------------
# 6. GUARDAR RESULTADOS
# ------------------------------------------------------------------------------

# Guardamos en tu carpeta de Descargas para que los encuentres rápido
write_csv(airbnb_clean, "C:/Users/oswal/Downloads/Airbnb_Cleaned_Final.csv")
write_csv(melb_clean, "C:/Users/oswal/Downloads/Melb_Cleaned_Final.csv")
write_csv(spotify_clean, "C:/Users/oswal/Downloads/Spotify_Cleaned_Final.csv")

print(">>> ¡PROYECTO COMPLETADO! Archivos guardados en tu carpeta Downloads.")
